#!/usr/bin/env python3
import json
import os
import re
import sys
import threading
import time
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import stanza


HOST = os.environ.get("SEMANTIC_UD_HOST", "127.0.0.1")
PORT = int(os.environ.get("SEMANTIC_UD_PORT", "4061"))
DEFAULT_LANG = os.environ.get("SEMANTIC_UD_LANG", "de")
RESOURCES_DIR = os.environ.get("SEMANTIC_UD_RESOURCES_DIR", os.environ.get("STANZA_RESOURCES_DIR", "")).strip()
AUTO_DOWNLOAD = os.environ.get("SEMANTIC_UD_AUTO_DOWNLOAD", "true").strip().lower() not in ("0", "false", "no")
# Languages to build at startup rather than on first request, so a missing model is a startup
# failure instead of a mid-corpus-run surprise. Empty string disables preloading.
# Treebank/encoder package for pos+depparse, empty meaning Stanza's default. Configurable because
# the default for German (`combined_charlm`, i.e. GSD+HDT) mis-parses spoken-register dialogue in ways
# `hdt_charlm` does not — see doc/parser-quality-plan.md step 2.
UD_PACKAGE = os.environ.get("SEMANTIC_UD_PACKAGE", "").strip()

PRELOAD_LANGS = [
    lang.strip().lower()
    for lang in os.environ.get("SEMANTIC_UD_PRELOAD", DEFAULT_LANG).split(",")
    if lang.strip()
]

_pipelines = {}
# Stanza pipelines are not safe to call concurrently, so the server is threaded for I/O but each
# pipeline is serialised behind its own lock: /health and short requests stay responsive during a
# long parse, and different languages still parse in parallel, without sharing pipeline state.
_pipeline_locks = {}
_registry_lock = threading.Lock()

ROLE_CONFIG = {
    "de": {
        "subject_exact": ("nsubj", "nsubj:pass", "csubj", "expl"),
        "subject_prefix": ("nsubj", "csubj"),
        "object_exact": ("obj", "iobj", "obl:arg", "xcomp", "ccomp"),
        "object_prefix": ("obj", "iobj"),
        "object_fallback_rel": ("obl", "nmod"),
        "object_fallback_cases": ("Acc", "Dat", "Gen"),
        "address_exact": ("vocative", "discourse"),
        "address_prefix": ("vocative",),
        "verb_root_preferred_upos": ("VERB", "AUX"),
        "verb_fallback_upos": ("VERB", "AUX"),
    },
    "en": {
        "subject_exact": ("nsubj", "nsubj:pass", "csubj", "expl"),
        "subject_prefix": ("nsubj", "csubj"),
        "object_exact": ("obj", "iobj", "obl:arg", "xcomp", "ccomp"),
        "object_prefix": ("obj", "iobj"),
        "object_fallback_rel": ("obl", "nmod"),
        "object_fallback_cases": ("Acc", "Dat", "Gen"),
        "address_exact": ("vocative", "discourse"),
        "address_prefix": ("vocative",),
        "verb_root_preferred_upos": ("VERB", "AUX"),
        "verb_fallback_upos": ("VERB", "AUX"),
    },
}

DEFAULT_ROLE_CONFIG = ROLE_CONFIG["de"]

ROLE_CONFIDENCE = {
    "subject": {"strong": 0.96, "medium": 0.82, "weak": 0.65},
    "verb": {"strong": 0.96, "medium": 0.82, "weak": 0.65},
    "object": {"strong": 0.95, "medium": 0.80, "weak": 0.60},
    "address": {"strong": 0.95, "medium": 0.80, "weak": 0.60},
    "predicate": {"strong": 0.95, "medium": 0.80, "weak": 0.60},
}

PLACEHOLDER_PATTERN = re.compile(r"\$[\w\-]+", re.UNICODE)
GREETING_HEADS = {"hallo", "hi", "hey", "hello", "moin", "servus", "guten"}


def placeholder_replacement(name: str, lang: str) -> str:
    base = str(name or "").strip().lstrip("$").lower()
    language = (lang or "de").strip().lower()
    if any(k in base for k in ("user", "person", "name", "kunde", "mensch", "nutzer")):
        return "User" if language.startswith("en") else "Benutzer"
    if any(k in base for k in ("agent", "speaker", "assistant")):
        return "Speaker" if language.startswith("en") else "Sprecher"
    if any(k in base for k in ("place", "location", "ort", "stadt")):
        return "place" if language.startswith("en") else "Ort"
    if any(k in base for k in ("obj", "thing", "item", "ding")):
        return "object" if language.startswith("en") else "Objekt"
    return "person" if language.startswith("en") else "Person"


def preprocess_text(text: str, lang: str):
    source = str(text or "")
    if not source:
        return source, []
    out = []
    index_map = []
    cursor = 0
    for m in PLACEHOLDER_PATTERN.finditer(source):
        if m.start() > cursor:
            chunk = source[cursor:m.start()]
            out.append(chunk)
            for i in range(len(chunk)):
                index_map.append(cursor + i)
        repl = placeholder_replacement(m.group(0), lang)
        out.append(repl)
        src_len = max(1, m.end() - m.start())
        for i, _ in enumerate(repl):
            # Map normalized replacement chars into the original placeholder span.
            mapped = m.start() + min(i, src_len - 1)
            index_map.append(mapped)
        cursor = m.end()
    if cursor < len(source):
        tail = source[cursor:]
        out.append(tail)
        for i in range(len(tail)):
            index_map.append(cursor + i)
    return "".join(out), index_map


def map_pos_to_original(pos: int, index_map, original_len: int):
    if pos is None or pos < 0:
        return -1
    if not index_map:
        return min(pos, max(0, original_len))
    if pos >= len(index_map):
        return max(0, original_len)
    return max(0, min(original_len, int(index_map[pos])))


def map_span_to_original(start: int, end: int, index_map, original_len: int):
    if start is None or end is None:
        return -1, -1
    if end <= start:
        return -1, -1
    if not index_map:
        return max(0, start), max(0, end)
    from_pos = map_pos_to_original(start, index_map, original_len)
    last_pos = map_pos_to_original(end - 1, index_map, original_len)
    to_pos = min(original_len, max(from_pos + 1, last_pos + 1))
    return from_pos, to_pos


def now_iso():
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def normalize_lang(lang: str) -> str:
    language = (lang or DEFAULT_LANG or "de").strip().lower()
    return language or "de"


def pipeline_lock(language: str) -> threading.Lock:
    with _registry_lock:
        lock = _pipeline_locks.get(language)
        if lock is None:
            lock = threading.Lock()
            _pipeline_locks[language] = lock
        return lock


def get_pipeline(lang: str):
    language = normalize_lang(lang)
    pipe = _pipelines.get(language)
    if pipe is not None:
        return pipe
    # Build under the registry lock so two concurrent first-requests for the same language do not
    # each pay for (and race on) constructing a pipeline.
    with _registry_lock:
        pipe = _pipelines.get(language)
        if pipe is not None:
            return pipe
        pipe = build_pipeline(language)
        _pipelines[language] = pipe
        return pipe


def build_pipeline(language: str):
    kwargs = {
        "lang": language,
        "processors": "tokenize,mwt,pos,lemma,depparse",
        "use_gpu": False,
        "tokenize_no_ssplit": False,
        "verbose": False,
        "download_method": None,
    }
    if RESOURCES_DIR:
        kwargs["dir"] = RESOURCES_DIR
    if UD_PACKAGE:
        # tokenize/mwt/lemma stay on the default: only tagging and parsing differ between treebanks.
        kwargs["package"] = {"pos": UD_PACKAGE, "depparse": UD_PACKAGE}
    try:
        pipe = stanza.Pipeline(**kwargs)
    except Exception:
        if not AUTO_DOWNLOAD:
            raise
        model_dir = RESOURCES_DIR if RESOURCES_DIR else str(Path.cwd() / "stanza_resources")
        Path(model_dir).mkdir(parents=True, exist_ok=True)
        stanza.download(language, processors="tokenize,mwt,pos,lemma,depparse", model_dir=model_dir, verbose=False)
        kwargs["dir"] = model_dir
        pipe = stanza.Pipeline(**kwargs)
    return pipe


def preload_pipelines():
    """Builds the configured pipelines before serving.

    With SEMANTIC_UD_AUTO_DOWNLOAD=false a missing model then fails at startup, which is what a
    corpus run wants: a clear error before the first sentence rather than an opaque HTTP 500 in the
    middle of a batch.
    """
    for language in PRELOAD_LANGS:
        started = time.monotonic()
        try:
            _pipelines[language] = build_pipeline(language)
        except Exception as exc:
            print(f"[semantic-ud] FATAL: cannot load model for '{language}': {exc}", file=sys.stderr)
            print("[semantic-ud] Set SEMANTIC_UD_RESOURCES_DIR to your stanza_resources directory, "
                  "or allow downloads with SEMANTIC_UD_AUTO_DOWNLOAD=true.", file=sys.stderr)
            raise SystemExit(1)
        print(f"[semantic-ud] loaded '{language}' in {time.monotonic() - started:.1f}s", flush=True)


def role_config(lang: str):
    language = (lang or DEFAULT_LANG or "de").strip().lower()
    return ROLE_CONFIG.get(language, DEFAULT_ROLE_CONFIG)


def has_rel(dep: str, exact=(), prefixes=()):
    if dep in exact:
        return True
    return any(dep.startswith(p) for p in prefixes)


def greeting_comma_id(words):
    ordered = sorted([w for w in words if word_id_value(w) is not None], key=lambda w: word_id_value(w))
    if not ordered:
        return None
    first = str(getattr(ordered[0], "text", "") or "").strip().lower()
    if first not in GREETING_HEADS:
        return None
    for w in ordered[1:7]:
        if str(getattr(w, "text", "") or "") == ",":
            return word_id_value(w)
    return None


def parse_case_set(word):
    feats = str(getattr(word, "feats", "") or "")
    if "Case=" not in feats:
        return set()
    out = set()
    for part in feats.split("|"):
        if part.startswith("Case="):
            values = part.split("=", 1)[1]
            for value in values.split(","):
                val = value.strip()
                if val:
                    out.add(val)
    return out


def select_subject(words, cfg):
    comma_id = greeting_comma_id(words)
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if has_rel(dep, cfg["subject_exact"], cfg["subject_prefix"]):
            wid = word_id_value(w)
            if comma_id is not None and wid is not None and wid < comma_id:
                continue
            return w
    return None


def select_verb(words, cfg):
    comma_id = greeting_comma_id(words)
    root = None
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if dep == "root":
            root = w
            break
    if root is None:
        # Prefer explicit auxiliaries/copulas before any broad fallback.
        for w in words:
            dep = str(getattr(w, "deprel", "") or "")
            if dep in ("cop", "aux", "aux:pass"):
                return w
        for w in words:
            if str(getattr(w, "upos", "") or "") in ("VERB", "AUX"):
                return w
        return None
    root_upos = str(getattr(root, "upos", "") or "")
    if root_upos in cfg["verb_root_preferred_upos"]:
        rid = word_id_value(root)
        if comma_id is not None and rid is not None and rid < comma_id:
            for w in words:
                wid = word_id_value(w)
                if wid is None or wid <= comma_id:
                    continue
                dep = str(getattr(w, "deprel", "") or "")
                upos = str(getattr(w, "upos", "") or "")
                if dep == "root" and upos in cfg["verb_root_preferred_upos"]:
                    return w
            for w in words:
                wid = word_id_value(w)
                if wid is None or wid <= comma_id:
                    continue
                if str(getattr(w, "upos", "") or "") in cfg["verb_fallback_upos"]:
                    return w
            return None
        return root
    # Copular clauses: root can be ADJ/NOUN and finite verb is attached as cop/aux.
    root_id = word_id_value(root)
    if root_id is not None:
        for w in words:
            dep = str(getattr(w, "deprel", "") or "")
            head = getattr(w, "head", None)
            if head == root_id and dep in ("cop", "aux", "aux:pass"):
                return w
    for w in words:
        wid = word_id_value(w)
        if comma_id is not None and wid is not None and wid < comma_id:
            continue
        if str(getattr(w, "upos", "") or "") in cfg["verb_fallback_upos"]:
            return w
    # If we cannot identify a verbal head (including cop/aux), skip verb instead of marking nouns/adjectives.
    return None


def select_object(words, cfg):
    comma_id = greeting_comma_id(words)
    for rel in cfg["object_exact"]:
        for w in words:
            dep = str(getattr(w, "deprel", "") or "")
            if dep == rel:
                wid = word_id_value(w)
                if comma_id is not None and wid is not None and wid < comma_id:
                    continue
                return w
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if has_rel(dep, (), cfg["object_prefix"]):
            wid = word_id_value(w)
            if comma_id is not None and wid is not None and wid < comma_id:
                continue
            return w
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if dep in cfg["object_fallback_rel"]:
            wid = word_id_value(w)
            if comma_id is not None and wid is not None and wid < comma_id:
                continue
            cases = parse_case_set(w)
            if not cfg["object_fallback_cases"] or any(c in cases for c in cfg["object_fallback_cases"]):
                return w
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if dep.startswith("obl"):
            wid = word_id_value(w)
            if comma_id is not None and wid is not None and wid < comma_id:
                continue
            return w
    return None


def select_address(words, cfg):
    comma_id = greeting_comma_id(words)
    if comma_id is not None:
        ordered = sorted([w for w in words if word_id_value(w) is not None], key=lambda w: word_id_value(w))
        for w in ordered:
            wid = word_id_value(w)
            if wid is None or wid <= 1 or wid >= comma_id:
                continue
            upos = str(getattr(w, "upos", "") or "")
            dep = str(getattr(w, "deprel", "") or "")
            if upos in ("PRON", "PROPN", "NOUN") and dep not in ("punct",):
                return w
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if has_rel(dep, cfg.get("address_exact", ()), cfg.get("address_prefix", ())):
            return w
    # Fallback for vocative-like fragments often tagged as "nsubj/appos" in imperative insults:
    # "... , du Scheißkuh!"
    sorted_words = sorted(
        [w for w in words if word_id_value(w) is not None],
        key=lambda w: word_id_value(w),
    )
    by_id = {word_id_value(w): w for w in sorted_words}
    for w in sorted_words:
        wid = word_id_value(w)
        if wid is None or wid <= 1:
            continue
        # Never treat the word right after a *greeting* comma as the addressee. In
        # "Hey, Ich habe eine Aufgabe für Dich." that word is the subject, and this fallback used to
        # report `Ich` as both subject and address. With a greeting comma the addressee, if any, sits
        # *before* it (handled by the first branch, "Hallo $user, …"); a greeting with no name after it
        # simply has no addressee, and reporting none beats reporting the wrong one.
        if comma_id is not None and wid == comma_id + 1:
            continue
        prev = by_id.get(wid - 1)
        if prev is None or str(getattr(prev, "upos", "") or "") != "PUNCT":
            continue
        if str(getattr(prev, "text", "") or "") != ",":
            continue
        upos = str(getattr(w, "upos", "") or "")
        dep = str(getattr(w, "deprel", "") or "")
        if upos not in ("PRON", "PROPN", "NOUN"):
            continue
        if dep not in ("nsubj", "appos", "dislocated", "parataxis"):
            continue
        return w
    return None


def select_predicate(words):
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        if dep != "root":
            continue
        if not has_copula_child(words, w):
            continue
        upos = str(getattr(w, "upos", "") or "")
        if upos in ("NOUN", "PROPN", "ADJ"):
            return w
    return None


def select_role_modifiers(words, role_word):
    if role_word is None:
        return []
    role_id = word_id_value(role_word)
    if role_id is None:
        return []
    out = []
    for w in words:
        head = getattr(w, "head", None)
        if head != role_id:
            continue
        dep = str(getattr(w, "deprel", "") or "")
        upos = str(getattr(w, "upos", "") or "")
        if dep.startswith("amod"):
            out.append((w, "adjective"))
            continue
        if dep == "advmod" and upos in ("ADV", "ADJ"):
            out.append((w, "adverb"))
    return out


def has_copula_child(words, head_word):
    head_id = word_id_value(head_word)
    if head_id is None:
        return False
    for w in words:
        if getattr(w, "head", None) != head_id:
            continue
        dep = str(getattr(w, "deprel", "") or "")
        if dep in ("cop", "aux", "aux:pass"):
            return True
    return False


def add_predicative_modifiers(words, role_word, modifiers):
    if role_word is None:
        return modifiers
    role_id = word_id_value(role_word)
    if role_id is None:
        return modifiers
    out = list(modifiers)
    seen = {(word_id_value(w), kind) for (w, kind) in out}
    for w in words:
        dep = str(getattr(w, "deprel", "") or "")
        upos = str(getattr(w, "upos", "") or "")
        if dep != "root" or upos != "ADJ":
            continue
        # Copular predicate adjective (e.g. "Gesicht ist billig").
        if not has_copula_child(words, w):
            continue
        # Ensure subject links to the same predicate head.
        if getattr(role_word, "head", None) != word_id_value(w):
            continue
        key = (word_id_value(w), "adjective")
        if key not in seen:
            seen.add(key)
            out.append((w, "adjective"))
        pred_id = word_id_value(w)
        for child in words:
            if getattr(child, "head", None) != pred_id:
                continue
            cdep = str(getattr(child, "deprel", "") or "")
            cupos = str(getattr(child, "upos", "") or "")
            if cdep == "advmod" and cupos in ("ADV", "ADJ"):
                ckey = (word_id_value(child), "adverb")
                if ckey not in seen:
                    seen.add(ckey)
                    out.append((child, "adverb"))
    return out


def word_by_id(words, target_id):
    if target_id is None:
        return None
    for w in words:
        if word_id_value(w) == target_id:
            return w
    return None


def select_address_head(words, addr_word):
    if addr_word is None:
        return None
    addr_id = word_id_value(addr_word)
    if addr_id is None:
        return None

    # Most common in insults/vocatives: pronoun "du" points to noun head.
    direct_head = word_by_id(words, getattr(addr_word, "head", None))
    if direct_head is not None and str(getattr(direct_head, "upos", "") or "") in ("NOUN", "PROPN"):
        return direct_head

    # Alternative: noun child of pronoun in flattened/appositional structures.
    for w in words:
        if getattr(w, "head", None) != addr_id:
            continue
        if str(getattr(w, "upos", "") or "") in ("NOUN", "PROPN"):
            return w

    # Fallback: nearest noun to the right before punctuation boundary.
    ordered = sorted([w for w in words if word_id_value(w) is not None], key=lambda w: word_id_value(w))
    idx = -1
    for i, w in enumerate(ordered):
        if word_id_value(w) == addr_id:
            idx = i
            break
    if idx < 0:
        return None
    for w in ordered[idx + 1: idx + 7]:
        upos = str(getattr(w, "upos", "") or "")
        text = str(getattr(w, "text", "") or "")
        if upos == "PUNCT" and text in (".", "!", "?", ";", ":"):
            break
        if upos in ("NOUN", "PROPN"):
            return w
    return None


def build_address_phrase(sentence, words, addr_word, base_offset, index_map, original_text_len):
    if addr_word is None:
        return None
    anchor = word_span(sentence, addr_word, base_offset, index_map, original_text_len)
    if anchor is None:
        return None
    head_word = select_address_head(words, addr_word)
    if head_word is None:
        return None
    head = word_span(sentence, head_word, base_offset, index_map, original_text_len)
    if head is None:
        return None
    modifiers = modifier_spans(sentence, words, head_word, "address", base_offset, index_map, original_text_len)
    return {
        "anchor": anchor,
        "head": head,
        "modifiers": modifiers,
    }


def modifier_spans(sentence, words, role_word, role_name, base_offset, index_map, original_text_len):
    spans = []
    seen = set()
    role_modifiers = select_role_modifiers(words, role_word)
    if role_name == "subject":
        role_modifiers = add_predicative_modifiers(words, role_word, role_modifiers)
    for modifier_word, pos_kind in role_modifiers:
        span = word_span(sentence, modifier_word, base_offset, index_map, original_text_len)
        if span is None:
            continue
        key = (int(span.get("from", -1)), int(span.get("to", -1)), str(pos_kind))
        if key in seen:
            continue
        seen.add(key)
        mod = dict(span)
        mod["role"] = role_name
        mod["pos"] = pos_kind
        mod["confidence"] = 0.78
        spans.append(mod)
    return spans


def word_token_char_span(sentence, word):
    if word is None:
        return None
    word_id = word_id_value(word)
    if word_id is None:
        return None
    for tok in sentence.tokens:
        for tw in tok.words:
            if word_id_value(tw) == word_id:
                start = int(getattr(tok, "start_char", -1))
                end = int(getattr(tok, "end_char", -1))
                if start >= 0 and end > start:
                    return start, end
    return None


def is_descendant_of(words, node_word, ancestor_word, max_depth=8):
    if node_word is None or ancestor_word is None:
        return False
    anc_id = word_id_value(ancestor_word)
    if anc_id is None:
        return False
    node = node_word
    depth = 0
    while node is not None and depth < max_depth:
        nid = word_id_value(node)
        if nid == anc_id:
            return True
        node = word_by_id(words, getattr(node, "head", None))
        depth += 1
    return False


def role_for_comparison_marker(words, marker_word, role_heads):
    for role in ("predicate", "object", "subject", "address"):
        head_word = role_heads.get(role)
        if head_word is None:
            continue
        if is_descendant_of(words, marker_word, head_word):
            return role
    return None


def comparison_phrase_span(sentence, words, marker_word):
    marker_span = word_token_char_span(sentence, marker_word)
    if marker_span is None:
        return None
    start, end = marker_span
    marker_id = word_id_value(marker_word)
    if marker_id is None:
        return None
    ordered = sorted([w for w in words if word_id_value(w) is not None], key=lambda w: word_id_value(w))
    by_id = {word_id_value(w): w for w in ordered}
    prev = by_id.get(marker_id - 1)
    if prev is not None and str(getattr(prev, "upos", "") or "") in ("ADJ", "ADV"):
        prev_span = word_token_char_span(sentence, prev)
        if prev_span is not None:
            start = min(start, prev_span[0])
    if prev is not None and str(getattr(prev, "lemma", "") or "").lower() in ("so", "as"):
        prev2 = by_id.get(marker_id - 2)
        prev2_span = word_token_char_span(sentence, prev2) if prev2 is not None else None
        if prev2_span is not None and str(getattr(prev2, "upos", "") or "") in ("ADJ", "ADV"):
            start = min(start, prev2_span[0])
        prev_span = word_token_char_span(sentence, prev)
        if prev_span is not None:
            start = min(start, prev_span[0])

    after = marker_id + 1
    while True:
        w = by_id.get(after)
        if w is None:
            break
        text = str(getattr(w, "text", "") or "")
        upos = str(getattr(w, "upos", "") or "")
        if upos == "PUNCT" and text in (".", "!", "?", ";", ":", ","):
            break
        span = word_token_char_span(sentence, w)
        if span is not None:
            end = max(end, span[1])
        after += 1
    return start, end


def comparison_modifiers(sentence, words, role_heads, base_offset, index_map, original_text_len):
    markers = {"als", "wie", "than", "as", "like"}
    out = []
    seen = set()
    for w in words:
        lemma = str(getattr(w, "lemma", "") or "").lower()
        text = str(getattr(w, "text", "") or "").lower()
        if lemma not in markers and text not in markers:
            continue
        role = role_for_comparison_marker(words, w, role_heads)
        if not role:
            continue
        span = comparison_phrase_span(sentence, words, w)
        if span is None:
            continue
        orig_start, orig_end = map_span_to_original(span[0], span[1], index_map, original_text_len)
        if orig_start < 0 or orig_end <= orig_start:
            continue
        key = (role, orig_start, orig_end)
        if key in seen:
            continue
        seen.add(key)
        out.append({
            "text": "",
            "from": base_offset + orig_start,
            "to": base_offset + orig_end,
            "confidence": 0.74,
            "role": role,
            "pos": "comparison",
        })
    return out


def role_strength(role, word, cfg):
    if word is None:
        return "weak"
    dep = str(getattr(word, "deprel", "") or "")
    upos = str(getattr(word, "upos", "") or "")
    if role == "subject":
        if has_rel(dep, cfg["subject_exact"], cfg["subject_prefix"]):
            return "strong"
        return "weak"
    if role == "verb":
        if dep == "root" and upos in cfg["verb_root_preferred_upos"]:
            return "strong"
        if upos in cfg["verb_fallback_upos"]:
            return "medium"
        return "weak"
    if role == "object":
        if dep in cfg["object_exact"] or has_rel(dep, (), cfg["object_prefix"]):
            return "strong"
        if dep in cfg["object_fallback_rel"] or dep.startswith("obl"):
            return "medium"
        return "weak"
    if role == "address":
        if has_rel(dep, cfg.get("address_exact", ()), cfg.get("address_prefix", ())):
            return "strong"
        if dep == "discourse":
            return "medium"
        return "weak"
    if role == "predicate":
        if dep == "root":
            return "strong"
        return "weak"
    return "weak"


def role_confidence(role, strength):
    table = ROLE_CONFIDENCE.get(role, {})
    return float(table.get(strength, table.get("weak", 0.5)))


def with_confidence(span, role, strength):
    if span is None:
        return None
    out = dict(span)
    out["confidence"] = role_confidence(role, strength)
    return out


def word_id_value(word):
    if word is None:
        return None
    try:
        return int(getattr(word, "id", 0) or 0)
    except Exception:
        return None


def sentence_debug_payload(sentence, cfg, subj_word, verb_word, obj_word, addr_word, pred_word):
    words = []
    for w in sentence.words or []:
        words.append(
            {
                "id": word_id_value(w),
                "text": str(getattr(w, "text", "") or ""),
                "lemma": str(getattr(w, "lemma", "") or ""),
                "upos": str(getattr(w, "upos", "") or ""),
                "xpos": str(getattr(w, "xpos", "") or ""),
                "deprel": str(getattr(w, "deprel", "") or ""),
                "head": getattr(w, "head", None),
                "feats": str(getattr(w, "feats", "") or ""),
            }
        )

    return {
        "sentenceText": str(getattr(sentence, "text", "") or ""),
        "selected": {
            "subject": {
                "id": word_id_value(subj_word),
                "text": str(getattr(subj_word, "text", "") or ""),
                "deprel": str(getattr(subj_word, "deprel", "") or ""),
                "upos": str(getattr(subj_word, "upos", "") or ""),
                "strength": role_strength("subject", subj_word, cfg),
            },
            "verb": {
                "id": word_id_value(verb_word),
                "text": str(getattr(verb_word, "text", "") or ""),
                "deprel": str(getattr(verb_word, "deprel", "") or ""),
                "upos": str(getattr(verb_word, "upos", "") or ""),
                "strength": role_strength("verb", verb_word, cfg),
            },
            "object": {
                "id": word_id_value(obj_word),
                "text": str(getattr(obj_word, "text", "") or ""),
                "deprel": str(getattr(obj_word, "deprel", "") or ""),
                "upos": str(getattr(obj_word, "upos", "") or ""),
                "strength": role_strength("object", obj_word, cfg),
            },
            "address": {
                "id": word_id_value(addr_word),
                "text": str(getattr(addr_word, "text", "") or ""),
                "deprel": str(getattr(addr_word, "deprel", "") or ""),
                "upos": str(getattr(addr_word, "upos", "") or ""),
                "strength": role_strength("address", addr_word, cfg),
            },
            "predicate": {
                "id": word_id_value(pred_word),
                "text": str(getattr(pred_word, "text", "") or ""),
                "deprel": str(getattr(pred_word, "deprel", "") or ""),
                "upos": str(getattr(pred_word, "upos", "") or ""),
                "strength": role_strength("predicate", pred_word, cfg),
            },
        },
        "config": {
            "subject_exact": list(cfg.get("subject_exact", ())),
            "subject_prefix": list(cfg.get("subject_prefix", ())),
            "object_exact": list(cfg.get("object_exact", ())),
            "object_prefix": list(cfg.get("object_prefix", ())),
            "object_fallback_rel": list(cfg.get("object_fallback_rel", ())),
            "object_fallback_cases": list(cfg.get("object_fallback_cases", ())),
            "address_exact": list(cfg.get("address_exact", ())),
            "address_prefix": list(cfg.get("address_prefix", ())),
            "verb_root_preferred_upos": list(cfg.get("verb_root_preferred_upos", ())),
            "verb_fallback_upos": list(cfg.get("verb_fallback_upos", ())),
        },
        "words": words,
    }


# ---------------------------------------------------------------------------
# Clause segmentation, multiple objects, phrase spans  (plan Phase 1.1-1.3)
#
# Motivation, both verified against the German model before writing any of this:
#
#   "Ich gebe dem Kind den roten Ball."  ->  the dative indirect object is `obl:arg`, NOT `iobj`,
#   and select_object() scans object_exact in order, matches `obj` (Ball) first and returns — so
#   `dem Kind` was never reported at all. Spans were also head-token-only: `Ball`, not
#   `den roten Ball`.
#
#   "Lass mich einen Vorschlag machen wie wir zusammen den Nachmittag gestalten."  ->  root is
#   `machen`, and `gestalten` hangs off it as `ccomp` carrying its own nsubj `wir`. With one flat
#   role set per sentence, `wir` (subordinate) was paired with `machen` (main). Roles have to be
#   resolved per clause, otherwise there is no constituent for a behavior command to anchor to.
#
# The flat `basic` block is deliberately left untouched: `clauses` is added alongside it, so v2
# consumers keep working while v3 consumers get the fine-grained view.
# ---------------------------------------------------------------------------

# Relations that can introduce a clause. Prefix-matched, so acl:relcl and csubj:pass-style
# language-specific subtypes are covered.
#
# `csubj` earns its place from the corpus: "Schön dass Du da bist." parses with `Schön` as root and
# `bist` as its *clausal subject*. Without csubj here, `Du`/`bist` leaked into the main clause — the
# same cross-clause mixing this segmentation exists to prevent — and `Schön` got no role, so no
# anchor slot existed where the author had in fact placed a command.
CLAUSE_DEPREL_PREFIXES = ("conj", "advcl", "ccomp", "xcomp", "acl", "parataxis", "csubj")

# Verb dependents that count as objects, and the kind we report for each.
OBJECT_DEPRELS = ("obj", "iobj", "obl:arg", "obl", "ccomp", "xcomp")


def children_map(words):
    """head id -> [words], for subtree walks."""
    out = {}
    for w in words:
        head = getattr(w, "head", None)
        if head is None:
            continue
        out.setdefault(int(head), []).append(w)
    return out


def subtree_ids(words, head_word, allowed_ids=None):
    """Ids of head_word and all its descendants, optionally restricted to allowed_ids."""
    head_id = word_id_value(head_word)
    if head_id is None:
        return set()
    kids = children_map(words)
    collected = set()
    stack = [head_id]
    while stack:
        current = stack.pop()
        if current in collected:
            continue
        if allowed_ids is not None and current not in allowed_ids:
            continue
        collected.add(current)
        for child in kids.get(current, []):
            child_id = word_id_value(child)
            if child_id is not None and child_id not in collected:
                stack.append(child_id)
    return collected


def has_child_deprel(words, head_word, deprels):
    head_id = word_id_value(head_word)
    if head_id is None:
        return False
    for w in words:
        if getattr(w, "head", None) == head_id:
            dep = str(getattr(w, "deprel", "") or "")
            if has_rel(dep, tuple(deprels), tuple(deprels)):
                return True
    return False


def is_clause_root(words, word):
    """Whether a word heads its own clause.

    A bare `conj` or `xcomp` is not enough: German coordinates nouns with `conj` too
    ("Äpfel und Birnen"), and that must not become a clause. Require a verbal head, or a copula, or
    an own subject.
    """
    dep = str(getattr(word, "deprel", "") or "")
    if dep == "root":
        return True
    if not any(dep.startswith(prefix) for prefix in CLAUSE_DEPREL_PREFIXES):
        return False
    upos = str(getattr(word, "upos", "") or "")
    if upos in ("VERB", "AUX"):
        return True
    if has_child_deprel(words, word, ("cop",)):
        return True
    return has_child_deprel(words, word, ("nsubj", "csubj"))


def clause_type_of(word):
    dep = str(getattr(word, "deprel", "") or "")
    if dep == "root":
        return "main"
    if dep.startswith("acl"):
        return "relative"
    if dep.startswith("conj"):
        return "coordinate"
    if dep.startswith("parataxis"):
        return "parataxis"
    return "subordinate"


def segment_clauses(words):
    """Partitions a sentence's words into clauses.

    Every word belongs to its nearest clause-root ancestor, so a subordinate clause's subject stays
    out of the main clause. Returns [{'root': word, 'ids': set(int)}] in surface order.
    """
    roots = [w for w in words if is_clause_root(words, w)]
    if not roots:
        return []
    root_ids = {word_id_value(w) for w in roots if word_id_value(w) is not None}
    by_id = {word_id_value(w): w for w in words if word_id_value(w) is not None}

    owner = {}
    for w in words:
        wid = word_id_value(w)
        if wid is None:
            continue
        node = w
        depth = 0
        # Walk up to the first clause root at or above this word.
        while node is not None and depth < 24:
            nid = word_id_value(node)
            if nid in root_ids:
                owner[wid] = nid
                break
            node = by_id.get(getattr(node, "head", None))
            depth += 1

    clauses = []
    for root in sorted(roots, key=lambda w: word_id_value(w) or 0):
        rid = word_id_value(root)
        ids = {wid for wid, oid in owner.items() if oid == rid}
        ids.add(rid)
        clauses.append({"root": root, "ids": ids})
    return clauses


def clause_verb(words, clause_root, clause_ids, cfg):
    """The finite/inflected verb of a clause.

    Within a clause there is no `root` deprel to rely on, and a copular clause's root is the
    predicate noun/adjective with the verb attached as `cop`.
    """
    upos = str(getattr(clause_root, "upos", "") or "")
    if upos in ("VERB", "AUX"):
        return clause_root
    root_id = word_id_value(clause_root)
    for w in words:
        if word_id_value(w) in clause_ids and getattr(w, "head", None) == root_id:
            if str(getattr(w, "deprel", "") or "") in ("cop", "aux", "aux:pass"):
                return w
    for w in words:
        if word_id_value(w) in clause_ids and str(getattr(w, "upos", "") or "") in cfg["verb_fallback_upos"]:
            return w
    return None


def case_child_text(words, head_word, clause_ids):
    """Text of a `case` dependent (the preposition), if any."""
    head_id = word_id_value(head_word)
    for w in words:
        if getattr(w, "head", None) != head_id:
            continue
        if word_id_value(w) not in clause_ids:
            continue
        if str(getattr(w, "deprel", "") or "") == "case":
            return str(getattr(w, "text", "") or "")
    return None


def np_case_set(words, head_word, clause_ids=None):
    """Case features of a noun phrase, read from its head *and its case-marking dependents*.

    German marks case across the whole NP, and treebanks disagree about where it ends up: for
    "Ich gebe dem Kind …" the combined model puts `Case=Dat` on both `dem` and `Kind`, while HDT puts
    it only on the determiner `dem` and leaves the noun with just Gender/Number. Reading the head
    word alone therefore loses the case on some treebanks — which is what made the dative object
    degrade to `oblique` under HDT.
    """
    cases = set(parse_case_set(head_word))
    head_id = word_id_value(head_word)
    if head_id is None:
        return cases
    for w in words:
        if getattr(w, "head", None) != head_id:
            continue
        if clause_ids is not None and word_id_value(w) not in clause_ids:
            continue
        if str(getattr(w, "deprel", "") or "") in ("det", "det:poss", "amod", "nummod"):
            cases |= parse_case_set(w)
    return cases


def object_kind(dep, cases, preposition):
    """Maps a verb dependent to an object kind, treebank-independently.

    Morphological case is the primary signal for the direct/indirect distinction in German —
    accusative is a direct object, dative an indirect one — because the *subtype label* is not
    portable: the dative object is `obl:arg` here, and a treebank that spells it differently, or omits
    the feature from the head noun, must still come out as `indirect`. The relation is used only for
    what case cannot express (clausal complements) and as the fallback for languages that do not mark
    case at all, such as English.
    """
    if dep in ("ccomp", "xcomp"):
        return "clausal"
    if preposition:
        # A `case` dependent (a preposition) makes it prepositional regardless of the noun's case.
        return "prepositional"
    if dep == "iobj":
        return "indirect"
    if "Dat" in cases:
        return "indirect"
    if dep == "obj":
        return "direct"
    if "Acc" in cases:
        return "direct"
    return "oblique"


def clause_objects(words, clause_ids, verb_word):
    """All object-ish dependents of the clause verb, in surface order.

    Dependents of a *noun* are excluded by requiring the head to be the clause verb: in
    "Ich habe eine Aufgabe für Dich", `Dich` is an `nmod` of `Aufgabe`, not an argument of `habe`.
    """
    if verb_word is None:
        return []
    verb_id = word_id_value(verb_word)
    found = []
    for w in words:
        wid = word_id_value(w)
        if wid is None or wid not in clause_ids:
            continue
        if getattr(w, "head", None) != verb_id:
            continue
        dep = str(getattr(w, "deprel", "") or "")
        if not has_rel(dep, OBJECT_DEPRELS, ("obj", "iobj", "obl")):
            continue
        if dep in ("ccomp", "xcomp") and is_clause_root(words, w):
            # Already reported as a clause of its own; listing it as an object too would double count.
            continue
        cases = np_case_set(words, w, clause_ids)
        preposition = case_child_text(words, w, clause_ids)
        found.append({
            "word": w,
            "kind": object_kind(dep, cases, preposition),
            "deprel": dep,
            "case": sorted(cases)[0] if cases else None,
            "preposition": preposition,
        })
    return sorted(found, key=lambda entry: word_id_value(entry["word"]) or 0)


def chars_to_span(start, end, original_text, index_map, base_offset):
    """Turns a clean-text char range into a span dict with original-text offsets and surface text."""
    if start is None or end is None or end <= start:
        return None
    orig_start, orig_end = map_span_to_original(start, end, index_map, len(original_text))
    if orig_start < 0 or orig_end <= orig_start:
        orig_start, orig_end = start, end
    return {
        "text": original_text[orig_start:orig_end],
        "from": base_offset + orig_start,
        "to": base_offset + orig_end,
    }


def phrase_span(sentence, words, head_word, clause_ids, original_text, index_map, base_offset):
    """Full phrase span of a role: the head's subtree, clipped to its clause, trailing punctuation
    trimmed. `den roten Ball`, where the head span alone gives `Ball`."""
    ids = subtree_ids(words, head_word, clause_ids)
    if not ids:
        return None
    by_id = {word_id_value(w): w for w in words if word_id_value(w) is not None}
    # Trim edge punctuation so a phrase never swallows a comma or full stop.
    ordered = sorted(ids)
    while ordered and str(getattr(by_id.get(ordered[-1]), "upos", "") or "") == "PUNCT":
        ordered.pop()
    while ordered and str(getattr(by_id.get(ordered[0]), "upos", "") or "") == "PUNCT":
        ordered.pop(0)
    if not ordered:
        return None
    start = None
    end = None
    for wid in ordered:
        span = word_token_char_span(sentence, by_id.get(wid))
        if span is None:
            continue
        start = span[0] if start is None else min(start, span[0])
        end = span[1] if end is None else max(end, span[1])
    return chars_to_span(start, end, original_text, index_map, base_offset)


def clause_char_span(sentence, words, clause_ids, original_text, index_map, base_offset):
    """Char span of a clause, edge punctuation trimmed.

    Trimming matters: the sentence-final full stop attaches to the main clause's verb, so without it
    a main clause containing a subordinate clause reports a span stretching to the end of the
    sentence. Note that an outer clause's span still legitimately *encloses* an embedded one — that
    is the structure, not an error — which is why anchor slots are derived from token positions
    rather than from these spans.
    """
    by_id = {word_id_value(w): w for w in words if word_id_value(w) is not None}
    ordered = sorted(clause_ids)
    while ordered and str(getattr(by_id.get(ordered[-1]), "upos", "") or "") == "PUNCT":
        ordered.pop()
    while ordered and str(getattr(by_id.get(ordered[0]), "upos", "") or "") == "PUNCT":
        ordered.pop(0)
    start = None
    end = None
    for wid in ordered:
        span = word_token_char_span(sentence, by_id.get(wid))
        if span is None:
            continue
        start = span[0] if start is None else min(start, span[0])
        end = span[1] if end is None else max(end, span[1])
    return chars_to_span(start, end, original_text, index_map, base_offset)


def role_entry(sentence, words, head_word, clause_ids, original_text, index_map, base_offset,
               role, cfg):
    """A role as {head, phrase, confidence}: the head token span and the full phrase span."""
    if head_word is None:
        return None
    head = word_span(sentence, head_word, base_offset, index_map, len(original_text))
    if head is None:
        return None
    entry = {"head": head}
    phrase = phrase_span(sentence, words, head_word, clause_ids, original_text, index_map, base_offset)
    if phrase is not None:
        entry["phrase"] = phrase
    entry["confidence"] = role_confidence(role, role_strength(role, head_word, cfg))
    return entry


def build_clauses(sentence, words, cfg, original_text, index_map, base_offset):
    """Per-clause roles with head and phrase spans, plus all objects. Plan Phase 1.1-1.3."""
    out = []
    for index, clause in enumerate(segment_clauses(words)):
        clause_ids = clause["ids"]
        clause_root = clause["root"]
        clause_words = [w for w in words if word_id_value(w) in clause_ids]

        verb_word = clause_verb(words, clause_root, clause_ids, cfg)
        subject_word = select_subject(clause_words, cfg)
        predicate_word = select_predicate(clause_words)
        address_word = select_address(clause_words, cfg)

        # Verbless predicative clause: "Schön" in "Schön dass Du da bist.", where the copula is
        # absent and the clausal subject carries the verb. Without this the clause has no role at
        # all, so no anchor slot exists next to it — and this register ("Schön …", "Toll …",
        # "Klasse!") is exactly where authors do place commands.
        if verb_word is None and predicate_word is None:
            if str(getattr(clause_root, "upos", "") or "") in ("ADJ", "NOUN", "PROPN", "ADV"):
                predicate_word = clause_root

        roles = {}
        for role, head_word in (("subject", subject_word),
                                ("verb", verb_word),
                                ("predicate", predicate_word),
                                ("address", address_word)):
            entry = role_entry(sentence, words, head_word, clause_ids, original_text, index_map,
                               base_offset, role, cfg)
            if entry is not None:
                roles[role] = entry

        objects = []
        for found in clause_objects(words, clause_ids, verb_word):
            entry = role_entry(sentence, words, found["word"], clause_ids, original_text, index_map,
                               base_offset, "object", cfg)
            if entry is None:
                continue
            entry["kind"] = found["kind"]
            entry["deprel"] = found["deprel"]
            if found["case"]:
                entry["case"] = found["case"]
            if found["preposition"]:
                entry["preposition"] = found["preposition"]
            objects.append(entry)

        span = clause_char_span(sentence, words, clause_ids, original_text, index_map, base_offset)
        clause_json = {
            "id": f"c{index}",
            "type": clause_type_of(clause_root),
            "roles": roles,
            "objects": objects,
        }
        if span is not None:
            clause_json["from"] = span["from"]
            clause_json["to"] = span["to"]
            clause_json["text"] = span["text"]
        out.append(clause_json)
    return out


# ---------------------------------------------------------------------------
# Anchor-slot inventory  (plan Phase 1.4)
#
# The label space the placement model predicts over. A slot is a *boundary*, named structurally
# ("before the direct object's phrase in clause c0") rather than numerically, because a structural
# name survives re-wording where a character offset does not — that is what makes ~26 authored
# examples generalisable at all.
#
# Slots carry `from`/`to` set to the same offset: they are positions, not spans. Encoding them that
# way means the server's generic span remapper rewrites them to script coordinates with no
# per-field code, and the Java side then adds the token index that makes a slot directly comparable
# with an authored command's gap index.
# ---------------------------------------------------------------------------

def anchor(slot, clause_id, at, role=None, kind=None):
    entry = {"slot": slot, "clauseId": clause_id, "from": at, "to": at}
    if role:
        entry["role"] = role
    if kind:
        entry["kind"] = kind
    return entry


def build_anchors(clauses, sentence_from, sentence_to, punct_from):
    """Candidate anchor slots for one sentence, in surface order.

    Derived from clause and phrase boundaries rather than from the clause char spans, since an outer
    clause's span encloses an embedded one and would give ambiguous boundaries.
    """
    out = []
    out.append(anchor("utterance-initial", None, sentence_from))

    for index, clause in enumerate(clauses):
        clause_id = clause.get("id")
        if index > 0 and clause.get("from") is not None:
            out.append(anchor("clause-initial", clause_id, clause["from"]))

        for role in ("subject", "verb", "predicate", "address"):
            entry = (clause.get("roles") or {}).get(role)
            if not entry:
                continue
            # The verb heads its clause, so its subtree phrase is the entire clause and
            # before/after-verb would degenerate to the clause bounds. Anchor the verb on its head
            # token; nominal roles anchor on the phrase, which is the constituent a command attaches
            # to ("den roten Ball", not "Ball").
            span = entry.get("head") if role == "verb" else (entry.get("phrase") or entry.get("head"))
            if not span:
                continue
            # An address is conventionally followed, not preceded, by a behavior command
            # ("Hallo $user, [emotion] …"), so only its trailing boundary is offered.
            if role != "address":
                out.append(anchor(f"before-{role}", clause_id, span["from"], role=role))
            out.append(anchor(f"after-{role}", clause_id, span["to"], role=role))

        for obj in clause.get("objects") or []:
            span = obj.get("phrase") or obj.get("head")
            if not span:
                continue
            kind = obj.get("kind")
            out.append(anchor("before-object", clause_id, span["from"], role="object", kind=kind))
            out.append(anchor("after-object", clause_id, span["to"], role="object", kind=kind))

    if punct_from is not None:
        out.append(anchor("before-final-punct", None, punct_from))
    out.append(anchor("utterance-final", None, sentence_to))

    # Several roles can share a boundary (a one-word clause's subject start is also the clause
    # start). Keep the first label offered at each offset and drop later duplicates, so the label
    # space stays a set of distinct positions.
    deduped = []
    seen = set()
    for entry in sorted(out, key=lambda e: (e["from"], e["slot"])):
        key = (entry["from"], entry["slot"], entry.get("role"), entry.get("kind"))
        if key in seen:
            continue
        seen.add(key)
        deduped.append(entry)
    return deduped


def sentence_offset(text, sentence_text, used_pos):
    if not sentence_text:
        return -1
    idx = text.find(sentence_text, used_pos)
    if idx >= 0:
        return idx
    return text.find(sentence_text)


def normalized_sentence_text(sentence, fallback_text):
    value = str(getattr(sentence, "text", "") or "").strip()
    if value:
        return value
    tokens = [str(getattr(tok, "text", "") or "").strip() for tok in (sentence.tokens or [])]
    compact = " ".join([t for t in tokens if t])
    return compact if compact else fallback_text.strip()


def pick_line(line_value, idx):
    try:
        return int(line_value) + idx
    except Exception:
        return idx + 1


def pick_base_offset(base_offset_value, sentence_begin):
    try:
        base = int(base_offset_value or 0)
    except Exception:
        base = 0
    if sentence_begin >= 0:
        return base + sentence_begin
    return base


def build_annotation(sentence, idx, lang, base_text, original_text, index_map, used_pos, line, speaker, base_offset, include_debug=False):
    cfg = role_config(lang)
    sentence_text = normalized_sentence_text(sentence, base_text)
    begin = sentence_offset(base_text, sentence_text, used_pos)
    orig_begin = map_pos_to_original(begin, index_map, len(original_text)) if begin >= 0 else begin
    abs_offset = pick_base_offset(base_offset, orig_begin)
    words = sentence.words or []
    subj_word = select_subject(words, cfg)
    verb_word = select_verb(words, cfg)
    obj_word = select_object(words, cfg)
    addr_word = select_address(words, cfg)
    pred_word = select_predicate(words)

    subject = with_confidence(
        word_span(sentence, subj_word, abs_offset, index_map, len(original_text)),
        "subject",
        role_strength("subject", subj_word, cfg),
    )
    verb = with_confidence(
        word_span(sentence, verb_word, abs_offset, index_map, len(original_text)),
        "verb",
        role_strength("verb", verb_word, cfg),
    )
    obj = with_confidence(
        word_span(sentence, obj_word, abs_offset, index_map, len(original_text)),
        "object",
        role_strength("object", obj_word, cfg),
    )
    address = with_confidence(
        word_span(sentence, addr_word, abs_offset, index_map, len(original_text)),
        "address",
        role_strength("address", addr_word, cfg),
    )
    predicate = with_confidence(
        word_span(sentence, pred_word, abs_offset, index_map, len(original_text)),
        "predicate",
        role_strength("predicate", pred_word, cfg),
    )
    address_phrase = build_address_phrase(
        sentence, words, addr_word, abs_offset, index_map, len(original_text)
    )
    subject_modifiers = modifier_spans(
        sentence, words, subj_word, "subject", abs_offset, index_map, len(original_text)
    )
    object_modifiers = modifier_spans(
        sentence, words, obj_word, "object", abs_offset, index_map, len(original_text)
    )
    address_modifiers = modifier_spans(
        sentence, words, addr_word, "address", abs_offset, index_map, len(original_text)
    )
    predicate_modifiers = modifier_spans(
        sentence, words, pred_word, "predicate", abs_offset, index_map, len(original_text)
    )
    role_heads = {
        "subject": subj_word,
        "object": obj_word,
        "predicate": pred_word,
        "address": addr_word,
    }
    comp_modifiers = comparison_modifiers(
        sentence, words, role_heads, abs_offset, index_map, len(original_text)
    )
    if address_phrase and isinstance(address_phrase.get("modifiers"), list):
        merged = list(address_phrase.get("modifiers"))
        seen = {(int(m.get("from", -1)), int(m.get("to", -1)), str(m.get("pos", ""))) for m in merged}
        for m in address_modifiers:
            key = (int(m.get("from", -1)), int(m.get("to", -1)), str(m.get("pos", "")))
            if key in seen:
                continue
            seen.add(key)
            merged.append(m)
        address_modifiers = merged

    basic = {}
    if subject:
        basic["subject"] = subject
    if verb:
        basic["verb"] = verb
    if obj:
        basic["object"] = obj
    if address:
        basic["address"] = address
    if predicate:
        basic["predicate"] = predicate
    if address_phrase:
        basic["addressPhrase"] = address_phrase
    if subject_modifiers:
        basic["subjectModifiers"] = subject_modifiers
    if object_modifiers:
        basic["objectModifiers"] = object_modifiers
    if address_modifiers:
        basic["addressModifiers"] = address_modifiers
    if predicate_modifiers:
        basic["predicateModifiers"] = predicate_modifiers
    if comp_modifiers:
        by_role = {"subject": [], "object": [], "predicate": [], "address": []}
        for mod in comp_modifiers:
            role = str(mod.get("role", "") or "")
            if role in by_role:
                by_role[role].append(mod)
        if by_role["subject"]:
            basic["subjectModifiers"] = (basic.get("subjectModifiers", []) + by_role["subject"])
        if by_role["object"]:
            basic["objectModifiers"] = (basic.get("objectModifiers", []) + by_role["object"])
        if by_role["predicate"]:
            basic["predicateModifiers"] = (basic.get("predicateModifiers", []) + by_role["predicate"])
        if by_role["address"]:
            basic["addressModifiers"] = (basic.get("addressModifiers", []) + by_role["address"])

    clauses = build_clauses(sentence, words, cfg, original_text, index_map, abs_offset)

    # Sentence bounds for the anchor inventory. Untrimmed, so utterance-final sits after the closing
    # punctuation while before-final-punct sits in front of it — two distinct, both useful, slots.
    all_ids = {word_id_value(w) for w in words if word_id_value(w) is not None}
    sentence_span = None
    if all_ids:
        by_id_all = {word_id_value(w): w for w in words if word_id_value(w) is not None}
        s_start = s_end = None
        for wid in sorted(all_ids):
            span = word_token_char_span(sentence, by_id_all.get(wid))
            if span is None:
                continue
            s_start = span[0] if s_start is None else min(s_start, span[0])
            s_end = span[1] if s_end is None else max(s_end, span[1])
        sentence_span = chars_to_span(s_start, s_end, original_text, index_map, abs_offset)

    final_punct = None
    for w in sorted([w for w in words if word_id_value(w) is not None],
                    key=lambda w: word_id_value(w)):
        if str(getattr(w, "upos", "") or "") == "PUNCT":
            final_punct = w
    punct_span = word_span(sentence, final_punct, abs_offset, index_map, len(original_text)) \
        if final_punct is not None else None

    ann = {
        "id": f"ud-{pick_line(line, idx)}-ann{idx}",
        "line": pick_line(line, idx),
        "speaker": str(speaker or ""),
        "text": (
            original_text[orig_begin:map_pos_to_original(begin + len(sentence_text), index_map, len(original_text))]
            if begin >= 0
            else sentence_text
        ),
        "basic": basic,
        # Fine-grained view (schema v3): per-clause roles with head *and* phrase spans, and all
        # objects rather than the first match. `basic` above is left exactly as it was, so v2
        # consumers are unaffected — see the section comment above segment_clauses().
        "clauses": clauses,
        # The label space a placement model predicts over — see the section comment above anchor().
        "anchors": build_anchors(
            clauses,
            sentence_span["from"] if sentence_span else abs_offset,
            sentence_span["to"] if sentence_span else abs_offset,
            punct_span["from"] if punct_span else None,
        ),
        "provenance": {
            "analyzedAt": now_iso(),
            "layers": {"basic": "ud", "clauses": "ud", "anchors": "ud"},
        },
    }
    debug = sentence_debug_payload(sentence, cfg, subj_word, verb_word, obj_word, addr_word, pred_word) if include_debug else None
    return ann, (begin if begin >= 0 else used_pos), debug


def word_span(sentence, word, base_offset, index_map=None, original_text_len=0):
    if word is None:
        return None
    word_id = int(getattr(word, "id", 0) or 0)
    for tok in sentence.tokens:
        for tw in tok.words:
            tw_id = int(getattr(tw, "id", 0) or 0)
            if tw_id == word_id:
                start = int(getattr(tok, "start_char", -1))
                end = int(getattr(tok, "end_char", -1))
                if start >= 0 and end >= start:
                    orig_start, orig_end = map_span_to_original(start, end, index_map, original_text_len)
                    if orig_start >= 0 and orig_end > orig_start:
                        return {
                            "text": tok.text,
                            "from": base_offset + orig_start,
                            "to": base_offset + orig_end,
                            "confidence": 1.0,
                        }
                    return {
                        "text": tok.text,
                        "from": base_offset + start,
                        "to": base_offset + end,
                        "confidence": 1.0,
                    }
    text = str(getattr(word, "text", "") or "").strip()
    if not text:
        return None
    return {"text": text, "from": base_offset, "to": base_offset + len(text), "confidence": 0.5}


def analyze(payload):
    text = str(payload.get("text", "") or "")
    lang = str(payload.get("language", DEFAULT_LANG) or DEFAULT_LANG)
    line = payload.get("line")
    speaker = payload.get("speaker", "")
    base_offset = int(payload.get("baseOffset", 0) or 0)
    include_debug = bool(payload.get("debug", False))
    normalized_text, index_map = preprocess_text(text, lang)
    language = normalize_lang(lang)
    pipe = get_pipeline(language)
    # Serialised per language — see the note on _pipeline_locks.
    with pipeline_lock(language):
        doc = pipe(normalized_text)
    annotations = []
    debug_sentences = []
    cursor = 0
    for idx, sentence in enumerate(doc.sentences):
        ann, begin, debug = build_annotation(
            sentence, idx, lang, normalized_text, text, index_map, cursor, line, speaker, base_offset, include_debug
        )
        if begin >= 0:
            cursor = begin + len(ann.get("text", ""))
        annotations.append(ann)
        if debug is not None:
            debug_sentences.append(debug)
    now = now_iso()
    result = {
        # v3: annotations carry `clauses` and `anchors` beside the v2-compatible flat `basic`.
        "version": 3,
        "schema": {"id": "vsm.semantic.annotations", "version": 3},
        "generatedAt": now,
        "updatedAt": now,
        "provenance": {
            "source": "semantic-ud",
            "service": "stanza-depparse",
            "model": lang,
            "analyzedAt": now,
            "layers": {"basic": "ud", "dialogueAct": "unknown", "themeRheme": "unknown"},
        },
        "annotations": annotations,
    }
    if include_debug:
        result["debug"] = {"language": lang, "sentences": debug_sentences}
    return result


def analyze_batch(payload):
    """Analyses many sentences in one request.

    A corpus run over a project makes one call per sentence; batching removes that per-sentence HTTP
    round trip. Each item is analysed independently and a failing item yields an `error` entry rather
    than failing the whole batch, so one unparseable sentence cannot cost a whole run.

    Request:  {"sentences": [{"text", "language"?, "line"?, "speaker"?, "baseOffset"?}], "language"?, "debug"?}
    Response: {"version": 2, "count": n, "results": [<same shape as /analyze>, ...]}
    """
    sentences = payload.get("sentences")
    if not isinstance(sentences, list):
        raise ValueError("'sentences' must be an array")
    default_lang = payload.get("language", DEFAULT_LANG)
    include_debug = bool(payload.get("debug", False))

    results = []
    for idx, item in enumerate(sentences):
        if not isinstance(item, dict):
            results.append({"error": "invalid_item", "index": idx})
            continue
        request = dict(item)
        request.setdefault("language", default_lang)
        if include_debug:
            request.setdefault("debug", True)
        try:
            results.append(analyze(request))
        except Exception as exc:
            results.append({"error": "analyze_failed", "index": idx, "message": str(exc)})
    return {"version": 2, "count": len(results), "results": results}


class Handler(BaseHTTPRequestHandler):
    def _json(self, code, payload):
        body = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/health":
            self._json(200, {
                "status": "ok",
                "service": "semantic-ud",
                "port": PORT,
                "loaded": sorted(_pipelines.keys()),
                "preload": PRELOAD_LANGS,
                "autoDownload": AUTO_DOWNLOAD,
                "resourcesDir": RESOURCES_DIR or None,
                "package": UD_PACKAGE or "(stanza default)",
            })
            return
        self._json(404, {"error": "not_found"})

    def do_POST(self):
        if self.path not in ("/analyze", "/analyze/batch"):
            self._json(404, {"error": "not_found"})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            raw = self.rfile.read(length).decode("utf-8") if length > 0 else "{}"
            payload = json.loads(raw)
            if self.path == "/analyze/batch":
                self._json(200, analyze_batch(payload))
            else:
                self._json(200, analyze(payload))
        except ValueError as exc:
            self._json(400, {"error": "bad_request", "message": str(exc)})
        except Exception as exc:
            self._json(500, {"error": "analyze_failed", "message": str(exc)})

    def log_message(self, fmt, *args):
        return


def main():
    if PRELOAD_LANGS:
        preload_pipelines()
    # Threaded: the former single-threaded HTTPServer could not even accept a second connection while
    # a parse was running, so /health looked dead and concurrent callers timed out on connect.
    try:
        server = ThreadingHTTPServer((HOST, PORT), Handler)
    except OSError as exc:
        print(f"[semantic-ud] FATAL: cannot bind {HOST}:{PORT}: {exc}", file=sys.stderr)
        print("[semantic-ud] Another instance is probably already running. Check with "
              f"`curl -s http://{HOST}:{PORT}/health`, or set SEMANTIC_UD_PORT.", file=sys.stderr)
        raise SystemExit(1)
    server.daemon_threads = True
    print(f"[semantic-ud] listening on http://{HOST}:{PORT}", flush=True)
    print("[semantic-ud] endpoints: POST /analyze, POST /analyze/batch, GET /health", flush=True)
    print(f"[semantic-ud] preloaded: {', '.join(PRELOAD_LANGS) if PRELOAD_LANGS else '(none)'}", flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
