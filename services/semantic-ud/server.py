#!/usr/bin/env python3
import json
import os
import re
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

import stanza


HOST = os.environ.get("SEMANTIC_UD_HOST", "127.0.0.1")
PORT = int(os.environ.get("SEMANTIC_UD_PORT", "4061"))
DEFAULT_LANG = os.environ.get("SEMANTIC_UD_LANG", "de")
RESOURCES_DIR = os.environ.get("SEMANTIC_UD_RESOURCES_DIR", os.environ.get("STANZA_RESOURCES_DIR", "")).strip()
AUTO_DOWNLOAD = os.environ.get("SEMANTIC_UD_AUTO_DOWNLOAD", "true").strip().lower() not in ("0", "false", "no")

_pipelines = {}

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


def get_pipeline(lang: str):
    language = (lang or DEFAULT_LANG or "de").strip().lower()
    if not language:
        language = "de"
    pipe = _pipelines.get(language)
    if pipe is not None:
        return pipe
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
    _pipelines[language] = pipe
    return pipe


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
        "provenance": {
            "analyzedAt": now_iso(),
            "layers": {"basic": "ud"},
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
    doc = get_pipeline(lang)(normalized_text)
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
        "version": 2,
        "schema": {"id": "vsm.semantic.annotations", "version": 2},
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
            self._json(200, {"status": "ok", "service": "semantic-ud", "port": PORT})
            return
        self._json(404, {"error": "not_found"})

    def do_POST(self):
        if self.path != "/analyze":
            self._json(404, {"error": "not_found"})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            raw = self.rfile.read(length).decode("utf-8") if length > 0 else "{}"
            payload = json.loads(raw)
            result = analyze(payload)
            self._json(200, result)
        except Exception as exc:
            self._json(500, {"error": "analyze_failed", "message": str(exc)})

    def log_message(self, fmt, *args):
        return


def main():
    server = HTTPServer((HOST, PORT), Handler)
    print(f"[semantic-ud] listening on http://{HOST}:{PORT}")
    print("[semantic-ud] endpoint POST /analyze, GET /health")
    server.serve_forever()


if __name__ == "__main__":
    main()
