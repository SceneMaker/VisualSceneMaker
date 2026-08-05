#!/usr/bin/env python3
"""Compare Stanza treebank/encoder packages on the evaluation set.

Answers one question: *would switching the parser package be an improvement?* Judged by the eval
harness, never by a handful of sentences — see doc/parser-quality-plan.md, where exactly that
distinction reversed a decision that looked obvious.

    python3 compare_packages.py                                    # default vs hdt_charlm
    python3 compare_packages.py combined_charlm hdt_charlm gsd_charlm
    python3 compare_packages.py combined_charlm combined_german-nlp-electra

Prints, per package: structural checks passing, exact-row S+V+O, known-weak cases unchanged, and
mean parse latency — latency matters because /analyze/batch runs over a whole corpus and the same
core targets Android.

The transformer packages (`*_german-nlp-electra`) additionally need their encoder from HuggingFace
(`german-nlp-group/electra-base-german-uncased`). If that host is unreachable the script says so
plainly instead of failing with a stack trace.
"""
import importlib
import io
import os
import re
import statistics
import sys
import time
from contextlib import redirect_stdout
from pathlib import Path

LATENCY_SENTENCES = [
    "Ich gebe dem Kind den roten Ball.",
    "Lass mich einen Vorschlag machen wie wir zusammen den Nachmittag gestalten.",
    "Hallo $user, wie geht es Dir heute?",
    "Schau mal, wie Bob das findet.",
]


def run_for_package(package, cases_path):
    """Runs the harness with SEMANTIC_UD_PACKAGE=package in a fresh module import."""
    if package:
        os.environ["SEMANTIC_UD_PACKAGE"] = package
    else:
        os.environ.pop("SEMANTIC_UD_PACKAGE", None)
    os.environ.setdefault("SEMANTIC_UD_AUTO_DOWNLOAD", "false")

    # server.py reads its env at import time, so reload it for each package.
    for mod in ("server", "eval_harness"):
        if mod in sys.modules:
            del sys.modules[mod]
    try:
        server = importlib.import_module("server")
        harness = importlib.import_module("eval_harness")
    except Exception as exc:
        return {"error": f"import failed: {exc}"}

    cases = harness.load_cases(Path(cases_path))
    buffer = io.StringIO()
    try:
        with redirect_stdout(buffer):
            results = [harness.evaluate_case(c) for c in cases]
            harness.print_report(cases, results)
    except Exception as exc:
        message = str(exc)
        if "huggingface.co" in message or "couldn't connect" in message.lower():
            return {"error": "needs the HuggingFace encoder "
                             "(german-nlp-group/electra-base-german-uncased), which is unreachable"}
        return {"error": message.splitlines()[0][:160]}

    text = buffer.getvalue()

    def grab(pattern, cast=str):
        m = re.search(pattern, text)
        return cast(m.group(1)) if m else None

    # Latency on a warm pipeline.
    timings = []
    for sentence in LATENCY_SENTENCES:
        t0 = time.perf_counter()
        server.analyze({"text": sentence, "language": "de", "baseOffset": 0})
        timings.append((time.perf_counter() - t0) * 1000)

    return {
        "structural": grab(r"Structural \(clauses/objects/anchors\): (\d+/\d+)"),
        "exact": grab(r"Exact-row match \(S\+V\+O\): (\d+/\d+)"),
        "weak": grab(r"Known-weak \(pinned mis-parses\): (\d+/\d+)"),
        "weakChanged": "BEHAVIOUR CHANGED" in text,
        "latencyMs": round(statistics.mean(timings), 1),
        "report": text,
    }


def main():
    packages = sys.argv[1:] or ["", "hdt_charlm"]
    cases_path = os.environ.get("SEMANTIC_UD_CASES", "references/eval-cases.json")
    if not Path(cases_path).exists():
        print(f"cases file not found: {cases_path}", file=sys.stderr)
        return 2

    rows = []
    for package in packages:
        label = package or "(stanza default)"
        print(f"running {label} ...", flush=True)
        rows.append((label, run_for_package(package, cases_path)))

    width = max(len(label) for label, _ in rows) + 2
    print()
    print(f"{'package'.ljust(width)}{'structural':>12}{'exact S+V+O':>14}{'known-weak':>13}{'ms/sent':>10}")
    print("-" * (width + 49))
    for label, r in rows:
        if r.get("error"):
            print(f"{label.ljust(width)}  SKIPPED — {r['error']}")
            continue
        flag = "  <-- weak cases changed, read them" if r["weakChanged"] else ""
        print(f"{label.ljust(width)}{r['structural']:>12}{r['exact']:>14}{r['weak']:>13}"
              f"{r['latencyMs']:>10}{flag}")
    print()
    print("Adopt a package only if structural does not regress AND exact-row does not regress.")
    print("A known-weak case that changed is a signal to read it, not a failure: the parse may have")
    print("become correct, in which case the case should graduate to a real assertion.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
