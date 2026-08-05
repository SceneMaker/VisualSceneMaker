#!/usr/bin/env python3
import argparse
import json
import sys
from pathlib import Path

from server import analyze


ROLES = ("subject", "verb", "object")


def norm_text(value):
    if value is None:
        return ""
    return " ".join(str(value).strip().lower().split())


def span_text(annotation, role):
    basic = annotation.get("basic") if isinstance(annotation, dict) else None
    if not isinstance(basic, dict):
        return ""
    span = basic.get(role)
    if isinstance(span, dict):
        return norm_text(span.get("text", ""))
    if isinstance(span, str):
        return norm_text(span)
    return ""


def first_annotation(payload):
    annotations = payload.get("annotations", []) if isinstance(payload, dict) else []
    if not annotations:
        return {}
    return annotations[0] if isinstance(annotations[0], dict) else {}


def clause_summary(annotation):
    """Compact per-clause view for comparison: type, verb, subject, and object kind/phrase pairs."""
    out = []
    for clause in (annotation.get("clauses") or []):
        roles = clause.get("roles") or {}

        def head_of(role):
            entry = roles.get(role) or {}
            return norm_text((entry.get("head") or {}).get("text", ""))

        def mods_of(role):
            entry = roles.get(role) or {}
            return sorted(norm_text(m.get("text", "")) for m in (entry.get("modifiers") or []))

        out.append({
            "type": str(clause.get("type") or ""),
            "verb": head_of("verb"),
            "subject": head_of("subject"),
            "predicate": head_of("predicate"),
            "linker": norm_text((clause.get("linker") or {}).get("text", "")),
            "verbModifiers": mods_of("verb"),
            "subjectModifiers": mods_of("subject"),
            "predicateModifiers": mods_of("predicate"),
            "objects": [
                {
                    "kind": str(obj.get("kind") or ""),
                    "phrase": norm_text((obj.get("phrase") or obj.get("head") or {}).get("text", "")),
                }
                for obj in (clause.get("objects") or [])
            ],
        })
    return out


def clause_diffs(predicted, expected):
    """Human-readable differences between predicted and expected clause summaries."""
    diffs = []
    if len(predicted) != len(expected):
        diffs.append(f"clause count {len(predicted)} != expected {len(expected)}")
    for idx, want in enumerate(expected):
        if idx >= len(predicted):
            diffs.append(f"c{idx}: missing")
            continue
        got = predicted[idx]
        for field in ("type", "verb", "subject", "predicate", "linker"):
            if field not in want:
                continue
            if norm_text(want[field]) != got.get(field, ""):
                diffs.append(f"c{idx}.{field}: '{got.get(field, '')}' != '{norm_text(want[field])}'")
        for field in ("verbModifiers", "subjectModifiers", "predicateModifiers"):
            if field not in want:
                continue
            want_mods = sorted(norm_text(t) for t in want[field])
            if want_mods != got.get(field, []):
                diffs.append(f"c{idx}.{field}: {got.get(field, [])} != {want_mods}")
        if "objects" in want:
            want_objs = [(norm_text(o.get("kind", "")), norm_text(o.get("phrase", ""))) for o in want["objects"]]
            got_objs = [(o["kind"], o["phrase"]) for o in got.get("objects", [])]
            if want_objs != got_objs:
                diffs.append(f"c{idx}.objects: {got_objs} != {want_objs}")
    return diffs


def anchor_diffs(annotation, required_slots):
    present = {str(a.get("slot") or "") for a in (annotation.get("anchors") or [])}
    missing = [slot for slot in required_slots if slot not in present]
    return [f"missing anchor slot(s): {', '.join(missing)}"] if missing else []


def effective_package(payload_language="de"):
    """Which parser the harness is actually measuring.

    Worth printing: production requests the transformer, while the harness runs whatever the service
    defaults to, so without this line a reader cannot tell whether a failure reflects the shipped
    configuration or a different parser. `analyze()` reports the package it used.
    """
    try:
        from server import analyze
        doc = analyze({"text": "Test.", "language": payload_language, "baseOffset": 0})
        return (doc.get("provenance") or {}).get("package", "(unknown)")
    except Exception:
        return "(unknown)"


def evaluate_case(case):
    payload = {
        "text": case.get("text", ""),
        "language": case.get("language", "de"),
        "line": case.get("line", 1),
        "speaker": case.get("speaker", ""),
        "baseOffset": 0,
    }
    predicted = first_annotation(analyze(payload))
    expected = case.get("expected", {}) if isinstance(case.get("expected"), dict) else {}
    role_result = {}
    for role in ROLES:
        pred = span_text(predicted, role)
        gold = norm_text(expected.get(role, ""))
        role_result[role] = {"pred": pred, "gold": gold}

    # v3 layers. Both checks are opt-in per case, so v2-era cases keep evaluating exactly as before.
    structural = []
    if isinstance(case.get("expectedClauses"), list):
        structural += clause_diffs(clause_summary(predicted), case["expectedClauses"])
    if isinstance(case.get("expectedAnchorSlots"), list):
        structural += anchor_diffs(predicted, case["expectedAnchorSlots"])
    # ROLES covers only subject/verb/object for the historical precision/recall table. A case may
    # still assert predicate or address, so check those here rather than letting them pass silently.
    for role in ("predicate", "address"):
        if role not in expected:
            continue
        got = norm_text(((predicted.get("basic") or {}).get(role) or {}).get("text", ""))
        want = norm_text(expected[role])
        if got != want:
            structural.append(f"{role}: expected '{want}', got '{got}'")
    # Modifier expectations, e.g. {"verbModifiers": ["Super"]}. Compared as sets of surface forms:
    # token order holds in practice but is not part of the contract worth asserting.
    for key in ("subjectModifiers", "verbModifiers", "objectModifiers",
                "predicateModifiers", "addressModifiers"):
        if not isinstance(expected.get(key), list):
            continue
        got = sorted(norm_text(m.get("text", ""))
                     for m in ((predicted.get("basic") or {}).get(key) or []))
        want = sorted(norm_text(t) for t in expected[key])
        if got != want:
            structural.append(f"{key}: expected {want}, got {got}")
    if case.get("expectedNoAddress"):
        addr = ((predicted.get("basic") or {}).get("address") or {}).get("text", "")
        if norm_text(addr):
            structural.append(f"expected no address, got '{norm_text(addr)}'")
    role_result["_structural"] = structural
    return role_result


def accumulate_metrics(all_results):
    metrics = {r: {"tp": 0, "pred": 0, "gold": 0} for r in ROLES}
    exact_rows = 0
    for row in all_results:
        all_ok = not row.get("_structural")
        for role in ROLES:
            pred = row[role]["pred"]
            gold = row[role]["gold"]
            if pred:
                metrics[role]["pred"] += 1
            if gold:
                metrics[role]["gold"] += 1
            if pred and gold and pred == gold:
                metrics[role]["tp"] += 1
            if pred != gold:
                all_ok = False
        if all_ok:
            exact_rows += 1
    return metrics, exact_rows


def prf(tp, pred, gold):
    precision = tp / pred if pred else 0.0
    recall = tp / gold if gold else 0.0
    f1 = (2 * precision * recall / (precision + recall)) if (precision + recall) else 0.0
    return precision, recall, f1


def print_report(cases, results):
    metrics, exact_rows = accumulate_metrics(results)
    structural_cases = sum(1 for c in cases
                           if isinstance(c.get("expectedClauses"), list)
                           or isinstance(c.get("expectedAnchorSlots"), list))
    structural_fail = sum(1 for idx, c in enumerate(cases)
                          if results[idx].get("_structural") and not c.get("knownWeak"))
    weak_total = sum(1 for c in cases if c.get("knownWeak"))
    # knownWeak cases assert the CORRECT reading and are expected to fail with the current default
    # model. Counting how many now PASS makes a model that fixes one show up as progress, rather than
    # as a regression against a pinned mistake — which is exactly how a genuine improvement was
    # first misread. A knownWeak case that passes should graduate to a normal case.
    weak_passing = sum(1 for idx, c in enumerate(cases)
                       if c.get("knownWeak") and not results[idx].get("_structural")
                       and all(results[idx][r]["pred"] == results[idx][r]["gold"] for r in ROLES))
    print("UD mapping evaluation")
    print(f"Parser package measured: {effective_package()}")
    print(f"Cases: {len(cases)}")
    real = [idx for idx, c in enumerate(cases) if not c.get("knownWeak")]
    exact_real = sum(1 for idx in real
                     if not results[idx].get("_structural")
                     and all(results[idx][r]["pred"] == results[idx][r]["gold"] for r in ROLES))
    print(f"Exact-row match (S+V+O): {exact_rows}/{len(cases)} all cases, "
          f"{exact_real}/{len(real)} excluding expected-fail")
    print(f"Structural (clauses/objects/anchors): {structural_cases - structural_fail}"
          f"/{structural_cases} passing")
    if weak_total:
        print(f"Known-weak (expected-fail, assert the CORRECT reading): {weak_passing}/{weak_total} "
              f"now passing"
              + ("  <-- one graduated, promote it to a normal case" if weak_passing else ""))
    print("")
    for role in ROLES:
        m = metrics[role]
        p, r, f1 = prf(m["tp"], m["pred"], m["gold"])
        print(
            f"{role:>7}  tp={m['tp']:>3}  pred={m['pred']:>3}  gold={m['gold']:>3}  "
            f"P={p:.3f}  R={r:.3f}  F1={f1:.3f}"
        )
    print("")
    print("Per case:")
    for idx, case in enumerate(cases):
        result = results[idx]
        label = case.get("id", f"case-{idx + 1}")
        print(f"- {label} [{case.get('language', 'de')}]")
        for role in ROLES:
            pred = result[role]["pred"] or "∅"
            gold = result[role]["gold"] or "∅"
            marker = "OK" if pred == gold else "DIFF"
            print(f"  {role:>7}: pred='{pred}' gold='{gold}' [{marker}]")
        if case.get("knownWeak"):
            print(f"     note: KNOWN WEAK — {case.get('why', 'upstream mis-parse')}")
        for diff in result.get("_structural", []):
            print(f"     structural DIFF: {diff}")


def load_cases(path):
    with path.open("r", encoding="utf-8") as fh:
        parsed = json.load(fh)
    if not isinstance(parsed, list):
        raise ValueError("cases file must contain a JSON array")
    return parsed


def main():
    parser = argparse.ArgumentParser(description="Evaluate semantic-ud S/V/O mapping.")
    parser.add_argument("--cases", default="references/eval-cases.json", help="Path to evaluation cases JSON.")
    args = parser.parse_args()

    cases_path = Path(args.cases)
    if not cases_path.exists():
        print(f"Cases file not found: {cases_path}", file=sys.stderr)
        return 2

    cases = load_cases(cases_path)
    if not cases:
        print("No cases found.", file=sys.stderr)
        return 2

    results = [evaluate_case(case) for case in cases]
    print_report(cases, results)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
