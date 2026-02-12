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
    return role_result


def accumulate_metrics(all_results):
    metrics = {r: {"tp": 0, "pred": 0, "gold": 0} for r in ROLES}
    exact_rows = 0
    for row in all_results:
        all_ok = True
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
    print("UD mapping evaluation")
    print(f"Cases: {len(cases)}")
    print(f"Exact-row match (S+V+O): {exact_rows}/{len(cases)}")
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
