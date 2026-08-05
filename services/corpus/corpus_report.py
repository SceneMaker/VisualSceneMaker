#!/usr/bin/env python3
"""Reports over the behavior-command placement corpus (JSONL from CorpusExtractCli).

    python3 corpus_report.py stats     corpus.jsonl        # plan 2.4
    python3 corpus_report.py agreement corpus.jsonl [...]  # plan 2.3

`stats` answers: is there enough data to fit a placement model, and where is it thin?
`agreement` answers: do different annotators place commands in the same structural slots? That
question **gates Phase 3** — if they disagree systematically, placement is an authoring style rather
than a shared convention, and a per-project adaptive model is the only honest design.

Both read only the corpus file, so they are cheap to re-run as annotation proceeds.
"""
import collections
import json
import itertools
import sys

# NEUROGES's own trained-rater interrater reliability for the Function category (EasyDIAg, Lausberg &
# Slötjes 2016), quoted so our numbers are read against the right yardstick. Function is the weakest
# category in the system and the only one annotators pick directly, so a naive kappa >= 0.8 target
# would reject a scheme performing as well as trained NEUROGES raters.
NEUROGES_FUNCTION_RELIABILITY = "0.43 – 0.79 (weakest: spatial relation presentation 0.43)"


def load(paths):
    records = []
    for path in paths:
        with open(path, "r", encoding="utf-8") as fh:
            for line in fh:
                line = line.strip()
                if line:
                    records.append(json.loads(line))
    return records


def bar(n, width=28, scale=1):
    return "#" * min(width, int(n * scale))


def function_label(placement):
    """Bucket for the Function histogram.

    Three cases that must not be conflated: a real Function; a command correctly carrying none
    because it is not a NEUROGES unit at all (a backdrop change, a control action); and a command we
    could not attribute to a plugin, which is the only one that is actually a gap.
    """
    if placement.get("function"):
        return placement["function"]
    if not placement.get("plugin"):
        return "(unattributed - taxonomy gap)"
    channel = placement.get("channel") or "?"
    return f"(no NEUROGES unit: {channel})"


# --------------------------------------------------------------------------- stats


def stats(records):
    placements = [p for r in records for p in r.get("placements", [])]
    cospeech = [p for p in placements if p.get("cospeech") is True]
    anchors_offered = sum(len(r.get("anchors") or []) for r in records)

    print("Placement corpus — statistics")
    print("=" * 72)
    print(f"sentences (records)      {len(records)}")
    print(f"  with >=1 placement     {sum(1 for r in records if r.get('placements'))}")
    print(f"  with none              {sum(1 for r in records if not r.get('placements'))}"
          "   <- negatives: positions deliberately left empty")
    print(f"placements               {len(placements)}")
    print(f"  co-speech              {len(cospeech)}   <- the only candidates a placement model may propose")
    print(f"  classified (Function)  {sum(1 for p in placements if p.get('function'))}")
    print(f"  anchored to a slot     {sum(1 for p in placements if p.get('anchor'))}")
    print(f"anchor slots offered     {anchors_offered}")
    if anchors_offered:
        print(f"base rate                {len(placements) / anchors_offered:.1%} of offered slots were used")
    print()
    print(f"projects    {len(({r.get('project') for r in records}))}: "
          f"{', '.join(sorted({r.get('project','?') for r in records}))}")
    print(f"scenarios   {len({r.get('scenario') for r in records})}: "
          f"{', '.join(sorted({r.get('scenario','?') for r in records}))}")
    print(f"annotators  {len({r.get('annotator') for r in records})}: "
          f"{', '.join(sorted({r.get('annotator','?') for r in records}))}")

    funcs = collections.Counter(function_label(p) for p in placements)
    print()
    print("by NEUROGES Function")
    for name, n in funcs.most_common():
        print(f"  {name:<32} {n:>3}  {bar(n)}")

    slots = collections.Counter(p.get("anchor") or "(no slot)" for p in placements)
    print()
    print("by anchor slot")
    for name, n in slots.most_common():
        print(f"  {name:<32} {n:>3}  {bar(n)}")

    # The cross-tab is what a placement model actually learns: P(slot | function).
    print()
    print("Function x anchor slot — the distribution a model would fit")
    # Only real Function values: the cross-tab is what a model fits, and rows for "not a NEUROGES
    # unit" would be noise in it.
    cross = collections.Counter(
        (p["function"], (p.get("anchor") or "(no slot)"))
        for p in placements if p.get("function"))
    for (func, slot), n in sorted(cross.items(), key=lambda kv: (-kv[1], kv[0])):
        print(f"  {func:<30} -> {slot:<24} {n:>3}")

    # Sparsity: the honest verdict on whether anything can be fitted yet.
    print()
    print("Sparsity")
    singletons = sum(1 for n in cross.values() if n == 1)
    print(f"  distinct (Function, slot) cells   {len(cross)}")
    print(f"  cells seen exactly once           {singletons}")
    thin = [f for f, n in funcs.items() if n < 5 and not f.startswith("(")]
    if thin:
        print(f"  Function values with <5 examples  {len(thin)}: {', '.join(sorted(thin))}")
    print()
    if len(cospeech) < 30:
        print(f"  VERDICT: {len(cospeech)} co-speech placements is far too few to fit a distribution.")
        print("  Use the hand-written prior with NEUROGES axis back-off (plan 3.2); treat any number")
        print("  measured on this corpus as a smoke test, not a result.")
    else:
        print(f"  VERDICT: {len(cospeech)} co-speech placements — enough for a frequency model with")
        print("  back-off, still not enough to trust per-cell estimates.")


# ----------------------------------------------------------------------- agreement


def placement_key(placement):
    """What two annotators must match on: the structural slot and the behavior category.

    Deliberately not the command name or its parameters — the plan's subject is *placement*, and two
    annotators choosing `happy` versus `smile` at the same slot for the same function agree about the
    thing being measured.
    """
    return (placement.get("anchor"), placement.get("function"))


def agreement(records):
    print("Placement corpus — inter-annotator agreement")
    print("=" * 72)

    annotators = sorted({r.get("annotator", "unknown") for r in records})
    print(f"annotators: {', '.join(annotators)}")

    # A comparable item is the same sentence text within the same scenario, seen from >=2 annotators.
    by_sentence = collections.defaultdict(dict)
    for r in records:
        key = (r.get("scenario"), r.get("cleanText"))
        by_sentence[key][r.get("annotator", "unknown")] = r
    shared = {k: v for k, v in by_sentence.items() if len(v) >= 2}

    print(f"sentences seen by >=2 annotators: {len(shared)} of {len(by_sentence)}")
    if not shared:
        print()
        print("  Nothing to compare yet: every sentence has a single annotator.")
        print("  This report becomes meaningful once the same scenario has been authored by two or")
        print("  more people. Extract each annotator's copy with --annotator=<id> and concatenate,")
        print("  or pass several corpus files to this command.")
        print()
        print("  It GATES Phase 3: if annotators systematically disagree about where a command goes,")
        print("  placement is an authoring style rather than a shared convention, which confirms the")
        print("  per-project adaptive design instead of a global model.")
        print()
        print(f"  Yardstick when it runs: NEUROGES's own trained raters reach {NEUROGES_FUNCTION_RELIABILITY}")
        print("  on the Function category. Set per-value targets against that, never a flat 0.8.")
        return

    # Per-slot binary decisions: for each offered anchor, did the annotator place a co-speech command
    # there? That turns placement into comparable items and makes Cohen's kappa well defined.
    pair_stats = collections.defaultdict(
        lambda: {"both": 0, "neither": 0, "only_a": 0, "only_b": 0, "union": 0, "inter": 0})
    exact_match = 0
    for (scenario, text), per_annotator in shared.items():
        for a, b in itertools.combinations(sorted(per_annotator), 2):
            ra, rb = per_annotator[a], per_annotator[b]
            slots = [s.get("slot") for s in (ra.get("anchors") or [])]
            used_a = {p.get("anchor") for p in ra.get("placements", []) if p.get("cospeech")}
            used_b = {p.get("anchor") for p in rb.get("placements", []) if p.get("cospeech")}
            st = pair_stats[(a, b)]
            st["union"] += len(used_a | used_b)
            st["inter"] += len(used_a & used_b)
            for slot in slots:
                in_a, in_b = slot in used_a, slot in used_b
                if in_a and in_b:
                    st["both"] += 1
                elif in_a:
                    st["only_a"] += 1
                elif in_b:
                    st["only_b"] += 1
                else:
                    st["neither"] += 1
            if {placement_key(p) for p in ra.get("placements", [])} \
                    == {placement_key(p) for p in rb.get("placements", [])}:
                exact_match += 1

    print(f"sentences where the full placement set matched exactly: {exact_match}/{len(shared)}")
    print()
    print("per annotator pair, over each offered anchor slot as a binary decision")
    for (a, b), st in sorted(pair_stats.items()):
        # Only the four contingency cells — not every key in the dict, which also holds the
        # union/intersection tallies used for the Jaccard figure below.
        n = st["both"] + st["neither"] + st["only_a"] + st["only_b"]
        if not n:
            continue
        observed = (st["both"] + st["neither"]) / n
        # Cohen's kappa on the 2x2 table.
        p_a = (st["both"] + st["only_a"]) / n
        p_b = (st["both"] + st["only_b"]) / n
        expected = p_a * p_b + (1 - p_a) * (1 - p_b)
        kappa = (observed - expected) / (1 - expected) if expected < 1 else float("nan")
        print(f"  {a} vs {b}:  items={n}  observed={observed:.3f}  kappa={kappa:.3f}")
        print(f"      both placed {st['both']}, neither {st['neither']}, "
              f"only {a} {st['only_a']}, only {b} {st['only_b']}")
        # Restricted to slots at least one annotator used. This is the number to lead with: kappa over
        # every offered slot is dominated by the ~96% nobody touches, so it measures mostly how much
        # blank space the inventory offers, not whether people agree about placement.
        if st["union"]:
            print(f"      agreement where anyone placed (Jaccard): "
                  f"{st['inter'] / st['union']:.3f}  ({st['inter']}/{st['union']} slots)")

    print()
    print(f"Yardstick: NEUROGES trained raters reach {NEUROGES_FUNCTION_RELIABILITY} on Function.")
    print("Lead with the Jaccard figure. Kappa over every offered slot is deflated by the many")
    print("positions nobody uses — 'neither' dominates, so it largely measures how much blank space")
    print("the anchor inventory offers rather than whether annotators agree about placement.")


def main():
    if len(sys.argv) < 3 or sys.argv[1] not in ("stats", "agreement"):
        print(__doc__)
        return 1
    records = load(sys.argv[2:])
    if not records:
        print("corpus is empty", file=sys.stderr)
        return 2
    (stats if sys.argv[1] == "stats" else agreement)(records)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
