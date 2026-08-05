# Parser quality for dialogue German — findings and plan

Written 2026-08-05, after Phase 1. Companion to `doc/behavior-command-placement-learning.md` and
`doc/semantic-analysis-current.md`.

## The problem, stated precisely

The semantic analysis feeds **spoken-register dialogue** — greetings, imperatives, ellipses,
vocatives, one-word affirmations — to a dependency parser trained on **newswire**. That is a
distribution mismatch, not a bug, and it shows up as confident-but-wrong parses.

Three cases found while verifying Phase 1 against the real example projects. They are pinned as
eval cases in `services/semantic-ud/references/eval-cases.json` so a change in behaviour is noticed:

| Sentence | What the parser does | What it should do |
|---|---|---|
| `Hallo ich bin Xenia.` | `Hallo` = root **VERB**, `bin` = its `ccomp` | one clause, verb `bin`, predicate `Xenia`, `Hallo` a discourse marker |
| `Schau mal, wie Bob das findet.` | `mal` = root **ADV**, `Schau` = `advmod`, so the main clause has **no verb** | imperative main clause with verb `Schau` |
| `Hey, Ich habe eine Aufgabe für Dich.` | `Ich` reported as **both** subject and address | no addressee; `Hey` is a discourse marker |

**The third was ours, not the parser's, and is fixed** — see below. The first two remain, marked
`knownWeak`: the eval cases assert the current, wrong output with a note on the correct answer, so a
model or heuristic change fails loudly instead of drifting unnoticed.

## How much this actually costs us

Less than it appears, and this matters for prioritisation. **The anchor-slot inventory already covers
10 of 10 authored command positions in `plugins/charamel-embed/ExampleProject` despite these
mis-parses**, because slots are derived from clause and phrase *boundaries*, which survive a wrong
root label. A verbless main clause still has a clause-initial boundary; a mis-labelled greeting still
ends where it ends.

So this is about **annotation quality and eventual model quality**, not a blocker for Phase 2 corpus
work. It should be improved deliberately, not urgently.

## Step 1 — Fix what is ours ✅ done

`select_address`'s third fallback existed for trailing vocatives (`…, du Blödmann!`): a noun or
pronoun immediately after a comma. It fired on a **leading greeting comma** too, so in
`Hey, Ich habe eine Aufgabe für Dich.` it returned the following subject as the addressee, and `Ich`
was reported in two roles at once.

Fix: with a greeting comma present, never treat the word right after it as the addressee. With a
greeting comma the addressee sits *before* it (`Hallo $user, …`, handled by the first branch); a
greeting with no name after it simply has no addressee. **Reporting none beats reporting the wrong
one.** Verified not to regress the trailing-vocative case or `Hallo $user, …`, and the eval case was
promoted from `knownWeak` to a regression test with an `expectedNoAddress` assertion.

*Note:* this is the one intentional change to the otherwise-frozen flat `basic` block. It is a bug
fix, not a schema change.

Still open in the same family, deliberately not touched: in `Hör auf, du Blödmann!` the word `du` is
reported as both subject and address. Here the *address* reading is the right one and the subject
reading is the parse artifact, so suppressing one requires deciding which — a judgement call worth
making with the eval set in front of us rather than in passing.

## Step 2 — Try a better model ✅ run, and the answer is *do not switch*

**Correction to an earlier assumption.** The recommendation originally read "switch from GSD to HDT".
That premise was wrong: German's default depparse in Stanza is already **`combined_charlm`**, i.e.
GSD *and* HDT combined.

The service now takes `SEMANTIC_UD_PACKAGE` so this experiment is repeatable:

```bash
SEMANTIC_UD_PACKAGE=hdt_charlm ./gradlew :services:semantic-ud:evaluateMapping
```

### Measured result

On the two failing sentences, `hdt_charlm` is clearly better — and interestingly the *combined*
default behaves like GSD, so the original instinct was right even though the premise was not:

| Sentence | `combined_charlm` (default) | `hdt_charlm` | `gsd_charlm` |
|---|---|---|---|
| `Schau mal, wie Bob das findet.` | root `mal`/ADV ✗ | root **`Schau`/VERB ✓** | root `mal`/ADV ✗ |
| `Hallo ich bin Xenia.` | `bin` as `ccomp` → spurious 2nd clause ✗ | `bin` as **`aux` → one clause ✓** | `bin` as `ccomp` ✗ |

But across the whole eval set it is a **net regression**, which is exactly why the rule is to judge by
the harness and not by the two sentences that annoyed you:

| | `combined_charlm` | `hdt_charlm` |
|---|---|---|
| Structural checks passing | **8/8** | 6/8 |
| Exact-row S+V+O | **7/14** | 3/14 |
| `dem Kind` classified | **`indirect` ✓** | `oblique` ✗ |
| ccomp clause split | **2 clauses, correct verbs ✓** | 3 clauses, wrong verbs ✗ |

HDT breaks both of the cases Phase 1 was built to fix. `dem Kind` degrades because HDT does not label
it `obl:arg`, so the dative-object path is lost; the `ccomp` sentence is re-analysed entirely.

**Decision: keep `combined_charlm`.** `SEMANTIC_UD_PACKAGE` stays as a knob so the comparison can be
re-run, not as a recommendation.

### Two follow-ups this surfaced

1. **Make `object_kind` treebank-robust.** It currently leans on `obl:arg`, which is a GSD/combined
   convention. Detecting a dative object from the morphological case alone, independent of the
   subtype label, would be an improvement *regardless of model* and would remove one of the two HDT
   regressions. Worth doing on its own merits.
2. **The transformer variant is still untested.** `combined_german-nlp-electra` is the genuinely
   different option — same treebank, better encoder — and it is the most promising untried lever. Its
   Stanza weights download fine (~200 MB, now present in `~/stanza_resources/de/{pos,depparse}/`),
   but building the pipeline additionally fetches the ELECTRA encoder from HuggingFace, which was not
   reachable from the machine this was run on:

   ```
   OSError: We couldn't connect to 'https://huggingface.co' to load the files
   ```

   Retry where HF is reachable, then judge by the harness the same way. Note the downloaded `.pt`
   files are inert until then, and a transformer parser is slower and needs more memory — measure
   latency on the example projects before adopting, since it affects `POST /analyze/batch` over a
   corpus and the Android target.

## Step 3 — Greeting pre-normalisation, only if step 2 doesn't settle it

The service already normalises `$user`-style placeholders before parsing and maps offsets back
afterwards (`preprocess_text` / `map_span_to_original`). The same machinery can strip a leading
greeting, parse the remaining clause, and re-attach the greeting as a discourse marker.

This is contained and honest — the parser only sees the clause it can handle — and it reuses tested
offset code rather than adding new heuristics. But it addresses **greetings only**, not imperatives,
so it is worth doing only if the model change leaves the greeting case broken.

## Step 4 — Do not build a rule cascade

Tempting and wrong. Post-hoc repair rules ("if root is a greeting VERB, demote to `discourse`";
"if root is a bare ADV with a verb-initial `advmod`, swap them") are whack-a-mole against a model:
each rule risks breaking a parse that was already correct, and the set only ever grows.

We have direct evidence of the failure mode: **the bug fixed in step 1 was itself caused by exactly
such a rule** — the trailing-vocative fallback, firing in a context its author never considered. One
targeted heuristic with a clear, tested boundary is defensible; a cascade of them is not.

## Step 5 — Fine-tune on annotated dialogue (the real fix)

A few hundred hand-corrected dialogue sentences would very likely resolve this whole class, because
the problem is domain, not capability.

**The annotation effort we are about to run is the corpus.** Several people will annotate several
scenarios for behavior-command placement (plan Phase 2.3). Their scripts are already analysed sentence
by sentence with the parse available per sentence, so a "this parse is wrong" affordance is a small
addition to the annotation loop rather than a separate project:

- surface the clause/role reading next to the sentence (Phase 1.7 rendering already does this)
- let an annotator flag a sentence and, optionally, correct the role assignment
- store corrections alongside the placement corpus, keyed the same way

That gives a gold set for evaluation immediately, and training data eventually. Sequence it as a
**by-product of Phase 2**, not as a prerequisite for it.

## Recommended order

1. ✅ **Step 1** — done. Our bug, fixed and regression-tested.
2. ✅ **Step 2** — run. `hdt_charlm` fixes the two annoying sentences but is a net regression, so the
   default stays `combined_charlm`. `SEMANTIC_UD_PACKAGE` remains as a knob.
3. **Next, and cheapest:** make `object_kind` treebank-robust (detect the dative object from
   morphological case rather than the `obl:arg` subtype). Improves things independently of any model
   choice.
4. **Then:** retry `combined_german-nlp-electra` somewhere HuggingFace is reachable; judge by the
   harness, and measure latency and memory, not only accuracy.
5. **Step 3 (greeting pre-normalisation)** only if the model work leaves the greeting case broken.
6. **Step 4** — actively avoid the rule cascade.
7. **Step 5** — build the parse-correction affordance into the Phase 2 annotation loop; treat
   fine-tuning as its later payoff.

Nothing here blocks Phase 2. The anchor inventory already covers 10 of 10 authored command positions
*with* these mis-parses present, which is what makes step 5 possible in the first place.

## What this episode is worth remembering for

Two sentences made a model switch look obviously right. The eval set said otherwise, and the eval set
was correct: the switch would have broken both defects Phase 1 exists to fix. The pinned `knownWeak`
cases also did their job — they flipped to failing, which is the signal to *read* them rather than a
reason to panic.

Cost of the discipline: one afternoon and a 200 MB download that is currently inert. Cost of skipping
it: a silent regression in the layer the whole placement plan depends on.
