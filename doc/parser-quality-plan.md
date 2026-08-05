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

Reproduce the whole comparison with the script added for it:

```bash
cd services/semantic-ud
python3 compare_packages.py "" hdt_charlm gsd_charlm combined_german-nlp-electra
```

```
package                        structural   exact S+V+O   known-weak   ms/sent
------------------------------------------------------------------------------
(stanza default)                      8/8          7/14          2/2      35.7
hdt_charlm                            7/8          4/14          0/2      33.0  <-- weak cases changed
gsd_charlm                            8/8          6/14          2/2      35.7
combined_german-nlp-electra    SKIPPED — needs the HuggingFace encoder, unreachable
```

**Decision: keep the Stanza default (`combined_charlm`).** It leads on both metrics. HDT trades the two
mis-parses for the `ccomp` sentence that Phase 1 was built to fix, and costs three exact-row matches.
`SEMANTIC_UD_PACKAGE` stays as a knob so the comparison is repeatable, not as a recommendation.

Latency is effectively identical across the charlm packages, so it is not a differentiator here — but
it is measured, because it will matter for a transformer.

### Correction: why HDT lost the dative object ✅ now fixed

The first write-up of this said HDT "does not label it `obl:arg`". **That was wrong.** HDT uses the
*same* `obl:arg` relation. What differs is where the morphological case ends up:

| | `dem` (DET) | `Kind` (NOUN) |
|---|---|---|
| `combined_charlm` | `Case=Dat` | `Case=Dat` |
| `hdt_charlm` | `Case=Dat` | *(no Case — only Gender, Number)* |

German marks case across the whole noun phrase, and treebanks disagree about whether it is propagated
to the head noun. `object_kind` read the head word's features only, so under HDT the dative was
invisible and the object degraded to `oblique`.

Fixed by `np_case_set()`, which unions the case features of the head **and its `det`/`amod`/`nummod`
dependents`**, and by rewriting `object_kind` so morphological case — not the relation subtype — is the
primary signal for direct vs indirect. The relation is now used only for what case cannot express
(clausal complements) and as the fallback for languages that do not mark case, such as English.

Effect: HDT structural 6/8 → **7/8**, exact-row 3/14 → 4/14, with the default unchanged at 8/8. The
dative object is now `indirect/obl:arg case=Dat` under *both* packages. This was worth doing on its own
merits regardless of which package we run — it removes a silent dependence on one treebank's
convention.

HDT's one remaining structural failure is the `ccomp` sentence, which it genuinely parses differently
(three clauses, root `Lass`). That is a parse difference, not a mapping bug.

### The transformer, measured ✅ — it wins, at 3× the latency

Run with the encoder cached locally:

```
package                        structural   exact S+V+O  xfail pass   ms/sent
------------------------------------------------------------------------------
(stanza default)                      8/8          6/12         0/2      33.1
hdt_charlm                            7/8          5/12         0/2      35.1
combined_german-nlp-electra           8/8          6/12         1/2     103.4
```

**`combined_german-nlp-electra` is strictly better on accuracy**: it ties the default on both accuracy
columns and additionally fixes the informal-greeting mis-parse — `Hallo ich bin Xenia.` comes out as
one clause with verb `bin`, which is the correct reading. It costs **3.1× the parse latency**.

Getting to that number required correcting two measurement faults, both of which had made the
transformer look *worse* than it is:

1. **The package override was applied to every language.** A German package handed to the English
   pipeline broke the English eval cases. `SEMANTIC_UD_PACKAGE` is now language-scoped —
   `de:hdt_charlm,en:gsd_charlm`, with a bare value applying to `SEMANTIC_UD_LANG` only. This was our
   bug, and it read as a model regression.
2. **`knownWeak` cases asserted the *wrong* answer.** They pinned the current mis-parse, so a model
   that produced the *correct* parse failed them, and the headline exact-row count dropped. They now
   assert the **correct** reading and are treated as expected failures: the harness reports how many
   now *pass*, and exact-row excludes them. A fix shows up as progress instead of as a regression.

Both faults pushed in the same direction — against the change — which is worth remembering: a
comparison that says "no improvement" deserves as much scrutiny as one that says "big win."

### Adopted: split by workload, chosen per request ✅

The parser package is now a **per-request** parameter rather than a per-process one, so one
`semantic-ud` instance serves both workloads. The service keeps a pipeline per
`(language, package)` pair.

- **Corpus / batch** — `./gradlew analyzeSemantics` requests
  `combined_german-nlp-electra` **by default**. Accuracy is the point and the run is offline; the whole
  charamel-embed project went from 0.4 s to 1.0 s. Override with `-PudPackage=<name>`, or
  `-PudPackage=` for the service default.
- **Interactive editing** — the editor sends no package and keeps the ~33 ms/sentence default, so the
  semantic panel stays responsive.
- **Android** — unaffected: the default is unchanged, and nothing requests a transformer there.
- **Whole-service default** — still available via `SEMANTIC_UD_PACKAGE=de:pkg` if a deployment wants
  one parser for everything.

Two hazards found while wiring this up, both fixed:

- **Stanza silently ignores an unknown package name** — it builds a pipeline and says nothing. A typo
  in `-PudPackage=` would therefore have produced default-quality output while the corpus provenance
  claimed the transformer. The service now validates the name against its resources index, warns, and
  reports the **effective** package; provenance records what actually ran, plus
  `udPackageRequested` and a document-level warning when the two differ. Verified with a deliberate
  typo.
- **A latent deadlock in the fallback path.** The unavailable-package handler recursed into
  `get_pipeline` while holding the non-reentrant registry lock; it would have hung whenever the
  fallback pipeline was not already cached. Now calls `build_pipeline` directly.

`de-weak-01-informal-greeting` still asserts the correct reading and remains an expected failure,
because the *default* parser still fails it. Under the transformer it passes — visible as
`xfail pass 1/2` in `compare_packages.py`. Promote it if the transformer ever becomes the global
default.

### If you need to reproduce this from scratch

`combined_german-nlp-electra` is the genuinely different lever: same treebank, transformer encoder
instead of a character LM. Its Stanza weights are already downloaded
(`~/stanza_resources/de/{pos,depparse}/combined_german-nlp-electra.pt`, ~200 MB) but **inert**, because
building the pipeline also fetches the encoder from HuggingFace:

```
OSError: We couldn't connect to 'https://huggingface.co' to load the files
```

The missing piece is exactly one HF repo: **`german-nlp-group/electra-base-german-uncased`**.

On a machine or network where HuggingFace is reachable:

```bash
# 1. populate the HF cache (~/.cache/huggingface) — a few hundred MB
python3 -c "from transformers import AutoModel, AutoTokenizer; \
  n='german-nlp-group/electra-base-german-uncased'; \
  AutoTokenizer.from_pretrained(n); AutoModel.from_pretrained(n)"

# 2. run the comparison and paste the table back
cd services/semantic-ud
python3 compare_packages.py "" combined_german-nlp-electra
```

Alternatives if the network is the obstacle rather than the machine: set `HF_ENDPOINT` to an internal
mirror if DFKI runs one, or copy `~/.cache/huggingface` from a machine that already has the model.

The encoder is ~1.3 GB in the HuggingFace cache and the Stanza weights ~200 MB; once both are present
the run is fully offline (`HF_HUB_OFFLINE=1`).

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
3. ✅ **`object_kind` made treebank-robust** via `np_case_set()` — case is read across the noun phrase,
   not just off the head word, and drives the direct/indirect decision instead of the relation
   subtype. Improved HDT from 6/8 to 7/8 structural with no change to the default.
4. ✅ **Transformer measured.** `combined_german-nlp-electra` is strictly better on accuracy and fixes
   the informal-greeting mis-parse, at 3.1× latency. **Adopt it for corpus/batch work, keep the
   default for interactive editing and Android** — see the split above. Two measurement faults had to
   be fixed first, both of which had hidden the improvement.
5. **Step 3 (greeting pre-normalisation)** only if the model work leaves the greeting case broken.
6. **Step 4** — actively avoid the rule cascade.
7. **Step 5** — build the parse-correction affordance into the Phase 2 annotation loop; treat
   fine-tuning as its later payoff.

Nothing here blocks Phase 2. The anchor inventory already covers 10 of 10 authored command positions
*with* these mis-parses present, which is what makes step 5 possible in the first place.

## What this episode is worth remembering for

It cut both ways, which is the useful part.

**Against a change that looked right.** Two sentences made `hdt_charlm` look like an obvious win. The
eval set disagreed and was correct: it would have broken the `ccomp` defect Phase 1 exists to fix.

**In favour of a change that looked wrong.** The transformer's first measurement said *worse* — and
that was two faults of our own: a package override leaking across languages, and expected-fail cases
pinned to the wrong answer so that fixing them scored as a regression. Both errors pushed the same
way, against the change.

So the rule is not "trust the harness over your intuition". It is **make the harness measure the thing
you actually mean**, then trust it. A comparison reporting "no improvement" deserves the same scrutiny
as one reporting a big win — especially when the metric contains cases you deliberately pinned to a
wrong answer.

Two concrete guardrails came out of it, both now in the tooling: expected-fail cases assert the
*correct* reading and are counted separately, and package overrides are language-scoped.
