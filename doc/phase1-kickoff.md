# Phase 1 kickoff — fine-grained sentence and constituent annotation

Written 2026-08-05, at the end of Phase 0. Start here in a fresh session; everything needed to begin
is on this page. Plan: `doc/behavior-command-placement-learning.md` (Phase 1).

Phase 0 is committed as `24702095`. Its verified state, plus the two defects Phase 1 exists to fix,
are the whole of the context you need.

---

## 1. Why Phase 1 exists — two defects, both reproduced

Start the UD service, then reproduce these. They are the acceptance criteria.

```bash
./gradlew :services:semantic-ud:startService     # or: cd services/semantic-ud && \
                                                 #   SEMANTIC_UD_AUTO_DOWNLOAD=false python3 server.py
curl -s http://127.0.0.1:4061/health | jq        # expect loaded: ["de"] or ["de","en"]
```

**Defect A — the indirect object is silently dropped, and spans are head-token-only.**

```bash
curl -s -X POST http://127.0.0.1:4061/analyze -H 'Content-Type: application/json' \
  -d '{"text":"Ich gebe dem Kind den roten Ball.","language":"de"}' \
  | jq '.annotations[0].basic'
```

Returns `subject=Ich`, `verb=gebe`, `object=Ball`. Two things wrong: `dem Kind` (the dative indirect
object) is **absent entirely**, and the object span is the head token `Ball`, not the phrase
`den roten Ball`. Cause: `select_object` (`services/semantic-ud/server.py`) returns the *first*
match by relation priority and is never asked for a second; `word_span` maps a single token.

**Defect B — roles are mixed across clauses.**

```bash
curl -s -X POST http://127.0.0.1:4061/analyze -H 'Content-Type: application/json' \
  -d '{"text":"Lass mich einen Vorschlag machen wie wir zusammen den Nachmittag gestalten.","language":"de"}' \
  | jq '.annotations[0].basic'
```

Returns `subject=wir` — from the *subordinate* clause — paired with `verb=machen` from the main
clause. One flat role set per sentence, no clause segmentation, so **there is no constituent a
command could be anchored to.** That is the blocker for Phase 3: the placement label space
(`before-object-phrase(direct)` in `clause 1`, …) cannot be derived from a flat sentence.

Two further real-corpus weaknesses seen while verifying Phase 0, worth eval cases (1.6):

- `Hallo ich bin Xenia.` → `verb=Hallo`, confidence 0.96. Informal German greeting parsed as a verb.
- `Hey, Ich habe eine Aufgabe für Dich.` → `subject=Ich` **and** `address=Ich`, the same span in two
  roles. The vocative is `Hey`, if anything.

---

## 2. What Phase 0 already gives you

- **`UtteranceProjection`** (`core/src/main/java/de/dfki/vsm/model/scenescript/`) — clean text,
  bidirectional offset map, command gap indices. 14 tests in
  `core/src/test/java/de/dfki/vsm/model/scenescript/UtteranceProjectionTest.java`.
- **`POST /api/v1/projects/{pid}/semantic/analyze-script`** and the HTTP-free
  `WebUiServer.analyzeScriptSemantics(pid, options)`.
- **`remapSpansDeep`** in `WebUiServer` rewrites *any* `{from,to}` pair at any depth from clean-text
  to script coordinates. **New nested spans added in Phase 1 are remapped automatically** — no
  change needed there, which is why it was written as a generic walk.
- **`./gradlew analyzeSemantics -PvsmProject=<dir>[,<dir>] [-Ppersist=true]`** — headless, parses
  without launching plugins, does not dirty project.xml. (`-Pproject` is reserved by Gradle.)
- **`POST /analyze/batch`** on semantic-ud, for many sentences per round trip.

Quick end-to-end loop while working on Phase 1:

```bash
./gradlew analyzeSemantics -PvsmProject=plugins/charamel-embed/ExampleProject
# expect: sentences=11 annotations=11 commands=10
```

---

## 3. Where the work goes

Almost all of 1.1–1.4 is in **`services/semantic-ud/server.py`**. Current shape:

| Function | Line ≈ | Role in Phase 1 |
|---|---|---|
| `select_subject` / `select_verb` / `select_object` / `select_address` / `select_predicate` | 200–360 | become **per-clause** selectors |
| `word_span` | 940 | head span only — needs a phrase/subtree sibling |
| `modifier_spans`, `comparison_modifiers` | 500–630 | already subtree-ish; reuse the traversal |
| `build_annotation` | 810 | currently emits one flat `basic`; must emit `clauses[]` |
| `analyze` | 1015 | per-sentence entry; `analyze_batch` wraps it |

Suggested order, each independently verifiable with the curl commands above:

1. **Clause segmentation (1.1).** Partition each sentence's words into clauses from the UD tree:
   `root` plus every `conj`, `advcl`, `ccomp`, `xcomp`, `acl`, `parataxis` head becomes a clause
   root; each word belongs to its nearest clause-root ancestor. Emit
   `clauses: [{id, from, to, type: main|subordinate|relative|coordinate, roles: {…}}]`.
   Fixes defect B: run the existing selectors *within* a clause's word set.
2. **Multiple objects (1.2).** Collect *all* object-ish dependents of the clause verb rather than the
   first: `objects: [{kind: direct|indirect|prepositional|clausal, head, phrase, case, preposition,
   confidence}]`. `kind` from deprel + morphological case (`obj`→direct, `iobj`/dative `obl`→indirect,
   `obl` with a `case` child→prepositional, `ccomp`/`xcomp`→clausal). Fixes defect A.
3. **Phrase spans (1.3).** For every role, add the full subtree span alongside the head span, clipped
   to its clause. `den roten Ball`, not `Ball`. Commands sit at phrase boundaries.
4. **Anchor-slot inventory (1.4).** Derive per utterance the candidate slot list — `utterance-initial`,
   `before-`/`after-` each role phrase, `clause-boundary`, `before-final-punct`. **This is the label
   space Phase 3 predicts over**, so it must be deterministic and stable; it is the real deliverable
   of Phase 1.

Then:

5. **Schema v3 (1.5).** `clauses` + `objects` added; keep the flat `basic` block as a v2 projection so
   nothing downstream breaks at once. Update `doc/semantic-annotations-v3.md` and
   `doc/semantic-analysis-current.md`.
6. **Eval cases (1.6).** Extend `services/semantic-ud/references/eval-cases.json`; run with
   `./gradlew :services:semantic-ud:evaluateMapping`. Include both defect sentences above, the two
   weaknesses in §1, and imperatives/fragments/vocatives — informal German is where this is weakest.
7. **Rendering (1.7)** and, bundled with it, **switching the Web UI to `analyze-script`** (the one
   Phase 0 item deferred). Both touch `App.svelte` / `ScriptEditor.svelte` and both depend on v3.
   `runSemanticAnalysis()` in `App.svelte` is the loop to replace; `extractSemanticSentenceUnits`,
   `shiftOffsetsDeep` and `mergeSentenceAnnotationLayers` become dead once it is gone.

---

## 4. Decisions already made — don't re-litigate

- **The taxonomy is pinned to NEUROGES 7-category** and its value names are verbatim. See
  `doc/behavior-taxonomy-neuroges.md`; `BehaviorTaxonomyTest` enforces it.
- **Placement labels are structural slots, not token offsets.** That is what makes ~26 authored
  examples generalisable.
- **A command's position is its gap index** (tokens preceding it). Adjacent commands share one gap —
  correct and tested.
- **`ActionObject.getLower()/getUpper()` span the actor-qualified name** (`time: init`), not the
  bracket. Verified, documented, easy to mistake for an off-by-something.
- **Selection ("which command") stays out of scope.** Phase 1 serves placement only.

## 5. Cautions

- **The JFlex/CUP lexer has no error recovery.** A script that fails to parse yields zero scenes, so
  `analyze-script` returns 422 rather than falling back. Don't reintroduce a regex fallback: that is
  the defect Phase 0 removed.
- **Don't call `RunTimeProject.parse()` for read-only work without
  `setPersistGeneratedUUID(false)`** — it writes a uuid into project.xml.
- **Stanza pipelines are not thread-safe.** Keep parsing inside `pipeline_lock(language)`.
- **`stats.commands` in the response is a useful invariant** — it must keep matching the
  `ActionObject` count in the stored XML (10 / 4 / 0 for charamel-embed / charamel-ws / IntakeInterview).
