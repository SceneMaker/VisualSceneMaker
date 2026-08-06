# Learning Behavior-Command Placement in Scene Script Utterances

Plan created: 2026-08-04. **This file is the canonical plan** — it lives with the code it describes
so it is reviewed and versioned alongside it.

Related documents:

- `doc/behavior-taxonomy-neuroges.md` — the NEUROGES®-based behavior taxonomy (Phase 2.2, done)
- `doc/semantic-analysis-current.md` — what the semantic analysis does today
- `doc/semantic-annotations-v3.md` — the stored annotation format (Phase 1.5 raises it to v3)
- `~/Code/Repo/xenia-animation/` — figure-specific animation specs (German, for Charamel)

Status at a glance (2026-08-05): **Phases 0, 1 and 2 complete.** Phase 3 (placement service) and
Phase 4 (authoring UX) not started.

Phase 2 deliverables: the NEUROGES®-based taxonomy with a typed loader and 19 guard tests (2.2/2.2a);
`./gradlew extractCorpus` producing the JSONL corpus (2.1); `./gradlew corpusStats` (2.4) and
`./gradlew corpusAgreement` (2.3, gates Phase 3).

### Corpus can be grown from existing projects ✅ (2026-08-05)

`/Users/gebhard/Code/Temp/EmmaAgent` (older charamel-ws-based project, 271 utterances, 30 placements)
extracts cleanly and **more than quadruples the co-speech corpus**: combined with the three in-repo
example projects it gives **308 sentences, 44 placements, 38 co-speech, 2 annotators, 4 scenarios**.
29 of its 30 commands classify against the existing taxonomy — the plugin class is the same
`charamelWs.charamelWsExecutor` we already tagged. The one that does not is `event`, an SSJ sensor
command, which is correctly not a behavior. Four of its plugins are absent from this repo and log
`ClassNotFoundException` during parse; harmless, since analysis never launches plugins.

**Do this before Phase 3.** Any older VSM project using a tagged plugin is usable corpus input at
essentially no cost, and the first real signal only appeared at this size:

| Function | anchor slot | n |
|---|---|---|
| emblem/social convention | after-subject | 10 |
| emotion/attitude | after-predicate | 4 |
| emotion/attitude | before-predicate | 4 |
| emotion/attitude | after-subject | 3 |

Emblems (`wave`, `nod`) cluster sharply at `after-subject`; emotions spread. That is the kind of
regularity Phase 3 is meant to learn, and it was invisible at 9 placements.

### Finding that changes a Phase 3 assumption: placements are not always boundary-aligned

**7 of EmmaAgent's 30 placements (23%) sit at no anchor slot at all** — not because the inventory is
incomplete at the boundaries, but because the command sits *inside* a phrase. Examples:

- `Ich wünsche ihnen eine gute Nacht!` — command at token 5, i.e. within the object phrase
  `eine gute Nacht` (tokens 3–5). Slots exist at 0,1,2,3,6.
- `Wunderbar [smile] Vielen Dank!` — token 1, inside a two-word fragment.

The anchor inventory offers **constituent boundaries** by design, which is what makes a slot label
generalisable. Authors evidently also place commands mid-phrase, plausibly on the accented word.
Phase 3 must decide explicitly, and this is a modelling decision rather than a bug:

1. predict boundaries only and **snap** a mid-phrase placement to the nearest slot when training
   (loses information, keeps the label space small);
2. add intra-phrase positions to the inventory (much larger label space, thinner counts);
3. treat mid-phrase placements as a separate `accented-word` affiliate, which the taxonomy already
   names for `emphasis-baton`, and predict them from prosodic prominence rather than syntax.

Option 3 is the most faithful and needs a prominence model we do not have; option 1 is the pragmatic
start. Either way the corpus records the true token index, so the choice is revisable without
re-extracting. **Do not silently drop these 23%.**

**What the corpus says today, and it decides Phase 3's design:** 37 sentences, 14 placements, of which
**8 co-speech**, spread over a single Function value (`emotion/attitude`), with only 3.7% of 377
offered anchor slots used. That is far too little to fit a distribution, so Phase 3.2 must lead with
the hand-written prior and NEUROGES axis back-off, and any accuracy number measured on this corpus is
a smoke test rather than a result. The agreement report is built and verified but has nothing to
compare until a second annotator exists — which is the real blocker on Phase 3, not code.

Also open, and worth settling before annotation starts: the taxonomy does not cover **core runtime
commands**. `[pause duration='50']` is the one placement the extractor cannot classify, and authors
use built-ins inline constantly.

Phase 1 deliverables: per-clause roles with head *and* phrase spans; all objects per clause with
kind/case/preposition; the anchor-slot inventory that is the label space Phase 3 predicts over;
schema v3 (`doc/semantic-annotations-v3.md`); eval cases covering both fixed defects, four clause
types, and three *pinned* upstream mis-parses. Measured: **10 of 10 authored commands in
`plugins/charamel-embed/ExampleProject` land exactly on a structural slot**, and the eval harness
reports 8/8 structural checks passing.

Phase 0 deliverables, all verified against the real example projects:
`UtteranceProjection` (core, 14 tests) · `POST …/semantic/analyze-script` ·
`WebUiServer.analyzeScriptSemantics()` (HTTP-free, so batch work can call it) ·
`SemanticAnalyzeCli` + `./gradlew analyzeSemantics` · a threaded `semantic-ud` with startup preload,
fail-fast on missing models and a `/analyze/batch` endpoint.

One item from 0.1 is deliberately deferred: **the Web UI still calls the old per-sentence endpoints.**
Switching it is bundled with 1.7 (rendering), because Phase 1 raises the annotation schema to v3 and
both changes touch the same components.

## Goal

A service that suggests **where** behavior commands (facial expression, gesture, gaze, background,
pause, …) belong inside a turn utterance, learned from the placements authors actually make,
expressed relative to the **semantic structure** of the utterance.

Two subproblems, deliberately separated:

1. **Placement** — given an utterance and a behavior category, which structural position?
   *This is the plan's subject.*
2. **Selection** — which concrete command/parameters? Out of scope for v1; the author keeps
   choosing the command (via `InsertActionDialog`/`ActionForm`), the service proposes the slot.

Learning mode (decided 2026-08-04): **per-project, adapting as the author writes**, with a
hand-written prior for back-off. No global model to train or ship in v1.

## Investigation findings (2026-08-04, verified)

### Semantic analysis in a server environment: parsing yes, pipeline no

- `services/semantic-ud/server.py` is headless Python/Stanza, no UI dependency. Started on port
  4071 with `SEMANTIC_UD_AUTO_DOWNLOAD=false`; DE+EN models already in `~/stanza_resources`;
  `/health` up in 1s; `/analyze` worked.
- `WebUiServer` already calls it server-side: `analyzeSemanticWithUd`
  (`core-webserver/src/main/java/de/dfki/vsm/web/WebUiServer.java:7032`), plus an LLM proxy at
  `/api/v1/llm/generate`.
- **The pipeline itself is browser-side.** `runSemanticAnalysis()`
  (`editor/web-ui/src/App.svelte:6663`) does sentence-unit extraction, the per-sentence loop,
  offset shifting (`shiftOffsetsDeep`) and layer merging (`mergeSentenceAnnotationLayers`). The
  server exposes only per-call primitives (`/semantic/syntax`, `/semantic/analyze`). There is no
  way today to analyse a whole project's script from a batch job — which corpus building needs.
- Server-env blockers in `semantic-ud`: single-threaded `HTTPServer` (`server.py:1031`), pipeline
  loaded lazily per language, no batch endpoint, model dir handling only via env var.

### Two live defects that would poison a training corpus

- **Inline commands reach the parser.** `extractSemanticSentenceUnits`
  (`App.svelte:2359`) takes the raw text after the `:` — brackets included. Same utterance with
  vs. without commands parses differently:
  - `[background …] Hallo [emotion …] ich bin Xenia.` → `verb=bin (0.82)`, `predicate=Xenia`
  - `Hallo ich bin Xenia.` → `verb=Hallo (0.96)`, `object=Xenia`

  The label (command position) perturbs the features it is supposed to be predicted from.
- **The sentence splitter cuts commands in half.** `/[^.!?]+[.!?]+|[^.!?]+$/` splits at a
  parameter's decimal point. Verified: `Schön [emotion type='happy' intensity='0.8'] dass Du da
  bist.` → `["Schön [emotion type='happy' intensity='0.", "8'] dass Du da bist."]`.

### Object annotation is head-token-only and sentence-flat

- `Ich gebe dem Kind den roten Ball.` → `object = "Ball"` only. The indirect object `dem Kind` is
  dropped (`select_object`, `server.py:269`, returns the *first* match and never a second); the
  span is the head token, not the phrase `den roten Ball`.
- `Lass mich einen Vorschlag machen wie wir zusammen den Nachmittag gestalten.` →
  `subject=wir` (subordinate clause) paired with `verb=machen` (main clause), `object=mich`. One
  flat role set per sentence, no clause segmentation — so there is no constituent for a command
  to be anchored to.

### Corpus and vocabulary

- **26 authored `ActionObject`s repo-wide**: charamel-embed ExampleProject 10, yallah TestProject
  6, studymaster-web 6, charamel-ws ExampleProject 4. Enough for rule induction and few-shot, not
  for a trained model. Several annotators across several scenarios are expected soon — the corpus
  format must be multi-author/multi-scenario from day one.
- **Structure is ideal for extraction**: `SceneUttr` (`core/.../scenescript/SceneUttr.java`)
  interleaves `SceneWord`/`SceneAbbrev`/`SceneParam`/`ActionObject` in one word list, every
  `ScriptEntity` carrying exact `mLower`/`mUpper` char offsets. Command position is directly
  recoverable, and `getCleanText()` already yields the words-only projection.
- **Label vocabulary is uneven**: commands are declared per plugin as typed `PluginCommand`s
  (`core/.../model/plugin/PluginCommand.java`). charamel-ws declares ~60 behavior commands but
  bare — no params, no `uiCategory`, no descriptions. charamel-embed's 4 are richly described with
  `uiCategory`. A behavior taxonomy over these is a required artifact, not a nice-to-have.

## Core representation

Three decoupled layers plus a label:

- **Text layer** — `cleanText`: the utterance with commands removed (what UD parses), with a
  bidirectional `cleanOffset ↔ scriptOffset` map.
- **Semantic layer** — clauses, roles, and head *and* phrase spans over `cleanText`.
- **Command layer** — each `ActionObject` with its position in the clean-token stream (gap index)
  and clean-text char offset.
- **Placement label** — the command's position expressed structurally, not numerically:
  `(clauseId, anchorSlot)` where a slot is e.g. `utterance-initial`, `before-subject-phrase`,
  `after-verb`, `before-object-phrase(direct)`, `clause-boundary`, `before-final-punct`.

A training example is therefore: *structural context → (behavior category, anchor slot)*.
Anchoring on slots rather than token indices is what lets 26 examples generalize.

## Phase 0 — Make the analysis server-runnable and command-aware

Prerequisite for everything else; also fixes both live defects.

- **0.1 Move the pipeline server-side.** New `POST /api/v1/projects/{pid}/semantic/analyze-script`
  in `WebUiServer` performing unit extraction + per-sentence loop + merge, returning the v2/v3
  document. Frontend calls it once; existing per-sentence endpoints stay for incremental use.
- **0.2 Drive it from the parsed `SceneScript`, not line regexes.** The server already has the
  parsed model (`ensureScriptLoaded`); iterate `SceneObject → SceneTurn → SceneUttr`. This
  removes the sentence-splitter defect by construction and yields real command boundaries.
- **0.3 Clean-text projection + offset map.** Per `SceneUttr`, build `cleanText` (words +
  punctuation, commands removed) and the bidirectional offset map; send `cleanText` to UD; map
  spans back to script offsets for rendering. Removes the parse-pollution defect.
- **0.4 Harden `semantic-ud` for server use.** `ThreadingHTTPServer`; preload pipelines at
  startup (fail fast when models are missing); add `POST /analyze/batch` taking many sentences in
  one call (per-call warmup dominates corpus runs); document the offline model-dir contract.
- **0.5 Headless entry point.** A Gradle task / runtime-server flag that analyses a project's
  script with no browser and writes `semantic-annotations.json` — the corpus tool's entry point.

*Done when*: `doc/IntakeInterview` and `plugins/charamel-embed/ExampleProject` analyse headless
and match browser output modulo the two fixed defects.

## Phase 1 — Fine-grained sentence and constituent annotation

> **Starting Phase 1? Read `doc/phase1-kickoff.md` first.** It has both defects as runnable curl
> commands, the file-and-line map of `semantic-ud/server.py`, a suggested order, and the decisions
> not to re-litigate.

- **1.1 Clause segmentation.** Split each sentence via UD (`root` plus `conj`, `advcl`, `ccomp`,
  `xcomp`, `acl`, `parataxis`): `clauses: [{id, from, to, type: main|subordinate|relative|coordinate, roles:{…}}]`.
- **1.2 Multiple objects.** Replace the single `object` with
  `objects: [{kind: direct|indirect|prepositional|clausal, head, phrase, case, preposition, confidence}]`;
  likewise allow one subject per clause.
- **1.3 Phrase spans for every role.** Head span *and* full subtree span (determiner, modifiers,
  PP attachment), clipped to the clause. Commands sit at phrase boundaries, not head tokens.
- **1.4 Anchor-slot inventory.** Derive, per utterance, the list of candidate anchor slots (the
  label space of Phase 3) from clause and phrase boundaries.
- **1.5 Schema v3.** `clauses` + `objects` added; the flat `basic` block retained as a v2
  projection for backward compatibility. Update `doc/semantic-annotations-v3.md` and
  `doc/semantic-analysis-current.md`.
- **1.6 Eval cases.** Extend `services/semantic-ud/references/eval-cases.json` with clause and
  multi-object DE/EN gold cases, including the two sentences that fail today.
- **1.7 Rendering.** Clause bracketing, per-object-kind colors, head-vs-phrase distinction in the
  semantic panel.

## Phase 2 — Corpus and annotation infrastructure

Built for several annotators × several scenarios from the start.

- **2.1 Placement-corpus extractor. ✅ DONE (2026-08-05).** `CorpusExtractCli` /
  `./gradlew extractCorpus`. Records carry the full anchor inventory (the negatives) alongside the
  placements actually made. Running it surfaced four defects — see the commit. Original spec:
  JSONL, one record per utterance:
  `{project, scenario, annotator, scene, turn, speaker, language, cleanText, structure, commands: [{name, actor, params, category, anchor:{clauseId, slot, role, side, tokenIndex, charOffset}}], analysisVersion}`.
  Deterministic and versioned so records can be re-derived when the analysis changes.
- **2.2 Behavior taxonomy — NEUROGES®-based. ✅ FIRST PASS DONE (2026-08-04).**
  `core/src/main/resources/behavior-taxonomy.json`: 7 pinned categories with authoritative values,
  orders, axis semantics and per-value reliabilities; 7 supplementary categories; all 65 behavior
  commands tagged and validated. See `doc/behavior-taxonomy-neuroges.md`.
  Key results feeding later phases: six Function values have **zero** command coverage — egocentric
  direction, pantomime, form/spatial-relation/motion-quality presentation (the representational
  branch utterance semantics most determines, all five requested as new animations) plus
  object-oriented action (deliberately not requested: a seated propless character has nothing to act
  on); 12 commands are rest/pose so no Function applies; only `hairback` is a genuine
  self-regulatory action. Remaining: ~40 assets need video coding before any tag is NEUROGES-grade
  (56 of 65 are `evidence: inferred-from-name`).
- **2.2a Typed loader + regression test. ✅ DONE (2026-08-04).**
  `core/src/main/java/de/dfki/vsm/model/behavior/` — `BehaviorTaxonomy` (classpath-loaded, cached),
  `TaxonomyCategory` (incl. `neighboursOf()` for polar back-off, `mainGroupOf()`),
  `BehaviorChannel` (three-valued NEUROGES scope), `BehaviorTag` (distinguishes no-unit /
  not-applicable / undetermined; `isCoSpeech()`, `isNeurogesGrade()`). Java 17, org.json only.
  `BehaviorTaxonomyTest` (15 tests) pins the manual's value sets and axis orders, cross-checks
  coverage against every declared plugin command in both directions, validates Type-under-Function,
  and asserts no tag claims video-coded evidence yet — that last one flips deliberately when the
  first Xenia clips land.
- **2.3 Multi-annotator agreement report. ✅ BUILT, awaiting data (2026-08-05).**
  `./gradlew corpusAgreement -Pcorpus=<jsonl>[,<jsonl>]`. With one annotator it says so plainly
  instead of inventing a number. Verified against a synthetic second annotator: it reports per-pair
  observed agreement, Cohen's κ, exact-set match, and **Jaccard over slots anyone used** — the figure
  to lead with, because κ across all offered slots is dominated by the ~96% nobody touches and so
  measures blank space rather than agreement. Original spec: same scenario authored by several people
  → agreement
  on (category, anchor slot). Calibrate expectations against NEUROGES's own trained-rater figures
  (modified κ 0.34–0.62 on Module I; Function is the harder layer) — see
  `doc/behavior-taxonomy-neuroges.md` §7. **This gates Phase 3**: if two authors disagree systematically,
  placement is a style model, not a shared model, and the per-project design is confirmed
  (or the categories need revising).
- **2.4 Corpus stats CLI. ✅ DONE (2026-08-05).** `./gradlew corpusStats -Pcorpus=<jsonl>`.
  Function × anchor-slot cross-tab (the distribution a model would fit), base rate, sparsity, and a
  verdict. Current corpus: 37 sentences, 14 placements, **8 co-speech**, base rate 3.7% of offered
  slots used, one Function value populated (`emotion/attitude`). Verdict: far too few to fit a
  distribution — use the hand-written prior with axis back-off, and treat anything measured on this
  corpus as a smoke test.

## Phase 3 — Placement service (per-project adaptive)

**Status (2026-08-06): 3.1, 3.2, 3.3 and 3.5 implemented; 3.4 not started.** Two deviations from
what is written below, both deliberate:

- **Not a `services/` module.** The text asks for a standalone Javalin service that also "runs
  in-process with VSM"; those are contradictory, and a separate process would need its own copies of
  `BehaviorTaxonomy` and the corpus plumbing. The model is plain Java in
  `core/model/behavior/placement/` (Java 17, Android-clean, no Javalin) and the endpoints are in
  `core-webserver`, exactly as the taxonomy already is.
- **The prior keys on `affiliate`, not on the names below.** `emotion-intrinsic`,
  `pointing-deictic` and `emphasis-baton` match no field in the taxonomy. `affiliate` —
  `referent | rheme | accented-word | clause | whole-utterance | none` — is described in
  `vsmFields` as "the bridge to the anchor slots of the placement service" and is present on 58 of
  66 tagged commands against Function's 24. Function remains the empirical conditioning variable
  and the axis the back-off runs along.

Endpoints (all under `/api/v1/projects/{pid}/placement/`): `GET model`, `POST suggest`,
`POST observe`, `POST sync`. State is `behavior-placement.json` in the project root.

3.3 is driven from the analysis rather than from individual editor events: the analysis is what
knows each command's anchor slot, and `sync` resolves slots through the *same* code the corpus
extractor uses, so the model cannot be trained on a labelling that differs from the one it is
evaluated against. Sending the whole document makes it idempotent and lets a deleted command
withdraw its evidence.

- **3.1 New module `services/behavior-placement`.** Java + Javalin, mirroring
  `services/semantic-analysis` so it can run in-process with VSM and stay Java 17-clean for
  Android; heavy NLP stays in `semantic-ud`. Endpoints: `POST /suggest` (structure + categories →
  ranked anchors with confidence), `POST /observe` (an authored placement), `GET /model`
  (inspectable per-project statistics), `GET /health`.
- **3.2 Model v1 — interpretable, backed off.** `P(anchor slot | NEUROGES function, clause type,
  dialogue act, position in turn)` estimated from the project's own corpus, backing off **along
  the NEUROGES polar axes** (to the neighbouring Focus/Function value) and then to a hand-written
  prior (emotion-intrinsic → before the rheme / after the address; pointing-deictic → onto the
  referent phrase; emphasis-baton → onto the accented word of the rheme; stage → utterance-initial;
  pause → clause boundary). Axis back-off is what makes 26 examples usable. Only commands with
  `cospeech: true` are candidates. Predict a **stroke target**, and let the runtime offset the
  trigger by the gesticon's `preparation-end` (see `doc/behavior-taxonomy-neuroges.md` §6).
  Persisted per project as `behavior-placement.json` next to `semantic-annotations.json`.
- **3.3 Online update.** Every authored/edited/deleted command in the script editor emits
  `observe`; the project model shifts immediately. This is the "adapts as you author" behavior.
- **3.4 Optional LLM second opinion.** Via the existing `/api/v1/llm/generate`, constrained to
  emit *only* slots from the Phase 1.4 inventory (no free text), for contexts the frequency model
  has never seen. Provenance-tagged so suggestions stay attributable.
- **3.5 Evaluation.** Leave-one-utterance-out; metrics: exact-slot accuracy, ±1-token tolerance,
  category-conditioned top-3. Baselines: always-utterance-initial, uniform-random slot,
  prior-only. Report n honestly — with 26 examples this is a smoke test, not a result.

## Phase 4 — Authoring UX

- **4.1 Ghost markers** in `ScriptEditor` at suggested anchors; accept inserts via the normal
  action path, dismiss is also signal — both feed `observe`.
- **4.2 "Suggest behaviors for this turn"** reusing `InsertActionDialog`/`ActionForm` so
  parameters stay author-controlled.
- **4.3 Feedback-loop guard.** Corpus records carry `origin: authored|accepted-suggestion`; the
  model trains on authored placements and accepted ones only with a discount, never on its own
  unreviewed output.
- **4.4 Docs.** Update `editor/web-ui/public/scenescript-help.html` (+ version pill) and
  `doc/semantic-analysis-current.md`.

## Risks

- **Tiny corpus.** Mitigated by structural (slot-based) labels, hierarchical back-off, and a
  hand-written prior that works at n=0.
- **Parser quality on informal/imperative German.** Already noted in
  `doc/semantic-analysis-current.md` §8; clause segmentation raises exposure. Eval cases must
  include imperatives, fragments and vocatives.
- **Author style variance.** Measured by 2.3 before any model is built.
- **Feedback loop.** Guarded by 4.3.
- **Scope creep into command selection.** Keep "which command" out of v1.
- **JFlex/CUP lexer fragility** (`project_jflex_lexer_fragility`) — Phase 0.2 leans on the parsed
  `SceneScript`, so scripts that fail to parse need a graceful degradation path.

## Recommended sequencing

Phase 0 and 1 first — they are prerequisites, they fix two live defects, and they are valuable on
their own. Phase 2 next, because it is what makes the coming multi-person annotation effort
usable. Phases 3–4 once the 2.3 agreement numbers exist.
