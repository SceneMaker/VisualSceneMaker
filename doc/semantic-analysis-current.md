# VSM Semantic Analysis (Current State)

Last updated: 2026-02-12

This document describes what the VSM semantic analysis can do today in the Web UI and backend. It is intended as a living document and should be extended as features evolve.

## 1. Scope

The current semantic workflow supports:

- Basic semantic roles (grammatical layer):
  - `subject`
  - `verb`
  - `object`
  - `predicate` (copular subject extension / predicative complement)
  - `address` (vocative/addressee)
- Address phrase structure (grammatical layer):
  - `addressPhrase.anchor` (typically `du`)
  - `addressPhrase.head` (address noun head, for example `Schlampe`)
  - `addressPhrase.modifiers[]` (for example `hässliche`, with `pos=adjective`)
- Role-linked modifiers:
  - adjective modifiers (`adjective`)
  - adverb modifiers (`adverb`)
  - extracted for `subject`, `object`, `predicate`, and `address` where available
- Pragmatic/discourse layer (LLM):
  - `dialogueAct`
  - `themeRheme`

## 2. Architecture

### 2.1 Services

- UD/Stanza service (`services/semantic-ud/server.py`)
  - endpoint: `POST /analyze`
  - provides grammatical analysis (S/V/O/address + modifiers)
- Web UI backend (`core-webserver/.../WebUiServer.java`)
  - exposes project semantic API:
    - `GET /api/v1/projects/{pid}/semantic`
    - `PUT /api/v1/projects/{pid}/semantic`
    - `POST /api/v1/projects/{pid}/semantic/syntax` — one sentence, caller-supplied text
    - `POST /api/v1/projects/{pid}/semantic/analyze` — one text, caller-supplied
    - `POST /api/v1/projects/{pid}/semantic/analyze-script` — **whole script, server-driven**
  - merges UD and LLM results when needed
- Headless entry point (`core-webserver/.../SemanticAnalyzeCli.java`, `./gradlew analyzeSemantics`)
  - analyses one or more projects with no browser and no plugin launch
- Web UI (`editor/web-ui/src/App.svelte`, `ScriptEditor.svelte`)
  - semantic panel, analysis controls, rendering, debug view

### 2.2 Data flow (high level)

Since 2026-08-05 the pipeline is **server-side** (`analyze-script`). Units come from the *parsed*
`SceneScript`, not from regexes over raw text:

1. Traverse `SceneObject → SceneTurn`, and group each turn's `SceneUttr`s into sentences —
   consecutive utterances merge until one ends in a sentence-final mark, because the script grammar
   ends an utterance at *any* punctuation including commas.
2. Per sentence, build a `UtteranceProjection`: the spoken text with all inline behavior commands
   removed, plus a bidirectional offset map and each command's gap index.
3. Send the **clean text** to UD; send it to the LLM for DA/TR if enabled.
4. Remap returned spans from clean-text coordinates back to script offsets through the projection.
5. Return one document; persist only when asked (the editor still persists on Save/Autosave).

The browser-side loop it replaced had two defects, both now impossible by construction:

- **Inline commands reached the parser.** The same utterance parsed differently with and without its
  brackets — fatal for learning where authors place commands, since the label perturbed its own
  features.
- **The sentence splitter cut commands in half.** `/[^.!?]+[.!?]+|[^.!?]+$/` split at a parameter's
  decimal point: `intensity='0.8'` became `…intensity='0.` + `8'] …`.

## 3. Current UI Capabilities

In the Semantic Analysis panel:

- Expand/collapse panel via `Semantic Analysis` button.
- Two independent toggles:
  - `S/V/O analysis`
  - `DA/TR analysis`
- LLM selection dropdown (used for DA/TR).
- Prompt editing (system + analysis prompt) for DA/TR.
- Stored selection preview (read-only):
  - `llmSelections.generate`
  - `llmSelections.semantic`
  - semantic provider (`ud`/`llm`)
  - UD URL
- Debug mode toggle (shows counters + traces when enabled).
- Hint to use meaningful placeholders (for better parsing quality).

Editor visualization:

- Base roles are colored by role.
- `address` has its own role color.
- Address phrase head is rendered with the same address color family.
- Modifiers are visually linked to parent role by color:
  - adjective: dashed underline
  - adverb: dotted underline
- DA/TR badges are displayed on the right side of script lines.
- Alternate even-line shading supports line readability.

## 4. Storage and Persistence

- Semantic output file: `semantic-annotations.json` in project root.
- Behavior-placement model: `behavior-placement.json` in project root (see §11).
- Semantic results are marked as unsaved in UI until Save/Autosave.
- Save/Autosave writes semantic data through backend semantic put path.
- Project configuration persistence in `project.xml` includes:
  - `LLMSelections`:
    - `generate`
    - `semantic`
  - `SemanticServices`:
    - `basicProvider` (`ud` or `llm`)
    - `udUrl`
    - `udTimeoutMs`
    - `analyzeSvo` (`true`/`false`)
    - `analyzeDaTr` (`true`/`false`)
    - `daTrLlm`

## 5. Language and Sentence Handling

- Analysis runs sentence-by-sentence.
- Scene language from `scene <lang> ...` is propagated per sentence.
- Only utterance text is sent for analysis (not scene title, not speaker tag).
- Incremental rendering acts as progress indicator during analysis.

## 6. Placeholder and Greeting Handling

Current UD preprocessing improves robust parsing for script-style text:

- Placeholder normalization before UD parse (for example `$user` -> language-appropriate noun).
- Offset remapping back to original text so highlights still target original script.
- Greeting guardrails for patterns like `Hallo $user, ...`:
  - greeting prefix is prevented from becoming main S/V/O structure
  - addressee before comma can be mapped to `address`

## 7. Debug Support

When semantic debug is enabled:

- UI shows span counters (provided/resolved).
- unresolved span list is shown if normalization fails.
- UD trace payload can be attached per sentence.

## 8. Known Limits (Current)

- UD quality still depends on model behavior for informal, insulting, fragmented, or highly idiomatic utterances.
- Imperatives and ellipses can still require heuristic fallback.
- Modifier extraction covers `amod` and `advmod`/`dep` for ADJ/ADV children of a role head, including
  the nested degree adverb of "sehr gut", and now runs for the **verb** as well as subject, object,
  address and predicate. Modifiers are labelled by word class with a separate `usage` field, so the
  adverbially-used adjective of "Super gemacht" reads as an adjective used adverbially.
- Roles and modifiers are taken per **clause**, not from the flat `basic` block. `basic` is still
  emitted for compatibility but holds one role set per sentence filled from whichever clause matched
  first, so on a two-clause sentence it can pair a subject from one clause with a verb from another.
- Fronted-object order is an upstream weakness: in `Das hast Du sehr gut gemacht.` Stanza assigns
  `Das`=`nsubj` and `Du`=`obj`, which is semantically reversed. Tracked as an expected-fail eval case.
- DA/TR quality depends on selected LLM model and prompt.

## 9. Extension Hooks

This implementation is intentionally extensible:

- Add new semantic roles (for example polarity, modality, sentiment targets).
- Add more UD-derived relation layers (for example compounds, negation, prepositional roles).
- Add configurable DA taxonomy profiles.
- Add confidence-driven filtering in UI.
- Add review mode to compare UD vs LLM role proposals.

## 10. Suggested Next Documentation Updates

When extending the feature, update this file with:

1. New role/category definitions and JSON schema changes.
2. Any new project.xml fields and defaults.
3. UI legend changes and rendering rules.
4. API request/response examples for new layers.

## 11. Behavior-Command Placement (Phase 3-4)

Built on the analysis rather than beside it: the placement service consumes clauses, anchors and
taxonomy-classified commands from the same document the editor renders.

- **Model** — `core/model/behavior/placement/` (Java 17, Android-clean, no Javalin). A count model
  with hierarchical back-off: function+affiliate+clause+turn-position, down to affiliate, then to
  neighbouring Function values on the **NEUROGES polar axis**, then to a hand-written prior keyed on
  the taxonomy's `affiliate` field, then uniform. Interpolation is Witten-Bell, with each level
  subtracting its nested child so one observation is not counted once per level.
- **Endpoints** — `/api/v1/projects/{pid}/placement/`: `GET model`, `POST suggest`, `POST observe`,
  `POST sync`, `POST ghosts`. State in `behavior-placement.json`, which stores the individual
  observations and rebuilds the model from them, making sync idempotent and deletion exact.
- **Online update** — driven from the analysis, not from editor events: the analysis is what knows
  each command's anchor slot, and slot resolution is shared with the corpus extractor so the model
  cannot be trained on labels that differ from those it is evaluated on.
- **Feedback-loop guard** — observations carry `origin`; an accepted suggestion weighs 0.35 against
  1.0 for an authored placement, and `sync` carries a recorded origin forward so re-analysis cannot
  relabel accepted suggestions as authored. Dismissals are recorded but deliberately not scored.
- **LLM second opinion** — opt-in per request, consulted only where the frequency model has no
  evidence, constrained to return one of the offered slot names, and returned in a separate
  `secondOpinion` field so counts and guesses never blend. Latency measured at 3-27s against a local
  Qwen3-30B, which is why it is off the interactive path.
- **UI** — faint dashed amber ghost markers plus a per-turn list in the Semantic Analysis panel;
  Ctrl+Shift+I suggests a position in the turn at the cursor. Accepting always opens the ordinary
  insert dialog, so command choice and parameters remain the author's.

Measured, leave-one-out over a four-project corpus of 31 evaluable placements: top-1 45.2%, top-3
83.9%, against 22.6% for the prior alone, 11.5% uniform-random and 6.5% always-utterance-initial.
Per Function: emblem/social convention 10/11, emotion/attitude 3/17. At this n it is a smoke test,
not a measurement.

Open, and unchanged by the implementation: **23% of real placements are mid-phrase**, sitting inside
a constituent rather than at a boundary. They are excluded rather than snapped, so the decision — snap
to nearest slot, extend the inventory, or model prosodic prominence — is still open. A second
annotator is still needed before the agreement report means anything.

See `doc/behavior-command-placement-learning.md` and `doc/behavior-taxonomy-neuroges.md`.
