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
- Modifier extraction currently focuses on direct dependency relations (`amod`, selected `advmod`) tied to chosen role heads.
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
