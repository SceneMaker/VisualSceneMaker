# SceneFlow Modelling Support — Concept

**Date:** 2026-08-13
**Status:** Conceptualisation phase (discussion draft)
**Audience:** Non-technical authors (psychologists, dialogue designers) building interactive flows

---

## 1. Goal

Provide non-computer/AI experts with **interaction-flow patterns** that help them build
(interactive) SceneFlows, and a **Flow Assistant** that walks authors through the information a
pattern needs (scenes, plugins, plugin content such as screens, variables), then creates the VSM
elements for them. The same assistant machinery should double as an onboarding/tutorial guide.

This document (a) consolidates the pattern inventory from all prior work and clusters it into
three complexity levels, and (b) conceptualises the assistant. It is the input for the
realisation phase, which will touch all tiers of the MVC design (model, runtime, web server,
web UI).

---

## 2. What already exists (prior work inventory)

The repository already contains three layers that address parts of this goal — they are just not
connected to each other or to the authoring UI:

| Layer | Artifacts | Status |
|---|---|---|
| **Didactic** — patterns taught to humans | `editor/web-ui/public/sceneflow-help.html` (4 named patterns: Waiting, Branching, Parallel launch, Reacting to events with BAD/OKAY/GOOD grading); `doc/DesignPatterns/` project (17 live example regions, gallery layout); `doc/IntakeInterview/` (the only *applied* flow: ask→wait→store idiom + async LLM call) | Shipped, current (help v1.5) |
| **Abstract meta-model** — patterns as formal objects | `doc/interactive-design-pattern-catalog.json` (ConstrainedActivityModel: constraint / constrained activity / policy / completion; 5 patterns with scientific sources); `doc/meta-to-sceneflow-mapping.json` (realisation matrix + semantic rules); `doc/sceneflow-generation-conceptual-summary.md` | 2 of 5 patterns implemented, 3 planned |
| **Executable** — patterns as generators | `src/main/java/de/dfki/vsm/sceneflow/ir/` — `SceneFlowSituationPipeline` (TEMPLATE/LLM/HYBRID), `SceneFlowIrTemplateLibrary`, `SceneFlowIrSemanticValidator`, `SceneFlowIrCompiler`, `SceneFlowIrOrchestrator`, `SceneFlowNarrativeExplainer` (reverse explanation with id-level evidence); IR schema `doc/sceneflow-ir.schema.json`; capability snapshot `doc/capability-snapshot.schema.json` | Working, but **CLI/Gradle-only** — no REST route, no UI |

Resource metadata the assistant can build on:

- **`plugin-properties.json`** (29 plugins + core): declared `commands[]` with typed params and
  examples, `variables.writes/reads`, config keys with `sceneflowtype`. Consumed by
  `WebUiServer` classpath scan, `PluginCreateCommandService` (already provisions plugin entry +
  SceneFlow variables from config), and the PluginDashboard "new variables available" update flow.
- **Screen templates** (`editor/web-ui/public/screen-templates/index.json`): each template
  declares `requires` (plugin ids) and `variables` ({name, type, description}) — exactly the
  dependency metadata an assistant needs, currently only *displayed*, never checked or provisioned.
- **Suggestion-UX precedents**: scene-title / dangling-PlayScene suggestions (embeddings service),
  behavior-placement ghost markers (Phase 4, complete — accept path always routes through
  `InsertActionDialog`, provenance `origin: authored|accepted-suggestion`), plugin `newVars`.
- **Tutorial infrastructure**: tutorial shelf (`GET /api/v1/projects/tutorials` + landing panel)
  with its first bundled entry installed at `editor/src/main/resources/res/tutorials/1-IntakeInterview/`;
  the First-Run Setup Wizard (B1) and contextual Shepherd.js tour (B3) from
  `doc/deployment-and-tutorial-plan.md` are specified but unbuilt.

**Conclusion:** the realisation phase is mostly a *connection and extension* effort, not a
green-field build.

---

## 3. Pattern catalogue, clustered in three levels

Clustering rationale: the levels follow the **author's mental model**, not implementation size.
Level 1 = one timeline. Level 2 = several timelines side by side. Level 3 = timelines that react
to events, preempt each other, or coordinate with each other.

Status legend: **H** = taught in sceneflow-help.html, **D** = modelled in DesignPatterns,
**I** = applied in IntakeInterview, **C** = in interactive-design-pattern-catalog.json,
**T** = executable template exists, **–** = new.

### Level 1 — Sequences (one timeline)

*"First this, then that" — linear questionnaires, scripted presentations.*

| # | Pattern | What the author wants | SceneFlow realisation | Status |
|---|---|---|---|---|
| 1.1 | **Sequence** | Do A, then B, then C | EEdge chain of nodes with PlayScene/PlayAction | H D |
| 1.2 | **Timed pause** | Wait a moment before continuing (fixed / random range / variable duration) | TEdge (timeout / min–max / variable) | H D |
| 1.3 | **Ask & wait for answer** | Ask the user something, wait for the reply, store it | PlayScene + clear var → TEdge self-loop + CEdge(`var != ""`) → Assignment (the IntakeInterview idiom); also covers "await async service result" (LLM call) | I |
| 1.4 | **Branch by condition** | If X then A, else B (optionally: else-after-timeout) | Ordered CEdges + EEdge fallback; timeframe variant with TEdge fallback | H D |
| 1.5 | **Branch by chance** | Vary behaviour randomly | PEdges (incl. the 1 % self-loop re-roll trick) | H D |
| 1.6 | **Retry / poll until** | Check again after a delay until a condition holds | CEdge + TEdge self-loop (DesignPatterns D12 is the placeholder — currently modelled incompletely, needs fixing) | D(broken) |
| 1.7 | **Grouped sub-dialogue** | Package a step sequence as one reusable unit; resume where it left off on re-entry | SuperNode (Prepare/Execute/End) + History node | D |
| 1.8 | **Questionnaire** | A whole linear interview: n × (ask & wait & store), then summary | Composition of 1.1 + 1.3 (+ 1.4 for skip logic) | I |

### Level 2 — Parallel sequences (several timelines)

*"Like level 1, but with one or more parallel tasks": change background at a certain time, change
music, change the idle behaviour of a SIA.*

| # | Pattern | What the author wants | SceneFlow realisation | Status |
|---|---|---|---|---|
| 2.1 | **Fork parallel tasks** | At this point, also start X and Y | FEdges from one node; targets run independently | H D |
| 2.2 | **Independent background track** | Something runs alongside the whole dialogue (ambient music, logging, sensor watch) | Multiple start nodes (used in DesignPatterns as layout, never *taught* as a pattern — should be) | H(partial) |
| 2.3 | **Timed cue track** | At time X (into the experience), do Y | Parallel track: TEdge chain firing PlayActions (background change, music change) | – |
| 2.4 | **Ambient/idle behaviour loop** | The SIA shows idle behaviour until told otherwise | Parallel track: loop of idle PlayActions, gated by a mode variable (CEdge), switched from the main track | – |
| 2.5 | **Parallel decoration of a step** | While scene S plays, also change screen/lighting | FEdge at step entry; decoration track ends on its own | – |
| 2.6 | **Fork–join (synchronisation barrier)** | Wait until *both* parallel tasks are done, then continue *once* | **Semantics gap** — threads accumulate at shared targets today; `doc/sceneflow-join-semantics.md` proposes `join` attribute / JoinEdge. Pattern is blocked on that runtime work | – (proposal) |

### Level 3 — Reactive & coordination patterns (event-driven, preemption, multi-party)

*"Do something unless a certain event happens" (user speaks, user appears in camera …), up to
turn-taking with changing listening behaviour.*

| # | Pattern | What the author wants | SceneFlow realisation | Status |
|---|---|---|---|---|
| 3.1 | **React to event** | When E happens, react — immediately, not at the next check | IEdge on node or supernode (help teaches the BAD/OKAY/GOOD progression: busy-wait → poll → interrupt) | H D |
| 3.2 | **Do-until-event (constrained activity, base)** | Keep something alive until the event/condition arrives, then exit | SuperNode + internal TEdge liveness loop + IEdge exit — `constrained_activity_base` | C T |
| 3.3 | **Periodic reminder while waiting** | While waiting, remind the user every n seconds | Waiting ⇄ Reminder timed cycle inside the constrained supernode — `periodic_reminder_while_waiting` | C T |
| 3.4 | **Engagement while waiting** | While waiting, keep the interaction meaningful (music, visuals, social behaviour) | Constrained activity with multimodal/social action nodes — `socially_assistive_engagement_while_waiting` | C (planned) |
| 3.5 | **Interruptibility policy** | Only interrupt/remind when it is acceptable for the user | Guard CEdge/score gate before the activity — `attention_aware_interruptibility` | C (planned) |
| 3.6 | **Barge-in** | If the user speaks while the SIA speaks: stop, handle, resume or abandon | IEdge on the speaking supernode + History for resume + explicit stop-speech PlayAction | – |
| 3.7 | **Mixed-initiative prompting** | System decides: act automatically, suggest, or stay quiet | Decision point over policy variables — `mixed_initiative_prompting` | C (planned) |
| 3.8 | **Turn-taking with listening behaviour** | Two+ parties (SIA(s), user) exchange turns; whoever is not speaking shows listening behaviour | Turn-owner variable + per-party supernodes (Speaking/Listening states) + IEdges on turn-change events + behaviour-switch PlayActions (SIA idle/listen/attend commands) | – |
| 3.9 | **Escalation** | Repeat with increasing insistence, give up after n tries | Counter variable + maxRepeats guard + levelled stages (already sketched in meta-to-sceneflow mapping `policy.escalation`) | C (partial) |

Notes on the clustering:

- The user-facing names must stay in **author language** ("Ask & wait for answer", not
  "CEdge-guarded TEdge poll loop"). The `humanDescription` field in the catalogue already models
  this.
- Level 3 is where the existing **constrained-activity meta-model** lives; levels 1–2 need a
  handful of much simpler meta-entries (sequence, branch, fork, timed cue). The meta-model
  generalises: a Level-1 questionnaire is just completion-chaining; a Level-2 idle loop is a
  constrained activity whose constraint is a mode variable.
- 2.6 (fork–join) and 3.8 (turn-taking) are the two patterns that need **runtime/model work**,
  not just templates: join semantics (existing proposal) and possibly turn-event plumbing
  (speech-state events from SIA plugins, camera/user-presence events).

---

## 4. One catalogue, four consumers

Central design decision proposed here: **extend `interactive-design-pattern-catalog.json` into
the single source of truth** for all pattern-related features. Each pattern entry gains:

```jsonc
{
  "id": "ask_and_wait",
  "level": 1,                          // NEW: complexity level 1|2|3
  "label": "Ask & wait for answer",
  "humanDescription": "...",           // exists
  "sceneFlowMapping": { ... },         // exists
  "supportsMeta": { ... },             // exists
  "scientificSources": [ ... ],        // exists
  "resourceRequirements": {            // NEW: what the assistant must elicit/provision
    "variables": [ {"name": "user_input", "type": "String", "role": "answer slot"} ],
    "plugins":   [ {"capability": "user-input", "examples": ["htmlgui-ws"]} ],
    "scenes":    [ {"role": "question", "count": "1..n"} ],
    "screens":   [ {"template": "chat", "optional": true} ]
  },
  "assistantScript": [                 // NEW: slot-elicitation dialogue (see §5)
    {"slot": "question", "ask": "What should be asked?", "kind": "scene-or-text"},
    {"slot": "answerVar", "ask": "Where should the answer be stored?", "kind": "variable", "suggest": "existing-or-new"}
  ],
  "tutorialScript": [ ... ]            // NEW: step-by-step build-it-yourself variant (see §6)
}
```

The four consumers:

1. **Documentation/gallery** — help file sections and DesignPatterns regions are generated from /
   checked against the catalogue (no more drift like D12's missing edge or D16's Eq/Neq slip).
2. **Generator** — the existing template library / situation pipeline (already catalogue-driven
   for selection; extend to levels 1–2).
3. **Flow Assistant** — elicitation + provisioning + generation (new, §5).
4. **Tutorial mode** — the same entries drive guided build-it-yourself sessions (new, §6).

---

## 5. The Flow Assistant — concept

### 5.1 Interaction model

A conversational **panel in the web UI** (precedents: the LLM generate panel, InsertActionDialog,
ScreenEditor template picker). The author either picks a pattern from a browsable, level-grouped
gallery ("I know what I want") or describes the situation in natural language ("I want the avatar
to keep waiting until the user presses OK, and remind them every 30 seconds") — the existing
prompt resolver already maps situation text to meta fields with confidence + ambiguity reporting.

### 5.2 Assistant phases

```
Intent → Pattern proposal → Resource check → Parameterisation → Preview → Create → Explain
```

1. **Intent capture.** Free text or gallery pick. NL path reuses the deterministic prompt
   resolver; low confidence or ambiguity → clarifying question instead of guessing (the
   resolver already emits ambiguity notes — surface them as questions).
2. **Pattern proposal.** Catalogue-driven selection (`supportsMeta`), presented in author
   language with the pattern's illustration (from the help/gallery assets) and its
   `humanDescription`. Author confirms or switches pattern.
3. **Resource check** (the core assistant value). Against a **live capability snapshot** of the
   project, walk the pattern's `resourceRequirements`:
   - exists already → offer to reuse (e.g. "use existing variable `user_input`?"),
   - missing plugin → propose adding it (provisioning via `PluginCreateCommandService`,
     which already creates the plugin entry + its sceneflow variables),
   - missing screen → propose a screen template (its `requires`/`variables` metadata finally
     gets *checked and provisioned*, not just displayed),
   - missing scene → create a named stub in the SceneScript (author fills the wording later),
   - missing variable → create with the right type.
   Nothing is created silently: each item is an accept/adjust/skip decision, consistent with the
   Phase-4 rule that the accept path stays author-mediated.
4. **Parameterisation.** Slot filling per `assistantScript`; plugin commands are parameterised
   through the existing **ActionForm** (schema-driven, already supports `suggestedSlot` and live
   test on preview-capable agents).
5. **Preview.** Before creating anything: a summary of *what will be added* — the
   **SceneFlowNarrativeExplainer output for the candidate IR** ("A waiting area 'WaitForOK' with
   a reminder every 30 s; it exits to 'Continue' when event == OkayButtonPressed") plus the list
   of resources to be created. This is the reverse-explanation pipeline earning its keep.
6. **Create.** IR patch through the existing orchestrator: schema validation → semantic
   validation → compile → XSD/model validation, with the bounded repair loop. Result appears as
   a normal edit (undoable, presence-visible, provenance-tagged
   `origin: assistant-generated`).
7. **Explain & hand off.** Highlight the created elements on the canvas; offer "walk me through
   what was built" (narrative explainer again) and next-step hints ("the scene 'AskName' is
   still empty — open the script editor?").

### 5.3 Positioning decisions (proposed)

- **Author-mediated, never autonomous.** The assistant proposes; every creation is confirmed.
  This extends the deliberate Phase-4 scope rule (command selection stays with the author) to
  pattern selection: the assistant may *propose* which pattern/command, the author decides.
- **Deterministic core, LLM at the edges.** Pattern selection, resource checking, IR compilation
  and validation are deterministic (auditable, reproducible). The LLM is used only for intent
  mapping and optional paraphrase of explanations — exactly the split the generation pipeline
  already implements (TEMPLATE / LLM / HYBRID candidate modes).
- **Patch mode into the open project** is the default; "new project from pattern" is a
  local-deployment feature only (the workspace-platform plan forbids self-serve project creation
  for regular users — Decision 5).

### 5.4 Example dialogue (pattern 3.3)

> **Author:** The avatar should wait until the user presses the OK button and remind them now and then.
> **Assistant:** That's a *"Remind while waiting"* pattern (Level 3). I need:
> ① the event to wait for — I found the Event variable `event`; is the value `OkayButtonPressed` right?
> ② the reminder — no scene "Reminder" exists yet; create it as an empty scene for you to write, or should the avatar use a spoken sentence I insert directly?
> ③ how often? (default: every 30 seconds)
> ④ where to continue afterwards — after the current node "Greeting"?
> **Author:** (answers / accepts defaults)
> **Assistant:** I will create: supernode *WaitForOK* (waiting loop + reminder every 30 s), an interrupt exit on `event == "OkayButtonPressed"` to a new node *Continue*, and scene *Reminder* (empty). — **Create / adjust / cancel?**

---

## 6. Tutorial & onboarding mode

The same catalogue and the same phase machinery, with one switch flipped: instead of *generating*
the elements, the assistant **guides the author to build them** and *verifies* each step.

- Each pattern's `tutorialScript` decomposes the build into steps ("Create a node and name it
  'Waiting'", "Now draw a timeout edge back onto the same node…"), anchored to real UI elements
  (this is where the planned Shepherd.js tour from the deployment plan plugs in).
- **Verification instead of generation:** after each step, the pattern detector (the reverse
  explainer's detector layer) checks the live flow against the expected partial structure —
  "I can see the timeout edge, but it has no interval yet" — a genuinely better tutor than a
  passive tour because it reads the author's actual work.
- Graduation path: tutorial mode (build it yourself, verified) → assisted mode (assistant builds,
  author confirms) → expert mode (gallery as reference only). This is the onboarding story:
  the tutorial shelf's first entries become "Level 1 patterns", using the IntakeInterview
  project as the capstone.

---

## 7. Gaps to close in the realisation phase

Ordered roughly by dependency:

1. ~~**Live capability snapshot service.**~~ *(done 2026-08-14.)* `CapabilitySnapshotBuilder` in
   `core-webserver` builds the snapshot from the loaded model, and `GET /api/v1/projects/{pid}/capabilities`
   serves it in both server modes. It replaced 392 lines of inline Groovy in `build.gradle`, which
   re-parsed the project XML; `generateCapabilitySnapshot` now calls the same class, so a build-time
   snapshot and a served one cannot drift. The snapshot carries the **scene inventory** as a
   top-level `script` section (scene group name, language variants, speakers, turn and word counts,
   parameters, referenced agents, inline commands) at version `1.1`.

   Still missing from the snapshot: **per-agent plugin command inventories and screens with their
   variable bindings**, both listed as intended in `llm-supported-flow-generation.md` and never
   delivered. Note also that node positions are absent, which is why a generated patch cannot avoid
   overlapping existing nodes (see `patterns/1.1-fixed-sequence.md` §6).
2. **Move/expose the IR pipeline.** `de.dfki.vsm.sceneflow.ir` lives in the root `src/` module
   with CLI entry points only. It must become reachable from the web server and get REST/WS routes
   for: situation → candidates, candidate → preview explanation, candidate → apply.
   *Verified 2026-08-13:* the package's only VSM dependencies are `model.sceneflow.*`,
   `util.xml.XMLUtilities` and `util.llm.LLMSupport` — all in `core` — plus `org.json`. So it
   relocates to **`core-webserver`** (Java 21, already depends on `core` + `org.json`) with no new
   dependencies. The Java-17 rule does **not** apply: the assistant is an *authoring* feature, and
   Android deploys the runtime, not the editor. Keep it out of `core` for exactly that reason.
3. **Extend the catalogue** per §4: `level`, `resourceRequirements`, `assistantScript`,
   `tutorialScript`; add the missing Level-1/2 entries (sequence, ask-and-wait, branch, fork,
   timed cue, idle loop) as executable templates — these are simpler than the constrained-activity
   templates that already exist.
4. **Resource provisioning unification.** Three sources of variable/dependency truth exist
   (plugin config `sceneflowtype`, plugin `variables.writes/reads`, screen-template `variables` +
   `requires`) and nothing reconciles them. The assistant's resource-check phase needs one
   resolver over all three; `PluginCreateCommandService` is the seed.
5. **Assistant UI** in the web UI (panel + gallery), reusing ActionForm, InsertActionDialog,
   ScreenEditor template picker, and the suggestion accept/dismiss idioms; shared provenance
   tagging (`origin: assistant-generated`).
6. **Tutorial mode** on top of the pattern detector (extend detector coverage beyond the
   constrained-activity wait pattern — branching, retries, parallel launch) + tour framework (B3)
   + first tutorial-shelf content.
7. **Runtime semantics work** for the two blocked patterns: join semantics
   (`doc/sceneflow-join-semantics.md`, Option A recommended there) for 2.6; event plumbing for
   3.8 turn-taking (speech-state/user-presence events as standard Event variables from SIA and
   sensing plugins — needs its own small concept).
8. **Housekeeping** surfaced by this research: ~~fix DesignPatterns D12 (missing retry edge) and
   D16 (Neq vs Eq inconsistency with its supernode twin S5)~~ *(done 2026-08-13, together with a
   regeneration of the stale `capability-snapshot.designpatterns.json` fixture)*; teach "multiple
   start nodes" and "History resume" as named patterns in the help; remove the stale root
   `plugin-properties.json` duplicate.

---

## 7a. Inconsistencies found while researching this concept

Recorded here so they are not rediscovered. None is in scope for the concept itself, and each is
cheap to fix once someone is in the relevant file. Semantics findings live separately in
[`patterns/1.1-fixed-sequence.md`](patterns/1.1-fixed-sequence.md) §2 and §9.

**Terminology**

- **`SceneFlow` / `chart` / `graph` / `flow` are used with four different scopes** in one help file
  and nothing tells the author: `SceneFlow` is both the whole artifact and specifically its root,
  `chart` is any container including supernodes, `graph` is the canvas rendering, and `flow` is the
  running behaviour. A supernode is therefore a "chart" that is not a "SceneFlow", while variables
  are "declared per chart".
- **Two mutually exclusive registers coexist.** The shipped `sceneflow-help.html` opens with
  "hierarchical state machine", "states", "transitions", while the 2026-08 author-facing material
  deliberately builds a step and session vocabulary with no graph terms. Whichever wins, they
  currently contradict each other in tone and in referent.
- **Label mismatches:** the help calls the edge type "Interrupt" while the web UI labels it
  "Interruptive edge"; the SceneScript help calls a turn's `Speaker:` a *speaker* while the resolved
  target is an *agent*.
- **VSM's one-line self-description differs in all four places it appears** (the About box, the
  installer README, `CLAUDE.md`, and the review sheet), and `README.md` has none at all. The
  review sheet's introduction is the best current formulation and is the one to standardise on.

**Runtime and tooling**

- **`SceneDoneEvent` is fired but sets no variable.** `ReactivePlayer` conveys it and only UI
  bridges consume it, so it cannot be used as a completion signal in a flow. Relevant to the
  completion handshake in `patterns/1.1-fixed-sequence.md` §3 variant B: the event an author would
  want already exists and is simply not exposed to the flow.
- **The capability snapshot fixture had never validated against its own schema.** The schema pinned
  variable `type` to a five-value enum while the generator emits parameterised Event types such as
  `Event(*, 10)`. Nothing ever ran a schema validator over it. Fixed 2026-08-13 by relaxing the type
  to a pattern and adding a key-parity check, which now runs inside `CapabilitySnapshotCli` and fails
  rather than emitting a snapshot the schema would reject.
- **Every snapshot ever generated reported zero commands for every node.** The Groovy counted
  `<Command>` elements, which the XML never contains: the real children are `PlayAction`, `PlayScene`
  and `Assignment`. A consumer asking whether a node does anything always got no. Fixed by reading
  the model, 2026-08-14.
- **The authored order of start nodes is not recoverable from the model.** `SuperNode` holds them in
  a `HashMap`, so the order in `project.xml` is lost on load. The Groovy generator appeared to
  preserve it only because it read the XML text; a snapshot of a live project never could. Start node
  ids are therefore sorted, which is the only deterministic option.
- **`doc/DesignPatterns` contains a scene that cannot play.** Scene `Welcome` has speaker `Anne`,
  but the project declares only the `timer` agent, so the speaker resolves to nothing. It is
  harmless today because no node plays that scene, and it is a useful test case for the resource
  check described in §5 phase 3.

---

## 8. Decisions and open questions

### Decided (2026-08-13)

- **Q3 — LLM dependency: the assistant relies on the built-in LLM service.** No separate
  deterministic-only fallback path is required. `plugins/llm` (`LlmExecutor`) and the existing
  `POST /api/v1/llm/*` routes are the assistant's LLM channel. The deterministic core
  (pattern selection, resource checking, IR compilation, validation) remains deterministic
  regardless — the decision is that natural-language intent capture may be a hard dependency,
  not that generation becomes free-form.
- **Q5 — the catalogue lives in `core`.** It ships as a served resource out of `core`'s
  resources (the same way `plugin-properties.json` and the tutorial shelf already do), so it is
  versioned with the release and reachable from the web server without a doc-path dependency.
  The `doc/` copies become generated/exported artifacts, not the source of truth.
- **Q1 — the clustering goes to psychologists for review before it is frozen.** Review
  instrument: `doc/pattern-review/sceneflow-pattern-review.html` — all 23 patterns in
  author-facing language with a SIA-grounded example each, a 0–5 understandability rating per
  item (0 = not understandable, 5 = completely understandable), optional per-item comments, and
  CSV export for analysis. Ratings are the input for the final level cut and the pattern naming.

### Still open

1. **Assistant entry point.** One assistant panel, or two entry points (gallery button on the
   canvas toolbar + NL field in the existing generate panel)?
2. **Turn-taking scope.** 3.8 needs event conventions across plugins (who emits
   `user_speaks`, `user_present`?). Standardise as a small "interaction events" spec, or keep
   per-project?
3. **Evaluation.** Beyond the understandability review above, pattern *coverage* could be
   validated with the annotator/scenario corpus coming for behavior placement — same authors,
   same scenarios. Plan a small authoring study?
4. **Deployment scope.** `vsm-workspace-platform-plan.md` Decision 5 forbids self-serve project
   creation for regular users, which limits "new project from pattern" to the local installer
   unless that decision is revisited.
