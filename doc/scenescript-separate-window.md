# Proposal: detaching the SceneScript editor (with SIA preview) into its own window

**Status:** proposal, nothing implemented
**Date:** 2026-08-12 (revised — supersedes the 2026-08-11 first draft)
**Scope:** let the scene script editor plus its SIA preview be *detached* from the main editor into a
separate browser window, so it can fill a second monitor while the SceneFlow graph fills the first,
and *merged* back on demand.

---

## 1. Where things stand today

Only one query parameter is read by the whole app: `session`
(`editor/web-ui/src/App.svelte:1483`). Everything else about what a window displays is internal
state, so **every window renders the entire editor** — menu, SceneFlow, script.

What already works, verified against a running server with two independent browser contexts on
one project:

- Both windows load the project fully.
- The server registers `subscriberCount: 2` with two distinct presence entries, each with its own
  colour and display name.
- In-progress script text already flows between windows: `scheduleScriptLive()`
  (`App.svelte:8276`) broadcasts `script.live`, and a receiving window applies it
  (`App.svelte:8502`).

So the collaboration plumbing — `Session.Subscribe`, presence, operation broadcast — treats two
windows on one machine exactly like two collaborators. **Multi-window sync is not the problem to
solve.** The only missing capability is telling a window *which part of the editor to show*.

## 2. Why the script area is the good candidate

The scene script is a clean cut, and this is the evidence rather than an impression:

**It never touches the SceneFlow snapshot.** A grep for `sceneFlow` across the entire
`.scenescript` block (`App.svelte:18152` to the end of that section) returns nothing. The script
area reads no node, edge, selection or graph state.

**Its inputs are all project-level or its own:**

| Needs | Source |
| --- | --- |
| script text, diagnostics, scenes, elements | REST `/script`, `/script/scenes`, `/script/elements` + WS |
| agents & plugin commands | `projectConfigView` + `pluginInterfaces` — `previewCapableAgents` is built from exactly those two (`App.svelte:3600`), *not* from the graph |
| semantic analysis | REST `/semantic`, `/semantic/analyze` |
| scene highlights during a run | runtime `sceneHistory` from `/runtime` + WS runtime events |
| SIA preview state | local to the window + the plugin preview REST endpoints |
| editor height, view mode, SIA panel height | per-project `ui-prefs` |

**The two script↔flow integrations are server-mediated, so they work from any window with no local
graph:**

- Renamed/removed scenes are reconciled against the SceneFlow via the WS command
  `SceneFlow.PlayScene.FindMany` (`App.svelte:11841`) — the server scans the flow.
- Runtime start/stop and the resulting scene history arrive over WS.

**The SIA preview belongs with the script, not with the graph.** `SiaPanel`'s `onInsertAtCursor`
(`App.svelte:18155-18173`) inserts commands at the *script editor's* cursor, and the per-turn Play
buttons drive the preview. These two are already coupled to each other and to nothing else — the
strongest argument for moving them together.

### 2.1 One thing the graph side *does* need from the script

The blocks panel's **SCENES list** (left rail of the SceneFlow area, `App.svelte:16787-16807`) is
driven by `scriptScenes` / `scriptScenesLive` / `scriptScenesFilter` / `scriptScenesLanguage`.

So detaching must stop *rendering* `.scenescript` while the main window keeps *loading* script data.
A naive "remove the script part" that also tore out its data loading would blank the scene palette
the graph side relies on. This is the single most likely implementation mistake.

## 3. Proposed design — detach and merge

The editor is **moved**, not duplicated. This is the decisive choice: it is what keeps the whole
thing simple (see §4).

### 3.1 Flow

1. The editor starts as today: one page, menu + SceneFlow + script (**main mode**).
2. Pressing a **detach** icon in the script toolbar:
   - the main window stops rendering `.scenescript` (editor, toolbar and SIA panel), while
     continuing to load script data for the SCENES list (§2.1);
   - a new window opens showing only the script editor and its preview.
   The script part is removed from the main window *before* the detached window renders, so the two
   never coexist.
3. Pressing **merge** in the detached window closes it and re-renders the script in the main window.
4. **At most one detached scenescript window per client.** Re-pressing detach focuses the existing
   window rather than opening a second.
5. **Every transition (detach and merge) reloads the SIA preview.** Cheap, and it removes any
   question about engine state surviving a move.
6. **Detach is only offered while the project is not running** — the button is disabled unless
   `runtimeState === "stopped"`. Note the asymmetry: *merging* stays available during a run, and a
   run may be started from the main window while a detached window is open. Both are fine — the
   detached window has no runtime controls (§3.3), and the preview suspends itself during a run via
   `SiaPanel`'s `suspended` prop regardless of which window hosts it.

### 3.2 Handing over the editing state

Two things must survive the move: the in-flight draft, and the undo history.

**Draft.** `script.live` is a broadcast, not retained server state, so a freshly opened window would
load only the last saved text from `/script`. The main window therefore hands the draft to the
detached window **directly** — the two are same-origin, so a `BroadcastChannel` (or the handle
returned by `window.open`) can carry the current text. Nothing has to be saved first, and it works
even when the script currently has parse errors, which is precisely when a bigger editing window is
wanted. (Requiring an apply first was rejected for that reason: the apply path is gated on parse-OK
with no diagnostics.)

**Undo history.** Handing over text alone would silently reset undo at every detach and merge, which
is unacceptable for an authoring tool — an author who detaches mid-edit would lose the ability to
step back. The control logic is therefore extended with explicit history hand-over on both
transitions.

This is feasible with the installed CodeMirror: undo history is enabled through `basicSetup`
(`codemirror/dist/index.js:53` calls `history()`), and `@codemirror/commands` exports `historyField`
(`dist/index.d.ts:110`), which is exactly the hook CM6 provides for serialising it:

```js
// sending window
const payload = view.state.toJSON({ history: historyField });
// receiving window
EditorState.fromJSON(payload, config, { history: historyField });
```

`ScriptEditor.svelte` currently imports only `indentWithTab` from `@codemirror/commands`, so this
adds one import and a serialise/restore pair either side of the channel. The payload travels over the
same `BroadcastChannel` as the draft, so text and history arrive together and cannot desynchronise.

### 3.2a Per-view preferences

`scriptViewMode` and `scriptEditorHeight` are **remembered separately for the detached window**, not
inherited from the main window. A detached window exists precisely to be larger and read differently
(often "text" mode on a second monitor), so carrying the inline layout's sizing across would defeat
the point.

Implementation: distinct `ui-prefs` keys per view rather than new storage — the server merges
incoming keys into the stored object (`WebUiServer.handleUiPrefsPut`), so adding keys is safe and
will not disturb existing prefs.

*Assumption to confirm:* the SIA panel height follows the same rule, for consistency with the editor
height. Only view mode and editor height were explicitly specified.

### 3.3 What the detached window shows

The script area's **own toolbar comes along unchanged** — Search, Generate Scenes, Title Generator,
Semantic Analysis and its mode selector, Preview, the view-mode cycle, help, apply. Above it sits a
**minimal top row**:

| In the top row | Deliberately absent |
| --- | --- |
| project name | node/edge tools, supernode field, breadcrumbs |
| dirty / saved state | canvas toggles (snap, info overlays, variable badges) |
| **Merge** button | runtime Start / Pause / Stop |
| Save | SceneFlow help |

Copy/paste/undo/redo need no buttons — the keyboard already covers them.

### 3.4 Lifecycle edge cases

- **Detached window closed directly** (not via merge) → treated as a merge: the main window
  re-renders the script. Avoids a state where the script is unreachable. Detected with the same
  `window.closed` polling already used for the runtime GUI popup (`runtimeGuiWindowOpen`).
- **Main window closed while detached** → the detached window **becomes a full editor**. It is
  already a complete app instance with `?session=`, so it renders the SceneFlow part too and takes
  over; the merge button simply disappears. No dead end, nothing to recover.
- **Enforcing one per client** → a named `window.open` target covers re-pressing detach in the same
  browser profile; a `BroadcastChannel` lock covers a manually pasted URL in that profile. A second
  *browser* on the same machine cannot be prevented client-side, and is not worth server enforcement.

## 4. Risks — and what the move design removes

### 4.1 Shared SIA preview across machines is a feature, not a bug

The first draft of this document claimed two engine instances on one licence break each other and
proposed a server-side ownership lease. **That was wrong on both counts and is withdrawn.**

Per the VuppetMaster developer (2026-07-31): `licenseKey` is validation-only, engine instances are
independent, there is no session limit — confirmed empirically with two projects running from
different clients on different machines with zero interference. `JettyTransport.send()` broadcasts
to *every* connected character page by design.

So several collaborators watching the same SIA act simultaneously is **intended behaviour and
valuable** — it is how a team reviews and refines an agent's delivery together. Nothing here should
restrict it, and the withdrawn lease would have actively broken it.

(The first draft also proposed reusing `CharacterPreviewPanel`'s `suspended` prop. That component is
now orphaned — `e8ddbb08` replaced the floating preview windows with the docked `SiaPanel`, and
nothing imports or renders `CharacterPreviewPanel` any more. The live machinery is `SiaPanel`'s own
`suspended` prop, `SiaPanel.svelte:53, 292-315`. The leftover file should be deleted separately.)

### 4.2 The residual preview risk is narrow, and the design handles it

What the 2026-07-18/07-20 wedging actually was: **two engine instances in the same browser**
(preview iframe plus a real run's character page), and/or the broadcast-starvation bug — one dead
socket aborting `send()`'s loop and starving every live one. That bug is fixed (`5d7574ca`; the
current `send()` has per-socket try/catch and evicts dead sockets).

The move design addresses the remaining same-browser case structurally rather than by locking: the
script and its preview exist in exactly one window per client at a time, and every transition
reloads the preview.

### 4.3 What the move design also removes

Three problems from the first draft simply do not arise once the editor is moved rather than copied:

- **Two windows editing one script.** `script.live` only applies an incoming snapshot when the
  receiving window has no unpublished draft (`if (!scriptDirty)`, `App.svelte:8502`), so two dirty
  windows would diverge silently. With a move there is only ever one script editor per client.
  Cross-machine collaborators still edit concurrently — unchanged pre-existing behaviour, out of
  scope here.
- **Double autosave.** The gate `canAutoApply` (`App.svelte:2969`) includes `showEditor`, which a
  second window would also satisfy. Only one window renders the editor, so only one autosaves.
- **`ui-prefs` tug-of-war.** `scriptEditorHeight`, `scriptViewMode` and the SIA panel height are
  stored per project. With one editor at a time there is no competing writer.

### 4.4 Remaining minor item: presence counts the detached window

The detached window is a real session subscriber, so a solo author appears in presence as two peers
with two colours. Cosmetic. Either mark same-token sessions as views rather than peers, or accept it.

## 5. Suggested phasing

| Phase | Work                                          | Result                  |
|-------|-----------------------------------------------|-------------------------|
| 1     | Detach/merge, state hand-over, per-view prefs | The feature, complete   |
| 2     | Lifecycle hardening                           | Removes the sharp edges |
| 3     | Documentation                                 | Users can discover it   |
| 4     | Tidy-up                                       | Cleanup                 |

**Phase 1** covers §3.1–3.2a, and must keep script data loading in the main window (§2.1).

**Phase 2** covers §3.4.

**Phase 3** — the detach control lives in the script toolbar, so
`editor/web-ui/public/scenescript-help.html` is the right home; bump its version pill and footer
together. It should cover: what detaching does, that the project must be stopped first, that unsaved
work and undo history survive the move, that only one detached window is allowed per client, and how
to merge back.

**Phase 4** covers §4.4.

No preview-lease phase is needed — that was an artefact of the withdrawn §4.1.

## 6. Open questions

All settled in discussion (2026-08-11/12):

- Draft hand-over mechanism → direct, same-origin channel (§3.2).
- Undo history → handed over explicitly on both transitions; feasible via `historyField` (§3.2).
- Detached window contents → script's own toolbar plus a minimal top row (§3.3).
- Orphan behaviour → the detached window becomes a full editor (§3.4).
- Per-view preferences → view mode and editor height remembered separately (§3.2a).
- When detach is offered → only while the project is stopped (§3.1).
- Documentation → tracked as phase 3 (§5).

One assumption left to confirm: whether the SIA panel height is also remembered per view (§3.2a).
