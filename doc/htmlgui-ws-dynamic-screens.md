# htmlgui-ws Dynamic Screen Elements

## Context and Motivation

The `htmlgui-ws` plugin provides web-based interaction screens for VSM
scenarios that research interaction with socially interactive agents (virtual
or physical). The first release (Phase A+B) established a schema-driven screen
builder with a visual editor and a comprehensive set of static element types
(buttons, inputs, media, charts, speech bubbles, panels).

This document specifies the next phases, which add **dynamic** element types
— elements whose *content grows or animates over time* driven by runtime
variable values, without requiring any modification to the SceneFlow graph.

### Design principle

> All dynamism lives in VSM variables. The SceneFlow graph stays static and
> auditable. Screens are pre-authored; runtime data fills them.

This principle ensures reproducibility (important for research), keeps the
authoring tool predictable, and avoids the complexity and risk of runtime
graph mutation.

---

## Use Case Overview

| Use case | Element type | Data source |
|---|---|---|
| Question buttons, media display | existing elements | pre-authored |
| Bar/line charts with live sensor data | `vsm-chart` | variable (JSON) |
| Living dialogue diary (speech bubbles grow as conversation progresses) | `vsm-feed` | variable (JSON array) |
| Reload and review a saved dialogue; annotate with LLM summaries | `vsm-feed` | variable (JSON array, restored by save plugin) |
| Animated overlays with variable-controlled parameters (beating heart, breathing indicator) | `vsm-animate` | variable (numeric) |

---

## Phase 1 — `vsm-feed`: Dynamic Dialogue Feed

### Purpose

Display an ongoing conversation as a scrollable list of role-aware speech
bubbles. New messages are appended at runtime by updating a single VSM
variable that holds the full conversation as a JSON array.

The same element renders a saved conversation identically when the variable
is restored from disk — making save/review/annotation a zero-cost addition.

### Data model

The bound variable holds a JSON array of message objects:

```json
[
  { "role": "agent",  "text": "Hello, how are you feeling today?" },
  { "role": "user",   "text": "A bit anxious actually." },
  { "role": "agent",  "text": "That's understandable. Can you tell me more?" },
  { "role": "system", "text": "Session summary: user reported anxiety at start." }
]
```

Optional per-message fields:

| Field | Description |
|---|---|
| `role` | `"agent"` \| `"user"` \| `"system"` — drives alignment and color |
| `text` | Message content |
| `speaker` | Override display name above the bubble (e.g. `"ARIA"`) |
| `timestamp` | ISO string or display string shown in small type |

### Schema element definition

```json
{
  "type": "vsm-feed",
  "dataVar": "conversation",
  "agentColor":  "#e8f4fd",
  "userColor":   "#eafbe8",
  "systemColor": "#f5f5f5",
  "agentLabel":  "Agent",
  "userLabel":   "You",
  "showTimestamps": false,
  "height": "400px"
}
```

### Rendering behaviour

- `agent` messages: bubble aligned left, tail bottom-left, `agentColor` background
- `user` messages: bubble aligned right, tail bottom-right, `userColor` background
- `system` messages: centered, no tail, `systemColor` background, italic, smaller font —
  used for LLM summaries, session annotations, timestamps or agent reflections
- On variable change the list re-renders and **auto-scrolls to the newest message**
- Height is constrained; the inner list is scrollable

### VSM-side usage (SceneScript)

Appending a message is a pure variable update — no new VSM infrastructure
needed. The conversation variable is a string holding JSON.

Recommended pattern: use a dedicated SceneFlow variable of type `String`
(e.g. `conversation_log`) and a helper `PlayAction` (or SceneScript
expression) that appends to its JSON array:

```
// Pseudocode — actual implementation depends on available SceneScript functions
conversation_log = appendJsonMessage(conversation_log,
  "{\"role\":\"agent\",\"text\":\"" + $agent_utterance + "\"}")
```

A utility action in the `htmlgui-ws` plugin can provide `AppendMessage`
as a named action to make this ergonomic:

```xml
<playaction ref="htmlgui-ws:AppendMessage"
            var="conversation_log"
            role="agent"
            text="$agent_utterance"/>
```

### Save / review workflow (placeholder — see Phase 3)

1. After the session, the save plugin serialises `conversation_log` to disk.
2. On review, the load plugin restores `conversation_log` from disk.
3. An LLM plugin analyses the conversation and appends `system`-role messages
   with summaries or annotations.
4. The `vsm-feed` element renders the complete annotated history identically.

No changes to the feed element are required for this workflow.

### Editor support

- New `+Feed` button in the screen and panel toolbars
- Property panel:
  - Variable selector (`dataVar`)
  - Color pickers for agent / user / system roles
  - Label overrides for agent and user
  - Height input
  - Timestamp toggle
- `typeLabel`: `"⬜"` (or `"Feed"`)
- `elementSummary`: `"Feed · [dataVar]"` or `"Feed (no variable)"`

---

## Phase 2 — `vsm-animate`: Variable-Driven Animated Overlays

### Purpose

Display pre-built animations (heartbeat, breathing, wave, progress pulse, …)
whose visual *parameters* (rate, amplitude, color) are controlled live by
VSM variables. The animation structure is fixed and pre-authored; variables
only drive CSS custom properties.

### Concept

```
VSM variable  →  numeric value  →  CSS custom property  →  animation parameter
heart_rate    →  72 (BPM)       →  --vsm-rate           →  animation-duration: 833ms
```

### Schema element definition

```json
{
  "type": "vsm-animate",
  "animation": "heartbeat",
  "rateVar":      "heart_rate",
  "amplitudeVar": "heart_amplitude",
  "colorVar":     "heart_color",
  "width":  "80px",
  "height": "80px"
}
```

### Built-in animation catalogue

| `animation` value | Description | Driven by |
|---|---|---|
| `heartbeat` | SVG heart that pulses | `rateVar` (BPM → ms), `colorVar` |
| `breathe` | Expanding/contracting circle | `rateVar` (breaths/min), `colorVar` |
| `wave` | Horizontal sine wave | `rateVar` (Hz), `amplitudeVar`, `colorVar` |
| `pulse` | Radial ripple (e.g. microphone active) | `rateVar`, `colorVar` |
| `spinner` | Rotating arc | `rateVar` (RPM), `colorVar` |

### Variable-to-parameter mapping

Each `animation` type publishes a fixed set of CSS custom properties.
Variables are mapped to those properties via the element schema:

| Schema key | Variable value semantics | CSS custom property |
|---|---|---|
| `rateVar` | Numeric; semantics are animation-specific (BPM, Hz, RPM) | `--vsm-rate` → `animation-duration` |
| `amplitudeVar` | 0–100 scale | `--vsm-amplitude` |
| `colorVar` | CSS colour string or hex | `--vsm-color` |
| `opacityVar` | 0–100 → 0.0–1.0 | `--vsm-opacity` |

### Rendering behaviour

- Each built-in animation is a self-contained SVG or CSS keyframe block
  registered by name in the renderer
- On `updateVar`, only the affected CSS custom property is updated —
  no DOM reconstruction
- If a variable is not bound, the animation uses its default parameter values
- Unknown `animation` values show a placeholder with an error label

### Editor support

- New `+Animate` button in the screen and panel toolbars
- Property panel:
  - Animation picker (dropdown of built-in catalogue)
  - Variable selector for each supported parameter (rate, amplitude, color, opacity)
  - Width / height inputs
  - Live preview note: animation plays in the preview iframe immediately
- `typeLabel`: `"Anim"`
- `elementSummary`: `"[animation] · rate: [rateVar]"` or just the animation name

---

## Phase 3 — Save / Review / Annotation Workflow

> **Status: requires further discussion before implementation.**

The save and reload of conversation data involves an existing plugin (to be
identified). Key open questions:

- Which plugin handles persistence, and what is its interface?
- How does the load event signal the screen that new data is available?
  (Variable update is the natural answer, but the timing must be clarified.)
- Should the review screen be a separate VSM project/scene, or a runtime
  mode of the same project?
- How are LLM summaries triggered and where do their results land?
  (Variable? Direct feed append? Streaming?)
- Should the feed support a read-only / replay mode (no new messages,
  scroll controls only)?

---

## Implementation Checklist

### Phase 1 — `vsm-feed`

- [ ] `VsmFeedElement` LitElement in `vsm-renderer.js`
  - [ ] Parses JSON array from bound variable
  - [ ] Renders role-aware bubble list
  - [ ] Auto-scrolls to newest entry on variable change
  - [ ] Handles `system` role (centered, italic, no tail)
  - [ ] Configurable colors and labels per role
- [ ] `AppendMessage` PlayAction in `HtmlGuiWsExecutor.java`
  - [ ] Reads current variable value, appends new message object, writes back
- [ ] ScreenEditor: `+Feed` button, property panel
- [ ] Sync to `editor/web-ui/public/vsm-renderer.js`
- [ ] Build and commit

### Phase 2 — `vsm-animate`

- [ ] `VsmAnimateElement` LitElement in `vsm-renderer.js`
  - [ ] Animation registry (name → SVG/CSS template function)
  - [ ] CSS custom property update on variable change (no re-render)
  - [ ] Built-in: `heartbeat`, `breathe`, `wave`, `pulse`, `spinner`
- [ ] ScreenEditor: `+Animate` button, property panel with animation picker
- [ ] Sync and build and commit

### Phase 3 — deferred (pending discussion)

- [ ] Identify persistence plugin interface
- [ ] Design load → variable → feed pipeline
- [ ] Define LLM annotation workflow
- [ ] Implement read-only replay mode in `vsm-feed` (if needed)
