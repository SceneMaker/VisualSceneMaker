# VSM Deployment and Tutorial Plan

**Audience**: Non-engineers (e.g. psychologists)
**Goal**: Make VSM installable and usable without technical knowledge
**Date**: 2026-05-25

---

## Overview

Two parallel tracks:

| Track | Goal |
|---|---|
| **A — Installer** | Native installers for Windows, macOS, Linux using `jpackage` with bundled JRE |
| **B — Tutorial** | First-run wizard + intake interview demo project + contextual authoring tour |

Track B is implemented first (platform-independent, highest immediate value).

---

## Track A — Installer (planned, not yet started)

### Platform strategy
- **Tool**: `jpackage` (Java 17+ built-in), bundled JRE, no separate Java install needed
- **Windows**: `.msi` installer
- **macOS**: `.dmg` (unsigned — see note below)
- **Linux**: `.deb` + `.AppImage`
- **Auto-update**: out of scope for v1, users re-download

### macOS code signing policy
VSM is free research software. The $99/year Apple Developer certificate is not viable.
**Approach**: Ship unsigned `.dmg` with a prominent "How to open on Mac" note:

> Right-click the app → Open → click "Open" in the security dialog.
> You only need to do this once.

This is the standard approach for free academic software (e.g. many neuroimaging tools, R packages with GUI).
Do NOT tell users to disable Gatekeeper system-wide — only the single-app bypass.

### Phases
| Phase | Work |
|---|---|
| A1 | Configure `jpackage` in Gradle for all 3 platforms, bundle JRE + web assets |
| A2 | macOS: build `.dmg`, write "how to open" note, test on clean machine |
| A3 | Windows: `.msi` installer |
| A4 | Linux: `.deb` + `.AppImage` |
| A5 | GitHub Actions CI to build all 3 installers on each release tag |

---

## Track B — Onboarding + Tutorial

### B0 — LLM Plugin ✅ (in progress)

New `plugins/llm/` module wrapping the existing `core/.../util/llm/LLMSupport.java`.

**Class**: `de.dfki.vsm.xtension.llm.LlmExecutor`

**Config** (in `project.xml`):
| Key | Default | Description |
|---|---|---|
| `base_url` | `https://api.openai.com/v1/` | Any OpenAI-compatible endpoint (OpenAI, LM Studio, Ollama) |
| `api_key` | _(empty)_ | Bearer token; omit for local servers |
| `model` | `gpt-4o-mini` | Model ID |
| `temperature` | `0.7` | Sampling temperature |
| `timeout_sec` | `30` | Request timeout |

**Action**: `send(prompt='...', system='...', responseVar='varName')`
- `prompt` and `system` support `{varName}` interpolation — placeholders are replaced with live SceneFlow variable values at call time.
- Response is written to `responseVar` (default: `llm_response`).
- The call is async; the SceneFlow polls the response variable via a CEdge loop.

**Supported providers in v1**: OpenAI + any OpenAI-compatible local server (LM Studio, Ollama).
Anthropic requires a separate adapter (different API format) — planned for v2.

### B0-ext — htmlgui-ws `appendMessage` extension

Small addition to `HtmlGuiWsExecutor.java`: if `text=@varName`, the executor reads the named SceneFlow variable and uses its value as the message text. This allows displaying dynamically generated text (e.g. an LLM summary) in the chat feed without requiring static text in SceneScript.

```xml
<!-- Display the LLM summary in the chat -->
<ActionObject actor="gui" name="appendMessage">
  <ActionFeature key="var" val="conversation_log"/>
  <ActionFeature key="role" val="agent"/>
  <ActionFeature key="text" val="@llm_summary"/>
</ActionObject>
```

### B1 — First-Run Setup Wizard (planned)

A multi-step linear screen shown instead of the main editor on first launch.

**Detection**: `~/.vsm/global-config.json` — created on first run, `setup.completed: true` suppresses the wizard on subsequent launches.

**New backend endpoints** in `WebUiServer`:
| Endpoint | Purpose |
|---|---|
| `GET /api/v1/setup/status` | Returns `{"firstRun": true/false}` |
| `POST /api/v1/setup/llm` | Saves provider + API key to global config |
| `POST /api/v1/setup/complete` | Marks wizard done |
| (reuse) `POST /api/v1/llm/test` | Tests LLM connectivity — already exists |

**Wizard steps** (Svelte component, shown full-screen):
```
Step 1 — Welcome
  "VisualSceneMaker lets you author dialogue systems with virtual characters.
   This wizard takes ~3 minutes."

Step 2 — LLM Provider
  Radio: ○ OpenAI   ○ Local LLM (LM Studio / Ollama)
  Info: links to openai.com/api for key signup

Step 3 — API Key / URL
  OpenAI: password field + "Test Connection" button
  Local: URL field (default http://localhost:1234) + "Test Connection"
  "Your key is stored locally in ~/.vsm/global-config.json"

Step 4 — Done
  [ Open Tutorial Project ]   [ Start with empty project ]
```

**Global config stored as**:
```json
{
  "llm": { "provider": "openai", "baseUrl": "https://api.openai.com/v1/", "apiKey": "sk-...", "model": "gpt-4o-mini" },
  "setup": { "completed": true }
}
```

### B2 — Tutorial Project: Intake Interview ✅ (in progress)

Location: `doc/IntakeInterview/`

**Scenario**: "Chat with Alex" — a digital intake assistant conducting a short anamnesis interview. After collecting 6 answers, the LLM generates a brief clinical summary.

**Technology**:
- `htmlgui-ws` plugin: serves `gui/chat.html` — static avatar image (Alex) + chat bubble feed + text input
- `llm` plugin: called once at the end to generate the intake summary
- User messages echoed locally by the HTML (no VSM roundtrip for display)
- Alex's messages sent via `appendMessage` from SceneScript
- LLM summary displayed via `appendMessage` with `text=@llm_summary`

**Interview flow** (6 questions + LLM summary):
1. Greeting
2. "What is your first name?"
3. "What is the main reason you're seeking support today?"
4. "How long have you been experiencing this?"
5. "How is this affecting your daily life — sleep, work, relationships?"
6. "Have you received professional support for this before?"
7. LLM generates 3–4 sentence clinical summary from collected answers
8. Summary displayed in chat
9. "Is there anything you'd like to add?"
10. Closing

**SceneFlow pattern** (per question):
```
[AskXxx] → PlayScene "ask_xxx", clear user_input → EEdge
[WaitXxx] → TEdge(500ms self-loop) + CEdge(user_input ≠ "" → StoreXxx)
[StoreXxx] → Assignment(xxx = user_input) → EEdge → [AskYyy]
```

**LLM call pattern**:
```
[CallLLM] → PlayScene "thinking", clear llm_summary → EEdge
[WaitLLM] → TEdge(1000ms self-loop) + CEdge(llm_summary ≠ "" → ShowSummary)
[ShowSummary] → PlayScene "show_summary" (appendMessage text=@llm_summary) → EEdge
```

**SceneFlow variables**:
- `gui_connected` (Bool) — set by htmlgui-ws when HTML loads
- `user_input` (String) — current typed user input
- `patient_name`, `main_concern`, `duration_text`, `daily_impact`, `previous_treatment`, `additional_notes` (String)
- `llm_summary` (String) — LLM-generated summary
- `conversation_log` (String) — JSON array managed by htmlgui-ws for chat display

**Avatar**: `doc/Alex.png` copied to `gui/alex.png` — professional AI-generated portrait.

### B3 — Contextual Authoring Tour (planned)

Shepherd.js overlay integrated into the Svelte web UI. Triggered automatically when the tutorial project first loads; re-accessible from a Help menu.

**Tour steps** (anchored to real UI elements):
1. "This is the SceneFlow — the brain of your dialogue." → graph canvas
2. "Each box is a Node — a moment in the conversation." → Start node
3. "Arrows are Edges — they define what happens next." → an edge
4. "Click ▶ Run to start the dialogue." → run button
5. "Try it — a browser window opens with Alex." → user runs project
6. "To add a node, right-click the canvas." → canvas
7. "That's the basics. Help menu has more." → tour ends

---

## Future Work

### F1 — Browser-close exits the app

**Problem**: When the user closes the browser tab or window, the VSM server process keeps running invisibly in the background. Non-technical users have no way to notice or quit it — this breaks the expected "closing the app closes the app" contract of a native desktop application.

**Proposed behaviour**:
- **Default (installer mode)**: when the last WebSocket client disconnects and no project is actively running, start a ~30-second grace period then call `System.exit(0)`.
- **Server mode (advanced users)**: a `--server` CLI flag suppresses auto-exit, keeping the process alive for headless or remote-access deployments. A toggle on the web landing page should also be able to flip this at runtime.

**Implementation sketch**:
- `WebUiServer` tracks WebSocket session count; on drop-to-zero, schedule the shutdown timer.
- `SceneMaker4` / `RuntimeMain` accept a `--server` flag that sets a static `serverMode = true` field which `WebUiServer` checks before scheduling exit.
- A `POST /api/v1/server-mode` endpoint (or landing-page toggle) lets advanced users switch modes without restarting.

---

## Dependency Graph

```
B0 (LLM plugin)  ─────────────────────────┐
B0-ext (htmlgui-ws @varName) ─────────────┤
                                           ▼
B1.1-B1.2 (backend) → B1.3-B1.5 (wizard UI)
                                           ▼
B2 (tutorial project) ─────────────────── ▼
                                           ▼
                              B3 (authoring tour)
```

B0, B0-ext, and B2 are being implemented now.
B1 and B3 follow after B2 is validated.
