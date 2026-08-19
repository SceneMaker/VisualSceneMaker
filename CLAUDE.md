# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VisualSceneMaker is a visual dialogue authoring system with a hierarchical state machine (SceneFlow) editor. The project is transitioning from a Swing/JavaFX desktop application to a modern web-based architecture while maintaining backward compatibility.

**Key Architecture**: Three-tier MVC with event-driven communication
- **Core** (`core/`): Pure domain logic, runtime execution engine, web server (Java 17)
- **Editor** (`editor/`): Desktop UI (Swing/JavaFX) and Web UI (Svelte) (Java 21)
- **Runtime Server** (`runtime-server/`): Standalone headless runtime (Java 17, Android-compatible)
- **Plugins** (`plugins/`): 24+ extensible runtime plugins (Java 17)

## ⚠️ Ongoing Refactoring (2026-02-03)

**Status**: Removing Swing UI and creating distributed runtime architecture

**Current Progress**: Phases 1, 6, 7, 8 Complete ✅

The project is undergoing a major refactoring to:
- Remove all Swing/JavaFX desktop UI code
- Create headless runtime server (Android-compatible, Java 17)
- Enable Web UI editor to connect remotely to runtime instances
- Support runtime execution on Android devices and Desktop Java

**Important Documents**:
- **Implementation Plan**: `/Users/gebhard/.claude/plans/curried-noodling-milner.md` - Full 8-phase plan
- **Runtime Server Guide**: `doc/runtime-server.md` - Standalone runtime documentation
- **Architecture Guide**: `doc/architecture-details.md` - Comprehensive architecture documentation

**Completed Phases**:
- ✅ Phase 1: Core module decoupled from editor (Android-ready)
- ✅ Phase 6: WebUiServer refactored for headless operation
- ✅ Phase 7: Standalone `runtime-server` module created
- ✅ Phase 8: Unified server with mode switching, remote connection UI

**Key architectural decision (Phase 8.3)**: Rather than maintaining separate server classes, `WebUiServer` supports two modes via `ServerMode` enum:
- `FULL_EDITOR` — Multi-project editing, full WS commands (default, used by SceneMaker3/4)
- `RUNTIME_ONLY` — Single project, runtime control only, editing commands rejected (used by RuntimeMain)

## Build Commands

### Standard Build
```bash
./gradlew build
```

### Build with JavaFX Runtime Bundled (Legacy)
```bash
./gradlew build -PincludeJavaFx=true
```

### Run Tests
```bash
./gradlew test
```

### Run Single Test
```bash
./gradlew test --tests "de.dfki.vsm.xtension.decad.DecadExecutorTest"
```

### Web UI Development
```bash
cd editor/web-ui
npm install
npm run dev      # Development server with hot reload
npm run build    # Production build → editor/src/main/resources/web-ui/
```

## Running the Application

### Desktop + Web UI (Hybrid Mode)
```bash
java -jar build/libs/VisualSceneMaker-*.jar
# OR
./gradlew run
```

### Web UI Only Mode
```bash
java -jar build/libs/VisualSceneMaker-*.jar --no-swing
# OR use SceneMaker4 entry point
```

### Standalone Runtime Server
Uses `WebUiServer` in `RUNTIME_ONLY` mode — editing commands are rejected, runtime REST endpoints are available.
```bash
# Build
./gradlew :runtime-server:jar

# Run (localhost only)
java -jar runtime-server/build/libs/runtime-server-*.jar --port=8091

# Run with project auto-load
java -jar runtime-server/build/libs/runtime-server-*.jar --project=/path/to/project --autostart

# Run with LAN access (for remote Web UI)
java -jar runtime-server/build/libs/runtime-server-*.jar --allow-lan --port=8091

# With explicit auth token
java -jar runtime-server/build/libs/runtime-server-*.jar --token=mysecret --port=8091
```

### Command-Line Options
- `--allow-lan` - Bind web server to 0.0.0.0 (allows external connections)
- `--no-browser` - Don't auto-open browser
- `--no-swing` - Web-only mode (no desktop UI)
- `runtime <file.xml>` - Headless runtime execution
- `editor <file.xml>` - Open project in editor

### Web UI Server
- Default URL: `http://localhost:8090`
- REST API: `http://localhost:8090/api/v1/`
- WebSocket: `ws://localhost:8090/ws`

## High-Level Architecture

### Module Structure

```
VisualSceneMaker/
├── core/               # Runtime engine, domain model, web server (Java 17)
│   ├── model/         # SceneFlow, SceneScript, ProjectConfig (pure POJOs)
│   ├── runtime/       # Interpreter, ActivityExecutor, plugin system
│   ├── event/         # EventDispatcher (pub/sub system)
│   ├── ui/protocol/   # UiEventBus, UiEventBridge (domain → UI translation)
│   └── web/           # WebUiServer (Javalin REST + WebSocket, dual-mode)
├── editor/            # UI layer (Java 21)
│   ├── src/.../editor/ # Swing/JavaFX desktop UI
│   └── web-ui/        # Svelte web UI (builds to resources/web-ui/)
├── runtime-server/    # Standalone headless runtime (Java 17, Android-compatible)
│   └── RuntimeMain    # Entry point, uses WebUiServer in RUNTIME_ONLY mode
├── plugins/           # Runtime plugin modules (Java 17)
│   ├── charamel-ws/   # Character animation (WebSocket)
│   ├── htmlgui-ws/    # Custom HTML UI during runtime
│   ├── unity/         # Unity3D integration
│   └── [21 more...]
└── src/main/java/de/dfki/vsm/
    ├── SceneMaker3.java  # Main entry (Swing + Web)
    └── SceneMaker4.java  # Web-only entry
```

### Event-Driven Communication

**Core Event Flow**:
1. Runtime events (node execution, variable changes) → `EventDispatcher`
2. `EventDispatcher` multicasts to all registered `EventListener`s
3. `UiEventBridge` translates domain events → `UiEvent` (UI-specific format)
4. `UiEventBus` emits to registered listeners (Desktop UI, WebSocket clients)
5. Web UI receives broadcasts via WebSocket, updates reactive state

**Key Classes**:
- `EventDispatcher` (core/event/) - Singleton pub/sub with `CopyOnWriteArrayList`
- `UiEventBridge` (core/ui/protocol/) - Implements `EventListener`, translates events
- `UiEventBus` (core/ui/protocol/) - UI-specific event sink with lazy evaluation
- `WebUiServer` (core/web/) - Javalin server with `ServerMode` (FULL_EDITOR / RUNTIME_ONLY)

### Runtime States

`ref.runtimeState` / `RuntimeOrchestrator.RuntimeState` are `stopped`, `running`, `paused` and
`finished`. **`finished` is not `stopped`**: a flow that reaches a node with no outgoing edge ends
that thread and nothing else, so the session is over while the devices it opened are still connected.
Only `stopped` releases the project's exclusive resources. Ending is the one transition the
interpreter never announces (`TerminationEvent` is fired on errors only), so
`WebUiServer.checkForFinishedRuntimes` polls `RunTimeProject.isRunning()` once a second for projects
whose state says running. Before that existed, a finished flow read as still running until someone
pressed Stop.

### Runtime Execution Model

**Execution Flow**:
1. `RunTimeProject.launch()` - Load SceneFlow, SceneScript, ProjectConfig
2. `RunTimeProject.start()` - Start Interpreter thread
3. `Interpreter` (extends Thread) - State machine executor
   - Maintains `Configuration` (active nodes/edges)
   - Maintains `Environment` (variable scopes)
   - Uses `Evaluator` for expression evaluation
   - Fires events: `NodeStartedEvent`, `EdgeExecutedEvent`, `NodeExecutedEvent`
4. `ActivityExecutor` - Async execution of actions via plugins
5. Plugins loaded dynamically via reflection: `de.dfki.vsm.xtension.<name>.<Name>Executor`

**Thread Architecture**:
- Main thread → spawns Swing EDT (if desktop mode)
- Javalin thread pool → HTTP/WebSocket handlers
- Interpreter thread (per project) → SceneFlow execution
- ActivityScheduler threads → plugin action execution

### Web UI Architecture

**Frontend** (Svelte):
- `App.svelte` - Root component, WebSocket connection, session management
- `SceneFlowView.svelte` - SVG-based graph editor
- `ScriptEditor.svelte` - CodeMirror integration for scene scripts
- Build: Vite → outputs to `editor/src/main/resources/web-ui/`

**Backend** (Javalin):
- REST API (`/api/v1/*`) - Project CRUD, configuration, runtime state
- WebSocket (`/ws`) - Real-time bidirectional communication
  - Request: `{ "id": "uuid", "method": "SceneFlow.Node.Add", "params": {...} }`
  - Response: `{ "id": "uuid", "status": "ok", "result": {...} }`
  - Broadcast: `{ "event": "sceneflow.snapshot", "projectId": "...", "snapshot": {...} }`

**Key Endpoints** (both modes):
- `GET /api/v1/info` - Server info (includes `mode` field)
- `GET /api/v1/projects` - List active projects
- `GET /api/v1/projects/{pid}/sceneflow` - Get SceneFlow graph snapshot
- `GET /api/v1/projects/{pid}/capabilities` - Capability snapshot: plugins with their declared
  commands, agents, scenes, screens with their variable bindings, and flow shape (built by
  `CapabilitySnapshotBuilder`; the contract is `doc/capability-snapshot.schema.json`). The command
  inventory reflects the plugin specs on the **serving deployment's** classpath, so a deployment
  missing a plugin jar reports that plugin with no commands. Fat JARs get those specs from
  `vsm-plugin-registry.json`, aggregated by `gradle/plugin-registry.gradle`; any module building a
  fat JAR must apply that script, or deduplication leaves it seeing a single plugin.
- `GET /api/v1/projects/{pid}/runtime` - Get runtime state

**Editor-only endpoints** (FULL_EDITOR mode):
- `POST /api/v1/projects/open` - Open project file
- `POST /api/v1/projects/{pid}/save` - Save project
- `GET /api/v1/sceneflow/patterns` - Interaction patterns the Flow Assistant can build
- `POST /api/v1/projects/{pid}/flow-assistant/propose` - Situation text → a proposal; changes nothing.
  Generated against the flow **as it stands in the editor**, not the file on disk. The compiled result
  stays server-side keyed by a proposal id; only the author-facing view is sent (never the IR).
  Body: `situation`, and `readinessGate` (default true) which puts a wait for the agents in front of a
  flow that would otherwise start by using one. Which language service the assistant may use comes
  from the **project**, not the request: `LLMSelections/flowAssistant` names one of the project's
  `<LLM>` entries. Unset means patterns only, which is the default; set, it is consulted only for
  situations no pattern recognises (`CandidateMode.TEMPLATE_THEN_LLM`).
- `POST /api/v1/projects/{pid}/flow-assistant/apply` / `discard` - Take or drop a proposal. Apply is
  one undoable step (`SceneFlow.FlowAssistant.Apply`), and consumes the proposal. It first carries
  out the proposal's `setup` steps (device, agent, screen; see `FlowAssistantSetup`) and then applies
  the flow, in that order, because the flow was generated against the project as it will be.

**Runtime-only endpoints** (RUNTIME_ONLY mode):
- `POST /api/v1/runtime/load` - Load project by path
- `POST /api/v1/runtime/start` / `stop` / `pause` / `resume` - Runtime control
- `GET /api/v1/runtime/status` - Runtime status

### Plugin System

**Loading Mechanism**:
- Reflection-based: `Class.forName("de.dfki.vsm.xtension.<name>.<Name>Executor")`
- Constructor: `(PluginConfig config, RunTimeProject project)`
- Lifecycle: `launch()` → active → `unload()`
- Plugins stored in `Map<String, RunTimePlugin>` in `RunTimeProject`
- **A device added to an already-open project needs `RunTimeProject.loadRunTimePlugin(config)`.**
  Plugin objects are otherwise created once, while the project is being read, so a device added later
  sits in the configuration with nothing behind it: starting the project opens no port and shows
  nothing, and only reopening the project helps. `applyProjectConfigFromJson` calls it for every
  plugin in an applied config, which covers the add-device dialog and the Flow Assistant alike.

**Plugin Interface** (core/runtime/plugin/RunTimePlugin.java):
```java
public abstract class RunTimePlugin {
    public abstract void launch();
    public abstract void unload();
    public abstract void execute(AbstractActivity activity);
}
```

**Important Plugins**:
- `htmlgui-ws` - WebSocket-based custom HTML UI (separate from main Web UI). Two ways to present
  something: a hand-written page under the project's `gui/` (legacy, e.g. `doc/IntakeInterview`), or
  screens declared in `screens.json` and drawn by the renderer served from the plugin jar
  (schema-driven, what the Flow Assistant sets up). In the schema-driven case the shell page and the
  renderer talk over `postMessage`, and the shell **buffers until the renderer reports
  `rendererReady`**: postMessage has no queue, so anything sent before the renderer's listener exists
  is lost outright, which used to swallow the first spoken line of every flow that speaks as soon as
  the browser connects. `plugins/htmlgui-ws/src/test/js/wsclient-buffering.test.mjs` guards it
  (`node --test`, not part of `gradle test`).
  An agent on this device needs a `var` feature naming where its lines go; see the agent features it
  declares under `config.agent.fixed`.
- `charamel-ws` - Character animation system
- `unity` - Unity3D integration
- `console`, `email`, `timer` - Utility plugins

## Java Compatibility Requirements

**Critical**: Core and plugins MUST remain Java 17 compatible for Android deployment.

- **Core + Plugins**: Java 17 (enforced via `options.release = 17`)
  - No Java 18+ language features or APIs
  - No desktop-only dependencies (Swing/JavaFX)
  - Ensures Android runtime compatibility

- **Editor + Root**: Java 21 allowed
  - Can use modern JVM features
  - Desktop UI toolkits permitted

**Enforcement**: See `build.gradle` lines 16-30 for conditional toolchain configuration.

## Key Patterns and Conventions

### Naming Conventions
- Classes: `PascalCase`
- Methods: `camelCase`
- Member variables: `mCamelCase` (prefix `m`)
- Static fields: `sCamelCase` (prefix `s`)
- Constants: `UPPER_SNAKE_CASE`

### Thread Safety
- `EventDispatcher`: Uses `CopyOnWriteArrayList` for thread-safe listener management
- `Interpreter`: Uses `ReentrantLock` + `Condition` for pause/resume
- `WebUiServer`: Uses `ConcurrentHashMap.newKeySet()` for WebSocket sessions
- `Preferences`: Synchronized methods for property access

### Logging
Use `LOGDefaultLogger.getInstance()`:
```java
mLogger.message("Info message");
mLogger.warning("Warning message");
mLogger.failure("Error message");
```

### Event Handling
- Fire events via `EventDispatcher.getInstance().convey(event)`
- Events are delivered synchronously - keep handlers fast
- Use lazy evaluation in `UiEventBus` to avoid payload construction overhead

## Critical Files Reference

### Entry Points
- `src/main/java/de/dfki/vsm/SceneMaker3.java` - Main launcher (Swing + Web)
- `src/main/java/de/dfki/vsm/SceneMaker4.java` - Web-only launcher
- `runtime-server/src/main/java/de/dfki/vsm/runtime/RuntimeMain.java` - Standalone runtime server
- `core/src/main/java/de/dfki/vsm/Core.java` - Headless runtime

### Core Runtime
- `core/src/main/java/de/dfki/vsm/runtime/project/RunTimeProject.java` - Runtime container
- `core/src/main/java/de/dfki/vsm/runtime/interpreter/Interpreter.java` - State machine executor
- `core/src/main/java/de/dfki/vsm/event/EventDispatcher.java` - Central event bus

### Web Server
- `core/src/main/java/de/dfki/vsm/web/WebUiServer.java` - Unified Javalin server (FULL_EDITOR + RUNTIME_ONLY modes)
- `core/src/main/java/de/dfki/vsm/web/SceneFlowSnapshotBuilder.java` - Shared snapshot JSON builder
- `core-webserver/src/main/java/de/dfki/vsm/web/CapabilitySnapshotBuilder.java` - Capability snapshot
  (what a project offers: plugins, agents, scenes, flow shape). Used by both the REST endpoint and
  `./gradlew generateCapabilitySnapshot`, so build-time and served snapshots cannot drift. Build it
  from a loaded project, or from a directory via `buildFromDirectory`, which uses
  `parseForInformation` so that describing a project never launches its plugins, and aborts the
  project's event dispatcher afterwards (its timer thread is not a daemon, so a CLI would otherwise
  write its output and then hang).
- `core-webserver/src/main/java/de/dfki/vsm/web/FlowAssistantSetup.java` - What a project is still
  missing (device, agent, screen) and why, plus the **projected** capability snapshot the flow is
  generated against. Deliberately omits the new device's flow variables so the generator declares
  them; scenes are never planned, since only the author can write them.
- `core-webserver/src/main/java/de/dfki/vsm/web/FlowAssistantService.java` - Situation → proposal,
  on top of `de.dfki.vsm.sceneflow.ir` (which lives in this module, not the root one). Holds the
  compiled flow until the author applies or discards it, and translates the generated operations
  into author-facing sentences. The IR must never reach a client; a test asserts the author-facing
  view carries none of the generator's vocabulary.
- `core/src/main/java/de/dfki/vsm/ui/protocol/UiEventBridge.java` - Domain → UI event translation

### Model Layer
- `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/SceneFlow.java` - Hierarchical state machine
- `core/src/main/java/de/dfki/vsm/model/scenescript/SceneScript.java` - Dialogue content
- `core/src/main/java/de/dfki/vsm/model/project/ProjectConfig.java` - Project configuration

### Desktop UI
- `editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java` - Main Swing window
- `editor/src/main/java/de/dfki/vsm/editor/project/ProjectEditor.java` - Project view

### Web UI
- `editor/web-ui/src/App.svelte` - Root component
- `editor/web-ui/src/SceneFlowView.svelte` - Graph editor
- `editor/web-ui/package.json` - Frontend build configuration

## Documentation

Additional architecture details are in `doc/`:
- `vsm-modelling-support.md` - Interaction patterns and the Flow Assistant. **Section 1a is the
  current state**: which patterns work, what the assistant provisions, and the traps that only showed
  up when a first author started from an empty project. The rest is the reasoning it was built from,
  including the four outcomes a requirement can have (§4a) and why creating a resource is not a flow
  operation (§4b). Was `sceneflow-modelling-support-concept.md` until 2026-08-19.
- `architecture-details.md` - Comprehensive architecture guide (unified server model)
- `runtime-server.md` - Standalone runtime server guide (deployment, API, usage)
- `scenemaker-java-compatibility.md` - Java 17/21 compatibility policy
- `scenemaker-ui-protocol.md` - UI event protocol specification
- `scenemaker-web-ui-api.md` - REST/WebSocket API documentation
- `scenemaker-web-ui-parity.md` - Web UI feature parity tracking
- `socialsignal-plugin.md` - SocialSignalStream plugin: variables, actions, local testing, and the
  open design question on switching signal groups off to save resources

**User-facing in-app guides** (served statically, source in `editor/web-ui/public/`):
- `scenescript-help.html` - SceneScript authoring guide (scenes/turns/commands/editing). Keep this
  current whenever script authoring gains a new construct or editing feature, and bump its version
  pill/footer when it changes.
- `sceneflow-help.html` - SceneFlow editing guide (canvas/panels, node & edge types, terminology,
  interaction-flow patterns drawn from `doc/DesignPatterns`). Opened via the "?" button next to the
  SceneFlow canvas's info-overlay toggle. Keep current whenever SceneFlow editing gains a new
  construct, and keep its pattern examples in sync with `doc/DesignPatterns` if one changes.
- `screen-element-reference.html` - htmlgui-ws screen element reference

## Plugin Spec Versioning

Each `plugin-properties.json` that participates in version tracking carries two fields:

```json
"specVersion": "1.1",   // human-readable semver — bump MANUALLY
"specHash":    "abc123" // 16-hex SHA-256 of structural content — updated by Gradle
```

### When you change a plugin-properties.json

**Structural changes** = anything that affects the generated SceneFlow variables or callable commands:
adding/removing/renaming config entries, changing `default` or `sceneflowtype`, adding/removing
`variables.writes` entries, adding/removing commands.

1. Make your structural change in the JSON.
2. Run `./gradlew updatePluginSpecs` — this rewrites `specHash` to reflect the new content.
3. **Manually bump `specVersion`** (e.g. `"1.1"` → `"1.2"`). The Gradle task never does this.
4. Commit both the JSON change and the updated hash together.

`./gradlew check` (and therefore `./gradlew build`) runs `verifyPluginSpecs`, which fails if
`specHash` is stale — so a forgotten bump will block the build before it reaches CI.

Non-structural changes (descriptions, tags, comments) do not require a version bump; just run
`updatePluginSpecs` to keep the hash consistent if it changes.

## Common Development Tasks

### Adding a New Plugin

1. Create plugin directory: `plugins/myplugin/`
2. Implement `MypluginExecutor extends RunTimePlugin`
3. Add to `settings.gradle`: `include 'plugins:myplugin'`
4. Create `build.gradle` with Java 17 compatibility
5. Use naming convention: `de.dfki.vsm.xtension.myplugin.MypluginExecutor`

### Adding a REST Endpoint

Edit `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`:
```java
private void registerRoutes() {
    mApp.get(API_PREFIX + "/new/endpoint", this::handleNewEndpoint);
}

private void handleNewEndpoint(Context ctx) {
    JSONObject response = new JSONObject();
    response.put("result", processLogic());
    writeJson(ctx, response);
}
```

### Adding a WebSocket Method

Edit `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`:
```java
private void handleWsMessage(...) {
    if ("Custom.Action".equals(method)) {
        // Handle custom action
        reply.accept(createResponse(msg.getString("id"), "ok", result));
    }
}
```

### Modifying the Web UI

1. Edit files in `editor/web-ui/src/`
2. Run `npm run dev` for hot reload during development
3. Run `npm run build` to create production bundle
4. Built files go to `editor/src/main/resources/web-ui/`
5. Commit both source and built files (built files served by Javalin)

## Migration Context

The project is actively migrating from Swing UI to Web UI:
- **Current state**: Both UIs coexist, share same backend
- **Goal**: Web UI as primary interface, optional desktop UI
- **Future**: Android deployment requires core/plugins at Java 17
- **Web UI benefits**: Cross-platform, remote access, collaborative editing
- Swing UI remains reference implementation during transition
