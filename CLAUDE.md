# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VisualSceneMaker is a visual dialogue authoring system with a hierarchical state machine (SceneFlow) editor. The project is transitioning from a Swing/JavaFX desktop application to a modern web-based architecture while maintaining backward compatibility.

**Key Architecture**: Three-tier MVC with event-driven communication
- **Core** (`core/`): Pure domain logic, runtime execution engine, web server (Java 17)
- **Editor** (`editor/`): Desktop UI (Swing/JavaFX) and Web UI (Svelte) (Java 21)
- **Runtime Server** (`runtime-server/`): Standalone headless runtime (Java 17, Android-compatible)
- **Plugins** (`plugins/`): 24+ extensible runtime plugins (Java 17)

## ⚠️ Ongoing Refactoring (2026-01-26)

**Status**: Removing Swing UI and creating distributed runtime architecture

**Current Progress**: Phases 1, 6, 7 Complete ✅

The project is undergoing a major refactoring to:
- Remove all Swing/JavaFX desktop UI code
- Create headless runtime server (Android-compatible, Java 17)
- Enable Web UI editor to connect remotely to runtime instances
- Support runtime execution on Android devices and Desktop Java

**Important Documents**:
- **Implementation Plan**: `/Users/gebhard/.claude/plans/curried-noodling-milner.md` - Full 8-phase plan
- **Runtime Server Guide**: `doc/runtime-server.md` - Standalone runtime documentation
- **Phase 2 Guide**: `doc/phase-2-implementation-guide.md` - Service extraction guide

**Completed Phases**:
- ✅ Phase 1: Core module decoupled from editor (Android-ready)
- ✅ Phase 6: WebUiServer refactored for headless operation
- ✅ Phase 7: Standalone `runtime-server` module created

**Next Phase**: Phase 8 - Service extraction and remote connection infrastructure

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
```bash
# Build
./gradlew :runtime-server:jar

# Run (localhost only)
java -jar runtime-server/build/libs/runtime-server-*.jar --port=8091

# Run with project auto-load
java -jar runtime-server/build/libs/runtime-server-*.jar --project=/path/to/project --autostart

# Run with LAN access (for remote Web UI)
java -jar runtime-server/build/libs/runtime-server-*.jar --allow-lan --port=8091
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
│   └── web/           # WebUiServer (Javalin-based REST + WebSocket)
├── editor/            # UI layer (Java 21)
│   ├── src/.../editor/ # Swing/JavaFX desktop UI
│   └── web-ui/        # Svelte web UI (builds to resources/web-ui/)
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
- `WebUiServer` (core/web/) - Javalin server, broadcasts to WebSocket clients

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

**Key Endpoints**:
- `GET /api/v1/projects` - List active projects
- `POST /api/v1/projects/open` - Open project file
- `GET /api/v1/projects/{pid}/sceneflow` - Get SceneFlow graph snapshot
- `GET /api/v1/projects/{pid}/runtime` - Get runtime state

### Plugin System

**Loading Mechanism**:
- Reflection-based: `Class.forName("de.dfki.vsm.xtension.<name>.<Name>Executor")`
- Constructor: `(PluginConfig config, RunTimeProject project)`
- Lifecycle: `launch()` → active → `unload()`
- Plugins stored in `Map<String, RunTimePlugin>` in `RunTimeProject`

**Plugin Interface** (core/runtime/plugin/RunTimePlugin.java):
```java
public abstract class RunTimePlugin {
    public abstract void launch();
    public abstract void unload();
    public abstract void execute(AbstractActivity activity);
}
```

**Important Plugins**:
- `htmlgui-ws` - WebSocket-based custom HTML UI (separate from main Web UI)
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
- `core/src/main/java/de/dfki/vsm/web/WebUiServer.java` - Javalin REST + WebSocket server
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
- `runtime-server.md` - Standalone runtime server guide (deployment, API, usage)
- `architecture-details.md` - Comprehensive architecture guide
- `scenemaker-java-compatibility.md` - Java 17/21 compatibility policy
- `scenemaker-ui-protocol.md` - UI event protocol specification
- `scenemaker-web-ui-api.md` - REST/WebSocket API documentation
- `scenemaker-web-ui-parity.md` - Web UI feature parity tracking

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
