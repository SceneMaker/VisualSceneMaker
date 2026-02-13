# VisualSceneMaker Architecture - Detailed Documentation

**Branch:** web2026
**Date:** 2026-01-11
**Purpose:** Developer guide for understanding the MVC architecture and code organization

---

## Table of Contents

1. [Overview](#overview)
2. [MVC Architecture](#mvc-architecture)
3. [Model Layer](#model-layer)
4. [View Layer](#view-layer)
5. [Controller Layer](#controller-layer)
6. [Communication Patterns](#communication-patterns)
7. [Key Workflows](#key-workflows)
8. [Plugin System](#plugin-system)
9. [Thread Safety](#thread-safety)
10. [Future Development Guidelines](#future-development-guidelines)

---

## Overview

VisualSceneMaker implements a **layered MVC + Event-Driven architecture** with support for both desktop (Swing/JavaFX) and web (Svelte) user interfaces. The architecture emphasizes:

- **Separation of concerns**: Pure model layer independent of UI
- **Dual UI support**: Desktop and Web UIs share the same runtime engine
- **Event-driven communication**: Asynchronous pub/sub for cross-cutting concerns
- **Plugin extensibility**: Dynamic loading of custom agents and actions
- **Thread safety**: Lock-based synchronization for concurrent execution

### Architecture Diagram

See `doc/architecture-overview.svg` for a visual representation of the system architecture.

---

## MVC Architecture

### Model Layer (Pure Domain)
- **Location**: `core/src/main/java/de/dfki/vsm/model/`
- **Purpose**: Domain entities, data structures, business logic
- **Dependencies**: None (pure POJOs with XML serialization)

### View Layer (User Interfaces)
- **Desktop UI**: `editor/src/main/java/de/dfki/vsm/editor/` (Swing/JavaFX)
- **Web UI**: `editor/web-ui/src/` (Svelte components)
- **Web Server**: `core/src/main/java/de/dfki/vsm/web/` (Javalin)

### Controller Layer (Coordination)
- **Event System**: `core/src/main/java/de/dfki/vsm/event/`
- **UI Protocol**: `core/src/main/java/de/dfki/vsm/ui/protocol/`
- **Runtime Control**: `core/src/main/java/de/dfki/vsm/runtime/project/`

---

## Model Layer

### Core Data Models

#### Project Configuration

**ProjectConfig** - Central project metadata
Path: `core/src/main/java/de/dfki/vsm/model/project/ProjectConfig.java`

```java
public class ProjectConfig extends ModelObject {
    private String mProjectName;
    private HashMap<String, AgentConfig> mAgentConfigMap;
    private HashMap<String, PluginConfig> mPluginConfigMap;
    private PlayerConfig mPlayerConfig;
    private EditorConfig mEditorConfig;
    // ... XML serialization methods
}
```

**Related Classes:**
- `AgentConfig.java` - Agent-specific configuration
- `PluginConfig.java` - Plugin-specific configuration
- `PlayerConfig.java` - Runtime player settings
- `EditorConfig.java` - Editor preferences

#### SceneFlow (Visual State Machine)

**SceneFlow** - Hierarchical graph of nodes and edges
Path: `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/SceneFlow.java`

```java
public class SceneFlow extends SuperNode {
    private HashMap<String, BasicNode> mStartNodeMap;
    private HashMap<String, CmdBadge> mCCmdMap;
    private HashMap<String, Typedef> mTypeDefMap;
    private HashMap<String, VarDef> mVarDefMap;
    // Inherited: NodeList, EdgeList, GraphicsData
}
```

**Node Types:**
- `BasicNode.java` - Atomic execution units (scenes)
- `SuperNode.java` - Composite nodes (hierarchical containers)

**Edge Types:**
Path: `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/edge/`
- `AbstractEdge.java` - Base class
- `GuargedEdge.java` (CEDGE) - Conditional transitions
- `RandomEdge.java` (PEDGE) - Probabilistic transitions
- `TimeoutEdge.java` (TEDGE) - Time-based transitions
- `InterruptEdge.java` (IEDGE) - Interrupt handling
- `EpsilonEdge.java` (EEDGE) - Epsilon transitions
- `ForkingEdge.java` (FEDGE) - Parallel execution

#### SceneScript (Dialogue Content)

**SceneScript** - Dialogue specification
Path: `core/src/main/java/de/dfki/vsm/model/scenescript/SceneScript.java`

```java
public class SceneScript extends ScriptEntity {
    private List<SceneGroup> mSceneGroupList;
    private List<SceneComment> mSceneCommentList;
    private HashMap<String, String> mLanguageMap;
    // Multi-language dialogue support
}
```

#### Glue Language (Scripting)

**Command/Expression System**
Path: `core/src/main/java/de/dfki/vsm/model/sceneflow/glue/`

- `Command.java` - Base command class
- `Expression.java` - Base expression class
- `Assignment.java` - Variable assignment
- `Definition.java` - Variable/function definitions
- `Invocation.java` - Function calls

**Expression Types:**
Path: `glue/command/expression/`
- Literals: `IntLiteral`, `FloatLiteral`, `StringLiteral`, `BooleanLiteral`
- Variables: `VariableExpression`
- Operations: `BinaryExpression`, `UnaryExpression`, `TernaryExpression`
- Conditions: `Comparison`, `LogicalExpression`, `TemporalExpression`
- Queries: `PrologQuery`, `RandomQuery`, `HistoryQuery`

#### Graphics Models

**Visualization Metadata**
Path: `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/graphics/`

- `NodeGraphics.java` - Node position, size, color
- `EdgeGraphics.java` - Edge paths, connection points
- `CommentGraphics.java` - Comment rectangles

**Badges:**
- `VariableBadge.java` - Variable display configuration
- `CommentBadge.java` - Comment annotation

### Runtime Components

#### RunTimeProject

**Central Runtime Container**
Path: `core/src/main/java/de/dfki/vsm/runtime/project/RunTimeProject.java`

```java
public class RunTimeProject {
    protected SceneFlow mSceneFlow;
    protected SceneScript mSceneScript;
    protected ProjectConfig mProjectConfig;
    protected Interpreter mInterpreter;
    protected ActivityExecutor mActivityExecutor;

    // Lifecycle methods
    public void launch();
    public void start();
    public void abort();
    public void unload();
}
```

**Responsibilities:**
- Loads and manages SceneFlow, SceneScript, ProjectConfig
- Coordinates Interpreter and ActivityExecutor
- Manages plugin instances
- Project lifecycle management

#### Interpreter (Execution Engine)

**State Machine Executor**
Path: `core/src/main/java/de/dfki/vsm/runtime/interpreter/Interpreter.java`

```java
public class Interpreter extends Thread {
    private ReentrantLock mLock;
    private Condition mPauseCond;
    private Configuration mConfiguration;
    private Environment mEnvironment;
    private Evaluator mEvaluator;

    @Override
    public void run() {
        // Execute sceneflow graph
    }
}
```

**Sub-components:**
- `Configuration.java` - Runtime state (current node, edge history)
- `Environment.java` - Variable environment (global/local scopes)
- `Evaluator.java` - Expression evaluation engine
- `SystemHistory.java` - Execution history tracking
- `TimeoutManager.java` - Timeout edge handling
- `SymbolTable.java` - Symbol resolution

#### Value System

**Runtime Values**
Path: `core/src/main/java/de/dfki/vsm/runtime/interpreter/value/`

- `AbstractValue.java` - Base value class
- Primitives: `IntValue`, `LongValue`, `FloatValue`, `DoubleValue`, `StringValue`, `BooleanValue`
- Collections: `ListValue.java`
- Composite: `StructValue.java`
- Plugin objects: `ObjectValue.java`

#### Activity System

**Asynchronous Action Execution**
Path: `core/src/main/java/de/dfki/vsm/runtime/activity/`

- `AbstractActivity.java` - Base activity class
- `ActionActivity.java` - Agent actions (gestures, animations)
- `SpeechActivity.java` - Speech synthesis/TTS
- `PauseActivity.java` - Delays
- `ActivityExecutor.java` - Execution orchestrator
- `ActivityScheduler.java` - Task scheduling
- `ActivityWorker.java` - Worker thread pool

**Feedback System:**
- Activities can emit feedback events
- Trigger system for conditional execution

---

## View Layer

### Desktop UI (Swing/JavaFX)

#### Main Entry Point

**SceneMaker3**
Path: `src/main/java/de/dfki/vsm/SceneMaker3.java`

```java
public final class SceneMaker3 {
    public static void main(final String[] args) {
        // Parse args: --allow-lan, --no-browser, --no-swing

        // Start Web UI server
        WebUiServer server = WebUiServer.getInstance();
        server.setAllowExternal(allowLan);
        server.start();

        // Open browser
        if (openBrowser) {
            openBrowser(server.getLocalUrl());
        }

        // Start Swing UI (unless --no-swing)
        if (!noSwing) {
            SwingUtilities.invokeLater(() -> {
                EditorInstance.getInstance().setVisible(true);
            });
        }
    }
}
```

**Launch Modes:**
- `editor` - Desktop editor with optional project
- `runtime` - Headless runtime execution
- `--no-swing` - Web-only mode (no desktop UI)

#### Editor Instance

**Main Window**
Path: `editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java`

```java
public class EditorInstance extends JFrame implements EventListener {
    private JTabbedPane mProjectTabbedPane;
    private EditorMenuBar mMenuBar;
    private HashMap<String, ProjectEditor> mProjectEditorMap;

    // Event handling
    @Override
    public void update(EventObject event) {
        if (event instanceof ProjectChangedEvent) {
            // Update UI
        }
    }
}
```

**Key Components:**
- Tabbed project workspace
- Menu bar (File, Edit, Run, Tools, Help)
- Clipboard system for copy/paste
- Event listener for runtime updates

#### Project Editor

**Per-Project View**
Path: `editor/src/main/java/de/dfki/vsm/editor/project/ProjectEditor.java`

- Graph visualization canvas
- Script editor integration
- Node/edge selection handling
- Variable badge display

### Web UI (Svelte + Javalin)

#### Frontend Architecture

**Root Component**
Path: `editor/web-ui/src/App.svelte`

```javascript
<script>
  let projectId = "";
  let activeTab = "sceneflow"; // sceneflow | script

  // WebSocket connection
  const ws = new WebSocket("ws://localhost:8090/ws");
  ws.onmessage = (msg) => {
    const data = JSON.parse(msg.data);
    handleSnapshot(data);
  };
</script>

<div id="app">
  {#if activeTab === "sceneflow"}
    <SceneFlowView {projectId} />
  {:else if activeTab === "script"}
    <ScriptEditor {projectId} />
  {/if}
</div>
```

**Components:**
- `SceneFlowView.svelte` - Graph editor (SVG-based)
- `SceneFlowMiniMap.svelte` - Navigation minimap
- `ScriptEditor.svelte` - CodeMirror-based script editor
- `icons/*` - 15+ SVG icon components

**Build Configuration:**
- `vite.config.js` - Vite bundler settings
- `svelte.config.js` - Svelte compiler options
- Output: `editor/src/main/resources/web-ui/`

#### Backend Server

**WebUiServer** (Unified, dual-mode)
Path: `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`

The server supports two modes via `ServerMode` enum:
- **`FULL_EDITOR`** (default) — Multi-project editing, all WS commands, used by SceneMaker3/4
- **`RUNTIME_ONLY`** — Single project, runtime control only, editing commands rejected, used by RuntimeMain

```java
public final class WebUiServer implements EventListener {
    public enum ServerMode { RUNTIME_ONLY, FULL_EDITOR }

    private ServerMode mMode = ServerMode.FULL_EDITOR;
    private String mAuthToken;
    private Javalin mApp;
    private Map<String, ProjectRef> projectStore;
    private Set<WsContext> wsSessions;

    // Default start (FULL_EDITOR mode)
    public void start(int port, boolean allowExternal) { ... }

    // Mode-specific start (used by RuntimeMain)
    public void start(int port, String bindHost, String token, ServerMode mode) { ... }

    // Runtime-only project management
    public boolean loadProject(String path) { ... }
    public boolean startRuntime() { ... }

    private void registerRoutes() {
        // Common endpoints (both modes)
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/projects", this::handleProjects);
        // ...

        // Editor-only endpoints (gated on FULL_EDITOR)
        if (mMode == ServerMode.FULL_EDITOR) { ... }

        // Runtime-only REST endpoints (gated on RUNTIME_ONLY)
        if (mMode == ServerMode.RUNTIME_ONLY) { ... }
    }
}
```

**REST API Endpoints** (`/api/v1`):

*Both modes:*

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/info` | GET | Server information (includes `mode` field) |
| `/token` | GET | Auth token |
| `/projects` | GET | List active projects |
| `/projects/{pid}/sceneflow` | GET | Get sceneflow graph |
| `/projects/{pid}/script` | GET | Get scene script |
| `/projects/{pid}/runtime` | GET | Get runtime state |
| `/projects/{pid}/config` | GET | Get project config |
| `/preferences` | GET | Get user preferences |
| `/devices` | GET | Get available devices |

*FULL_EDITOR mode only:*

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/projects/open` | POST | Open project file |
| `/projects` | POST | Create new project |
| `/projects/{pid}/save` | POST | Save project |
| `/projects/{pid}/close` | POST | Close project |

*RUNTIME_ONLY mode only:*

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/runtime/load` | POST | Load project by path |
| `/runtime/start` | POST | Start runtime |
| `/runtime/pause` | POST | Pause runtime |
| `/runtime/resume` | POST | Resume runtime |
| `/runtime/stop` | POST | Stop runtime |
| `/runtime/unload` | POST | Unload project |
| `/runtime/status` | GET | Get runtime status |
| `/runtime/variables` | GET | Get runtime variables |

**WebSocket Methods** (both modes):

| Method | Purpose | Mode |
|--------|---------|------|
| `SceneFlow.Get` | Fetch current sceneflow snapshot | Both |
| `Runtime.Start` | Start execution | Both |
| `Runtime.Pause` | Pause execution | Both |
| `Runtime.Stop` | Stop execution | Both |
| `Runtime.Variable.Set` | Set runtime variable | Both |
| `SceneFlow.Node.Add` | Create new node | FULL_EDITOR only |
| `SceneFlow.Node.Update` | Update node properties | FULL_EDITOR only |
| `SceneFlow.Node.Delete` | Delete node | FULL_EDITOR only |
| `SceneFlow.Edge.Add` | Create new edge | FULL_EDITOR only |
| `SceneFlow.Edge.Update` | Update edge properties | FULL_EDITOR only |
| `SceneFlow.Edge.Delete` | Delete edge | FULL_EDITOR only |
| `SceneFlow.Comment.*` | Comment management | FULL_EDITOR only |
| `Script.Update` | Update scene script | FULL_EDITOR only |
| `Project.Save` | Save project | FULL_EDITOR only |

Editing commands sent in RUNTIME_ONLY mode return `{"error": "EDITING_NOT_SUPPORTED", "message": "..."}`.

---

## Controller Layer

### Event System

#### EventDispatcher

**Central Event Bus**
Path: `core/src/main/java/de/dfki/vsm/event/EventDispatcher.java`

```java
public class EventDispatcher {
    private static EventDispatcher sInstance;
    private final CopyOnWriteArrayList<EventListener> mListeners;
    private final Timer mTimer;

    public synchronized void register(EventListener listener) {
        mListeners.add(listener);
    }

    public synchronized void unregister(EventListener listener) {
        mListeners.remove(listener);
    }

    public void convey(EventObject event) {
        for (EventListener listener : mListeners) {
            listener.update(event);
        }
    }
}
```

**Thread Safety:**
- Singleton instance
- `CopyOnWriteArrayList` for thread-safe iteration
- Synchronization on register/unregister

#### Event Types

Path: `core/src/main/java/de/dfki/vsm/event/event/`

**Selection Events:**
- `NodeSelectedEvent` - Node clicked/selected
- `EdgeSelectedEvent` - Edge clicked/selected
- `SceneSelectedEvent` - Scene activated

**Execution Events:**
- `NodeStartedEvent` - Node execution began
- `NodeExecutedEvent` - Node execution completed
- `NodeTerminatedEvent` - Node execution aborted
- `EdgeExecutedEvent` - Edge traversed

**Runtime Events:**
- `VariableChangedEvent` - Variable value updated
- `SceneStoppedEvent` - Scene playback stopped
- `TimeoutEdgeStartedEvent` - Timeout edge activated

**Project Events:**
- `ProjectChangedEvent` - Project modified
- `FunctionCreatedEvent/ModifiedEvent/RemovedEvent` - Function lifecycle
- `ForceShutdownEvent` - Emergency shutdown

### UI Protocol Bridge

#### UiEventBus

**UI-Specific Event Aggregator**
Path: `core/src/main/java/de/dfki/vsm/ui/protocol/UiEventBus.java`

```java
public class UiEventBus {
    private boolean mActive = false;
    private final List<UiEventListener> mListeners;

    public void activate() {
        mActive = true;
    }

    public void emitLazy(UiEventChannel channel, String method, Supplier<Map> payloadSupplier) {
        if (!mActive) return;

        UiEvent event = new UiEvent(channel, method, payloadSupplier.get());
        for (UiEventListener listener : mListeners) {
            listener.onUiEvent(event);
        }
    }
}
```

**Channels:**
- `VARS` - Variable updates
- `RUNTIME` - Execution state
- `SCENEFLOW` - Graph mutations
- `SCRIPT` - Scene script changes

#### UiEventBridge

**Domain → UI Translation**
Path: `core/src/main/java/de/dfki/vsm/ui/protocol/UiEventBridge.java`

```java
public class UiEventBridge implements EventListener {
    private final UiEventBus mUiEventBus;

    @Override
    public void update(EventObject event) {
        if (event instanceof NodeStartedEvent) {
            NodeStartedEvent e = (NodeStartedEvent) event;
            mUiEventBus.emitLazy(RUNTIME, "node.active", () -> {
                Map<String, Object> payload = new HashMap<>();
                payload.put("nodeId", e.getNode().getId());
                return payload;
            });
        }
        // ... handle other event types
    }
}
```

**Translation Examples:**
- `NodeStartedEvent` → `RUNTIME:node.active`
- `VariableChangedEvent` → `VARS:var.<name>`
- `EdgeExecutedEvent` → `RUNTIME:edge.active`
- `TimeoutEdgeStartedEvent` → `RUNTIME:timeout.progress`

#### UiProtocol

**Protocol Installation**
Path: `core/src/main/java/de/dfki/vsm/ui/protocol/UiProtocol.java`

```java
public class UiProtocol {
    public static final int VERSION = 1;
    private static UiEventBridge sBridge;

    public static void install(UiEventBus bus) {
        if (sBridge != null) {
            EventDispatcher.getInstance().unregister(sBridge);
        }
        sBridge = new UiEventBridge(bus);
        EventDispatcher.getInstance().register(sBridge);
    }
}
```

---

## Communication Patterns

### WebSocket Protocol

#### Request Format

```json
{
  "id": "req-uuid-1234",
  "method": "SceneFlow.Node.Add",
  "params": {
    "projectId": "project-abc123",
    "name": "NewNode",
    "type": "BasicNode",
    "position": { "x": 100, "y": 200 }
  }
}
```

#### Response Format

```json
{
  "id": "req-uuid-1234",
  "status": "ok",
  "result": {
    "snapshot": {
      "nodes": [
        {"id": "node1", "name": "Start", "type": "BasicNode", ...},
        {"id": "node2", "name": "NewNode", "type": "BasicNode", ...}
      ],
      "edges": [...],
      "comments": [...]
    }
  }
}
```

#### Broadcast Events

```json
{
  "event": "sceneflow.snapshot",
  "projectId": "project-abc123",
  "timestamp": 1704985200000,
  "snapshot": {
    "nodes": [...],
    "edges": [...]
  }
}
```

### Event Flow Example

**User Action: Start Runtime Execution**

1. **Web UI**: User clicks "Play" button
2. **WebSocket**: Send `{ "method": "Runtime.Start", "params": { "projectId": "..." } }`
3. **WebUiServer**: `handleWsMessage()` → calls `rtp.start()`
4. **RunTimeProject**: Starts Interpreter thread
5. **Interpreter**: Begins executing sceneflow, fires `NodeStartedEvent`
6. **EventDispatcher**: Multicasts event to all listeners
7. **UiEventBridge**: Translates to `UiEvent(RUNTIME, "node.active", {nodeId: "..."})`
8. **UiEventBus**: Emits to registered listeners
9. **WebUiServer**: Broadcasts to all WebSocket clients
10. **Web UI**: Receives event, updates SVG visualization

### REST vs WebSocket Usage

**REST (Stateless):**
- Fetch project lists
- Load project configurations
- Get static content (scripts, preferences)
- One-time operations

**WebSocket (Stateful):**
- Real-time sceneflow editing
- Runtime execution updates
- Variable monitoring
- Collaborative editing

---

## Key Workflows

### Opening a Project

```
Desktop UI Path:
1. User: File → Open → select project.xml
2. EditorInstance.openProject(file)
3. Create new EditorProject(file)
4. EditorProject extends RunTimeProject
5. RunTimeProject.launch()
   - Parse projectconfig.xml
   - Load sceneflow.xml
   - Load scenescript.xml
   - Initialize Interpreter
6. Add tab to JTabbedPane
7. Register event listeners

Web UI Path:
1. User: Click "Open Project" → browse file
2. Send: POST /api/v1/projects/open { "path": "..." }
3. WebUiServer.handleProjectOpen()
4. Create RunTimeProject, call launch()
5. Generate projectId UUID
6. Store in ProjectRef map
7. Return: { "projectId": "...", "config": {...} }
8. Frontend: Fetch sceneflow/script/runtime data
9. Render UI with loaded data
```

### Runtime Execution Flow

```
1. User initiates start (UI button or API call)
2. RunTimeProject.start()
3. Interpreter thread started
4. Loop:
   a. Get current active nodes (Configuration)
   b. For each active node:
      - Fire NodeStartedEvent
      - Execute node's script/commands
      - Evaluate edge conditions
      - Fire EdgeExecutedEvent
      - Transition to next nodes
      - Fire NodeExecutedEvent
   c. Check for interrupts/timeouts
   d. Sleep/yield if paused
5. On stop: Fire NodeTerminatedEvent, cleanup
```

### Variable Update Flow

```
1. Interpreter evaluates assignment: x = 5
2. Environment.setVariable("x", IntValue(5))
3. Fire VariableChangedEvent(name="x", value=5)
4. EventDispatcher multicasts to listeners
5. UiEventBridge translates to UiEvent(VARS, "var.x", {value: 5})
6. UiEventBus emits to registered listeners
7. Desktop UI: Update variable badge display
8. Web UI: Broadcast to WebSocket clients
9. Svelte component: Update reactive state, re-render
```

---

## Plugin System

### Plugin Interface

Path: `core/src/main/java/de/dfki/vsm/runtime/plugin/RunTimePlugin.java`

```java
public abstract class RunTimePlugin {
    protected final LOGDefaultLogger mLogger;
    protected String mPluginPath;
    protected PluginConfig mPluginConfig;
    protected RunTimeProject mProject;

    // Lifecycle
    public abstract void launch();
    public abstract void unload();

    // Execution
    public abstract void execute(AbstractActivity activity);

    // Optional overrides
    public void parse(String cmd) { }
    public Object eval(String expr) { return null; }
}
```

### Plugin Loading

**Dynamic Reflection**
Path: `core/src/main/java/de/dfki/vsm/runtime/project/RunTimeProject.java`

```java
private void loadPlugin(String pluginName, PluginConfig config) {
    try {
        // Construct class name: de.dfki.vsm.xtension.<name>.<Name>Executor
        String className = "de.dfki.vsm.xtension." + pluginName + "."
                         + pluginName.substring(0, 1).toUpperCase()
                         + pluginName.substring(1) + "Executor";

        Class<?> clazz = Class.forName(className);
        Constructor<?> ctor = clazz.getConstructor(PluginConfig.class, RunTimeProject.class);
        RunTimePlugin plugin = (RunTimePlugin) ctor.newInstance(config, this);

        plugin.launch();
        mPluginMap.put(pluginName, plugin);
    } catch (Exception e) {
        mLogger.failure("Failed to load plugin: " + pluginName);
    }
}
```

### Available Plugins (24 modules)

Path: `plugins/*/`

**Character/Avatar Plugins:**
- `charamel` - Charamel character system
- `charamel-ws` - WebSocket variant
- `reeti` - Reeti robot control
- `unity` - Unity3D integration

**Communication Plugins:**
- `ssi` - Social Signal Interpretation
- `ssj` - Social Signal Processing
- `sockets` - Generic socket communication

**Interface Plugins:**
- `htmlgui-ws` - HTML-based responsive GUI
- `studymaster-web` - Study management web interface
- `AndroidGui` - Android UI integration

**Utility Plugins:**
- `console` - Console I/O
- `email` - Email sending
- `timer` - Timer/scheduling
- `wizard` - Wizard of Oz interface
- `fortunecookie` - Random message generation

**Specialized Plugins:**
- `alma` - ALMA emotion model
- `user-cue-service` - User modeling
- `decad` - Decision-making
- `odp` - ODP protocol
- `qrwebcam` - QR code webcam scanning
- `yallah` - Custom action system
- `tricatworld` - Virtual world integration
- `DriveSimulator` - Driving simulation

---

## Thread Safety

### Synchronization Mechanisms

#### ReentrantLock (Interpreter)

```java
public class Interpreter extends Thread {
    private final ReentrantLock mLock = new ReentrantLock();
    private final Condition mPauseCond = mLock.newCondition();

    public void run() {
        while (!mTerminated) {
            mLock.lock();
            try {
                while (mPaused) {
                    mPauseCond.await();
                }
                // Execute node
            } finally {
                mLock.unlock();
            }
        }
    }

    public void pause() {
        mLock.lock();
        try {
            mPaused = true;
        } finally {
            mLock.unlock();
        }
    }

    public void resume() {
        mLock.lock();
        try {
            mPaused = false;
            mPauseCond.signalAll();
        } finally {
            mLock.unlock();
        }
    }
}
```

#### CopyOnWriteArrayList (EventDispatcher)

```java
public class EventDispatcher {
    private final CopyOnWriteArrayList<EventListener> mListeners
        = new CopyOnWriteArrayList<>();

    // Thread-safe iteration without explicit locks
    public void convey(EventObject event) {
        for (EventListener listener : mListeners) {
            listener.update(event);
        }
    }
}
```

#### Synchronized Methods (Preferences)

```java
public class Preferences {
    protected static final Properties sPROPERTIES = new Properties();

    public static synchronized String getProperty(String key) {
        return sPROPERTIES.getProperty(key);
    }

    public static synchronized Object setProperty(String key, String value) {
        return sPROPERTIES.setProperty(key, value);
    }

    public static synchronized void save() {
        // Save to file
    }
}
```

#### WebSocket Session Pool

```java
public class WebUiServer {
    private final Set<WsContext> wsSessions = ConcurrentHashMap.newKeySet();

    // Thread-safe add/remove
    mApp.ws("/ws", ws -> {
        ws.onConnect(ctx -> wsSessions.add(ctx));
        ws.onClose(ctx -> wsSessions.remove(ctx));
    });
}
```

### Thread Architecture

```
Main Thread
├─ Swing EDT (Desktop UI)
├─ Javalin Thread Pool (Web Server)
│  ├─ HTTP Request Handlers
│  └─ WebSocket Handlers
├─ Interpreter Thread (per project)
├─ ActivityScheduler Threads (per project)
│  └─ ActivityWorker Pool
└─ EventDispatcher Timer Thread
```

---

## Future Development Guidelines

### Adding New Features

#### 1. New SceneFlow Node Type

```java
// 1. Define model class
public class CustomNode extends BasicNode {
    private String mCustomProperty;

    @Override
    public String writeXMLString() {
        // XML serialization
    }
}

// 2. Update parser to recognize new type

// 3. Add Interpreter support for execution

// 4. Update UI rendering (Desktop & Web)
```

#### 2. New REST API Endpoint

```java
// In WebUiServer.java
private void registerRoutes() {
    // Add new endpoint
    mApp.get(API_PREFIX + "/custom/endpoint", this::handleCustomEndpoint);
}

private void handleCustomEndpoint(Context ctx) {
    String param = ctx.queryParam("param");
    JSONObject response = new JSONObject();
    response.put("result", processCustomLogic(param));
    writeJson(ctx, response);
}
```

#### 3. New WebSocket Method

```java
// In WebUiServer.java
private void handleWsMessage(String rawMessage, Consumer<String> reply, Consumer<String> broadcast) {
    JSONObject msg = new JSONObject(rawMessage);
    String method = msg.getString("method");

    if ("Custom.Action".equals(method)) {
        handleCustomAction(msg, reply, broadcast);
    }
}

private void handleCustomAction(JSONObject msg, Consumer<String> reply, Consumer<String> broadcast) {
    // Process custom action
    JSONObject response = new JSONObject();
    response.put("id", msg.getString("id"));
    response.put("status", "ok");
    reply.accept(response.toString());
}
```

#### 4. New Event Type

```java
// 1. Define event class
public class CustomEvent extends EventObject {
    private final String mData;

    public CustomEvent(Object source, String data) {
        super(source);
        mData = data;
    }
}

// 2. Fire event from appropriate location
EventDispatcher.getInstance().convey(new CustomEvent(this, data));

// 3. Add listener support in UiEventBridge if needed

// 4. Handle in UI components
```

#### 5. New Plugin

```java
// 1. Create plugin module: plugins/myplugin/

// 2. Implement executor
public class MypluginExecutor extends RunTimePlugin {
    @Override
    public void launch() {
        // Initialize
    }

    @Override
    public void execute(AbstractActivity activity) {
        // Execute custom activity
    }

    @Override
    public void unload() {
        // Cleanup
    }
}

// 3. Add to settings.gradle
include 'plugins:myplugin'

// 4. Add build.gradle

// 5. Register in ProjectConfig
```

### Code Style Guidelines

1. **Naming Conventions:**
   - Classes: `PascalCase`
   - Methods: `camelCase`
   - Constants: `UPPER_SNAKE_CASE`
   - Member variables: `mCamelCase` (prefix m)
   - Static fields: `sCamelCase` (prefix s)

2. **Package Organization:**
   - Model classes: `de.dfki.vsm.model.*`
   - Runtime: `de.dfki.vsm.runtime.*`
   - UI: `de.dfki.vsm.editor.*` or `de.dfki.vsm.web.*`
   - Events: `de.dfki.vsm.event.*`

3. **Thread Safety:**
   - Always document thread safety in comments
   - Use appropriate synchronization (locks, atomic types)
   - Avoid synchronized blocks in hot paths

4. **Event Handling:**
   - Use EventDispatcher for cross-cutting concerns
   - Keep event handlers lightweight (no blocking I/O)
   - Document event firing locations

5. **Logging:**
   - Use `LOGDefaultLogger` consistently
   - Log levels: `message()`, `warning()`, `failure()`
   - Include context in log messages

### Testing Guidelines

1. **Unit Tests:**
   - Test model classes in isolation
   - Mock EventDispatcher for testing controllers
   - Use JUnit 5 (`testImplementation 'org.junit.jupiter:junit-jupiter-api'`)

2. **Integration Tests:**
   - Test RunTimeProject lifecycle
   - Verify event propagation
   - Test WebSocket protocol

3. **UI Tests:**
   - Desktop: Use TestFX for JavaFX/Swing
   - Web: Use Playwright/Cypress for E2E tests

### Performance Considerations

1. **Event Overhead:**
   - Events are fired synchronously on current thread
   - Keep event handlers fast
   - Use lazy evaluation (`Supplier`) in UiEventBus

2. **WebSocket Broadcasting:**
   - Snapshots can be large (many nodes/edges)
   - Consider delta updates for large projects
   - Throttle high-frequency events

3. **XML Parsing:**
   - Large projects can be slow to load
   - Consider binary format for faster I/O
   - Lazy loading for hierarchical sceneflows

4. **Interpreter Performance:**
   - Avoid allocations in hot loops
   - Cache evaluated expressions
   - Profile with VisualVM/JProfiler

### Debugging Tips

1. **Enable verbose logging:**
   ```bash
   export LOG_LEVEL=FINE
   java -jar SceneMaker.jar
   ```

2. **WebSocket debugging:**
   - Use browser DevTools → Network → WS tab
   - Log messages in `handleWsMessage()`

3. **Event flow tracing:**
   - Add breakpoint in `EventDispatcher.convey()`
   - Trace call stack to find event source

4. **Thread deadlock detection:**
   - Use JConsole or VisualVM
   - Thread dumps: `jstack <pid>`

---

## Appendix: Key File Paths Reference

### Entry Points
- `src/main/java/de/dfki/vsm/SceneMaker3.java` - Main launcher (Swing + Web, FULL_EDITOR)
- `src/main/java/de/dfki/vsm/SceneMaker4.java` - Web-only launcher (FULL_EDITOR)
- `runtime-server/src/main/java/de/dfki/vsm/runtime/RuntimeMain.java` - Standalone runtime (RUNTIME_ONLY)
- `core/src/main/java/de/dfki/vsm/Core.java` - Headless runtime entry

### Core Model
- `core/src/main/java/de/dfki/vsm/model/project/ProjectConfig.java`
- `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/SceneFlow.java`
- `core/src/main/java/de/dfki/vsm/model/scenescript/SceneScript.java`

### Runtime
- `core/src/main/java/de/dfki/vsm/runtime/project/RunTimeProject.java`
- `core/src/main/java/de/dfki/vsm/runtime/interpreter/Interpreter.java`
- `core/src/main/java/de/dfki/vsm/runtime/activity/executor/ActivityExecutor.java`

### Events
- `core/src/main/java/de/dfki/vsm/event/EventDispatcher.java`
- `core/src/main/java/de/dfki/vsm/ui/protocol/UiEventBus.java`
- `core/src/main/java/de/dfki/vsm/ui/protocol/UiEventBridge.java`

### Desktop UI
- `editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java`
- `editor/src/main/java/de/dfki/vsm/editor/project/EditorProject.java`
- `editor/src/main/java/de/dfki/vsm/editor/EditorMenuBar.java`

### Web UI
- `core/src/main/java/de/dfki/vsm/web/WebUiServer.java` - Unified server (FULL_EDITOR + RUNTIME_ONLY)
- `core/src/main/java/de/dfki/vsm/web/SceneFlowSnapshotBuilder.java` - Shared snapshot builder
- `editor/web-ui/src/App.svelte`
- `editor/web-ui/src/SceneFlowView.svelte`
- `editor/web-ui/src/ScriptEditor.svelte`

### Configuration
- `core/src/main/java/de/dfki/vsm/Preferences.java`
- `editor/src/main/java/de/dfki/vsm/PreferencesDesktop.java`

---

**Document Version:** 1.1
**Last Updated:** 2026-02-03
**Author:** Architecture Analysis
**Repository:** `/Users/gebhard/Code/Repo/VisualSceneMaker`
**Branch:** `web2026`
