# Runtime Server Documentation

The `runtime-server` module provides a standalone, headless runtime server for VisualSceneMaker projects. It enables remote execution and monitoring of SceneFlow state machines without requiring the full editor.

## Overview

```
┌─────────────────┐                              ┌──────────────────┐
│   Web UI        │         HTTP/WebSocket       │  Runtime Server  │
│   (Browser)     │◄────────────────────────────►│  (Java 17)       │
│                 │                              │                  │
│   - Monitor     │  REST: /api/v1/*             │  - Load projects │
│   - Control     │  WS:   /ws                   │  - Execute flows │
│   - Debug       │                              │  - Broadcast     │
└─────────────────┘                              └──────────────────┘
```

**Key Features:**
- Headless operation (no GUI required)
- Java 17 compatible (Android-ready)
- Uses `WebUiServer` in `RUNTIME_ONLY` mode (unified server, editing commands rejected)
- REST API for runtime control and project management
- WebSocket for real-time events and runtime commands
- Auth token support (auto-generated or user-specified)
- Configurable port and network binding
- Auto-load projects on startup

## Building

```bash
# Build the runtime-server JAR
./gradlew :runtime-server:jar

# Output location
ls runtime-server/build/libs/runtime-server-*.jar
```

The resulting JAR (~6.5MB) includes:
- Core runtime engine
- WebUiServer (Javalin-based)
- Console and Timer plugins
- SLF4J logging

## Command-Line Usage

```bash
java -jar runtime-server.jar [options]
```

### Options

| Option | Description | Default |
|--------|-------------|---------|
| `--port=PORT` | Server port | 8091 |
| `--allow-lan` | Bind to 0.0.0.0 (allow external connections) | localhost only |
| `--project=PATH` | Auto-load project on startup | none |
| `--autostart` | Start runtime after loading project | false |
| `--token=TOKEN` | Set authentication token | auto-generated |
| `--help`, `-h` | Show help message | - |

### Examples

```bash
# Start on default port (8091), localhost only
java -jar runtime-server.jar

# Start on custom port
java -jar runtime-server.jar --port=9000

# Allow LAN connections (for remote Web UI)
java -jar runtime-server.jar --allow-lan --port=8091

# Load and auto-start a project
java -jar runtime-server.jar --project=/path/to/project --autostart

# Full example: LAN-accessible with auto-start
java -jar runtime-server.jar --allow-lan --port=8091 --project=/path/to/project --autostart
```

## Project Loading

Projects can be loaded in three ways:

### 1. Command-Line (at startup)

```bash
java -jar runtime-server.jar --project=/path/to/project
```

The project path should point to a directory containing `project.xml`.

### 2. REST API (at runtime)

```bash
curl -X POST http://localhost:8091/api/v1/projects/open \
  -H "Content-Type: application/json" \
  -d '{"path": "/path/to/project"}'
```

Response:
```json
{
  "projectId": "31141636-99aa-4275-8f9d-ef6f4dc466fc",
  "path": "/path/to/project",
  "name": "MyProject"
}
```

### 3. Programmatically (Java)

```java
import de.dfki.vsm.web.WebUiServer;

// Start server in RUNTIME_ONLY mode
WebUiServer server = WebUiServer.getInstance();
server.start(8091, "127.0.0.1", null, WebUiServer.ServerMode.RUNTIME_ONLY);

// Load project (parses, launches, stores in project map)
server.loadProject("/path/to/project");

// Optionally start execution
server.startRuntime();
```

## REST API

Base URL: `http://localhost:8091/api/v1`

### Server Information

```
GET /api/v1/info
```

Response:
```json
{
  "name": "SceneMaker Web",
  "port": 8091,
  "mode": "runtime_only",
  "tokenRequired": true
}
```

The `mode` field indicates the server's operating mode:
- `"runtime_only"` — Runtime control only, editing commands rejected
- `"full_editor"` — Full editing and runtime support

### Project Management

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/projects` | GET | List loaded projects |
| `/projects/{pid}/sceneflow` | GET | Get sceneflow snapshot |
| `/projects/{pid}/runtime` | GET | Get runtime state and variables |
| `/projects/{pid}/config` | GET | Get editor config |
| `/projects/{pid}/project-config` | GET | Get project config |

### Runtime Control (RUNTIME_ONLY mode)

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/runtime/load` | POST | Load project by path (`{"projectPath": "..."}`) |
| `/runtime/start` | POST | Start runtime execution |
| `/runtime/pause` | POST | Pause execution |
| `/runtime/resume` | POST | Resume execution |
| `/runtime/stop` | POST | Stop execution |
| `/runtime/unload` | POST | Unload project |
| `/runtime/status` | GET | Get runtime status |
| `/runtime/variables` | GET | Get runtime variables |
| `/runtime/sceneflow` | GET | Get sceneflow (legacy) |

#### List Projects

```
GET /api/v1/projects
```

Response:
```json
{
  "projects": [
    {
      "projectId": "31141636-99aa-4275-8f9d-ef6f4dc466fc",
      "name": "TestSuite",
      "path": "/path/to/project",
      "dirty": false,
      "pending": false
    }
  ]
}
```

### Runtime State

```
GET /api/v1/projects/{pid}/runtime
```

Response:
```json
{
  "state": "running|paused|stopped",
  "vars": [
    { "name": "counter", "type": "Int", "value": "5" }
  ],
  "project": "/path/to/project"
}
```

### SceneFlow Data

```
GET /api/v1/projects/{pid}/sceneflow
```

Response:
```json
{
  "nodes": [...],
  "edges": [...],
  "comments": [...],
  "path": [],
  "superNodeId": ""
}
```

## WebSocket API

Connect to: `ws://localhost:8091/ws`

### Message Format

**Request:**
```json
{
  "id": "unique-request-id",
  "method": "Runtime.Start",
  "params": {
    "projectId": "31141636-99aa-4275-8f9d-ef6f4dc466fc"
  }
}
```

**Response:**
```json
{
  "id": "unique-request-id",
  "status": "ok",
  "result": { ... }
}
```

**Broadcast Event:**
```json
{
  "event": "runtime.state",
  "projectId": "31141636-99aa-4275-8f9d-ef6f4dc466fc",
  "state": "running"
}
```

### Runtime Control Commands

| Method | Description |
|--------|-------------|
| `Runtime.Start` | Start project execution |
| `Runtime.Pause` | Pause execution |
| `Runtime.Stop` | Stop execution |

Example:
```json
{
  "id": "1",
  "method": "Runtime.Start",
  "params": { "projectId": "..." }
}
```

### SceneFlow Commands

| Method | Description |
|--------|-------------|
| `SceneFlow.Get` | Get current sceneflow snapshot |
| `SceneFlow.Snapshot` | Alias for SceneFlow.Get |

### Broadcast Events

The server broadcasts these events to all connected WebSocket clients:

| Event | Trigger |
|-------|---------|
| `runtime.state` | Runtime state changes (start/pause/stop) |
| `sceneflow.snapshot` | SceneFlow modifications |

## Connecting from Web UI

The Web UI editor can connect to a remote runtime server:

### 1. Start Runtime Server

```bash
# On the runtime machine (e.g., Android device, Raspberry Pi)
java -jar runtime-server.jar --allow-lan --port=8091 --project=/path/to/project
```

### 2. Configure Web UI

In the Web UI, connect to the runtime server URL:
- URL: `http://<runtime-ip>:8091`
- The Web UI will use REST for snapshots and WebSocket for real-time updates

### 3. Connection Flow

```
1. Web UI calls GET /api/v1/info to verify server
2. Web UI calls GET /api/v1/projects to list available projects
3. Web UI opens WebSocket to ws://<host>:8091/ws
4. Web UI receives real-time events (runtime.state, sceneflow.snapshot)
5. Web UI sends commands via WebSocket (Runtime.Start, etc.)
```

## Deployment Scenarios

### Desktop Development

```bash
# Terminal 1: Start runtime server
java -jar runtime-server.jar --port=8091 --project=./my-project

# Terminal 2: Start editor with Web UI
java -jar VisualSceneMaker.jar --port=8090

# Browser: Open http://localhost:8090, connect to runtime at :8091
```

### Headless Server

```bash
# Run as background service
nohup java -jar runtime-server.jar \
  --allow-lan \
  --port=8091 \
  --project=/opt/vsm/project \
  --autostart \
  > /var/log/vsm-runtime.log 2>&1 &
```

### Android Deployment

The runtime-server JAR is Java 17 compatible and can run on Android:

1. Include `core` module as dependency in Android project
2. Create Android Service wrapping `WebUiServer` in `RUNTIME_ONLY` mode
3. Start server on device, note IP address and auth token
4. Connect from desktop Web UI

```kotlin
// Android Service example
class RuntimeService : Service() {
    override fun onStartCommand(intent: Intent, flags: Int, startId: Int): Int {
        val server = WebUiServer.getInstance()
        server.start(8091, "0.0.0.0", null, WebUiServer.ServerMode.RUNTIME_ONLY)

        val projectPath = intent.getStringExtra("projectPath")
        if (projectPath != null) {
            server.loadProject(projectPath)
            server.startRuntime()
        }

        // Show notification with connection info
        showNotification("Runtime on :8091", server.authToken)
        return START_STICKY
    }
}
```

### Docker

```dockerfile
FROM eclipse-temurin:17-jre

COPY runtime-server/build/libs/runtime-server-*.jar /app/runtime-server.jar
COPY my-project /app/project

EXPOSE 8091

CMD ["java", "-jar", "/app/runtime-server.jar", \
     "--allow-lan", "--port=8091", \
     "--project=/app/project", "--autostart"]
```

```bash
docker build -t vsm-runtime .
docker run -p 8091:8091 vsm-runtime
```

## Security Considerations

### Network Binding

By default, the server binds to `127.0.0.1` (localhost only). Use `--allow-lan` to bind to `0.0.0.0` for external access.

```bash
# Secure (localhost only)
java -jar runtime-server.jar --port=8091

# External access (use with caution)
java -jar runtime-server.jar --allow-lan --port=8091
```

### Authentication

Token-based authentication is available:
- Auth token is auto-generated on startup (or set via `--token=`)
- Token is printed to the console on startup
- `GET /api/v1/token` returns the session token
- Include token in WebSocket URL: `ws://host:port/ws?token=...`
- Include token in HTTP header: `Authorization: Bearer <token>`

### Firewall

When using `--allow-lan`, ensure proper firewall configuration:

```bash
# Linux: Allow port 8091
sudo ufw allow 8091/tcp

# macOS: Use System Preferences > Security & Privacy > Firewall
```

## Troubleshooting

### SLF4J Warning

```
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder"
```

This is informational only. The server uses NOP logger by default. To enable logging, add an SLF4J implementation to the classpath.

### Plugin Not Found

```
java.lang.ClassNotFoundException: de.dfki.vsm.xtension.ssi.SSICmdExecutor
```

The runtime-server includes only minimal plugins (console, timer). Projects using other plugins will show warnings but continue running.

### Connection Refused

1. Check the server is running: `curl http://localhost:8091/api/v1/info`
2. Check firewall settings
3. Verify `--allow-lan` is used for remote connections
4. Verify the correct port is specified

## API Reference

See also:
- `doc/scenemaker-web-ui-api.md` - Full REST/WebSocket API specification
- `doc/scenemaker-ui-protocol.md` - Event protocol and message formats
- `doc/architecture-details.md` - System architecture overview

## Architecture

The runtime server uses the same `WebUiServer` class as the full editor, but in `RUNTIME_ONLY` mode:

```
WebUiServer (core module)
├── ServerMode.FULL_EDITOR  ← used by SceneMaker3/4 (multi-project, editing)
└── ServerMode.RUNTIME_ONLY ← used by RuntimeMain (single project, no editing)
```

In `RUNTIME_ONLY` mode:
- Editor-only REST endpoints are not registered (open, save, close, etc.)
- Runtime control REST endpoints are registered (`/runtime/load`, `/start`, etc.)
- WebSocket editing commands return `EDITING_NOT_SUPPORTED` error
- Read-only and runtime WS commands work normally

## Module Dependencies

```
runtime-server
├── core (runtime engine, WebUiServer in RUNTIME_ONLY mode)
├── plugins:console
├── plugins:timer
└── org.slf4j:slf4j-simple
```

To add more plugins, update `runtime-server/build.gradle`:

```gradle
dependencies {
    implementation project(':core')
    implementation project(':plugins:console')
    implementation project(':plugins:timer')
    // Add more plugins as needed:
    // implementation project(':plugins:charamel-ws')
    // implementation project(':plugins:unity')
}
```
