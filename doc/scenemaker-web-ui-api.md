# SceneMaker Web UI API (v1)

This document defines the HTTP + WebSocket API contract for the Swing-to-Web
refactor of the `de.dfki.vsm.editor` UI. Java remains the source of truth;
the web UI is a client. The API is LAN-capable, token-protected, and supports
multiple simultaneous editors with optimistic concurrency control.

## Principles
- Server: Javalin in core, serving static UI + JSON/WS APIs.
- LAN: bind to `0.0.0.0` (configurable).
- Startup: default binds `127.0.0.1`; use `--allow-lan` to expose LAN, `--no-browser` to disable auto-open.
- Auth: bearer token required for HTTP and WebSocket.
- Concurrency: optimistic. All mutations include revision/version; conflicts
  return a fresh snapshot and an error response.
- Multi-editor: all changes broadcast via WebSocket; clients ignore their own
  updates by `sourceClientId`.

## Auth
- HTTP: `Authorization: Bearer <token>`
- WebSocket: `ws://host:port/ws?token=<token>`
- Token is generated on startup (or configured) and logged on server side.
- Exception: `GET /api/v1/token` is unauthenticated but restricted to localhost.

## HTTP API

### Session
- `GET /api/v1/info`
  - Returns version/build, server capabilities, token requirement.
- `GET /api/v1/token`
  - Localhost only. Returns `{ "token": "...", "tokenRequired": true|false }` for bootstrap.

### Projects
- `GET /api/v1/projects`
  - List open projects.
- `GET /api/v1/projects/recent`
- `GET /api/v1/projects/samples`
- `GET /api/v1/projects/tutorials`
- `POST /api/v1/projects/open`
  - Body: `{ "path": "/abs/path/to/project" }`
- `POST /api/v1/projects`
  - Body: `{ "name": "MyProject", "baseDir": "/abs/path" }`
- `POST /api/v1/projects/{id}/save`
- `POST /api/v1/projects/{id}/save-as`
  - Body: `{ "path": "/abs/path/to/project" }`
- `POST /api/v1/projects/{id}/close`
- `GET /api/v1/projects/{id}`
  - Project metadata, runtime state, dirty state, active super node.

### SceneFlow
- `GET /api/v1/projects/{id}/sceneflow?superNodeId=N1`
  - Full graph snapshot + revision.
- `POST /api/v1/projects/{id}/sceneflow/navigate`
  - Body: `{ "superNodeId": "N1" }`

### Script
- `GET /api/v1/projects/{id}/script`
  - Returns full scene script text + version + parse errors.
- `POST /api/v1/projects/{id}/script/diagnostics`
  - Body: `{ "text": "..." }`
  - Returns parse diagnostics without applying changes.
- `GET /api/v1/projects/{id}/script/scenes`
  - Scene groups by language.
- `GET /api/v1/projects/{id}/script/elements`
  - Gesticon/Acticon/Visicon lists for DnD.
  - Response shape:
    ```json
    {
      "acticon": [
        { "name": "Smile", "script": "Smile()" }
      ],
      "gesticon": [
        {
          "agent": "Agent",
          "icon": "agent.png",
          "gestures": [
            {
              "character": "A",
              "animName": "Wave",
              "animPath": "path/to/anim",
              "category": "Gesture",
              "blendable": true,
              "duration": 1200,
              "script": "Gesture(Agent, Wave)"
            }
          ]
        }
      ],
      "visicon": [
        {
          "agent": "Agent",
          "icon": "agent.png",
          "visemes": [
            { "key": "AA", "value": "aa" }
          ]
        }
      ]
    }
    ```

### Functions
- `GET /api/v1/projects/{id}/functions`

### Types
- `GET /api/v1/projects/{id}/types`
  - Returns primitives + custom data type definitions.

### Preferences
- `GET /api/v1/projects/{id}/config`
  - EditorConfig (per-project).
- `GET /api/v1/preferences`
  - Global PreferencesDesktop values.
- `GET /api/v1/projects/{id}/project-config`
  - Project settings (devices, agents, player).
  - Response shape:
    ```json
    {
      "config": {
        "name": "ProjectName",
        "plugins": [
          {
            "type": "device",
            "name": "DeviceName",
            "className": "de.dfki.vsm.xtension.SomeDevice",
            "load": true,
            "features": [
              { "key": "host", "value": "127.0.0.1" }
            ]
          }
        ],
        "agents": [
          {
            "name": "AgentName",
            "device": "DeviceName",
            "features": [
              { "key": "voice", "value": "default" }
            ]
          }
        ],
        "player": {
          "features": [
            { "key": "fps", "value": "30" }
          ]
        }
      }
    }
    ```

### Devices
- `GET /api/v1/devices`
  - Returns available device/plugin classes (short name + class name).
- `GET /api/v1/projects/{id}/project-config/keys?device=DeviceName&scope=plugin|agent`
  - Returns required/optional key hints exported by the device plugin.
  - Optional: `className=fully.qualified.ClassName` to resolve keys by class instead of device name.
  - Response shape:
    ```json
    {
      "device": "DeviceName",
      "className": "de.dfki.vsm.xtension.SomeDevice",
      "scope": "plugin",
      "supported": true,
      "required": [
        { "name": "host", "description": "Remote host address" }
      ],
      "optional": [
        { "name": "port", "description": "Remote port" }
      ]
    }
    ```

### Runtime
- `GET /api/v1/projects/{id}/runtime`
  - Running/paused state + variables snapshot.

### Filesystem (optional)
- `GET /api/v1/fs/roots`
- `GET /api/v1/fs/list?path=/abs/path`
  - If unused, manual path entry is sufficient.

## WebSocket API

### Envelope
All WS messages use a shared envelope:
```json
{
  "type": "cmd|event|response|error",
  "id": "uuid-optional",
  "name": "SceneFlow.Node.Move",
  "sourceClientId": "client-uuid",
  "payload": {}
}
```

## Remote Core Connection Flow (Desktop UI)
Goal: run the core on an Android device and use the desktop UI as a client-only
editor that connects to the remote core.

### Client-only mode
- Start the desktop UI with `--connect http://<host>:<port>` (no local core).
- Optional `--token <token>` can be provided to skip manual entry.

### Handshake and auth
1) Desktop calls `GET /api/v1/info` to confirm protocol and token requirements.
2) If token is required:
   - Option A (manual): Android UI shows token; user pastes into desktop.
   - Option B (pairing): Android shows a short pairing code; desktop sends:
     `POST /api/v1/session/pair` with `{ "code": "123456" }`
     and receives `{ "token": "..." }`.
3) Desktop opens WebSocket:
   `ws://<host>:<port>/ws?token=...`

### Session lifecycle
- On connect: server sends `system.hello`.
- Client subscribes to channels and requests snapshots.
- On disconnect: client retries with backoff and re-subscribes.

### Guarantees
- No project files are loaded locally in client-only mode.
- All project operations are remote (`project.load/save/...`).

### Commands (client -> server)

#### Project
- `Project.Open` `{ "path": "..." }`
- `Project.New` `{ "name": "...", "baseDir": "..." }`
- `Project.Save` `{ "projectId": "p1" }`
- `Project.SaveAs` `{ "projectId": "p1", "path": "..." }`
- `Project.Close` `{ "projectId": "p1" }`
- `Project.Activate` `{ "projectId": "p1" }`

#### SceneFlow
- `SceneFlow.Navigate` `{ "projectId": "p1", "superNodeId": "N1" }`
- `SceneFlow.Node.Create`
  - `{ "projectId": "p1", "revision": 42, "type": "Basic|Super", "name": "...", "x": 100, "y": 200 }`
- `SceneFlow.Node.Update`
  - `{ "projectId": "p1", "revision": 42, "nodeId": "N1", "fields": { ... } }`
- `SceneFlow.Node.Move`
  - `{ "projectId": "p1", "revision": 42, "moves": [{ "nodeId": "N1", "x": 120, "y": 220 }] }`
- `SceneFlow.Node.Size.Set`
  - `{ "projectId": "p1", "revision": 42, "nodeId": "N1", "size": { "w": 120, "h": 90 } }`
- `SceneFlow.Node.Anchor.Set`
  - `{ "projectId": "p1", "revision": 42, "nodeId": "N1", "anchor": { "x": 200, "y": 180 } }`
- `SceneFlow.Node.Delete`
  - `{ "projectId": "p1", "revision": 42, "nodeId": "N1" }`
- `SceneFlow.Node.TypeDef.Add|Update|Delete|Move`
  - Add: `{ "projectId": "p1", "nodeId": "N1", "typeDef": { ... }, "index": 0 }`
  - Update/Delete: `{ "projectId": "p1", "nodeId": "N1", "typeDef": { ... }, "index": 0 }`
  - Move: `{ "projectId": "p1", "nodeId": "N1", "from": 0, "to": 1 }`
  - Note: `nodeId` may be empty to target the current active super node (SceneFlow root).
- `SceneFlow.Node.VarDef.Add|Update|Delete|Move`
  - Add: `{ "projectId": "p1", "nodeId": "N1", "varDef": { ... }, "index": 0 }`
  - Update/Delete: `{ "projectId": "p1", "nodeId": "N1", "varDef": { ... }, "index": 0 }`
  - Move: `{ "projectId": "p1", "nodeId": "N1", "from": 0, "to": 1 }`
  - Note: `nodeId` may be empty to target the current active super node (SceneFlow root).
- `SceneFlow.Node.Cmd.Add|Update|Delete|Move`
  - Add: `{ "projectId": "p1", "nodeId": "N1", "command": { "text": "..." }, "index": 0 }`
  - Update/Delete: `{ "projectId": "p1", "nodeId": "N1", "command": { "text": "..." }, "index": 0 }`
  - Move: `{ "projectId": "p1", "nodeId": "N1", "from": 0, "to": 1 }`
  - Note: `nodeId` may be empty to target the current active super node (SceneFlow root).
- `SceneFlow.Edge.Create`
  - `{ "projectId": "p1", "revision": 42, "type": "EEDGE|TEDGE|CEDGE|PEDGE|IEDGE|FEDGE", "sourceId": "N1", "targetId": "N2", "points": [...] }`
- `SceneFlow.Edge.Update`
  - `{ "projectId": "p1", "revision": 42, "edgeId": "E1", "fields": { ... } }`
- `SceneFlow.Edge.ControlPoints.Set`
  - `{ "projectId": "p1", "revision": 42, "edgeId": "E1", "controlPoints": [{ "x": 140, "y": 210 }, { "x": 200, "y": 260 }] }`
- `SceneFlow.Edge.Label.Set`
  - `{ "projectId": "p1", "revision": 42, "edgeId": "E1", "labelPosition": { "x": 180, "y": 220 } }`
- `SceneFlow.Edge.Delete`
  - `{ "projectId": "p1", "revision": 42, "edgeId": "E1" }`
- `SceneFlow.Comment.Create|Update|Delete`
  - `{ "projectId": "p1", "revision": 42, ... }`
- `SceneFlow.Undo|Redo` `{ "projectId": "p1" }`

#### Script
- `Script.Update`
  - `{ "projectId": "p1", "version": 99, "text": "..." }`
  - Or patch form: `{ "projectId": "p1", "version": 99, "changes": [{ "from": 10, "to": 20, "text": "..." }] }`
- `Script.Undo|Redo` `{ "projectId": "p1" }`

#### Functions
- `Function.Create|Update|Delete`
  - `{ "projectId": "p1", "functionDef": { ... } }`

#### Types
- `Type.Create|Update|Delete`
  - `{ "projectId": "p1", "typeDef": { ... } }`

#### Variables / Monitor
- `Variable.Create|Update|Delete`
  - `{ "projectId": "p1", "scope": "local|global|node", "nodeId": "N1", "varDef": { ... } }`
- `Variable.Assign`
  - `{ "projectId": "p1", "scope": "local|global|node", "nodeId": "N1", "name": "x", "valueExpr": "..." }`
- `Runtime.Query`
  - `{ "projectId": "p1", "query": "..." }`

#### Runtime
- `Runtime.Play|Pause|Stop` `{ "projectId": "p1" }`

#### Preferences
- `Config.Update` `{ "projectId": "p1", "values": { ... } }`
- `Preferences.Update` `{ "values": { ... } }`

### Events (server -> client)
- `Project.Opened|Closed|Activated|DirtyChanged`
- `SceneFlow.GraphLoaded`
- `SceneFlow.Node.Added|Updated|Moved|Deleted`
- `SceneFlow.Edge.Added|Updated|Deleted`
- `SceneFlow.Comment.Added|Updated|Deleted`
- `SceneFlow.PathChanged`
- `Script.Changed`
- `Script.ParseErrors`
- `Functions.Changed`
- `Types.Changed`
- `Variables.Changed`
- `Runtime.StateChanged`
- `Error.Raised`

### Error responses
Example error response:
```json
{
  "type": "error",
  "id": "uuid",
  "name": "REVISION_MISMATCH",
  "payload": {
    "expected": 42,
    "actual": 45,
    "snapshot": { ... }
  }
}
```

## Data Models

### Project
```json
{
  "projectId": "p1",
  "name": "MyProject",
  "path": "/abs/path/to/project",
  "dirty": true,
  "runtimeState": "stopped|running|paused",
  "activeSuperNodeId": "N1",
  "config": { "node_width": 140, "node_height": 70 }
}
```

### SceneFlow Snapshot
```json
{
  "projectId": "p1",
  "superNodeId": "N1",
  "superNodeData": { ... },
  "path": ["Root", "SubFlow"],
  "revision": 42,
  "nodes": [...],
  "edges": [...],
  "comments": [...]
}
```

`superNodeData` follows the Node shape and includes an `isRoot` boolean for the active super node.

### Node
```json
{
  "id": "N1",
  "type": "Basic|Super",
  "name": "Intro",
  "comment": "",
  "flavour": "ENODE|TNODE|CNODE|PNODE|INODE|FNODE|NONE",
  "isStart": true,
  "isAltStart": false,
  "isHistory": false,
  "graphics": { "x": 120, "y": 220 },
  "size": { "w": 140, "h": 70 },
  "typeDefs": [TypeDefinition],
  "varDefs": [VariableDefinition],
  "commands": [CommandExecution],
  "typeOptions": ["Int", "Bool", "Float", "String", "CustomType"],
  "typeCatalog": [TypeDefinition]
}
```

### Edge
```json
{
  "id": "E123",
  "type": "EEDGE|TEDGE|CEDGE|PEDGE|IEDGE|FEDGE",
  "sourceId": "N1",
  "targetId": "N2",
  "graphics": {
    "points": [
      { "x": 10, "y": 10, "cx": 20, "cy": 20 }
    ]
  },
  "condition": "x > 3",
  "timeoutMs": 1000,
  "probability": 0.5,
  "altStartMap": [{ "startId": "N1", "altStartId": "N3" }]
}
```

### Comment
```json
{
  "id": "C1",
  "text": "Note...",
  "rect": { "x": 400, "y": 120, "w": 220, "h": 120 }
}
```

### FunctionDefinition
```json
{
  "name": "foo",
  "class": "java.lang.System.out",
  "method": "println",
  "args": [{ "name": "text", "type": "String" }],
  "active": true
}
```

### TypeDefinition
```json
{
  "name": "Scores",
  "flavour": "List",
  "elementType": "Int",
  "syntax": "ListTypeDefinition(Int, Scores)",
  "scope": "local|inherited",
  "ownerId": "N1",
  "ownerName": "Intro"
}
```
```json
{
  "name": "Person",
  "flavour": "Struct",
  "members": [
    { "name": "age", "type": "Int" },
    { "name": "name", "type": "String" }
  ],
  "syntax": "StructTypeDefinition(Person)",
  "scope": "local|inherited",
  "ownerId": "N1",
  "ownerName": "Intro"
}
```

### VariableDefinition
```json
{
  "name": "x",
  "type": "Int|Float|Bool|String|CustomTypeName",
  "expression": "0",
  "syntax": "Int x = 0"
}
```

### CommandExecution
```json
{
  "text": "PlayScenesActivity(\"intro\")",
  "syntax": "PlayScenesActivity(\"intro\")"
}
```

## Concurrency Notes
- Every mutation includes the last known `revision` (graph) or `version` (script).
- The server rejects stale updates with `REVISION_MISMATCH` and sends a snapshot.
- The server broadcasts accepted changes to all clients.
