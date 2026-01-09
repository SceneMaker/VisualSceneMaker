# SceneMaker UI Protocol (Draft)

This document defines the UI protocol between the core runtime and any UI client.
The goal is to keep the protocol UI-agnostic, transport-agnostic, and resource
efficient while enabling desktop (web UI) and future Android clients.

## Goals
- Provide a stable, UI-agnostic event stream and command API.
- Avoid rendering-specific details; expose domain state and layout only.
- Minimize overhead when no UI client is connected.
- Support multiple clients (local web UI, remote UI, future Android UI).

## Non-goals
- Define the web UI UX or rendering rules.
- Define a transport-specific HTTP API in this document.

## Architecture overview
- Core defines protocol types and an event bus with an active flag.
- Transport adapters (desktop: Javalin WS/HTTP) bridge messages to/from core.
- UI clients subscribe to event channels and send commands.

### Module ownership (desktop focus)
- `core`: protocol DTOs, event bus interface, core-side event publisher.
- `editor`: Javalin transport, token auth, message routing.
- `plugins`: may emit domain events via core (no UI dependency).

## Resource efficiency (no client connected)
- Event creation is gated by an `UiEventSink.isActive()` check.
- Use lazy event suppliers for heavier payloads.
- The transport sets active=true only when at least one client is connected.

Example (conceptual):
```
if (!uiEvents.isActive()) return;
uiEvents.emitLazy(() -> new UiEvent(...));
```

## Message envelope (conceptual)
All messages share a common envelope. Message content depends on `type`.
```
{
  "v": 1,
  "id": "uuid",
  "type": "request|response|event",
  "channel": "runtime|sceneflow|script|project|diagnostics|system",
  "ts": 1736000000000
}
```

## JSON schemas (informal)
Request:
```
{
  "v": 1,
  "id": "uuid",
  "type": "request",
  "channel": "project",
  "ts": 1736000000000,
  "action": "project.load",
  "payload": { ... }
}
```

Response (ack):
```
{
  "v": 1,
  "id": "uuid",
  "type": "response",
  "channel": "project",
  "ts": 1736000000000,
  "requestId": "uuid",
  "ok": true,
  "payload": { ... }
}
```

Response (error):
```
{
  "v": 1,
  "id": "uuid",
  "type": "response",
  "channel": "project",
  "ts": 1736000000000,
  "requestId": "uuid",
  "ok": false,
  "error": {
    "code": "INVALID_REQUEST",
    "message": "Missing project path.",
    "details": { ... },
    "retryable": false
  }
}
```

Event:
```
{
  "v": 1,
  "id": "uuid",
  "type": "event",
  "channel": "runtime",
  "ts": 1736000000000,
  "seq": 123,
  "event": "runtime.nodeActive",
  "payload": { ... }
}
```

## Acknowledgement and error semantics
- Requests are acknowledged only via `response` messages.
- `ok=true` indicates success; `ok=false` indicates failure with `error`.
- Events are fire-and-forget (no ack); clients can request a fresh snapshot
  if they detect a gap.
- If `requestId` is unknown, reply with `ok=false` and `code=INVALID_REQUEST`.

Common error codes (non-exhaustive):
- `INVALID_REQUEST`
- `NOT_FOUND`
- `CONFLICT`
- `AUTH_REQUIRED`
- `AUTH_INVALID`
- `INTERNAL_ERROR`

## Channel payload schemas (concrete)
Below are the concrete payload shapes per channel. Field lists are exhaustive for
the initial protocol version; additive fields are allowed in minor updates.

### system
Event: `system.hello`
```
{
  "serverVersion": "4.0.0",
  "build": "2026-01-07",
  "revision": "alpha-bright-larch",
  "tokenRequired": true,
  "wsProtocol": 1
}
```

Event: `system.auth`
```
{
  "ok": true
}
```

Event: `system.preferences`
```
{
  "preferences": {
    "workspace_fontsize": "11",
    "node_width": "90"
  }
}
```

### project
Event: `project.loaded`
```
{
  "project": {
    "id": "p123",
    "name": "Example",
    "path": "/abs/path",
    "dirty": false
  }
}
```

Event: `project.saved`
```
{
  "projectId": "p123",
  "path": "/abs/path",
  "dirty": false
}
```

Event: `project.closed`
```
{
  "projectId": "p123"
}
```

Event: `project.dirty`
```
{
  "dirty": true,
  "areas": ["sceneflow", "script", "config"]
}
```

Event: `project.config`
```
{
  "config": projectConfig
}
```

Request: `project.load`
```
{
  "path": "/abs/path/to/project"
}
```

Request: `project.save`
```
{}
```

Request: `project.saveAs`
```
{
  "path": "/abs/path/to/new/project"
}
```

Request: `project.close`
```
{}
```

Project config object:
```
{
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
```

Project config key hints:
```
{
  "device": "DeviceName",
  "className": "de.dfki.vsm.xtension.SomeDevice",
  "scope": "plugin|agent",
  "supported": true,
  "required": [
    { "name": "host", "description": "Remote host address" }
  ],
  "optional": [
    { "name": "port", "description": "Remote port" }
  ]
}
```

### sceneflow
Node object:
```
{
  "id": "N1",
  "name": "Start",
  "type": "basic|super|history",
  "x": 120,
  "y": 200,
  "w": 80,
  "h": 80,
  "isStart": true,
  "isAltStart": false
}
```

Edge object:
```
{
  "id": "E1",
  "type": "epsilon|conditional|probabilistic|interruptive|timeout|fork",
  "sourceId": "N1",
  "targetId": "N2",
  "label": "a == b",
  "probability": 50,
  "timeoutExpr": "1000",
  "controlPoints": [{"x": 140, "y": 210}, {"x": 200, "y": 260}]
}
```

Comment object:
```
{
  "id": "C1",
  "text": "Note",
  "x": 300,
  "y": 240,
  "w": 160,
  "h": 90
}
```

Event: `sceneflow.snapshot`
```
{
  "sceneflowId": "S1",
  "nodes": [node],
  "edges": [edge],
  "comments": [comment]
}
```

Event: `sceneflow.nodeAdded|sceneflow.nodeUpdated`
```
{ "node": node }
```

Event: `sceneflow.nodeRemoved`
```
{ "id": "N1" }
```

Event: `sceneflow.edgeAdded|sceneflow.edgeUpdated`
```
{ "edge": edge }
```

Event: `sceneflow.edgeRemoved`
```
{ "id": "E1" }
```

Event: `sceneflow.commentAdded|sceneflow.commentUpdated`
```
{ "comment": comment }
```

Event: `sceneflow.commentRemoved`
```
{ "id": "C1" }
```

Request: `node.create`
```
{ "node": node }
```

Request: `node.update`
```
{ "id": "N1", "patch": { "name": "New", "x": 200, "y": 180 } }
```

Request: `node.delete`
```
{ "id": "N1" }
```

Request: `edge.create`
```
{ "edge": edge }
```

Request: `edge.update`
```
{ "id": "E1", "patch": { "label": "x > 0" } }
```

Request: `edge.delete`
```
{ "id": "E1" }
```

Request: `comment.create`
```
{ "comment": comment }
```

Request: `comment.update`
```
{ "id": "C1", "patch": { "text": "Updated" } }
```

Request: `comment.delete`
```
{ "id": "C1" }
```

Request: `selection.set`
```
{
  "selection": {
    "nodes": ["N1", "N2"],
    "edges": ["E1"],
    "comments": []
  }
}
```

### runtime
Event: `runtime.state`
```
{
  "status": "running|paused|stopped",
  "activeNodeId": "N1",
  "activeEdgeId": "E1"
}
```

Event: `runtime.nodeActive`
```
{ "nodeId": "N1" }
```

Event: `runtime.nodeStopped`
```
{ "nodeId": "N1" }
```

Event: `runtime.edgeActive`
```
{ "edgeId": "E1" }
```

Event: `runtime.timeoutProgress`
```
{
  "edgeId": "E1",
  "elapsedMs": 350,
  "timeoutMs": 1000,
  "ratio": 0.35
}
```

Request: `runtime.start`
```
{}
```

Request: `runtime.pause`
```
{}
```

Request: `runtime.stop`
```
{}
```

### vars
Variable item:
```
{
  "name": "cnt",
  "type": "int",
  "value": "3",
  "initialValue": "0"
}
```

Event: `vars.snapshot`
```
{
  "scope": "global|local",
  "ownerId": "S1",
  "variables": [var]
}
```

Event: `vars.updated`
```
{
  "scope": "global|local",
  "ownerId": "S1",
  "variables": [{ "name": "cnt", "value": "4" }]
}
```

### script
Event: `script.errors`
```
{
  "items": [
    { "line": 12, "column": 5, "message": "Unexpected token", "severity": "error" }
  ]
}
```

Event: `script.warnings`
```
{
  "items": [
    { "line": 20, "column": 1, "message": "Unused variable", "severity": "warning" }
  ]
}
```

Request: `script.save`
```
{ "text": "..." }
```

Request: `script.format`
```
{}
```

Event: `script.elements`
```
{
  "elements": scriptElements
}
```

Script elements object:
```
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

## Event stream (S2C)
Events are domain-level, not UI-specific.
- Runtime: `runtime.started`, `runtime.stopped`, `runtime.state`, `runtime.nodeActive`,
  `runtime.nodeStopped`, `runtime.edgeActive`, `runtime.timeoutProgress`.
- Sceneflow: `sceneflow.snapshot`, `sceneflow.nodeAdded`, `sceneflow.nodeUpdated`,
  `sceneflow.edgeAdded`, `sceneflow.edgeUpdated`, `sceneflow.selection`.
- Variables: `vars.snapshot`, `vars.updated`.
- Diagnostics: `script.errors`, `script.warnings`.
- Project: `project.loaded`, `project.saved`, `project.closed`, `project.dirty`.
- System: `system.hello`, `system.auth`, `system.preferences`.

Snapshot vs delta:
- UI receives a `snapshot` on first subscribe.
- Subsequent changes are deltas to reduce payload.

Event stream payloads (detailed):
- `system.preferences`
  ```
  { "preferences": { "workspace_fontsize": "11", "node_width": "90" } }
  ```
- `project.loaded`
  ```
  { "project": { "id": "p1", "name": "Example", "path": "/abs/path", "dirty": false } }
  ```
- `project.saved`
  ```
  { "projectId": "p1", "path": "/abs/path", "dirty": false }
  ```
- `project.closed`
  ```
  { "projectId": "p1" }
  ```
- `project.dirty`
  ```
  { "dirty": true, "areas": ["sceneflow", "script"] }
  ```
- `project.config`
  ```
  { "config": projectConfig }
  ```
- `sceneflow.snapshot`
  ```
  { "sceneflowId": "S1", "nodes": [node], "edges": [edge], "comments": [comment] }
  ```
- `sceneflow.nodeAdded|sceneflow.nodeUpdated`
  ```
  { "node": node }
  ```
- `sceneflow.edgeAdded|sceneflow.edgeUpdated`
  ```
  { "edge": edge }
  ```
- `sceneflow.commentAdded|sceneflow.commentUpdated`
  ```
  { "comment": comment }
  ```
- `vars.snapshot`
  ```
  { "scope": "global|local", "ownerId": "S1", "variables": [var] }
  ```
- `vars.updated`
  ```
  { "scope": "global|local", "ownerId": "S1", "variables": [{ "name": "cnt", "value": "4" }] }
  ```
- `script.errors`
  ```
  { "items": [{ "line": 12, "column": 5, "message": "Unexpected token", "severity": "error" }] }
  ```
- `script.elements`
  ```
  { "elements": scriptElements }
  ```
- `runtime.state`
  ```
  { "status": "running|paused|stopped", "activeNodeId": "N1", "activeEdgeId": "E1" }
  ```
- `runtime.nodeActive`
  ```
  { "nodeId": "N1" }
  ```
- `runtime.nodeStopped`
  ```
  { "nodeId": "N1" }
  ```
- `runtime.edgeActive`
  ```
  { "edgeId": "E1" }
  ```
- `runtime.timeoutProgress`
  ```
  { "edgeId": "E1", "elapsedMs": 350, "timeoutMs": 1000, "ratio": 0.35 }
  ```

## Command API (C2S)
Commands are request/response pairs sent over the `request` envelope.

### Conventions
- Every mutating command includes `projectId`.
- Sceneflow mutations include `revision` (optimistic concurrency).
- On conflict, respond with `ok=false`, `code=CONFLICT`, and a fresh snapshot.
- Responses may include updated `revision` and the updated object.

### Project
`project.load`
```
{ "path": "/abs/path/to/project" }
```
Response:
```
{ "project": { "id": "p1", "name": "Example", "path": "/abs/path", "dirty": false } }
```

`project.save`
```
{ "projectId": "p1" }
```

`project.saveAs`
```
{ "projectId": "p1", "path": "/abs/path/to/new/project" }
```

`project.close`
```
{ "projectId": "p1" }
```

### Project config
`project.config.update`
```
{ "projectId": "p1", "config": projectConfig }
```
Response:
```
{ "config": projectConfig }
```

### Editor config (per-project)
`config.update`
```
{ "projectId": "p1", "values": { "workspace_fontsize": "12" } }
```

### Sceneflow
`sceneflow.navigate`
```
{ "projectId": "p1", "superNodeId": "N1" }
```
Response:
```
{ "sceneflowId": "S1", "revision": 42 }
```

`node.create`
```
{ "projectId": "p1", "revision": 42, "node": node }
```

`node.update`
```
{ "projectId": "p1", "revision": 42, "id": "N1", "patch": { "name": "New" } }
```

`node.size.set`
```
{
  "projectId": "p1",
  "revision": 42,
  "id": "N1",
  "size": { "w": 120, "h": 90 }
}
```

`node.anchor.set`
```
{
  "projectId": "p1",
  "revision": 42,
  "id": "N1",
  "anchor": { "x": 200, "y": 180 }
}
```

`node.delete`
```
{ "projectId": "p1", "revision": 42, "id": "N1" }
```

`edge.create`
```
{ "projectId": "p1", "revision": 42, "edge": edge }
```

`edge.update`
```
{ "projectId": "p1", "revision": 42, "id": "E1", "patch": { "label": "x > 0" } }
```

`edge.label.set`
```
{
  "projectId": "p1",
  "revision": 42,
  "id": "E1",
  "labelPosition": { "x": 180, "y": 220 }
}
```

`edge.controlPoints.set`
```
{
  "projectId": "p1",
  "revision": 42,
  "id": "E1",
  "controlPoints": [
    { "x": 140, "y": 210 },
    { "x": 200, "y": 260 }
  ]
}
```

Control-point updates can also be expressed via `edge.update`:
```
{
  "projectId": "p1",
  "revision": 42,
  "id": "E1",
  "patch": {
    "controlPoints": [
      { "x": 140, "y": 210 },
      { "x": 200, "y": 260 }
    ]
  }
}
```

`edge.delete`
```
{ "projectId": "p1", "revision": 42, "id": "E1" }
```

`comment.create`
```
{ "projectId": "p1", "revision": 42, "comment": comment }
```

`comment.update`
```
{ "projectId": "p1", "revision": 42, "id": "C1", "patch": { "text": "Updated" } }
```

`comment.delete`
```
{ "projectId": "p1", "revision": 42, "id": "C1" }
```

`selection.set`
```
{ "projectId": "p1", "selection": { "nodes": ["N1"], "edges": [], "comments": [] } }
```

`sceneflow.undo|sceneflow.redo`
```
{ "projectId": "p1" }
```

### Definitions (vars, types, commands)
`def.var.add|def.var.update|def.var.delete`
```
{ "projectId": "p1", "scope": "global|local|node", "ownerId": "S1", "index": 0, "varDef": { "name": "cnt", "type": "Int", "init": "0" } }
```

`def.var.move`
```
{ "projectId": "p1", "scope": "global|local|node", "ownerId": "S1", "from": 0, "to": 1 }
```

`def.type.add|def.type.update|def.type.delete`
```
{ "projectId": "p1", "index": 0, "typeDef": { "name": "Person", "flavour": "STRUCT", "members": [{ "name": "age", "type": "Int" }] } }
```

`cmd.add|cmd.update|cmd.delete`
```
{ "projectId": "p1", "ownerId": "N1", "index": 0, "command": { "text": "PlayScene(\"welcome\")" } }
```

`cmd.move`
```
{ "projectId": "p1", "ownerId": "N1", "from": 0, "to": 2 }
```

### Script
`script.save`
```
{ "projectId": "p1", "version": 12, "text": "..." }
```

`script.undo|script.redo`
```
{ "projectId": "p1" }
```

### Runtime
`runtime.start|runtime.pause|runtime.stop`
```
{ "projectId": "p1" }
```

`runtime.variable.set`
```
{ "projectId": "p1", "name": "cnt", "value": "42" }
```

`runtime.query`
```
{ "projectId": "p1", "query": "state(X)" }
```

Responses:
- `runtime.variable.set`: `{ "name": "cnt", "value": "42" }`
- `runtime.query`: `{ "count": 3 }`

Each command gets a response with success/error plus optional updated snapshot.

## Versioning and compatibility
- Envelope `v` is the protocol version.
- Additive changes are allowed within the same major version.
- Breaking changes require a new major version.

## Transport (desktop)
- WebSocket for event stream and command requests.
- HTTP for asset delivery and initial bootstrapping.
- Token auth for `/api` and `/ws`.

## Android notes (future)
- Transport must be Android-compatible (no Javalin/Jetty).
- Protocol DTOs stay in core; transport implementations are replaceable.

## Remote core connection flow (desktop UI)
Goal: run core on Android (server), run editor on desktop (client-only mode).

### Client-only mode
- Desktop UI starts with `--connect http://<host>:<port>` (no local core).
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
