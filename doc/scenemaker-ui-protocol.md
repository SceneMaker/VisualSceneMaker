# SceneMaker UI Protocol (Current)

This document describes the protocol currently implemented by:

- `/Users/gebhard/Code/Repo/VisualSceneMaker/core-webserver/src/main/java/de/dfki/vsm/web/WebUiServer.java`
- `/Users/gebhard/Code/Repo/VisualSceneMaker/core/src/main/java/de/dfki/vsm/runtime/api/RuntimeWsProtocol.java`

It replaces the older conceptual draft and focuses on the wire format that is live in the codebase.

## Scope

- Transport: HTTP (`/api/v1/*`) + WebSocket (`/ws`)
- Server modes:
  - `FULL_EDITOR`
  - `RUNTIME_ONLY`
- Command path: WebSocket-first (runtime REST mutation endpoints are deprecated)

## Module Ownership

- Protocol envelope codec: `:core` (`RuntimeWsProtocol`)
- REST/WS transport and dispatch: `:core-webserver` (`WebUiServer`)
- Web client implementation: `editor/web-ui`

## WebSocket Protocol

## Request Envelope

Preferred request shape:

```json
{
  "id": "req-1",
  "method": "Runtime.Start",
  "params": {
    "projectId": "..."
  }
}
```

Accepted aliases:

- `name` instead of `method`
- `payload` instead of `params`

## Success Response Envelope

```json
{
  "type": "response",
  "id": "req-1",
  "status": "ok",
  "payload": {
    "...": "command result"
  }
}
```

## Error Response Envelope

```json
{
  "type": "error",
  "status": "error",
  "payload": {
    "message": "error text"
  }
}
```

## Server-Pushed Event Envelope

Domain events are pushed as plain event JSON objects (not wrapped in the WS response envelope), for example:

```json
{
  "type": "event",
  "ts": 1736000000000,
  "channel": "runtime",
  "event": "runtime.nodeActive",
  "payload": {
    "projectId": "...",
    "nodeId": "N1"
  }
}
```

Additional push messages are emitted for snapshot updates:

- `sceneflow.snapshot`
- `script.snapshot`
- `runtime.state`

## WebSocket Command Catalog

Registered WS methods (current):

| Group | Methods |
|---|---|
| SceneFlow snapshots | `SceneFlow.Get`, `SceneFlow.Snapshot` |
| Node CRUD/move | `SceneFlow.Node.Add`, `SceneFlow.Node.Create`, `SceneFlow.Node.Update`, `SceneFlow.Node.Delete`, `SceneFlow.Node.Move`, `SceneFlow.Node.MoveGroup` |
| Edge CRUD/layout | `SceneFlow.Edge.Add`, `SceneFlow.Edge.Create`, `SceneFlow.Edge.Update`, `SceneFlow.Edge.Delete`, `SceneFlow.Edge.Normalize`, `SceneFlow.Edge.Straighten`, `SceneFlow.Edge.NormalizeAll`, `SceneFlow.Edge.StraightenAll`, `SceneFlow.Edge.NormalizeGroup`, `SceneFlow.Edge.StraightenGroup`, `SceneFlow.Edge.Retarget`, `SceneFlow.Edge.PEdge.UpdateGroup` |
| Comments | `SceneFlow.Comment.Add`, `SceneFlow.Comment.Create`, `SceneFlow.Comment.Update`, `SceneFlow.Comment.Delete` |
| Selection/history | `SceneFlow.Selection.Copy`, `SceneFlow.Selection.Paste`, `SceneFlow.Undo`, `SceneFlow.Redo` |
| PlayScene helpers | `SceneFlow.PlayScene.Find`, `SceneFlow.PlayScene.FindMany`, `SceneFlow.PlayScene.Rename` |
| Node defs/cmds | `SceneFlow.Node.VarDef.Add`, `SceneFlow.Node.VarDef.Update`, `SceneFlow.Node.VarDef.Delete`, `SceneFlow.Node.VarDef.Move`, `SceneFlow.Node.TypeDef.Add`, `SceneFlow.Node.TypeDef.Update`, `SceneFlow.Node.TypeDef.Delete`, `SceneFlow.Node.TypeDef.Move`, `SceneFlow.Node.Cmd.Add`, `SceneFlow.Node.Cmd.Update`, `SceneFlow.Node.Cmd.Delete`, `SceneFlow.Node.Cmd.Move` |
| Project/config/preferences | `Script.Update`, `Config.Update`, `ProjectConfig.Plugin.Create`, `Project.Templates.Install`, `ProjectConfig.Update`, `Preferences.Update`, `Project.Save`, `Project.SaveAs`, `Project.Close`, `Embeddings.Start` |
| Runtime | `Runtime.Load`, `Runtime.Play`, `Runtime.Start`, `Runtime.Resume`, `Runtime.Pause`, `Runtime.Stop`, `Runtime.Unload`, `Runtime.Variable.Set`, `Runtime.Query` |

## Mode Gating

- In `RUNTIME_ONLY`, editing commands are rejected with:
  - `error = "EDITING_NOT_SUPPORTED"`
  - message: editing not supported in runtime-only mode
- Editing is defined as `SceneFlow.Node.*`, `SceneFlow.Edge.*`, `SceneFlow.Comment.*`, `SceneFlow.Undo`, `SceneFlow.Redo`, `SceneFlow.PlayScene.Rename`, `Script.*`, `Config.Update`, `ProjectConfig.Update`, `Preferences.Update`, `Embeddings.Start`.

## REST API

All REST endpoints are under `/api/v1`.

## Common Endpoints (Both Modes)

- `GET /info`
- `GET /transport`
- `GET /token`
- `GET /projects`
- `GET /projects/recent`
- `GET /projects/samples`
- `GET /projects/tutorials`
- `GET /preferences`
- `GET /devices`
- `GET /projects/{pid}/config`
- `GET /projects/{pid}/project-config`
- `GET /projects/{pid}/project-config/keys`
- `GET /projects/{pid}/validate/vars`
- `GET /projects/{pid}/plugin-interfaces`
- `GET /projects/{pid}/script`
- `GET /projects/{pid}/script/scenes`
- `GET /projects/{pid}/script/elements`
- `GET /projects/{pid}/semantic`
- `PUT /projects/{pid}/semantic`
- `POST /projects/{pid}/semantic/syntax`
- `POST /projects/{pid}/semantic/analyze`
- `GET /projects/{pid}/sceneflow`
- `GET /projects/{pid}/runtime`
- `GET /projects/{pid}/history/commands`
- `POST /projects/{pid}/sceneflow/navigate`
- `POST /llm/models`
- `POST /llm/test`
- `POST /llm/generate`

## FULL_EDITOR-Only REST Endpoints

- `POST /projects/open`
- `POST /projects`
- `POST /projects/{pid}/save`
- `POST /projects/{pid}/save-as`
- `POST /projects/{pid}/close`
- `POST /projects/recent/remove`
- `POST /projects/recent/add`
- `POST /projects/opened`
- `POST /projects/saved`
- `POST /projects/{pid}/script/diagnostics`
- `GET /images/{file}`

## RUNTIME_ONLY REST Endpoints

Mutation endpoints (deprecated transport path, prefer WS):

- `POST /runtime/load`
- `POST /runtime/start`
- `POST /runtime/pause`
- `POST /runtime/resume`
- `POST /runtime/stop`
- `POST /runtime/unload`

Read endpoints:

- `GET /runtime/status`
- `GET /runtime/variables`
- `GET /runtime/sceneflow`

Runtime mutation deprecation behavior:

- REST mutations include deprecation headers (`Warning`, `Deprecation`, `X-VSM-Preferred-Transport`).
- If runtime REST mutations are disabled, endpoints return HTTP `410` with `ENDPOINT_DEPRECATED`.

## Capability Fields

Many responses include runtime capability fields:

- `logicEnabled`
- `platform` (`desktop` or `android`)
- `preferredTransport` (`ws`)
- `commandTransport` (`ws`)
- `eventTransport` (`ws`)
- `bootstrapTransport` (`http`)
- `runtimeRestMutationsEnabled`
- `wsPath` (`/ws`)
- `apiPrefix` (`/api/v1`)

Clients should read these fields dynamically rather than hard-coding transport assumptions.

## Error Shape

Internal command errors are normalized as:

```json
{
  "status": "error",
  "error": "ERROR_CODE",
  "message": "human-readable message"
}
```

Some handlers may additionally include capability fields in the same response object.

## Notes for Client Authors

- Use HTTP for bootstrap (`/info`, `/token`, project lists/snapshots).
- Use WebSocket for runtime control and all interactive mutations.
- Expect both:
  - WS response envelopes (`type=response`/`type=error`)
  - asynchronous event pushes (`type=event` or event-only snapshot messages).
- Treat unknown JSON fields as forward-compatible extensions.

## Minimal Client Bootstrap Sequence

Recommended concrete startup order:

1. `GET /api/v1/info`
   - Read `mode`, transport/capability fields, build metadata.
2. `GET /api/v1/token`
   - Cache token if your client uses token-auth headers/query params.
3. `GET /api/v1/projects`
   - Choose an already-loaded project (or prepare to open/load one).
4. `GET /api/v1/projects/{pid}/sceneflow`
   - Fetch initial sceneflow snapshot for first render.
5. Open WebSocket `ws://<host>:<port>/ws`
   - Start receiving runtime/sceneflow/script events.
6. Send WS request `SceneFlow.Get` with `{ "projectId": "<pid>" }`
   - Confirm current snapshot over WS channel.
7. Send runtime control commands over WS as needed:
   - `Runtime.Start`, `Runtime.Pause`, `Runtime.Resume`, `Runtime.Stop`, `Runtime.Variable.Set`, `Runtime.Query`.

For `RUNTIME_ONLY` deployments, load first if needed:

1. `POST /api/v1/runtime/load` (deprecated path) or WS `Runtime.Load` (preferred).
2. `GET /api/v1/runtime/status`
3. Open `/ws`, then continue with WS runtime commands.
