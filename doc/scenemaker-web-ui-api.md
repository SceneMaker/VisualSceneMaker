# SceneMaker Web UI API (Current)

This document describes the API currently implemented in:

- `/Users/gebhard/Code/Repo/VisualSceneMaker/core-webserver/src/main/java/de/dfki/vsm/web/WebUiServer.java`
- `/Users/gebhard/Code/Repo/VisualSceneMaker/core/src/main/java/de/dfki/vsm/runtime/api/RuntimeWsProtocol.java`

Base API prefix: `/api/v1`  
WebSocket path: `/ws`

## Server Modes

- `FULL_EDITOR`: multi-project editing + runtime control
- `RUNTIME_ONLY`: runtime control only; editing commands rejected

Mode is returned by `GET /api/v1/info` in the `mode` field.

## Transport and Bootstrap

Use these first:

1. `GET /api/v1/info`
2. `GET /api/v1/transport`
3. `GET /api/v1/token`
4. `GET /api/v1/projects`
5. `GET /api/v1/projects/{pid}/sceneflow`
6. open WebSocket `/ws`

`GET /api/v1/transport` returns transport hints such as:

- `preferred`, `commandTransport`, `eventTransport` (currently `ws`)
- `bootstrapTransport` (`http`)
- `runtimeRestMutationsEnabled`
- `wsPath`
- `apiPrefix`
- `bootstrapEndpoints`

## HTTP Endpoints

## Common (Both Modes)

- `GET /api/v1/info`
- `GET /api/v1/transport`
- `GET /api/v1/token`
- `GET /api/v1/projects`
- `GET /api/v1/projects/recent`
- `GET /api/v1/projects/samples`
- `GET /api/v1/projects/tutorials`
- `GET /api/v1/preferences`
- `GET /api/v1/devices`
- `GET /api/v1/projects/{pid}/config`
- `GET /api/v1/projects/{pid}/project-config`
- `GET /api/v1/projects/{pid}/project-config/keys`
- `GET /api/v1/projects/{pid}/validate/vars`
- `GET /api/v1/projects/{pid}/plugin-interfaces`
- `GET /api/v1/projects/{pid}/script`
- `GET /api/v1/projects/{pid}/script/scenes`
- `GET /api/v1/projects/{pid}/script/elements`
- `GET /api/v1/projects/{pid}/semantic`
- `PUT /api/v1/projects/{pid}/semantic`
- `POST /api/v1/projects/{pid}/semantic/syntax`
- `POST /api/v1/projects/{pid}/semantic/analyze`
- `GET /api/v1/projects/{pid}/sceneflow`
- `GET /api/v1/projects/{pid}/runtime`
- `GET /api/v1/projects/{pid}/history/commands`
- `POST /api/v1/projects/{pid}/sceneflow/navigate`
- `POST /api/v1/llm/models`
- `POST /api/v1/llm/test`
- `POST /api/v1/llm/generate`

## FULL_EDITOR Only

- `POST /api/v1/projects/open`
- `POST /api/v1/projects`
- `POST /api/v1/projects/{pid}/save`
- `POST /api/v1/projects/{pid}/save-as`
- `POST /api/v1/projects/{pid}/close`
- `POST /api/v1/projects/recent/remove`
- `POST /api/v1/projects/recent/add`
- `POST /api/v1/projects/opened`
- `POST /api/v1/projects/saved`
- `POST /api/v1/projects/{pid}/script/diagnostics`
- `GET /images/{file}`

## RUNTIME_ONLY Only

Runtime read endpoints:

- `GET /api/v1/runtime/status`
- `GET /api/v1/runtime/variables`
- `GET /api/v1/runtime/sceneflow`

Runtime mutation endpoints (deprecated path, WS preferred):

- `POST /api/v1/runtime/load`
- `POST /api/v1/runtime/start`
- `POST /api/v1/runtime/pause`
- `POST /api/v1/runtime/resume`
- `POST /api/v1/runtime/stop`
- `POST /api/v1/runtime/unload`

When runtime REST mutations are disabled, these return `410` with `ENDPOINT_DEPRECATED`.

## WebSocket API

## Request Format

Preferred:

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

## Response Format

Success:

```json
{
  "type": "response",
  "id": "req-1",
  "status": "ok",
  "payload": {}
}
```

Error:

```json
{
  "type": "error",
  "status": "error",
  "payload": {
    "message": "..."
  }
}
```

## Server Push Events

Typical event envelope:

```json
{
  "type": "event",
  "ts": 1736000000000,
  "channel": "runtime|vars",
  "event": "runtime.nodeActive",
  "payload": {
    "projectId": "...",
    "...": "..."
  }
}
```

Snapshot pushes are also emitted (event-focused objects):

- `sceneflow.snapshot`
- `script.snapshot`
- `runtime.state`

## WS Command Methods (Registered)

- SceneFlow:
  - `SceneFlow.Get`, `SceneFlow.Snapshot`
  - `SceneFlow.Node.Add`, `SceneFlow.Node.Create`, `SceneFlow.Node.Update`, `SceneFlow.Node.Delete`, `SceneFlow.Node.Move`, `SceneFlow.Node.MoveGroup`
  - `SceneFlow.Edge.Add`, `SceneFlow.Edge.Create`, `SceneFlow.Edge.Update`, `SceneFlow.Edge.Delete`
  - `SceneFlow.Edge.Normalize`, `SceneFlow.Edge.Straighten`, `SceneFlow.Edge.NormalizeAll`, `SceneFlow.Edge.StraightenAll`, `SceneFlow.Edge.NormalizeGroup`, `SceneFlow.Edge.StraightenGroup`
  - `SceneFlow.Edge.Retarget`, `SceneFlow.Edge.PEdge.UpdateGroup`
  - `SceneFlow.Comment.Add`, `SceneFlow.Comment.Create`, `SceneFlow.Comment.Update`, `SceneFlow.Comment.Delete`
  - `SceneFlow.Selection.Copy`, `SceneFlow.Selection.Paste`
  - `SceneFlow.Undo`, `SceneFlow.Redo`
  - `SceneFlow.PlayScene.Find`, `SceneFlow.PlayScene.FindMany`, `SceneFlow.PlayScene.Rename`
  - `SceneFlow.Node.VarDef.Add`, `SceneFlow.Node.VarDef.Update`, `SceneFlow.Node.VarDef.Delete`, `SceneFlow.Node.VarDef.Move`
  - `SceneFlow.Node.TypeDef.Add`, `SceneFlow.Node.TypeDef.Update`, `SceneFlow.Node.TypeDef.Delete`, `SceneFlow.Node.TypeDef.Move`
  - `SceneFlow.Node.Cmd.Add`, `SceneFlow.Node.Cmd.Update`, `SceneFlow.Node.Cmd.Delete`, `SceneFlow.Node.Cmd.Move`
- Project/config:
  - `Script.Update`
  - `Config.Update`
  - `ProjectConfig.Plugin.Create`
  - `Project.Templates.Install`
  - `ProjectConfig.Update`
  - `Preferences.Update`
  - `Project.Save`, `Project.SaveAs`, `Project.Close`
  - `Embeddings.Start`
- Runtime:
  - `Runtime.Load`, `Runtime.Play`, `Runtime.Start`, `Runtime.Resume`, `Runtime.Pause`, `Runtime.Stop`, `Runtime.Unload`, `Runtime.Variable.Set`, `Runtime.Query`

## Mode Rules for WS

In `RUNTIME_ONLY`, editing commands are blocked with:

```json
{
  "status": "error",
  "error": "EDITING_NOT_SUPPORTED",
  "message": "Editing not supported in runtime-only mode"
}
```

Editing command groups include `SceneFlow.Node.*`, `SceneFlow.Edge.*`, `SceneFlow.Comment.*`, `SceneFlow.Undo`, `SceneFlow.Redo`, `SceneFlow.PlayScene.Rename`, `Script.*`, `Config.Update`, `ProjectConfig.Update`, `Preferences.Update`, and `Embeddings.Start`.

## Runtime State Events

The runtime emits state and execution events over WS, including:

- `runtime.state`
- `runtime.nodeActive`
- `runtime.nodeStopped`
- `runtime.edgeActive`
- `runtime.timeoutProgress`
- `runtime.scene.playing`
- `runtime.scene.done`
- `runtime.scene.turn`
- `runtime.scene.turnDone`
- `vars.updated`

## Error/Capability Fields

Error payloads are normalized to:

```json
{
  "status": "error",
  "error": "ERROR_CODE",
  "message": "..."
}
```

Many responses include capability fields:

- `logicEnabled`
- `platform`
- `preferredTransport`
- `commandTransport`
- `eventTransport`
- `bootstrapTransport`
- `runtimeRestMutationsEnabled`
- `wsPath`
- `apiPrefix`
