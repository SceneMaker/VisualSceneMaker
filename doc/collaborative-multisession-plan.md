# Collaborative & Multi-Session VisualSceneMaker — Architecture Plan

**Branch:** server2026
**Date:** 2026-03-10
**Purpose:** Planning document for extending VisualSceneMaker to support simultaneous collaborative editing and parallel multi-session runtime execution

---

## Table of Contents

1. [Motivation](#motivation)
2. [Current State Assessment](#current-state-assessment)
3. [Target Architecture](#target-architecture)
4. [New Components](#new-components)
5. [Implementation Phases](#implementation-phases)
6. [Impact on Existing Usage](#impact-on-existing-usage)
7. [Parallel Execution Capacity](#parallel-execution-capacity)
8. [Network & Infrastructure Requirements](#network--infrastructure-requirements)

---

## Motivation

**Collaboration** means multiple users on different machines can edit the same VisualSceneMaker project simultaneously, with changes reflected in real time for all participants.

**Multi-session** means multiple independent VSM projects can be executed in parallel on one server, each with its own runtime interpreter, event bus, and plugin set.

Together these two capabilities enable VSM to be deployed as a shared service — for research groups, creative teams, and multi-agent installations — rather than as a single-user desktop tool.

---

## Current State Assessment

### What already works

- `WebUiServer` holds a `projectStore: Map<String, ProjectRef>` — already capable of tracking multiple projects
- `RunTimeProject` is a non-singleton, instantiable class — can be created multiple times in one JVM
- `ServerMode` (`FULL_EDITOR` / `RUNTIME_ONLY`) provides a clean mode boundary
- `WebUiServer` serves WebSocket connections and REST to an arbitrary number of browser clients
- The `--token` authentication mechanism provides a baseline security boundary

### Structural blockers

Three constraints must be resolved before collaboration and multi-session can work correctly:

| Blocker | Description | Phase |
|---|---|---|
| `EventDispatcher` is a global singleton | All projects share one event bus; events from project A reach project B's listeners | Phase A |
| `wsSessions` is a flat set | All WebSocket clients receive all broadcasts; no per-project routing | Phase B |
| No user identity | A single shared token cannot distinguish users; presence and attribution are impossible | Phase F |

---

## Target Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│  Web UI (Svelte)  — extended with presence indicators,           │
│                     collaborative awareness, session picker       │
└──────────────────────────────┬───────────────────────────────────┘
                               │  WebSocket + REST
┌──────────────────────────────▼───────────────────────────────────┐
│  WebUiServer  (Javalin)                                          │
│  ┌─────────────────┐  ┌──────────────┐  ┌─────────────────────┐ │
│  │  SessionGate    │  │ PresenceBus  │  │   OperationLog      │ │
│  │  (auth/routing) │  │ (awareness)  │  │   (edit history)    │ │
│  └────────┬────────┘  └──────┬───────┘  └──────────┬──────────┘ │
│           └──────────────────┴──────────────┬───────┘            │
│                                    CollaborationSession          │
└────────────────────────────────────────────┬─────────────────────┘
                                             │
┌────────────────────────────────────────────▼─────────────────────┐
│  SessionRegistry                                                  │
│  Map<projectId, CollaborationSession>                             │
│                                                                   │
│  Each CollaborationSession owns:                                  │
│    RunTimeProject      (existing — one per session)               │
│    EventDispatcher     (de-singletoned — one per session)         │
│    UiEventBridge       (per-session)                              │
│    OperationLog        (new)                                      │
│    PresenceManager     (new)                                      │
│    Set<WsContext>      subscribers  (per-session WS routing)      │
└───────────────────────────────────────────────────────────────────┘
```

---

## New Components

### Component 1 — De-singletoned `EventDispatcher` *(structural change)*

**Current:** `EventDispatcher.getInstance()` is a JVM-wide singleton. Any event fired by any project reaches all registered listeners, making project isolation impossible.

**Change:** `EventDispatcher` becomes a regular instantiable class. Each `CollaborationSession` creates one instance and injects it into its `RunTimeProject` and `Interpreter` via constructor. The `UiProtocol.ensureBridge(dispatcher)` call already accepts an instance — the wiring is compatible.

This is the prerequisite for all other phases. It is a mechanical refactoring with no behavioral change for single-project use.

### Component 2 — `CollaborationSession`

Replaces the bare `ProjectRef` in `WebUiServer.projectStore`. Serves as the container for all per-project state:

```
CollaborationSession {
  String             projectId
  RunTimeProject     runtimeProject
  EventDispatcher    eventDispatcher     // own instance, not singleton
  UiEventBridge      uiEventBridge       // scoped to this session
  OperationLog       operationLog
  PresenceManager    presenceManager
  Set<WsContext>     subscribers         // only these clients get broadcasts
  RuntimeState       runtimeState
}
```

WebSocket routing changes: on `subscribe(projectId)` message the client's `WsContext` moves from a global pool into the session's `subscribers` set. Broadcasts from a session go only to that set.

### Component 3 — `OperationLog` (collaborative editing)

**Problem:** The current model is: client sends command → server applies → full snapshot broadcast. With multiple concurrent users this produces lost updates and undefined ordering.

**Solution:** Server-serialized operation log with sequence numbers.

Every mutating WebSocket command becomes a `SceneFlowOperation`:

```
SceneFlowOperation {
  long       seq           // monotonically increasing per project
  String     userId
  long       timestamp
  String     method        // e.g. "SceneFlow.Node.Move"
  JSONObject params
  long       basedOnSeq    // client's last-known seq when it sent this
}
```

The server applies operations one at a time. Before applying, it checks `basedOnSeq` against `currentSeq`:

- **Clean apply** (`basedOnSeq == currentSeq`): apply, increment seq, broadcast `operation.applied`
- **Conflict window** (`basedOnSeq < currentSeq`): run conflict resolver

Conflict resolution rules for graph editing:

| Operation pair | Resolution |
|---|---|
| `MoveNode` vs `MoveNode` (same node) | Last-write-wins |
| `AddNode` vs `AddNode` (different nodes) | Both apply — commutative by UUID |
| `DeleteNode` vs `AddEdge` (referencing deleted node) | Reject `AddEdge`, notify sender |
| `RenameNode` vs `RenameNode` (same node) | Last-write-wins |
| Any vs `AddNode` (different nodes) | Both apply independently |

This avoids full OT/CRDT complexity while remaining correct for a graph editor where true conflicts are rare.

**Protocol additions:**

```jsonc
// Client sends (adds basedOnSeq)
{ "id": "...", "method": "SceneFlow.Node.Move", "params": {...}, "basedOnSeq": 47 }

// Server broadcasts to all session subscribers on success
{ "event": "operation.applied", "seq": 48, "userId": "alice",
  "method": "SceneFlow.Node.Move", "params": {...} }

// Server replies to sender on conflict
{ "id": "...", "status": "conflict", "currentSeq": 48,
  "resolution": "rejected" | "merged" }

// Late-joining client requests catch-up
// GET /api/v1/sessions/{id}/operations?since=30
// → [ {seq:31,...}, {seq:32,...}, ... ]
```

The log is kept in memory with periodic snapshot checkpointing to disk. It also serves as the shared undo/redo stack.

### Component 4 — `PresenceManager`

Tracks ephemeral per-user awareness state within a session. Never persisted — exists only while the user is connected.

```
UserPresence {
  String   userId
  String   displayName
  String   color        // assigned on join, stable for session duration
  String   activeNodeId // node currently hovered or being edited
  ViewBox  viewport     // current pan/zoom position in the canvas
  long     lastSeen
}
```

Events broadcast to all session subscribers:

```jsonc
// User connects to a project session
{ "event": "presence.joined", "userId": "alice", "displayName": "Alice", "color": "#e07b54" }

// Cursor/viewport update (throttled ~10Hz)
{ "event": "presence.update", "userId": "alice", "viewport": {...}, "activeNodeId": "n42" }

// User disconnects or navigates away
{ "event": "presence.left", "userId": "alice" }
```

Web UI additions: remote users' viewport rectangles shown as colored overlays on the minimap; nodes being actively edited by a remote user highlighted in that user's color.

### Component 5 — `SessionRegistry`

Replaces the `projectStore` map with a managed lifecycle registry:

```
SessionRegistry {
  create(projectPath) → CollaborationSession
  join(projectId, userId) → CollaborationSession
  leave(projectId, userId)
  destroy(projectId)
  list() → List<SessionSummary>
}
```

New REST endpoints:

```
GET  /api/v1/sessions                    — list all active sessions
POST /api/v1/sessions                    — create session from project path
GET  /api/v1/sessions/{id}/presence      — who is in the session
GET  /api/v1/sessions/{id}/operations?since={seq}  — catch-up log
DELETE /api/v1/sessions/{id}             — destroy session
```

### Component 6 — `RuntimeOrchestrator`

Manages multiple `RunTimeProject` + `Interpreter` pairs for parallel execution:

- **Resource arbitration:** prevents two sessions from loading conflicting hardware-exclusive plugins simultaneously
- **Cross-session messaging:** a `CrossSessionBus` lets one runtime trigger a named event in another (for multi-agent installations)
- **Lifecycle independence:** stopping or crashing one interpreter has no effect on others

### Component 7 — `SessionGate` (identity & auth)

Replaces the single shared `--token` with a lightweight named-user model:

```
UserToken {
  String       token        // opaque, URL-safe random string
  String       userId       // stable identifier
  String       displayName
  Set<String>  roles        // "editor", "viewer", "runtime-admin"
}
```

The `SessionGate` sits in front of all WebSocket and REST handlers. It resolves tokens to `UserToken` objects and attaches them to the request context. This enables:

- **Role-based filtering:** viewers receive broadcasts but edit commands are rejected
- **Attribution:** every `OperationLog` entry is stamped with the real user identity
- **Presence identity:** each connected user has a stable, named identity

Token provisioning remains simple: the server admin generates tokens via CLI flag or a protected admin endpoint (`POST /api/v1/admin/tokens`). No login UI is required.

---

## Implementation Phases

| Phase | Scope | What it unlocks |
|---|---|---|
| **A** | De-singleton `EventDispatcher`, constructor-inject into `RunTimeProject` | True session isolation; prerequisite for all subsequent phases |
| **B** | `CollaborationSession` + per-project WS subscriber routing | Multiple projects without event bleed; multi-session is usable |
| **C** | `OperationLog` + `basedOnSeq` protocol + conflict resolver | Safe collaborative editing with ordering guarantees |
| **D** | `PresenceManager` + presence WebSocket events | Real-time awareness of other users |
| **E** | `SessionRegistry` lifecycle + `RuntimeOrchestrator` | Full multi-session runtime with resource arbitration |
| **F** | `SessionGate` + named user tokens | Role-based access, proper attribution, audit trail |
| **G** | Web UI: presence overlay on minimap and canvas | User-facing collaboration indicators |

**Phase A is the critical prerequisite** and must be completed and tested before any other phase begins. It is a pure refactoring — no behavioral change, no new external dependencies.

---

## Impact on Existing Usage

### Single-project, single-machine (current primary usage)

No functional impact. All proposed changes are either transparent structural refactorings (Phase A) or additive layers that are idle with a single user (Phases B–G). Specific details:

- The `EventDispatcher` de-singletoning changes *how* the dispatcher is obtained (injection vs. static call) but not *what* it does
- The `OperationLog` adds negligible memory overhead (bytes per operation)
- The `PresenceManager` is idle when only one user is connected
- The `SessionGate` token lookup is a single `HashMap.get()` call per WebSocket message
- The `--token` CLI flag continues to work unchanged

The only risk is regression during the Phase A refactoring. A full test suite run after Phase A is mandatory.

### Android runtime connectivity

Fully preserved. The Android runtime path (`RuntimeMain` → `WebUiServer(RUNTIME_ONLY)`) is unchanged in behavior:

- In `RUNTIME_ONLY` mode, `SessionRegistry` contains exactly one `CollaborationSession`
- Runtime control commands (`start/stop/pause/resume`) and their REST/WebSocket endpoints are unchanged
- The Web UI connects to Android via `ws://[android-ip]:port/ws` — this endpoint does not change
- All new components (`CollaborationSession`, `OperationLog`, `PresenceManager`) are implemented in Java 17, maintaining Android-compatibility of the `core-webserver` module
- The existing `--token` flag continues to authenticate remote Web UI connections to Android runtimes

### Performance impact from the refactoring itself

The de-singletoning of `EventDispatcher` is actually a net performance improvement for multi-project scenarios: currently all events from all projects serialize through one shared listener list. After de-singletoning, each project's events dispatch independently with no cross-project lock contention.

---

## Parallel Execution Capacity

This section estimates how many VSM projects can run in parallel before real-time execution timing is noticeably affected. Assumes plugins require no external resources (pure runtime perspective).

### Thread model

Each running project spawns at least one `Process extends java.lang.Thread` for the state machine interpreter. While waiting for transitions (which is almost all of the time — transitions are triggered by events or timeouts, not polled), the thread blocks on `Condition.awaitUninterruptibly()` consuming:

- **CPU:** essentially 0%
- **Stack memory:** 512 KB (JVM default)

### Resource per project (medium project, ~50 nodes)

| Resource | Estimate |
|---|---|
| JVM thread stack (one `Process`) | 512 KB |
| `SceneFlow` + `SceneScript` model heap | 5–20 MB |
| `Interpreter` + `Configuration` + `Environment` | 3–10 MB |
| `EventDispatcher` + bridge + listeners | ~0.5 MB |
| `OperationLog` ring buffer | ~1–2 MB |
| **Total per project** | **~10–35 MB** |

### Capacity table (8-core machine, 8 GB JVM heap)

| Concurrent projects | Heap used | Thread stacks | Assessment |
|---|---|---|---|
| 10 | ~200 MB | ~5 MB | Completely invisible |
| 50 | ~750 MB | ~25 MB | No measurable impact |
| 100 | ~1.5 GB | ~50 MB | Well within limits |
| 200 | ~3 GB | ~100 MB | First GC pressure with G1GC |
| 400 | ~6 GB | ~200 MB | GC pauses possible; tuning required |
| 500+ | heap pressure | ~250 MB | Requires JVM tuning or project hibernation |

### Bottleneck: garbage collection, not CPU

The primary real-time risk at high project counts is GC pause length, not CPU saturation. Waiting interpreter threads use negligible CPU. The concern is allocation pressure from concurrent transition evaluation, event creation, and `OperationLog` entries across many sessions simultaneously.

| GC algorithm | Typical pause | Impact at 200 projects |
|---|---|---|
| G1GC (JVM default) | 10–50 ms | Possible, within existing timing imprecision |
| ZGC (`-XX:+UseZGC`) | < 1 ms | Effectively eliminated |

**Context:** The `Interpreter`'s timing is already limited by `Thread.sleep()` accuracy (~10–15 ms on Linux/macOS under load). Real-time precision for dialogue state machines is therefore already in the 10–50 ms range. A 20 ms GC pause at 200 parallel projects falls within this existing imprecision — it is not noticeable to a user or connected system.

### Practical guidance

- **< 50 projects:** No tuning needed. Suitable for all current VSM deployments.
- **50–150 projects:** Consider ZGC. No other tuning required.
- **150–300 projects:** Requires ZGC + explicit heap sizing (`-Xmx`, `-Xms`). Thread scheduling jitter measurable but below dialogue timing resolution.
- **300+ projects:** Requires architectural extension: project hibernation (serialize idle projects to disk, restore on activity) or partitioning across multiple JVM processes behind a coordinator.

---

## Network & Infrastructure Requirements

This section is addressed to system administrators responsible for deploying collaborative or multi-session VSM installations.

### Port allocation

| Port | Protocol | Purpose | Configurable |
|---|---|---|---|
| 8090 | HTTP + WebSocket | Default editor server (FULL_EDITOR mode) | Yes, `--port=N` |
| 8091 | HTTP + WebSocket | Default runtime server (RUNTIME_ONLY mode) | Yes, `--port=N` |

Both ports serve HTTP (REST API) and WebSocket (`/ws`) on the same port. No separate ports are needed for WebSocket.

If multiple runtime servers run on the same machine (multi-session partitioned across JVMs), each instance requires a distinct port. A reverse proxy can expose them under a unified base path (see below).

### Network binding

By default VSM servers bind to `localhost` (127.0.0.1) — inaccessible from other machines.

To allow remote browser connections or Android runtime connections:

```bash
# Bind to all interfaces (LAN + internet)
java -jar runtime-server.jar --allow-lan --port=8091

# Or bind to a specific interface
# (not yet a CLI flag — configure via reverse proxy)
```

**Recommendation:** Do not expose VSM directly on a public interface. Place it behind a reverse proxy (nginx or Caddy) that handles TLS and access control (see below).

### TLS / HTTPS

VSM servers currently speak plain HTTP and unencrypted WebSocket (`ws://`). For any deployment where traffic crosses a network boundary (LAN, VPN, internet):

- **Terminate TLS at the reverse proxy** — do not run VSM behind a self-signed cert directly
- The browser Web UI requires HTTPS if accessed from a non-localhost origin (browser security policy blocks mixed content and restricts WebSocket on plain HTTP for cross-origin connections)
- Android runtime connections from a remote Web UI likewise require `wss://` (WebSocket over TLS) in any non-localhost setup

**Minimum nginx configuration for a single runtime server:**

```nginx
server {
    listen 443 ssl;
    server_name vsm.example.org;

    ssl_certificate     /etc/ssl/vsm.crt;
    ssl_certificate_key /etc/ssl/vsm.key;

    location / {
        proxy_pass http://localhost:8091;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 3600s;  # keep WebSocket alive
    }
}
```

`proxy_read_timeout 3600s` is important — WebSocket connections for active editing sessions are long-lived and must not be killed by the proxy's default timeout.

### Firewall rules

| Source | Destination | Port | Protocol | Required for |
|---|---|---|---|---|
| Browser clients | VSM server (or reverse proxy) | 443 (TLS) or 8090/8091 | TCP | Web UI access |
| Remote Web UI | Android device | 8091 (or custom) | TCP | Remote runtime control |
| Android device | Internet (or LAN) | outbound any | TCP | Connecting back to push events (if reverse-proxied: 443) |

For Android runtime deployments on a private network, ensure the Android device and the browser clients share a routable path. VPN is the recommended approach for remote Android devices.

### DNS / hostname

For multi-session deployments where different runtime instances serve different projects, assign distinct subdomains or paths:

```
vsm-editor.example.org      → editor server (port 8090)
vsm-runtime.example.org     → shared runtime server (port 8091)
vsm-runtime-2.example.org   → second runtime server (port 8092)
```

Or use path-based routing via the reverse proxy:

```
vsm.example.org/editor/     → port 8090
vsm.example.org/runtime/1/  → port 8091
vsm.example.org/runtime/2/  → port 8092
```

Note: path-based routing requires VSM's asset paths to be relative, which they currently are in the Svelte build (`/web-ui/` → relative). This is suitable for path-prefixing.

### Authentication tokens

VSM uses a shared secret token for authentication (passed as `Authorization: Bearer <token>` header or `?token=<token>` query parameter).

**Current model:** one token shared by all clients — any holder has full access.

**Recommended minimal hardening for LAN/internet deployments:**

- Generate a strong token: `openssl rand -hex 32`
- Pass it via `--token=<value>` at startup
- Do not embed the token in browser bookmarks or log files
- Rotate tokens when team membership changes

**After Phase F** (named user tokens): each user has an individual token with a role (`editor`, `viewer`, `runtime-admin`). The admin provisions tokens via `POST /api/v1/admin/tokens` (protected endpoint, requires admin token). No login page or identity provider integration is required.

### Session persistence across restarts

Currently: all in-memory state is lost on server restart. Projects must be reloaded manually.

**After Phase E** (`SessionRegistry` with checkpointing): the `OperationLog` is periodically checkpointed to disk. On restart, the server can restore sessions from the last checkpoint. This requires write access to a designated data directory:

```bash
java -jar runtime-server.jar --data-dir=/var/lib/vsm --port=8091
```

The data directory requires approximately 10–100 MB per active session (project files + operation log checkpoint). Standard filesystem permissions apply — the VSM process user must have read/write access.

### JVM tuning for production deployments (> 50 parallel sessions)

```bash
# Recommended flags for multi-session production
java \
  -Xms2g -Xmx8g \           # explicit heap bounds
  -XX:+UseZGC \              # low-pause GC for real-time timing
  -XX:+ZGenerational \       # ZGC generational mode (Java 21+)
  -Xss512k \                 # reduce stack per thread (safe for VSM)
  -jar runtime-server.jar
```

For Java 17 (Android-compatible runtime): omit `-XX:+ZGenerational`, use `-XX:+UseZGC` alone.

### Summary checklist for sysadmins

- [ ] Open inbound TCP on the VSM port (default 8090/8091) or on 443 if behind a reverse proxy
- [ ] Configure reverse proxy with `proxy_read_timeout >= 3600s` for WebSocket keepalive
- [ ] Provision a TLS certificate for any non-localhost access
- [ ] Ensure Android devices and browser clients share a routable network path (VPN if remote)
- [ ] Generate and distribute auth tokens securely; do not commit tokens to version control
- [ ] Allocate at minimum 512 MB JVM heap per 10 expected concurrent projects
- [ ] For > 50 concurrent projects: configure ZGC and explicit heap bounds
- [ ] Provide write access to a data directory for session checkpointing (Phase E onward)
- [ ] Assign distinct ports or reverse-proxy paths for each runtime server instance in partitioned deployments
