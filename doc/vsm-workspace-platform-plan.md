# VSM Multi-User Workspace Platform — Architecture Plan

**Branch:** collaboration
**Date:** 2026-07-28
**Purpose:** Planning document for turning the single-project "Confidence - SIA
Layer" server deployment into a general VSM teaching/coursework platform —
one shared runtime server, each DFKI staff member and each student gets
their own assigned VSM workspace, authenticated via DFKI's Keycloak SSO.

**Status:** Decided — all originally-open questions now have answers (see
Decisions 9, 11, and 13–16). Two low-stakes implementation details remain (exact pool
size, nginx scripting engine choice — see end of doc) but nothing here is
blocking; implementation can start. Nothing in here is implemented yet.

---

## Table of Contents

1. [Motivation](#motivation)
2. [Relationship to the existing collaborative-multisession-plan](#relationship-to-the-existing-collaborative-multisession-plan)
3. [Decisions made so far](#decisions-made-so-far)
4. [Target architecture](#target-architecture)
5. [New components](#new-components)
6. [Implementation phases](#implementation-phases)
7. [Remaining implementation details](#remaining-implementation-details)
8. [Out of scope](#out-of-scope)

---

## Motivation

The current deployment (`vsm-server` GitLab repo, `docker-compose.yml`,
`docker-entrypoint.sh`) runs one VSM `FULL_EDITOR` server auto-opening a
single fixed project ("Confidence - SIA Layer") for anyone who reaches it.
That's no longer the actual requirement: the sysadmin wants this server to
become a general platform where **each DFKI staff member and each student
has their own VSM workspace**, starting with Confidence - SIA Layer as the
first assigned project, with login via DFKI's Keycloak
(`https://keycloak.scaai.dfki.dev/`).

This is a materially different problem from "add a login page in front of
the existing deployment" — it requires VSM itself to know *who* is asking,
and to scope *which* project(s) they're allowed to see, edit, preview, and
run.

---

## Relationship to the existing collaborative-multisession-plan

`doc/collaborative-multisession-plan.md` (2026-03-10) already identified
"no user identity" as a structural blocker (its Phase F) and sketched a
`SessionGate`/`UserToken` model — but for a *different* goal: attributing
edits in a **shared collaborative editing** session, explicitly *without*
SSO/OIDC ("no login UI or identity provider integration is required").

This plan has a different goal — **workspace isolation between mutually
distinct users**, not collaboration on one shared project — but can reuse
that plan's existing, dormant `SessionGate`/`UserToken` data model
(`core-webserver/src/main/java/de/dfki/vsm/web/SessionGate.java`,
`UserToken.java`) as the attachment point for real identity, rather than
building a parallel mechanism. `SessionGate.provision(userId, displayName,
roles)` already does roughly the right thing; what's missing is anything
that *calls* it based on a verified login, and anything that *enforces* the
result on incoming requests.

That plan's own infrastructure section also already documents the
port-per-process constraint this plan has to solve: *"if multiple runtime
servers run on the same machine ... each instance requires a distinct
port."* We're solving that within one process via a port pool (see below)
rather than one process per session.

---

## Decisions made so far

| # | Decision | Why |
|---|---|---|
| 1 | One shared VSM runtime server (not one container per user) | Simpler ops, one deployment to maintain |
| 2 | Login via DFKI Keycloak (`keycloak.scaai.dfki.dev`), predefined accounts, no self-registration | Sysadmin wants to control who's registered |
| 3 | **VSM's own Java code validates JWTs directly** (not a proxy-injected trusted header) | Explicit choice — keeps VSM self-contained; also enables future non-browser API clients to authenticate directly against VSM without going through the proxy |
| 4 | Every authenticated user — staff and students alike — gets full editor capability (not view-only), including SIA preview | No feature-level restriction by role |
| 5 | *Which* project(s) a user can reach is restricted via an **assignment model**: an admin assigns specific project(s) to specific users upfront. No open/self-serve "create new project" for regular users. No general per-user workspace directory browsing. | Sysadmin wants controlled access, not an open authoring free-for-all |
| 6 | There **is** a distinct in-app **admin role** that can see all projects/users and manage assignments through VSM itself | Someone needs to do the assigning without shelling into the server |
| 7 | Public-facing URLs are **path-based, not port-based** (`https://host/projects/{id}/...`, REST-style) | Sysadmin's explicit ask, also just cleaner |
| 8 | Internally, ports are still used — `htmlgui-ws`/`charamel-embed` keep spinning up their own bound-port servers exactly as today; **nginx** maps path → internal port. Plugin internals are not rewritten. | Rewriting those plugins to be path-mounted sub-routes of the shared Javalin app was considered and explicitly rejected as too much rework/risk for this phase |
| 9 | Target concurrency: **~20 projects running simultaneously** as a baseline, but the pool is **shared between SIA preview and full Run** (not split into separate pools) and sized with **headroom above 20** to absorb both — exact final number is a Phase 4/6 tuning detail, not an architectural one. | From the sysadmin's stated scale; preview and Run are the same kind of resource consumer, so one pool with margin is simpler than two pools with a starvation risk between them |
| 10 | The path→port mapping is resolved by a **second nginx, fully under our own control**, living in our `docker-compose.yml` alongside `vsm-server` — not by the SCAAI-managed outer nginx. The outer nginx gets one static vhost forwarding everything to one fixed port; all dynamism lives on our side. | Minimizes the ask of the SCAAI sysadmin (a one-time static config, never touched again) and keeps all churn inside infrastructure we can iterate on freely |
| 11 | That inner nginx resolves the dynamic path→port mapping live (**"fancier"** option — no config-regen/reload cycle) by **reading a shared file/registry on a Docker volume shared with `vsm-server`** (not a subrequest to a VSM lookup endpoint) | Chosen over both the simpler regen-and-reload approach and the lookup-endpoint alternative — no new network hop, no new endpoint to firewall |
| 12 | **The frontend does the OIDC login dance** (e.g. `keycloak-js` in the Svelte app: redirect to Keycloak, handle callback, hold/refresh the token) — VSM's backend is a pure OAuth2 **resource server** that only ever validates an already-obtained Bearer JWT. No redirect handling, no auth-code exchange, no server-side session cookie in VSM itself. | Standard SPA+API split; keeps backend scope to validation only, matches Keycloak's own client tooling model |
| 13 | **WS authentication is first-message, not query-param.** Client opens `/ws` with no token in the URL, then sends an auth message immediately after connect; server holds the connection unauthenticated (with a timeout) until that message validates. | Avoids the JWT ever landing in nginx access logs/browser history — same leakage class as the earlier GitLab-token discussion |
| 14 | **`ProjectAssignmentTable` is a flat file**, not a database. | Simplest option that fits; no DB currently exists in this stack |
| 15 | **No refresh-token flow in VSM.** On JWT expiry, sessions re-authenticate rather than silently refreshing server-side — the browser's `keycloak-js` handles its own token refresh, and the WS connection detects an expired/rejected token mid-session and re-authenticates (re-sends the first-message auth step with a fresh token). | Keeps the backend stateless; avoids VSM having to manage refresh-token storage/rotation |
| 16 | **Pool exhaustion (e.g. a 21st concurrent Run/preview request) returns an explicit error** telling the user to wait and retry — no silent queueing, no preempting another user's session. | Predictable behavior over automatic (and potentially surprising) preemption |

---

## Target architecture

```
Browser (Svelte app + keycloak-js)
  │  1. Redirect to Keycloak, log in, come back with an access token —
  │     entirely browser-side, VSM/nginx never see this exchange.
  │  2. All subsequent requests: HTTPS, path-based, carrying
  │     `Authorization: Bearer <jwt>`:
  │     https://vsm.example.dfki.de/projects/{id}/...
  ▼
nginx — OUTER, SCAAI-managed
  │  TLS termination. ONE static vhost, ONE fixed upstream port.
  │  Forwards everything (path + WS upgrade headers) unchanged to:
  │  http://exs-91204.sb.dfki.de:{VSMPORT}/projects/{id}/...
  │  Never needs to change again regardless of how ports/projects churn.
  ▼
nginx — INNER, ours, a service in our own docker-compose.yml
  │  Does the actual dynamic path→port resolution, live (no reload cycle):
  │    /projects/{id}/edit           → vsm-server:8090 (always fixed)
  │    /projects/{id}/gui/*          → whichever port PortPoolManager
  │                                     currently has bound for this
  │                                     project's htmlgui-ws instance
  │    /projects/{id}/avatar/xenia/* → same, for that project's
  │                                     charamel-embed instance
  │  Resolved by reading a shared file/registry on a volume shared with
  │  vsm-server (Decision 11) — scripting engine (njs vs Lua) is the
  │  one remaining implementation detail, see end of doc.
  ▼
VSM (single FULL_EDITOR process, "vsm-server")
  ┌─────────────────────────────────────────────────────────────┐
  │  JwtAuthFilter          — pure resource-server validation:   │
  │    verifies the Bearer JWT against Keycloak's JWKS on every   │
  │    REST request, and on the WS connection's first message     │
  │    (Decision 13) rather than a query param.                   │
  │  ProjectAssignmentTable — user → [projectId, ...], + admin   │
  │    flag; stored as a flat file (Decision 14)                 │
  │  PortPoolManager        — ~20+ port-sets, shared between Run  │
  │    and SIA preview with headroom (Decision 9); assigns one to │
  │    a project's plugins when its runtime/preview starts,       │
  │    releases on stop; writes the registry file the inner nginx │
  │    reads; returns an explicit error (not a queue) when         │
  │    exhausted (Decision 16)                                    │
  │  (existing) projectStore, SessionGate, ProjectRef, etc.       │
  └─────────────────────────────────────────────────────────────┘
```

Nothing about `RunTimeProject`, `Interpreter`, or the plugin execution model
changes. The load-bearing new pieces are: the frontend's OIDC integration,
the backend's JWT validation, the assignment table, the port pool, and the
inner nginx that bridges the pool to path-based public URLs. The outer
(SCAAI) nginx is intentionally kept as simple/static as possible.

---

## New components

### Component 1 — `JwtAuthFilter` (pure resource-server validation)

A Javalin `before()` filter (the thing that's conspicuously absent today —
see the earlier investigation: zero enforcement exists anywhere in
`WebUiServer`) that:

- Reads `Authorization: Bearer <jwt>` from REST requests. For WS
  connections, the token isn't in the handshake URL at all — the
  connection opens unauthenticated and must send it as its first message
  (Decision 13); the filter holds the connection open-but-inert until that
  message arrives and validates, with a timeout if it never does.
- Validates the JWT against Keycloak's JWKS endpoint (signature, `iss`,
  `aud`, `exp`). Likely library: Nimbus JOSE+JWT (mature, widely used for
  exactly this in Java, handles JWKS fetch/caching).
- On success, resolves/creates a `UserToken` via the existing
  `SessionGate.provision(sub, preferred_username, roles)` and attaches it
  to the request context.
- On failure, 401s the request (REST) or refuses the WS upgrade.

**Explicitly does not**: redirect anyone to Keycloak, handle the OIDC
callback/authorization code, or exchange anything for a token — that's all
Component 2, in the browser. This filter only ever sees a token that
already exists and decides whether it's valid.

This is the piece that turns `SessionGate` from "inert data model" into
"actually enforced."

### Component 2 — Frontend OIDC integration (`keycloak-js`)

Lives entirely in `editor/web-ui` (Svelte), not the backend. On load, checks
for a valid session; if none, redirects the browser to Keycloak; on return,
`keycloak-js` parses the callback, holds the access token, and silently
refreshes it. Every REST call from the Svelte app attaches
`Authorization: Bearer <access_token>`. The WS connection sends that same
token as its first message right after opening, not in the URL
(Decision 13) — and re-sends a fresh one if the server rejects it as
expired mid-session (Decision 15; VSM itself does no refresh-token
handling, `keycloak-js`'s own refresh is what supplies the fresh token to
resend).

VSM/nginx are never involved in the browser's Keycloak exchange; by the
time a request reaches the outer nginx, it already carries a token or it
doesn't.

### Component 3 — `ProjectAssignmentTable`

A persistent `user → [projectId]` mapping, plus an `isAdmin` flag per user.
Every project-touching handler (`handleProjectOpen`, `handleProjects`,
`handleProjectSave`, runtime start/stop, etc.) checks this table before
proceeding — non-admins only ever see their own assigned project(s);
"Open by path"/arbitrary filesystem browsing is removed or admin-gated
entirely. Admins can see/assign everything.

Storage: a flat file (Decision 14) — no new database dependency.

### Component 4 — `PortPoolManager`

Manages a pool of port-sets — baseline ~20, sized with headroom since the
pool is shared between full Run and SIA preview rather than split into two
pools (Decision 9). When a project's runtime (or SIA preview) is about to
start, it asks the pool for a free set and overrides that project's
`PluginConfig` port values before plugin `launch()` — instead of trusting
whatever's literally written in that project's own `project.xml`. Releases
the set back to the pool on stop/disconnect/idle timeout. If none are free,
returns an explicit error rather than queueing (Decision 16).

The exact final pool size, and exactly how many ports "a set" reserves
(Confidence - SIA Layer needs 4 today — `html_port`, `ws_port`, two
`charamel-embed` ports — a different student project could need more or
fewer), is tuned during Phase 4/6 rather than fixed here.

### Component 5 — Two-tier nginx (outer SCAAI-managed, inner ours)

**Outer nginx (SCAAI-managed, not ours to configure day-to-day):** one
static vhost. TLS termination, forwards everything — path and WS upgrade
headers unchanged — to one fixed port on the host
(`http://exs-91204.sb.dfki.de:{VSMPORT}`). This is a one-time setup; it
never needs to change again regardless of how projects/ports churn behind
it. This is deliberately the *only* thing asked of the sysadmin's
infrastructure.

**Inner nginx (ours — a new service in `docker-compose.yml`, alongside
`vsm-server`):** does the actual dynamic path→port resolution, chosen to
resolve live rather than via config-regen-and-reload:
- `/projects/{id}/edit` (and other core-app paths) → always `vsm-server:8090`,
  static.
- `/projects/{id}/gui/*`, `/projects/{id}/avatar/*` → whichever port
  `PortPoolManager` currently has bound for that project's plugin instance.
  Resolved per new connection by reading a shared registry file on a volume
  shared with `vsm-server`, written by `PortPoolManager` on every
  assignment change (Decision 11) — not a subrequest to a VSM lookup
  endpoint. No `nginx -s reload` cycle needed; resolution happens
  per-connection. Scripting engine to read that file (`njs` vs
  Lua/OpenResty) is the one remaining implementation detail — see end of
  doc.

Because this nginx is entirely within our own compose stack, we can change
its config, add modules, or swap the resolution mechanism at any time
without coordinating with SCAAI.

### Component 6 — Admin UI/API

Minimal at first: list users, list projects, assign/unassign, mark a user
as admin. Could piggyback on the existing Svelte web UI (a new
admin-only panel, gated by `isAdmin`) rather than a separate tool.

---

## Implementation phases

| Phase | Scope | What it unlocks |
|---|---|---|
| **1** | `JwtAuthFilter` (backend) + `keycloak-js` integration (frontend) + Keycloak client registration + `SessionGate` wiring | Every request has a verified identity; nothing is authorized yet, just authenticated |
| **2** | `ProjectAssignmentTable` + authorization checks on all project handlers + lock down "Open by path" for non-admins | Users can only reach their own assigned project(s) |
| **3** | Admin role + minimal admin UI (list/assign) | Assignment doesn't require shelling into the server |
| **4** | `PortPoolManager` + override plugin ports at runtime-start instead of trusting `project.xml` literally | More than one project can run concurrently without port collisions |
| **5** | Two-tier nginx: outer SCAAI vhost (one-time static setup) + inner nginx service in our compose stack doing dynamic path→port resolution | Public URLs are path-based; ports never appear in a browser URL; SCAAI's nginx never needs to change again |
| **6** | Load/soak test at ~20 concurrent runtime sessions | Confidence in the target scale before real rollout |

Each phase is independently testable against the current single-project
deployment before moving to the next — Phase 1 alone, for instance, can be
verified by confirming unauthenticated requests now 401.

---

## Remaining implementation details

Everything that was an open architectural question is now Decisions 13–16
(and the nginx data-source half of Decision 11). Two non-blocking specifics
are still just implementation-time choices, not design decisions — pick
during the phase that needs them:

1. **Exact port-pool size and per-slot port count.** Decision 9 settled the
   *shape* (one shared pool, headroom above ~20) but not the final number —
   tune this during Phase 4 (build) and Phase 6 (load test).
2. **Inner nginx's scripting engine for reading the registry file** — `njs`
   (stock nginx + official module, lighter-weight) vs OpenResty/Lua (a
   separate nginx distribution, richer ecosystem, the more battle-tested
   choice for this exact "dynamic upstream" pattern). Decision 11 settled
   *what* it reads (a shared file); this is just *which tool* reads it —
   pick during Phase 5.

---

## Out of scope (for now)

- Per-user resource/CPU/memory quotas — not mentioned as a requirement,
  revisit if 20 concurrent sessions turns out to strain the host.
- Self-registration for external users — explicitly rejected; Keycloak
  accounts are predefined.
- Rewriting `htmlgui-ws`/`charamel-embed` to be path-mounted instead of
  port-bound — explicitly rejected for this phase (Component 5/Decision 8).
- Real collaborative co-editing of one project by multiple users
  simultaneously — that's `collaborative-multisession-plan.md`'s problem,
  not this one. This plan is about isolating *separate* users' *separate*
  projects, not merging edits within one shared project.
