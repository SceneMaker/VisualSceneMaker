# VSM Multi-User Deployment — Next Steps

Written 2026-07-29, after the first real end-to-end deployment session on
`exs-91204.sb.dfki.de` (`vsm.scaai.dfki.dev`). Phases 1-6 of
`doc/vsm-workspace-platform-plan.md` are implemented, committed, and — as of
today — confirmed working against the real SCAAI Keycloak, the real
two-tier nginx, and Podman (not Docker) on the actual deploy host. This
document is the forward-looking punch list; see git history (both this repo
and `vsm-server-git`) for the full account of what was found and fixed to
get here.

## Confirmed working today

- Real Keycloak login (`vsm-frontend` client, PKCE S256) end-to-end through
  the outer nginx.
- Admin bootstrap (`project-assignments.json`) and **Open by path**.
- Recovering from an expired Keycloak access token during an idle session —
  previously got permanently stuck retrying the pre-OIDC `fetchLocalToken()`
  fallback forever ("Missing or invalid token", kicked to the landing page
  after a few idle minutes); `autoConnect()` now refreshes via
  `keycloak.updateToken()` instead when OIDC is on.
- `PortPoolManager` dynamically allocating ports on the real deployment.
- The "follow the player" GUI popup and the SIA preview panel both correctly
  routing through `inner-nginx`'s `/plugin/{projectId}/{pluginInstanceName}/
  {portKey}/...` scheme (Option C), including a real WebSocket handshake.
- **The SIA preview panel rendering real, live VuppetMaster avatars** — both
  `CharamelEmbedXenia` and `CharamelEmbedBob` side by side, full engine load,
  emotion controls working — through the complete stack (Keycloak auth,
  dynamic port allocation, nginx path routing, Option C's WS-prefix
  routing). This was the most integration-heavy piece built across this
  whole effort and it works.
- `inner-nginx`'s dynamic-route resolver working under Podman (not just
  Docker) via envsubst templating instead of a hardcoded DNS address.

## 1. Hardcoded character URL under dynamic port pooling — FIXED 2026-07-30

**Confirmed on the real deployment 2026-07-30**: Xenia loads in the Run
popup through `/plugin/<pid>/CharamelEmbedXenia/port/character.html`.
Getting there surfaced two additional root causes stacked on top of the
missing rewrite mechanism, both fixed the same day:

- **Saving a launched project corrupted project.xml**:
  `RunTimeProject.write()` serializes the live in-memory `ProjectConfig` —
  the object `PortPoolManager` mutates — so a save baked pool ports and
  `_pathPrefix` into the file, destroying the authored ports the rewrite
  map is derived from. Fixed via
  `PortPoolManager.withOriginalConfig(...)` around both save handlers
  (authored state restored for the write, live allocation re-applied
  after). The deployment's already-corrupted file needed a one-time manual
  repair (authored ports restored, synthetic properties removed).
- **Browser-cached plugin JS pinned old behavior**: the Run popup is a
  reused named window that gets navigated, not reloaded, and the plugins
  served their infrastructure files with no cache headers — a returning
  browser kept executing the previous deployment's JS indefinitely. All
  infrastructure files (both plugins) now answer `Cache-Control: no-cache`.

**Resolution**: the "promising direction" below was implemented essentially
as sketched. `PortPoolManager` now records, at the only moment the
correlation exists (right before overwriting each port property), a map of
original literal port → live nginx path prefix including the port key
(`_portRewrites`, e.g. `{"3040": "/plugin/<pid>/CharamelEmbedXenia/port/"}`),
set on every config that got a port (one plugin's page embeds iframes
pointing at *other* plugins' ports). `HtmlGuiWsExecutor` injects it as
`window.VSM_GUI_CONFIG.portRewrites` (and `screens.html` now loads
`vsm-gui-config.js`, which the renderer iframe previously never saw);
`vsm-renderer.js`'s `_resolveHost()` rewrites any URL whose port matches —
but only when the URL points at localhost or the page's own hostname, so a
coincidentally-matching port on a genuinely external host is left alone.
Applies generally (any authored URL: `character` key, `srcVar` values,
iframe elements), not just the character case. Verified end-to-end against
a real server + real inner-nginx container: the exact broken URL from
2026-07-29 now rewrites to a path that serves charamel-embed's real
character page. Original analysis kept below for reference.

### Original analysis (2026-07-29)

**Symptom**: on the "Confidence - SIA Layer" project, the schema-driven
`character` iframe (rendered by `vsm-renderer.js`, from `screens.json`'s
`character` key / `character.srcVar`) loaded at
`https://vsm.scaai.dfki.dev:3040/character.html?appName=Xenia` — `3040` is
the **literal, original `project.xml` port** for `CharamelEmbedXenia`, not
the port `PortPoolManager` actually allocated for this run (something in the
20000s, like everything else). The outer nginx only routes port 8040, so
this always fails to load, regardless of anything nginx-side.

**Root cause**: this URL is a **static value authored into the project's own
content** (`screens.json`, presumably), fixed at authoring time when ports
were still literal. `PortPoolManager` overwrites the live `PluginConfig`'s
port property, but has no way to reach into project content and update a
hardcoded string — the "3040 means Xenia" correlation only ever existed in
the original `project.xml`, and nothing at runtime can reconstruct it from
the URL alone. `_resolveHost()` (`vsm-renderer.js`) only rewrites the
*hostname* of `localhost`-style URLs for remote viewers; it was never meant
to (and can't, without more information) also fix the port.

**This is a structural gap, not a bug in Option C** — it's the same class of
thing Component 5 already flagged as out of scope ("Rewriting htmlgui-ws/
charamel-embed to be path-mounted instead of port-bound — explicitly
rejected for this phase"), just discovered via a concrete, real failure
rather than anticipated in the abstract.

**Promising direction, not yet designed or validated**: have
`PortPoolManager` additionally record a reverse mapping (original literal
port from `project.xml` → the plugin instance name, and thus its current
port/`_pathPrefix`) at allocation time. `_resolveHost()` (or a new,
similarly-scoped helper) could then look up *any* hardcoded literal port it
encounters against this mapping and rewrite it to the correct live
`/plugin/.../` URL — fixing every project with this pattern generically,
without requiring project content itself to change. Needs:
- Confirming this is actually how `screens.json`'s `character` key gets its
  value in this project (private project content — not in this repo, never
  seen by the assistant that did today's work).
- Deciding whether the lookup lives server-side (injected into
  `window.VSM_GUI_CONFIG` alongside `pathPrefix`) or is exposed via a new
  small REST endpoint the renderer calls once at load.
- Whether this should apply narrowly (only `character`-key URLs) or more
  generally (any project-authored URL pointing at a plugin's own port).

## 2. Cleanup — real but non-blocking, found during today's debugging

- **Legacy `fetchLocalToken()` path fires unconditionally on every page
  load** (`autoConnect()` in `App.svelte`), even when OIDC is enabled — it's
  a pre-Phase-1 fallback that now always 401s (`/api/v1/token`,
  `/api/v1/projects/{id}/ui-prefs` before Keycloak's own token is ready,
  `/api/v1/embeddings/health`). Harmless today (tolerated by design,
  real auth goes through `ensureKeycloakAuth()` separately) but noisy in
  the console/server logs. Should skip this path entirely once
  `info.oidcEnabled` is true.
- **`checkAndShowPreflight()`'s raw `fetch()` call doesn't attach the Bearer
  token** (`/api/v1/projects/{id}/preflight`, seen 401ing in logs) — should
  use the app's authenticated fetch wrapper like everything else. Currently
  harmless (the function treats any non-ok response as "proceed", so it
  fails open) but worth fixing for consistency.
- **Recent-projects/`Preferences` state isn't on any mounted volume** — lost
  on every container restart/rebuild, so **Open by path** needs repeating
  each time (already documented in `vsm-server-git`'s README). Consider
  mounting a volume for wherever VSM's `Preferences` store writes, if this
  becomes a recurring annoyance.
- **One occurrence, not reproduced since**: pressing Stop once kicked the
  session back to the landing page unexpectedly (recovered via a hard
  reload). Didn't recur in later testing. If it happens again, capture the
  browser console log from that exact moment before doing anything else —
  we don't have enough to diagnose it from a single non-reproduced
  occurrence.
- **The editor page's own "close project" button doesn't actually close
  anything server-side.** It sends the WS `Project.Close` command, which
  turns out to be a no-op stub (`registerWsCommands(... => {"status":"ok"},
  "Project.Save", "Project.SaveAs", "Project.Close")`) — no real unload,
  no port release. The *actual* close only happens via the Projects panel's
  REST call (confirmed 2026-07-29 alongside the `HtmlGuiWsExecutor.unload()`
  NPE fix — see that commit). Worth deciding: should the editor's own close
  button be wired to the real close path, or is the current split
  (editor-page close = navigate away only, Projects-panel close = the real
  thing) intentional? Either way, a button that silently does nothing while
  implying success is worth resolving rather than leaving as-is.

## 3. Known, deliberately deferred (from earlier in this effort)

- **charamel-embed's real VuppetMaster engine loading under the nginx
  prefix** — verified at the transport level (a real WebSocket handshake
  through `/plugin/.../port/ws` succeeds), but never tested with a real
  license session actually rendering an avatar through nginx end-to-end
  (deliberately avoided to not risk the single-session-per-license
  constraint during iteration). Worth a real check once convenient.
- **Refreshing `vsm-server-git`'s `vsm/` snapshot** is a solved, repeatable
  process now (`./gradlew syncOpsRepoVsmSnapshot`, run from this repo,
  reviewed and committed in `vsm-server-git` by hand) — just remember to run
  it whenever a change here needs to reach the deployment, as we did several
  times today.

## Suggested order

1. Decide whether the hardcoded character URL (section 1 above) is worth
   fixing now or acceptable to leave for a while — it only affects the
   schema-driven character iframe specifically, not the GUI, not the SIA
   preview panel (confirmed working), not the SceneFlow runtime itself.
2. Batch the cleanup items (section 2) together sometime — none are urgent,
   all are quick once picked up.
