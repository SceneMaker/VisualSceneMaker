# VSM Runtime Server — Docker Deployment

> **Not the semantic parser.** For deploying `semantic-ud` (the UD parser behind Semantic Analysis)
> as a systemd service, see **[`services/semantic-ud/deploy/README.md`](../services/semantic-ud/deploy/README.md)**.
> That is a separate service with its own install steps and shares nothing with this package.

Deploys VisualSceneMaker's standalone `runtime-server` (headless, `RUNTIME_ONLY`
mode — this is not the full editor: project editing is disabled, only
runtime start/stop/monitoring) in a Docker container, pre-loaded with a single
project and auto-started on container boot.

This package currently targets the **"Confidence - SIA Layer"** project
(4 device plugins: `CharamelEmbedXenia`, `CharamelEmbedBob`, `webpage`
(htmlgui-ws), `Timer`). The image itself is generic — any VSM project can be
pointed at via `VSM_PROJECT_DIR` — but the plugin set baked into the image
(`runtime-server/build.gradle`) currently includes only the plugins this
project needs: `timer`, `charamel-embed`, `htmlgui-ws`. If you deploy a
different project that uses other plugins, add them to that dependency list
and rebuild.

## Architecture / what you're exposed to

`runtime-server` has **no built-in TLS**. It always serves plain HTTP/WS. VSM's
own `--secure` flag (mkcert-based) only exists in the desktop editor and
relies on a locally-trusted dev CA — not usable for a public server. **TLS
termination is expected to happen at your existing reverse proxy** (per your
call — this package does not bundle one).

Five independent HTTP/WebSocket origins run inside the container, each on its
own port. Each of `CharamelEmbedXenia`/`CharamelEmbedBob`/`webpage` has
**browser-side JS that derives its WebSocket URL from `location.host`** (not
hardcoded), so each one works correctly behind a reverse proxy **as long as
your proxy forwards each port under its own hostname (or the same hostname on
a distinct port) with WebSocket upgrade enabled and TLS terminated**. Do not
put more than one of these behind the same path on one hostname — they are
separate servers, not one app with sub-routes.

| Port | Plugin (project.xml) | Purpose | Needs WS upgrade in proxy |
|------|----------------------|---------|---------------------------|
| 8091 | *(runtime-server itself)* | Runtime control API + Web UI (monitoring/admin) | Yes (`/ws`) |
| 3040 | `CharamelEmbedXenia` | Avatar "Xenia" page + WebSocket | Yes |
| 3041 | `CharamelEmbedBob` | Avatar "Bob" page + WebSocket | Yes |
| 8080 | `webpage` (htmlgui-ws, `html_port`) | Participant-facing GUI page | No (plain HTTP page) |
| 4041 | `webpage` (htmlgui-ws, `ws_port`) | WebSocket channel for the GUI | Yes |

`project.xml` also configures a `wss_port` (4040) for `webpage`, but that
connector never binds — `runtime-server` has no TLS context to serve it with,
so it's not exposed here. If you later need htmlgui-ws to speak TLS directly
(bypassing a proxy), that requires provisioning a real certificate into the
plugin's `certificate` config — not the mkcert dev flow.

**Known unrelated bug, does not affect this project:** the *separate*
`charamel-ws` plugin's character-config URL synthesis
(`WebUiServer.synthesiseCharacterConfig`, `HtmlGuiWsExecutor` line ~500)
hardcodes `ws://localhost:<port>` and will break for remote users. This
project uses `charamel-embed`, not `charamel-ws`, so it isn't hit — just don't
reuse that code path for a future charamel-ws-based deployment without fixing
it first.

## One-time setup

1. Get the project files onto the host (not via this repo — `project.xml`
   contains a VuppetMaster license key, so it's handed over separately, e.g.
   scp/secure transfer). Place them anywhere on the host, e.g.
   `/srv/vsm-projects/confidence-sia-layer/`.
2. `cd deploy`
3. `cp .env.example .env` and fill in:
   - `VSM_TOKEN` — a fixed secret (e.g. `openssl rand -hex 24`). Anyone who
     can reach port 8091 with this token can control the runtime, so route it
     through your proxy's auth/access control if exposed beyond the proxy's
     trusted network.
   - `VSM_PROJECT_DIR` — absolute host path from step 1.
4. Configure your reverse proxy to forward the 5 ports above (see table) to
   this host, one hostname/subdomain (or distinct port) per row, all under
   HTTPS, all with WebSocket upgrade enabled except port 8080.

## Running

```bash
cd deploy
docker compose up -d --build
```

Restart on project changes (no rebuild needed — the project dir is bind-mounted):

```bash
docker compose restart
```

Rebuild after a code change to VSM itself:

```bash
docker compose up -d --build
```

## Verifying

```bash
docker compose logs -f
curl -s http://localhost:8091/api/v1/info      # server info + mode
curl -s -o /dev/null -w '%{http_code}\n' http://localhost:8080/   # GUI page (expect 200/302)
```

The log on startup shows each plugin's Jetty server binding
(`Started ServerConnector@...{0.0.0.0:<port>}`) — confirm all four expected
ports (3040, 3041, 4041, 8080) appear in addition to 8091.

## Notes

- The container writes back into the mounted project directory during
  runtime (e.g. `screens-assets/`, `semantic-annotations.json` caches), so
  the volume must stay read-write — do not mount it `:ro`.
- `restart: unless-stopped` means the container (and therefore the project,
  since `VSM_AUTOSTART=true`) comes back up automatically after a host
  reboot or crash.
