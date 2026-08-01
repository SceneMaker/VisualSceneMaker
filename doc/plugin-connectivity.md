# Plugin connectivity: where "the other end" lives, and whether the server can reach it

When VSM runs on a laptop, every plugin's counterpart — the avatar page, the GUI, the ASR
service, the robot, the Unity app — is on that same laptop or its LAN, so "localhost" and
"the local network" just work. When VSM runs as a **server** (the SCAAI deployment) with users
in browsers, that stops being true: "localhost" now means the *server container*, and a
plugin's counterpart might be on a dedicated internal host, on the user's laptop at the office,
or on the user's laptop at home. Whether that works — and how much new machinery it needs —
depends entirely on **which direction the plugin connects** and **where the other end is**.

This doc is the map. It also answers the concrete question that prompted it: *can the deployed
server reach an ASR service (fast-asr) exposed only on an internal org address?* (Yes — see
§"The internal-network case".)

## Two connection directions (the first thing to determine)

VSM plugins fall into two groups by how they reach their counterpart:

- **A — the plugin *listens* (hosts a server); the peer dials in.** `htmlgui-ws`,
  `charamel-embed`, `unity` (`accept(Socket)`), `reeti`'s local side (`lhost`/`lport`). The
  browser reaching the GUI/avatar through inner-nginx today is exactly this.
- **B — the plugin *dials out* to a `host:port`.** `asr` opens a WebSocket to the fast-asr
  service; `reeti` dials the robot at `rhost`/`rport`; `unity` can connect to a Unity port.

Same plugin can do both (reeti listens locally *and* dials the robot). But for "can the other
end run elsewhere," these two directions have completely different answers.

## Direction A — the counterpart dials *in* (already solved)

If the plugin listens and its counterpart is something you can point at a URL (a Unity app, a
browser page, a native client), then **the inner-nginx `/plugin/{projectId}/{pluginInstance}/
{portKey}/…` routing already does this.** The client-side app connects to
`wss://vsm.scaai.dfki.dev/plugin/…/port/…` instead of `localhost:<port>` — the same path the
browser GUI uses. The only work is telling the client app the public URL (and the `_pathPrefix`
plugin-config machinery already advertises it). So: **if the other end can be configured with a
URL, this is mostly a configuration exercise, not new infrastructure.**

## Direction B — the plugin dials *out* to the other end

Here the *server* initiates the connection, so the only question that matters is: **is the
target address routable from the server?** That splits three ways.

### B-direct — the target is directly routable from the server

A dedicated internal service (fast-asr on an internal host), or a device on a network the
server can reach. **The server just connects to it** — no tunnel, no bridge, nothing new.
Configure the plugin's target address and you're done. This is the internal-network case below.

### B-browser — the target is a browser-reachable capability on the user's machine

Microphone, camera, speakers, a WebSerial/WebUSB device, a `localhost` HTTP service the page
can `fetch`. The server *can't* dial into the user's laptop, but there's already a persistent
client→server channel — the main `/ws`. So invert it: the plugin talks to the **browser page**,
and the page uses browser APIs to reach the local resource. Concretely for ASR-from-the-user's-
mic: the browser captures audio with `getUserMedia` and streams it over WS to the server-side
plugin, instead of the plugin pulling from a mic it can't see. **No new infrastructure — just a
plugin that sources/sinks through the browser.** Covers a lot (anything media- or WebAPI-shaped).

### B-tunnel — the target is a native app / LAN device not routable from the server

A native app on the user's laptop at home (behind NAT), a robot on the user's home LAN, an SSI
pipeline. Browsers can't open raw sockets, and the server can't route to the machine. This
needs a **client-side bridge agent**: a small program on the user's machine that dials *out* to
the server over WSS (NAT-friendly, like the browser does), registers against the user's
session + `projectId`, and relays between the server plugin and the local service — a reverse
tunnel scoped to VSM sessions. This is the only genuinely new component in the whole space, and
worth building only when a concrete plugin needs it.

## The internal-network case (the prompting question)

> Sitting in the org where the vsm-server is hosted, can the server connect to a plugin service
> (e.g. fast-asr) exposed only on an internal org address?

**Yes.** This is **B-direct**, the easy case. The vsm-server is itself inside the org network;
its container's outbound connections egress through the host (Podman NAT/masquerade), so the
container can reach anything the *host* (`exs-91204`) can reach — including internal-only
addresses. The ASR plugin dials out, so you point its target URL at fast-asr's internal address
and it connects.

Note this is a **server→service** connection: it does **not** depend on where the *user's*
browser is. A user browsing from home still gets ASR, as long as the server can reach fast-asr.
(The *only* time "am I in the office?" matters is if fast-asr runs on the **user's own laptop** —
then it's routable from the server only while that laptop is on the org LAN, which is really the
B-tunnel problem in disguise; see the caveats.)

### Gotchas when configuring it

1. **`localhost` in plugin config now means the container, not the host.** If fast-asr runs on
   a *different* internal host, use that host's internal hostname/IP. If fast-asr runs on
   `exs-91204` *itself*, use **`host.containers.internal`** (Podman injects it into the
   container's `/etc/hosts`) or the host's LAN IP — never `localhost`.
2. **DNS for internal hostnames.** If you use a name like `asr.internal.dfki.de`, the container
   must resolve it. Rootless Podman's `aardvark-dns` forwards non-container names to the host's
   upstream resolvers, so it usually works — but if it doesn't, use the IP or add a hosts entry.
3. **Firewalls.** The ASR host must accept inbound from `exs-91204`, and nothing between them
   may block the port. (A *timeout* rather than "connection refused" is the fingerprint of a
   firewall in the way — same signature as the deployment's earlier outer-nginx 502.)
4. **Per-user config if it's a laptop.** If the "internal service" is actually the user's own
   laptop on the office LAN, its IP is per-user and often DHCP-dynamic, and its local firewall
   must allow the server in — so the shared project config can't hardcode one address. This is
   why laptop-hosted services are better treated as B-browser or B-tunnel than as B-direct.

### How to verify (on the server)

The `vsm-server` image ships `curl`, so test reachability from *inside the container* (which is
what actually matters — not just from the host):

```bash
# does the container resolve + reach the service? (adjust host/port/scheme)
podman exec vsm-server curl -sv --max-time 5 http://asr.internal.example:PORT/health
# if fast-asr runs on exs-91204 itself:
podman exec vsm-server curl -sv --max-time 5 http://host.containers.internal:PORT/health
```

If that connects, the plugin will too — set the plugin's target to the same address.

## The other architectural pole: run the runtime on the client

Tunnelling individual plugins is the *middle* of a spectrum. The other end already exists:
Phase 7/8 built a standalone `runtime-server` and a "connect to a remote runtime" Web UI
(`isRemoteConnection` / `remoteServerUrl`). If a project is *mostly* about client-local devices,
the cleaner answer is to **run the runtime on the user's laptop** and have the browser editor
connect to it remotely — then *all* plugins are naturally local, zero tunnelling. So the real
decision is:

- **Runtime on server + reach out to the few external/local plugins** — when 1–2 plugins need a
  non-server counterpart, and especially when those are B-direct (internal services).
- **Runtime on the client + edit remotely** — when *most* of a project's plugins are
  client-local (a user's own robot, sensors, local apps).

## How this fits the existing code

- **Transport abstraction is the seam.** `charamel-embed` already has `CharamelTransport` with
  `JettyTransport` (desktop) and `AndroidBridgeTransport` (WebView bridge). A "browser-mediated"
  or "client-bridge" transport is just another implementation — plugin logic unchanged, only
  *how it reaches its peer.*
- **Session identity is the routing key.** For anything client-side (B-browser, B-tunnel), the
  browser/bridge must be tied to the right user's project instance (Keycloak user + `projectId`)
  so a plugin in braun's project reaches *braun's* machine, never volkert's.
- **Trust matters most here.** The server initiating connections toward client machines (or an
  unauthenticated tunnel) is sensitive: scope strictly to the user's own session, require
  explicit opt-in from the browser/bridge, have the plugin config declare intent, and
  authenticate the bridge. The "server token isn't enforced" caveat bites hardest on a tunnel.

## Decision guide

1. Which **direction** does the plugin use — does the counterpart dial *in* (A: use nginx
   routing) or does the plugin dial *out* (B)?
2. For B, **where is the target and is it routable from the server?**
   - Internal/dedicated, server-reachable → **B-direct** (just configure the address; the
     internal-network case above).
   - A browser capability on the user's machine → **B-browser** (mediate through `/ws`; no new
     infra).
   - A native app / LAN device not routable from the server → **B-tunnel** (needs the client
     bridge agent — the one real build).
3. If *most* plugins are client-local, step back and consider **runtime-on-client** instead.
