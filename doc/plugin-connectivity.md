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

The "native client" case above was hypothetical until 2026-09-08, when `asr` became its first
realization — see the worked example below. `studymaster`'s friendly `/studymaster/<slug>` URL
predates it and is browser-page-only (the page is served *by* studymaster itself); `asr`'s
`/asr/<slug>` is the same slug-redirect mechanism (now shared code, not copy-pasted) pointed at an
external native process instead.

## Direction B — the plugin dials *out* to the other end

Here the *server* initiates the connection, so the only question that matters is: **is the
target address routable from the server?** That splits three ways.

### B-direct — the target is directly routable from the server

A dedicated internal service (fast-asr on an internal host), or a device on a network the
server can reach. **The server just connects to it** — no tunnel, no bridge, nothing new.
Configure the plugin's target address and you're done. This is the internal-network case below.

### B-browser — the target is a browser-reachable capability on the user's machine

Microphone, camera, speakers, a WebSerial/WebUSB device. The server *can't* dial into the user's laptop, but there's already a persistent
client→server channel — the main `/ws`. So invert it: the plugin talks to the **browser page**,
and the page uses browser APIs to reach the local resource. Concretely for ASR-from-the-user's-
mic: the browser captures audio with `getUserMedia` and streams it over WS to the server-side
plugin, instead of the plugin pulling from a mic it can't see. **No new infrastructure — just a
plugin that sources/sinks through the browser.** Covers a lot (anything media- or WebAPI-shaped).

> **Correction (2026-08-07):** an earlier version of this section also listed "a `localhost` HTTP
> service the page can `fetch`". That does **not** generally work from the deployed editor. The page
> is served from a public HTTPS origin, so a request to `http://127.0.0.1:…` is subject to Chrome's
> **Private Network Access** rules: it requires a CORS preflight that the local service must answer
> with `Access-Control-Allow-Private-Network: true`. We control that for VSM-authored services and
> not for third-party engines, and browsers are tightening this over time rather than relaxing it.
> A page can reach browser *capabilities*; it cannot be relied on to reach local *servers*. For that
> see B-extension below.

### B-tunnel — the target is a native app / LAN device not routable from the server

A native app on the user's laptop at home (behind NAT), a robot on the user's home LAN, an SSI
pipeline. Browsers can't open raw sockets, and the server can't route to the machine. This
needs a **client-side bridge agent**: a small program on the user's machine that dials *out* to
the server over WSS (NAT-friendly, like the browser does), registers against the user's
session + `projectId`, and relays between the server plugin and the local service — a reverse
tunnel scoped to VSM sessions. This is the only genuinely new component in the whole space, and
worth building only when a concrete plugin needs it.

### B-extension — a browser extension as the connector

Between B-browser and B-tunnel, and the option most likely to be worth building first.

A browser extension holding `host_permissions` **is not subject to CORS or Private Network Access**,
so it can reach `http://127.0.0.1:*` and `ws://localhost:*` where the page cannot. It needs no
OS-level install, no Gatekeeper approval and no code signing: $5 once for the Chrome Web Store, free
for Firefox, and the user clicks "Add to Chrome".

It still cannot open raw TCP, so it covers the same plugin subset as the page — but that subset is
exactly the exploration set (see the inventory below). Combined with third-party engines that are
already signed and notarised (Ollama and similar), nothing VSM ships needs a certificate and the
user never sees a security warning.

## What the plugin inventory says about how much of this is worth building

Measured 2026-08-07 across the 24 plugins, because the value of B-tunnel depends entirely on how many
plugins can never use anything lighter:

| Transport | Plugins | Reachable via page/extension? |
|---|---|---|
| Raw TCP/UDP | charamel, odp, reeti, sockets, ssi, ssj, tricatworld, unity | **No** — browsers have no raw socket API. B-tunnel only. |
| HTTP/WS, dials **out** | asr\*, decad, heartflow, voicetts | **Yes** — the B-extension candidate set |
| HTTP/WS, **listens** | charamel-embed, charamel-ws, studymaster-web, yallah | Not needed — these are Direction A, already solved |

So B-extension serves 4 plugins and B-tunnel adds 8 more. If the goal is *exploring* what ASR, TTS
and LLMs can do, the four are the ones that matter and the extension is sufficient.

\* As of 2026-09-08, `asr` no longer fits this table cleanly: it kept its dial-out mode (B-direct,
for a server-reachable fast-asr instance) but also gained a **listen** mode — Direction A, not
B-extension — for exactly the case B-extension doesn't cover well: a whole native service
(fast-asr, with its own microphone capture, VAD, diarization) rather than a single browser-API
capability. See the worked example below.

## Design principle: ship symbols, not signals

Whatever the transport, the bridge should carry **results, not streams**. `heartflow` already does
this and is the model to copy: it receives fourteen derived scalars (`hf_bpm`, `hf_rmssd`,
`hf_breathing_phase`, …) over `ws://localhost:7878` and the PPG waveform never reaches VSM at all.
Likewise ASR should send the transcript, not the audio, and TTS should receive the text and
synthesise locally.

**But size is not the axis — synchrony is.** `heartflow` also carries `beat_offset_ms` and a
heartbeat-prediction message, i.e. behaviour aligned to the user's actual heartbeat. A 100-byte beat
event is *less* WAN-tolerant than a 2 KB LLM prompt:

| Class | Example | Survives a WAN round trip? |
|---|---|---|
| Turn-scale symbols | ASR transcript, LLM reply, TTS request | Yes — invisible at seconds-scale interaction |
| Event-synchronous symbols | heartbeat events, breathing phase for mirroring | **No** — tiny, but jitter destroys the alignment |
| Signals | audio, PPG | Never shipped regardless |

The worst case is a sensor on the user's body driving a character rendered in the user's own browser:
both endpoints local, the decision remote, jitter paid twice. If beat-aligned mirroring is a design
goal, it belongs in a locally-executed runtime, not behind any bridge.

## Distribution reality for B-tunnel (the deciding constraint)

The bridge agent is small — `ServerMode` is already `{RUNTIME_ONLY, FULL_EDITOR}` and `RuntimeMain`
is 166 lines, so a third `SATELLITE` mode is an addition to something already built and packaged for
Mac/Windows/Linux via jpackage, which bundles a JRE (so users never install Java). Tier 1 and Tier 2
would then be the same download with a switch, which is a much better story than two artifacts.

**What decides whether it is usable by non-programmers is code signing, not architecture.** No
signing is configured today. Unsigned, on macOS Sequoia the right-click → Open bypass is gone and the
user must visit System Settings → Privacy & Security → Open Anyway; Windows SmartScreen shows a
comparable warning. That is precisely the barrier the audience cannot cross, and no UI polish fixes
it. Apple is €99/year; Windows via Azure Trusted Signing is roughly €10/month. There is no academic
budget line for this at DFKI (2026-08-07), which is the main argument for B-extension.

Two things worth checking before designing around the absence: DFKI may already hold an Apple
Developer Organization account (adding an identity to an existing team is free), and an app copied
from a USB stick at a workshop carries no quarantine flag and opens normally — viable for an audience
onboarded in person.

## Audience and tiers

The target users are SIA researchers who are **not computer scientists**. That constrains the design
more than any technical factor: every install, dependency and security dialog is a real loss.

| Tier | User installs | Covers |
|---|---|---|
| **0 — Explore** | nothing | browser TTS (`speechSynthesis`), heart rate over Web Bluetooth, hosted LLM |
| **0.5 — Extension** | a browser extension | local HTTP/WS engines: the four plugins above |
| **1 — Satellite** | one signed app | everything incl. raw-TCP plugins; needs the signing decision |
| **2 — Full local** | editor or runtime-engine | everything, offline, studies. Exists today. |

**Tier 1 targets the author's machine during authoring and exploration — not the participant's
device during a study** (decided 2026-08-07). It therefore never has to satisfy a school's IT policy,
which removes the hardest constraint.

One caution on Tier 0 ASR: Chrome's `SpeechRecognition` **streams audio to Google's servers**. For a
population of 10-12 year olds discussing bullying in a therapeutic frame, treat that as
disqualifying before it is a technical question. The local alternative is Whisper in-browser via
transformers.js — no cloud, but a 40-200 MB first download.

## Verify before relying on this

Reasoned from the codebase and from general platform knowledge, not tested here:

- that an extension's `host_permissions` bypasses PNA for both `fetch` and WebSocket under
  Manifest V3, and that an MV3 service worker can hold a long-lived WS — worth a one-day spike
  before committing
- the current macOS Sequoia Gatekeeper behaviour and Azure Trusted Signing pricing
- whether `heartflow`'s beat prediction is actually used for synchronous behaviour, or only for
  slower arousal-level state — this decides whether the synchronous tier needs to exist

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

## Worked example: internal `fast-tts` → a specific browser's VuppetMaster

A concrete case that combines the pieces: replace VuppetMaster's built-in Azure TTS with an
internal `fast-tts` server (`audio-processing/fast-tts`, a Qwen3-TTS/MLX service), so the avatar
speaks with in-house TTS. This is subtle because the audio must end up **inside a specific
user's browser** (where VuppetMaster runs), not on the server.

**Why it's not just "point the plugin at fast-tts".** VuppetMaster does the TTS *itself* today,
inside the browser engine (VSM only sends `vm.speak(text)`; the engine calls Azure and gets
audio + viseme timing back). So swapping the TTS source means feeding the engine externally
produced audio — which only works because the VuppetMaster maintainer **confirmed the engine can
be configured to accept streamed audio + streamed visemes in Azure's format** (capability (b) in
this doc's terms).

**Topology (server-brokered, Topology 2 of §"Direction B").** fast-tts stays internal; the VSM
server brokers:

1. SceneFlow triggers speech → VSM's (new) tts plugin, server-side.
2. Server opens a WS to fast-tts (internal address — **B-direct**, the easy reachability case).
3. fast-tts streams back **audio + visemes + word timings** (see below).
4. Server **translates to VuppetMaster's expected format** and pushes it to the *specific* user's
   browser over the existing `/plugin/{projectId}/...` channel (routing to the right browser is
   free — the server already knows which session/`projectId` owns it).
5. The browser engine plays the injected audio and lip-syncs from the injected visemes.
6. `${...}$` markers fire from fast-tts's word timings (see the marker note below).

Location-independent (home users work identically), and fast-tts is never exposed publicly.

**What fast-tts already provides** (documented WS protocol,
`audio-processing/fast-tts/docs/websocket-interface.md`):
- `audio.chunk` — PCM s16le @ 24 kHz, with a **sample-clock** (`start_sample`/`sample_rate`) that
  is the sync source of truth.
- `viseme.frame` — with a `weights` dict already doing blend-shape morph blending.
- `word.final`/`word.provisional` — CTC forced-aligned word timings.

So the three needed streams exist; fast-tts was built for character animation.

**The one real gap — viseme *format*.** fast-tts emits **Preston Blair 9-class** visemes
(`rest, MBP, FV, L, AI, E, O, U, WQ`); VuppetMaster wants **Azure's** taxonomy (22 viseme IDs
0–21, optionally blend-shape animation frames). A translation layer is required. The
information-preserving route: fast-tts already computes CTC **phoneme/character spans** in
`full_utterance` mode, and Azure viseme IDs derive from a published phoneme→viseme table — so
fast-tts (or the VSM broker) can produce Azure-ID visemes *from the phoneme alignment directly*,
rather than squashing through the 9-class set. Smaller items: resample/reencode 24 kHz PCM to
whatever the injection API wants; and prefer `full_utterance` mode for quality visemes
(`streaming` mode falls back to a crude 4-class RMS heuristic — a latency-vs-lip-sync tradeoff).

**Marker timing survives.** VSM relies on marker timing to fire co-located SceneFlow actions
mid-utterance; today that comes from the TTS path. fast-tts's `word.final` timings are the
replacement source — the raw timing exists, it just needs mapping to VSM's `${...}$` markers.

**Still open (one vendor sub-question).** Which Azure flavor does the injection want — **viseme
IDs** (easy: phoneme table → IDs) or **blend-shape animation JSON** (harder: ~55 per-frame morph
channels — though fast-tts's `weights` dict and portrait-blendshape tooling give a starting
point)? That single answer sets the adapter's difficulty. Everything else is settled.

## Worked example: live wizard video (participant webcam → studymaster)

A second worked example, because it clarifies a distinction that's easy to get wrong. Adding live
video from a participant's `htmlgui-ws` screen to the `studymaster` wizard console (for real-time
Wizard-of-Oz observation and control) has to work in two topologies: same-LAN (low latency is the
priority) and DFKI-hosted (the wizard remote, VSM behind a single fixed ingress port — `8040`,
proxied through `inner-nginx` — with no arbitrary UDP/firewall passthrough available). The natural
question this raises: *is VSM basically running a STUN server here?* No — and the "why not" is
worth spelling out, because WebRTC deliberately splits this into four roles that are easy to
conflate into one.

| Role | Who runs it | Protocol | Crosses the VSM/DFKI boundary? | Carries |
|---|---|---|---|---|
| **Signaling** | `studymaster` (a new producer role alongside its existing wizard connection) | JSON over the existing WS | **Yes** — the one thing that does | SDP offer/answer, ICE candidates |
| **STUN** | A public service (e.g. Google's) | STUN, raw UDP | No | "What's my public address?" — nothing else |
| **TURN** | An external relay, hosted independently of DFKI | TURN, UDP/TCP/TLS | No | The media, only if a direct path fails |
| **Media path** | The two browsers, natively (`RTCPeerConnection`) | ICE + DTLS/SRTP | No | The live, encrypted video and audio |

**Signaling is the only genuinely new thing to build**, because WebRTC deliberately leaves it
unstandardized — every WebRTC app writes its own. `studymaster`'s WS server (today a single "one
wizard" connection) gains a second connection role for the participant's browser, and simply
relays `webrtc.offer` / `webrtc.answer` / `webrtc.ice-candidate` messages between whichever
producer and wizard are currently connected. In this doc's terms, that's **Direction A**
(studymaster already listens; the browser dials in) combined with **B-browser** on the capture
side (`getUserMedia` reached via the page, no new infrastructure).

**STUN and TURN are reused, not built.** VSM never implements either protocol — it configures
`ice_servers` (a plain list of URLs) that both browsers' native `RTCPeerConnection` uses directly.
STUN is free and generic (point at any public one); TURN needs to be hosted somewhere, but
deliberately **not** on DFKI's network — DFKI's single fixed ingress port has no path for
arbitrary inbound UDP, and getting one opened is a real infrastructure ask. Hosting the TURN relay
independently sidesteps that entirely. See `doc/turn-relay-setup.md` for the self-hosted coturn
setup actually used for this.

**The payoff: media never touches the DFKI server, in either topology.** Once signaling has
exchanged SDP/ICE over studymaster's WS, the two browsers' own WebRTC stacks negotiate a route
(direct, or via the external TURN relay) and the actual video/audio flows outside VSM's
infrastructure altogether. One design covers both the LAN case (no STUN/TURN needed at all —
direct host candidates connect) and the DFKI-hosted case (STUN + an external TURN relay
configured in `ice_servers`), without VSM's own server ever sitting in the media path or needing
any firewall change to carry it.

## Worked example: fast-asr on a researcher's own laptop → a hosted VSM project (reverse-dial)

A third worked example, because it's the first plugin to actually use Direction A's "native
client" case (studymaster's is a browser page it serves itself; this is an independent process).

**The problem B-direct doesn't solve.** `asr` dials out to fast-asr's `ws_url` — fine when
fast-asr runs on a server-reachable internal host (the internal-network case above), but fast-asr
increasingly runs on a researcher's own Mac (`packaging/macos/FastASRLauncher.swift` in
`audio-processing/fast-asr`), which `vsm.scaai.dfki.de` can never dial into: it's on an arbitrary
network, often behind NAT, with no stable address the server could be configured with. That's
B-tunnel's problem shape — except fast-asr already *is* a substantial native app with its own
process lifecycle UI, so building a separate generic bridge agent (Tier 1 "Satellite" above) would
duplicate machinery fast-asr already has.

**The fix: give `asr` a second, opposite-direction mode instead of building a bridge.** `mode=dial`
(the default) is untouched. `mode=listen` makes the plugin embed its own Jetty/Javalin server —
same shape as `charamel-embed`'s `JettyTransport` — and wait for fast-asr to dial *it*, exactly
mirroring `studymaster`'s wizard socket:

- **Auth**: a static shared secret (`asr_key`), sent as the first WebSocket message and checked
  by string-equals — the same contract as studymaster's `wizard_key`, deliberately: this is the
  safe direction (client dials to server), so the "trust matters most" caveat below doesn't apply
  the way it does for B-tunnel.
- **Friendly URL**: `https://vsm.scaai.dfki.de/asr/<asr-slug>`, resolved by the *same* redirect
  handler studymaster's `/studymaster/<slug>` already used — refactored from a studymaster-specific
  method into one parameterized by which config property holds the slug, so a third plugin wanting
  this needs no new server route at all.
- **Single connection**: a fresh authenticated connection replaces the old one, same as
  studymaster's wizard socket — there is exactly one fast-asr instance per project.
- **Wire protocol unchanged**: once authenticated, inbound frames go through the *same* message
  handler the dial-out `AsrWsListener` already used (`ready`/`sad`/`vad`/`asr`/`turn_end`/
  `diarization`/`recording`) — only the transport differs, not the protocol.

**fast-asr's side is the more interesting half**, because its WebSocket handling
(`@app.websocket("/stream")`) is 100% accept-role — it has never dialed *out* before. A new
`RemoteBridge` does this explicitly, in two steps that mirror what a browser does implicitly when
it follows a link: (1) a plain HTTP GET to `/asr/<slug>`, following the redirect to the resolved
`.../ws` endpoint (`urllib.request.urlopen` auto-follows and exposes the final URL via
`geturl()`); (2) a `websockets.connect()` to that resolved address, sending the auth message
first. Once authenticated, the bridge registers itself into the *same* `_clients` set the local
`/stream` connections use — `broadcast()` doesn't know or care that one of its recipients is a
reverse-dialed connection to a server on the other side of a NAT rather than a socket accepted a
moment ago. `FastASRLauncher.swift` gets one new UI row (a link field, a key field, a
Connect/Disconnect toggle) that talks only to fast-asr's own new `/remote/connect`,
`/remote/disconnect`, `/remote/status` endpoints — the launcher itself still never opens a
WebSocket, exactly as before.

**Why this, and not B-extension.** fast-asr isn't a single browser-API capability (a mic, a TTS
engine) — it's a whole service with its own VAD/diarization/turn-end pipeline and a packaged
native app. B-extension's "reach `localhost` from a page" doesn't fit a process that's meant to
run independently of any browser tab being open.

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
   - A browser capability on the user's machine — mic, camera, speakers, Web Bluetooth →
     **B-browser** (mediate through `/ws`; no new infra).
   - An HTTP/WS service on the user's `localhost` → **B-extension**. Not B-browser: a public
     HTTPS page cannot be relied on to reach a local server (Private Network Access).
   - A native app / LAN device speaking raw TCP → **B-tunnel** (needs the client bridge agent —
     the one real build, and the one gated on code signing).
3. Then ask what the bridge should *carry*: results, not streams. And check whether the data is
   event-synchronous — if behaviour must align to it in time, no bridge is fast enough and it
   belongs in a local runtime.
4. If *most* plugins are client-local, step back and consider **runtime-on-client** instead.
