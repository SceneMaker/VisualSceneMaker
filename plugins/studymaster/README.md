# studymaster

Wizard-of-Oz remote control. A single authenticated wizard connects over WebSocket to a running
SceneFlow — observing every concurrently active flow (nodes visited, scenes played, turns spoken)
and setting author-wired variables, either spontaneously (`set`) or blockingly when the flow calls
`request(var, prompt)`. See `StudyMasterExecutor.java`'s class javadoc for the full runtime model.

Config lives in `plugin-properties.json`: `ws_port`/`wizard_key` (required), `wizard_slug`/
`controls_file` (optional). `wizard-controls.json` (project-relative, path set by `controls_file`)
declares which SceneFlow variables the wizard may see and set.

**`connected_var`** (optional, default `studymaster_connected`) names a Bool SceneFlow variable
this plugin flips to `true` while a wizard is connected and back to `false` the moment it
disconnects (default `false`, same pattern as htmlgui-ws's `sceneflowStateVar`/`gui_connected` —
must be declared as a `Bool` variable in the SceneFlow).

If this project has **no** `producer_key` configured (no live video feature at all), that's the
whole story — it flips true right on wizard auth. If `producer_key` **is** configured, it means
something stricter: not just "a wizard is connected," but "the wizard's video pipeline actually has
a live track it could record" — it waits for the wizard's own `RTCPeerConnection` to report a real
track (`ontrack`, not just a signaling handshake) before going true, and flips back to false the
moment that connection is lost (producer disconnects, ICE fails, etc.). This exists specifically so
a `start_recording()` sent right after `studymaster_connected` becomes true doesn't silently do
nothing — video negotiation with a producer takes real time after the wizard itself is connected,
and `MediaRecorder` needs an actual track to attach to. One consequence worth knowing: with video
configured, `studymaster_connected` can go false and true again mid-session (producer navigates
away from the webcam screen and back, a network hiccup) even though the wizard console itself never
disconnected — if that volatility is a problem for other uses of the variable, worth a follow-up.

## Live wizard video

For real-time human-SIA interaction studies, a participant-facing `htmlgui-ws` screen can add a
`vsm-webcam` element (see `screen-element-reference.html`) that streams the participant's camera
straight to this plugin's wizard console — live, low-latency, and **direct peer-to-peer WebRTC**
between the two browsers. studymaster's WebSocket server plays only the **signaling** role: it
relays a handful of small SDP/ICE JSON messages between whichever `vsm-webcam` element ("the
producer") and wizard are currently connected. It never touches the video/audio itself.

That design is deliberate, not incidental — see `doc/plugin-connectivity.md`'s "Worked example:
live wizard video (participant webcam → studymaster)" section for the full reasoning. Short
version: this plugin is often deployed on a shared host (e.g. DFKI's SCAAI server) behind a single
fixed ingress port with no path for arbitrary inbound UDP. Routing media through that host would
need a firewall change that may not be grantable. Direct P2P sidesteps the problem entirely —
studymaster's server never needs to carry media, so nothing about its hosting has to change to
support this feature. NAT traversal (STUN/TURN) is configured via `ice_servers`, both hosted
externally, never on the same network as this plugin.

**New config** (both optional — omitting either simply disables live video for the project):

- `producer_key` — shared secret a `vsm-webcam` element submits to register as the video
  producer. Unrelated to `wizard_key`: a producer never sees SceneFlow state or wizard controls,
  only exchanges WebRTC signaling.
- `ice_servers` — JSON array of ICE server entries (STUN and/or TURN), e.g.
  `[{"urls":"stun:stun.l.google.com:19302"}]`. Empty (the default) is correct for same-LAN
  deployments, where direct host candidates connect without any STUN/TURN at all. A remote/hosted
  deployment needs at least a STUN entry and, for restrictive networks, a TURN relay — hosted
  independently of wherever this plugin runs. See `doc/turn-relay-setup.md` for standing up a
  self-hosted coturn relay and the exact `ice_servers` value it produces.

**Wire protocol additions** (alongside the existing `auth`/`set`/`reply`/`request` messages):

- `producerHello` (producer → server, `{key}`) registers the video producer, mirroring `auth`'s
  "reconnect replaces" behavior. Replies `producerHelloResult` (`{ok, iceServers}` or
  `{ok:false, reason}`).
- `webrtc.offer` / `webrtc.answer` / `webrtc.ice-candidate` — relayed verbatim between whichever
  producer and wizard are connected; studymaster never parses SDP/ICE content.
- `producerJoined` / `producerLeft` (→ wizard) and `wizardReady` / `wizardLeft` (→ producer) —
  presence notifications so either side can (re)negotiate regardless of connection order.

**Recording** is deliberately not implemented server-side yet. The wizard console can record the
stream it receives locally (`MediaRecorder`, a "Record" button in the console) for early piloting.
That has real caveats — recording quality tracks live-view network conditions rather than being
independently engineered, and the file lands on the wizard's own machine rather than central
storage — worth revisiting with a proper server-side pipeline before this becomes real study data
collection, not before.

Recording can also be driven from SceneFlow with `start_recording()` / `stop_recording()`
(no params) — e.g. `PlayAction("[sm start_recording]")`. Both just send the wizard console the
same signal its own Record/Stop & Save button does; the recording state lives entirely in that
browser, so these are fire-and-forget (a no-op if no wizard is connected, or nothing is playing).

## Screen Editor

`vsm-webcam` shows up under Media in the visual screen editor's element picker, alongside
Image/Video/Audio/Embed. Adding it to a screen is what "enables" the feed for that screen (removing
it disables it, same as every other optional element) — its only property is an **Audio** checkbox
(default on) that controls whether the microphone is captured alongside the video.

Note that the element renders **no local preview** on the participant's own page by design — it's
a source toward the wizard, not a self-view. It only shows a small placeholder message on that page
if there's no `studymaster` device in the project, or if the camera couldn't be opened, so a screen
author can tell those apart from "it's just working invisibly." Worth thinking through separately:
whether a participant should see *some* visible indicator that their camera is live, for consent/
transparency reasons — not implemented here, flagging it as a real design question rather than
deciding it silently.
