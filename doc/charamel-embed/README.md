# charamel-embed — VuppetMaster JS-API probe & notes

This folder documents how the **`charamel-embed`** plugin drives a Charamel **VuppetMaster**
avatar through the engine's JavaScript API, and ships a standalone test page
(`vm-api-probe.html`) for exercising that API directly — without VSM in the loop.

## Why a probe?

`charamel-embed` is a chain: **SceneFlow → plugin (Java) → WebSocket → character page → VuppetMaster
engine**. When something doesn't work (no speech, no emotion, no colour change) it's important to know
*which hop* is broken. The probe removes VSM entirely and calls the engine directly in the browser, so
you can answer one question in isolation: **does the raw engine API do what we expect?** Once the probe
confirms a feature works, any remaining problem is in the VSM integration (envelope, wiring,
variable substitution), not the engine.

The probe is how we confirmed every API fact the plugin relies on (see *Findings* below).

## Using the probe

1. Open `vm-api-probe.html` in **Chrome** (it works from `file://`).
2. Enter your **licenseKey** (and optional **appName**) — get it from the scene's *Show Snippet* in the
   [Vuppetmaster dashboard](https://dashboard.vuppetmaster.com). *No key is committed here.*
3. Click **1) Load engine** and wait for `Main model loaded successfully`.
4. Exercise the sections:

| Section | What it calls | Confirms |
|---|---|---|
| **speak + callbacks** | `vm.speakCommand(text, {onStart,onEnd,onMarker})` | TTS, and that `onStart`/`onEnd` fire |
| **speak with `${...}$`** | marked text | inline markers fire `onMarker(name,value)` time-synced to audio |
| **Emotion** | `vm.setEmotion(type, {intensity,attack,hold,decay})` | facial emotion animates |
| **Background** | sets `document.body` / `#vuppetmaster` background | backdrop shows behind the transparent canvas |
| **inspect canvas/DOM** | reads the WebGL context attributes | whether the canvas is transparent (`alpha=true`) |
| **dump vm methods** | enumerates the instance | the real public method surface |

Everything is logged to the on-page console (and the browser console).

## Findings (the API the plugin builds on)

Confirmed against the live bundle `https://engine.vuppetmaster.com/api/engine/vuppetmaster.iife.js`
and matching the dashboard's *API — Basic* docs:

- **Construction:** `new Vuppetmaster.VuppetMaster(config, events)`.
  - `config = { windowElement, licenseKey, appName? }`
  - `events = { onLoad, onProgress, onError }` — **`onLoad` is the "ready" signal**; only then does a
    model exist, so call `speak()`/`setEmotion()` after it.
- **Speech:** `vm.speak(text, events)` (default voice) or **`vm.speakCommand({text, voice, speed, volume}, events)`**
  (the plugin uses `speakCommand` to set the voice). `events = { onStart, onEnd(completed,error), onMarker(name,value) }`.
- **Markers:** the engine natively extracts VSM's `${'name':'value'}$` markers from speak text and fires
  `onMarker(name, value)` at the right time — this is exactly charamel-ws's convention, so the marked
  text passes through verbatim.
- **Emotions:** `vm.setEmotion(type, {intensity, attack, hold, decay})`, `vm.clearEmotion()`,
  `vm.setEmotionCoordinates(valence, arousal, opts)`, `vm.setBaseEmotion(v, a)`.
  Types: happy, sad, angry, tear, disgust, surprise, smile, excited, fear, bored, relaxed.
- **Bone animation:** `vm.animateBone(boneName, euler, {attack, hold, decay, additive})` — added by
  Charamel and confirmed against the live bundle 2026-08-11. This supersedes the earlier finding that
  there was no public head primitive.
  - `boneName` is matched **case-insensitively** against the skeleton; an unknown name logs a console
    warning and is otherwise ignored. Only **`Head`** is exposed by the current rig.
  - `euler` is `{x, y, z}` in **radians**, applied in `XYZ` order (it goes straight into a
    `THREE.Euler`). The plugin's own command surface is in **degrees** and converts at the engine
    boundary — see `animateBoneDegrees()` in `vm-adapter.js`.
  - **Which axis does what is rig-specific, and the Head bone does *not* follow the usual
    pitch/yaw/roll ordering.** Established by testing the live Xenia rig (2026-08-11):

    | Axis | Movement |
    |---|---|
    | `x` | **yaw** — turn / shake ("no") |
    | `y` | **pitch** — nod ("yes"); **negative drops the chin** |
    | `z` | **roll** — head tilt (by elimination; not separately confirmed) |

    Each bone carries its own local frame, so re-verify if Charamel exposes further bones rather
    than assuming this mapping generalises.
  - `attack`/`decay` default to **500 ms** each. **An omitted `hold` means "hold indefinitely"**
    (`holdInfinite`), *not* zero — so a one-shot movement must pass `hold` explicitly, and a
    sustained pose is released by re-issuing the command at the neutral angle with an explicit hold.
  - `additive` defaults to **true**: the rotation layers on top of the running idle animation.
    `false` replaces the bone's pose and restores the base pose when the envelope ends.
  - Envelope phases are attack → hold → decay; re-issuing for the same bone replaces its envelope,
    which is what makes repeated calls usable as procedural motion.
- **Body animation:** `vm.playAnimationByName(name)` — confirmed 2026-09-03 against the public
  `api-basic` docs (the engine's `dumpApi()` enumeration only told us the method existed, not its
  signature). `name` is matched **server-side** against the animation clips configured for the
  current scene in the VuppetMaster dashboard — it is not VSM vocabulary, and a name valid in one
  scene may not exist in another. Returns a `Promise` resolving to `{ok: true}` once the clip is
  **enqueued**, or `{ok: false, error}` if the avatar isn't ready or no such clip exists for this
  scene — like `setEmotion`/`animateBone`, there is **no completion signal**, and unlike those two
  there isn't even an attack/hold/decay envelope to estimate a duration from.
- **Other:** `setAnimationSpeed`, `setBlinkInterval`, camera methods (`saveStartCamera`, the in-embed
  cheat-code reframe flow). Realtime *gaze* still has no public primitive (internal only).
- **Audio:** the engine uses **Howler**, whose AudioContext only resumes on a **user gesture in the
  character page's own document**. `allow="autoplay"` does *not* resume a covered cross-origin iframe
  (verified: state stays `suspended`). Hence the character page's click-to-start overlay, and — when
  embedded behind the htmlgui-ws screens — the renderer brings the character frame to front for that one
  click, then drops it behind.

## Relation to the plugin

- The character page served by the plugin (`renderer/character.html` + `renderer/vm-adapter.js`) is the
  production counterpart of this probe: the adapter maps JSON command envelopes
  (`{cmd:"speak"|"emotion"|"background"|"clearEmotion"|"bone"|"nod"|"animation", …}`) to these same
  engine calls, and maps `onMarker`/`onEnd` back to VSM feedback strings.
- **Authoring named animation clips**: `[Xenia animation name='VPM-22010_Wave-Hello']` (generic form,
  any clip configured for the scene) or a convenience command for the ten NEUROGES-classified clips
  shipped as of 2026-09-03: `acknowledgenod`, `applause`, `handstogether`, `headtilt`, `listennode`,
  `openarmsoffer`, `pointleft`, `selfreference`, `thumbsup`, `wavehello`. The dashboard clip ids carry
  a `VPM-<number>_<label>` prefix (e.g. `VPM-22001_Acknowledge-Nod`) that `vm.playAnimationByName`
  matches against verbatim — `ANIMATION_ALIASES` in `CharamelEmbedExecutor.java` maps each VSM
  command keyword to its full clip id; only that map needs editing if the dashboard's ids change,
  not the author-facing command keywords. An originally-reported second "Wave Hello" clip was dropped
  by the scene owner in favor of the single `VPM-22010_Wave-Hello`, so there is no ambiguity to
  resolve there. `blocking='true'` needs an explicit `duration='<ms>'` estimate from the author —
  there is no engine-side signal to derive one from (see Findings above).
- **Authoring the bone API** (SceneFlow `PlayAction` or an inline scene marker):

  | Command | Example | Notes |
  |---|---|---|
  | `bone` | `[Xenia bone name='Head' y='-12' attack='150' hold='300' decay='250']` | Full generic access. Degrees; `x` yaw / `y` pitch / `z` roll (see the axis table above). |
  | `bone` (sustained) | `[Xenia bone name='Head' z='8']` | No `hold` ⇒ held until changed. Release with `[Xenia bone name='Head' z='0' hold='0']`. |
  | `nod` | `[Xenia nod]`, `[Xenia nod repeats='3' amplitude='8' period='320']` | Procedural nodding ("yes") on the pitch axis, starting downward. `amplitude` is the **full peak-to-peak** swing **centred on the neutral pose** — 12 means 6° down, 12° across to 6° up, 6° back to centre — at uniform angular speed, ending at rest. |
  | `shake` | `[Xenia shake]`, `[Xenia shake repeats='3' amplitude='20']` | The "no" counterpart, same centred oscillation on the yaw axis (default amplitude 16). |

  Both accept `blocking='true'` to hold the scene for the movement's estimated duration (`nod` uses
  `repeats × period`); as with `emotion`, that duration is an estimate — the engine gives no
  completion callback for bone animation.
- Full design & status: this README plus `core/src/main/resources/behavior-taxonomy.json` and
  `doc/behavior-taxonomy-neuroges.md` (the original plan doc this pointed to no longer exists).
