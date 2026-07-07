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
- **Other:** `setAnimationSpeed`, `setBlinkInterval`, `playAnimation`, camera methods. **No gaze/head
  primitive** on the public surface (internal head-bone only) — realtime gaze/head still needs Charamel.
- **Audio:** the engine uses **Howler**, whose AudioContext only resumes on a **user gesture in the
  character page's own document**. `allow="autoplay"` does *not* resume a covered cross-origin iframe
  (verified: state stays `suspended`). Hence the character page's click-to-start overlay, and — when
  embedded behind the htmlgui-ws screens — the renderer brings the character frame to front for that one
  click, then drops it behind.

## Relation to the plugin

- The character page served by the plugin (`renderer/character.html` + `renderer/vm-adapter.js`) is the
  production counterpart of this probe: the adapter maps JSON command envelopes
  (`{cmd:"speak"|"emotion"|"background"|"clearEmotion", …}`) to these same engine calls, and maps
  `onMarker`/`onEnd` back to VSM feedback strings.
- Full design & status: `~/.claude/plans/charamel-embed-vuppetmaster-jsapi.md` (plan doc).
