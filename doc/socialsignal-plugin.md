# SocialSignalStream plugin

Real-time social-signal input for VSM. The plugin connects to the SocialSignalStream service
(`~/Code/Repo/SocialSignalStream`), a Python pipeline that reads a webcam at 30 FPS and streams
structured upper-body, head and face features, and maps those features to SceneFlow variables.

- Module: `plugins/socialsignal`
- Class: `de.dfki.vsm.xtension.socialsignal.SocialSignalExecutor`
- Descriptor: `plugins/socialsignal/src/main/resources/plugin-properties.json`
- Variable prefix: `sss_`

## What crosses the wire

Only the `FeatureFrame` JSON. The SocialSignalStream pipeline has exactly one broadcast site
(`pipeline.py`, `await self._server.broadcast(ff)`), and `broadcast` sends `frame.to_json()`. No
pixels ever reach VSM.

The service's only image endpoint is `GET /preview/{index}`, a JPEG used by the device picker in
its own browser UI. This plugin never calls it. Recordings are written on the SocialSignalStream
host; VSM learns only the session id.

`FeatureFrame` also carries overlay landmarks (`face_cx/cy/w/h`, `nose_x/y`, eye positions) and
latency counters. Those are geometry and profiling data rather than abstract information, so the
plugin deliberately does not map them.

## First test on macOS

The service and VSM run as two processes on the same machine.

1. Start SocialSignalStream:

   ```bash
   cd ~/Code/Repo/SocialSignalStream
   ./run.sh                 # opens http://localhost:7070 and waits for a device choice
   ```

   `run.sh` calls `uv run python main.py`, so `uv` has to be installed. On first run macOS asks
   the terminal application for camera access, since the prompt goes to whatever launched Python,
   not to Python itself. If no prompt appears, check System Settings under Privacy & Security,
   Camera.

2. Pick a camera in the page and confirm that the feature values move. On a MacBook the built-in
   camera is usually index 0, but a connected iPhone acting as Continuity Camera can take that
   slot. `GET http://localhost:7070/devices` lists what the service found.

3. Start VSM, add the **Social Signal Stream** plugin to a project, and run it. The 42 `sss_`
   variables are created with the plugin. Watch `sss_connected` and `sss_streaming` go true, then
   nod at the camera and watch `sss_nod` fire.

Once that works, set `auto_start = true` and the plugin issues the device selection itself on
connect, so the browser page is no longer needed.

### Ports

SocialSignalStream serves on **7070**. It used to default to 8080, which collides with
`htmlgui-ws` (`html_port = 8080`) and with `studymaster-web`, so the service default was moved on
both sides: `streaming.websocket.port` in the service's `config/defaults.yaml`, and `ws_url` in
this plugin. VSM's own web UI is on 8090 and the gRPC side of the service is on 50051, so nothing
else clashes.

If you point the plugin at a service on another port or host, set `ws_url`. The REST base URL for
the actions is derived from it, so `http_url` only needs setting when the two differ.

## Variables

All 42 are created when the plugin is added. A signal whose SceneFlow variable does not exist is
probed for `variable_timeout_ms` after launch, then logged once and switched off for the run, so
**deleting the variables you do not need is the intended way to trim the plugin down**.

### Connection and presence

| Variable | Type | Meaning |
|---|---|---|
| `sss_connected` | Bool | WebSocket to the service is open |
| `sss_streaming` | Bool | Camera frames are arriving; false again `stream_timeout_ms` after they stop |
| `sss_face_visible` | Bool | A face is detected in the current frame |

### Head, mouth, expression

| Variable | Type | Meaning |
|---|---|---|
| `sss_head_pitch` | Float | Degrees, positive = nodding down |
| `sss_head_yaw` | Float | Degrees, positive = turning right |
| `sss_head_roll` | Float | Degrees, positive = tilting right |
| `sss_mouth_state` | String | `open`, `closed`, `pressed` |
| `sss_mouth_openness` | Float | 0 to 1 |
| `sss_expression` | String | 7-class label |
| `sss_expression_confidence` | Float | 0 to 1 |

### Events

| Variable | Type | Meaning |
|---|---|---|
| `sss_nod` | Event(String) | Fires once per nod, value `nod` |
| `sss_shake` | Event(String) | Fires once per shake, value `shake` |
| `sss_blink` | Event(String) | Fires once per blink as the eyes reopen, value `blink` |

### Gaze, blink rate, lean

| Variable | Type | Meaning |
|---|---|---|
| `sss_gaze_zone` | String | `center`, `top_left`, `top_right`, `bot_left`, `bot_right` |
| `sss_gaze_x` | Float | -1 left to 1 right |
| `sss_gaze_y` | Float | -1 down to 1 up |
| `sss_blink_rate` | Float | Blinks per minute, 60 s window |
| `sss_lean_state` | String | `neutral`, `forward`, `backward` |
| `sss_lean_angle` | Float | Degrees from vertical |

### Smile sub-classification

| Variable | Type | Meaning |
|---|---|---|
| `sss_smile_type` | String | `none`, `non_duchenne`, `duchenne` |
| `sss_smile_category` | String | Bänninger-Huber and extensions: `none`, `enjoyment`, `non_duchenne`, `asymmetric`, `masking`, `miserable`, `dampened`, `contempt` |
| `sss_smile_asymmetry` | Float | -1 left-dominant to 1 right-dominant |
| `sss_smile_contempt` | Bool | Unilateral smile, one hemiface far stronger |
| `sss_smile_onset_side` | String | `none`, `left`, `right`, `simultaneous` |
| `sss_smile_onset_delta` | Float | ms between left and right onset, positive = right later |

### Eyebrows

| Variable | Type | Meaning |
|---|---|---|
| `sss_brow_inner_raise` | Float | AU1, bilateral inner raise, 0 to 1 |
| `sss_brow_outer_left` | Float | AU2 left, 0 to 1 |
| `sss_brow_outer_right` | Float | AU2 right, 0 to 1 |
| `sss_brow_furrow_left` | Float | AU4 left, 0 to 1 |
| `sss_brow_furrow_right` | Float | AU4 right, 0 to 1 |
| `sss_brow_outer_asym` | Float | Right minus left, positive = right dominant |
| `sss_brow_inner_asym` | Float | Inner brow height difference, positive = left higher |

### Hand-face self-touch

| Variable | Type | Meaning |
|---|---|---|
| `sss_hand_face_touch` | Bool | A hand is at the face |
| `sss_hand_face_gesture` | String | `none`, `hand_over_mouth`, `chin_rest`, `head_scratch`, `neck_touch`, `face_rub` |
| `sss_hand_face_hand` | String | `none`, `left`, `right`, `both` |
| `sss_hand_face_confidence` | Float | 0 to 1, proximity weighted by duration |

### Turn context and recording

| Variable | Type | Meaning |
|---|---|---|
| `sss_turn_state` | String | `listening`, `speaking`, `system_turn`, `unknown` |
| `sss_speech_ratio` | Float | Voice-activity speech ratio, 0 to 1 |
| `sss_vad_active` | Bool | Speech in the current audio chunk |
| `sss_time_since_utterance` | Float | Seconds since the last recognised utterance, -1 until the first |
| `sss_recording` | Bool | A recording is running on the service host |
| `sss_recording_session` | String | Session id of the current or last recording |

## Actions

| Action | Params | Effect |
|---|---|---|
| `start` | `device_index`, `width`, `height` (all optional) | `POST /start`, the device selection the browser UI otherwise performs |
| `calibrate_gaze` | none | `POST /gaze/calibrate`, takes ~2 s of samples as the neutral direction |
| `patch` | `key`, `value` | Hot config change over the feature WebSocket |
| `recording_start` | `marks`, `scenarios`, `lang` (all optional) | Starts a recording on the service host |
| `recording_stop` | none | Closes the session and starts encoding |
| `mark` | `id`, `type`, `label`, `text` | Stamps a moment into the running recording |

### Gaze calibration

Worth triggering in a welcome scene while the person looks at the camera. It removes the
head-pitch baseline from `gaze_x`/`gaze_y`, which matters because MediaPipe's `eyeLook` blendshapes
have asymmetric ranges: upward gaze saturates around 0.12 while downward reaches about 0.33.

### Config patches

`patch` reaches the thresholds the service exposes at runtime, using the dotted key from its
config:

```
[socialsignal patch key='nod.min_amplitude_deg' value='4.0']
[socialsignal patch key='mouth.mar_open_threshold' value='0.35']
```

Numbers and `true`/`false` are sent as such, anything else as text.

### Recording and marks

`recording_start` defaults to a **marked** session, which uses the service's prompted recorder.
That matters: `mark_prompt` refuses outside a prompted session, so a plain `/recording/start`
would make `mark` fail. A marked session additionally writes `prompts.csv` with `start_ms` and
`end_ms` on the recording clock, which is the artefact you want for a study. The flow stamps what
the agent was doing, and it lines up with `features.csv` at review time. Each mark closes the
previous one, so the result is a sequence of intervals rather than points.

```
[socialsignal recording_start lang='de']
...
[socialsignal mark id='greeting' type='instruction' text='Guten Tag, wie geht es Ihnen?']
...
[socialsignal recording_stop]
```

Pass `marks='false'` for a plain recording without `prompts.csv`. Pass `scenarios` as a
comma-separated list when the session follows a predefined sequence from the service's scenario
list; leave it empty to mark with your own ids.

`POST /recording/discard` and `POST /recording/retry_encode` are deliberately not exposed. Both are
operator recovery after a failed encode rather than flow logic.

## Configuration

| Key | Default | Meaning |
|---|---|---|
| `ws_url` | `ws://localhost:7070/ws` | Feature stream |
| `http_url` | *(empty)* | Base URL for the REST actions; derived from `ws_url` when empty |
| `auto_start` | `false` | Start the camera pipeline on connect, so no browser interaction is needed |
| `device_index` | `0` | Camera index for `auto_start` and `start` |
| `capture_width` | `640` | |
| `capture_height` | `480` | |
| `reconnect_delay_ms` | `2000` | Retry delay after connection loss |
| `update_interval_ms` | `100` | Minimum interval between continuous-signal updates |
| `angle_deadband` | `0.5` | Minimum change in degrees before an angle is rewritten |
| `score_deadband` | `0.02` | Minimum change before a normalised 0 to 1 value is rewritten |
| `write_when_invalid` | `false` | Keep writing feature variables while no face is detected |
| `variable_timeout_ms` | `20000` | How long to keep looking for a missing SceneFlow variable |
| `stream_timeout_ms` | `1500` | Time without a frame after which `sss_streaming` goes false |

Every variable name is also a config key, so a signal can be renamed or blanked out.

## Why the plugin throttles

The service broadcasts one frame per camera frame, 30 FPS, with about 50 fields.
`Interpreter.setVariable` takes the interpreter lock and calls `notifyVariableChanged`,
`markDirty`, `update` and `signalStateChange` on every write, so each write re-evaluates the
conditional edges. Written naively that would be roughly 1200 writes per second.

So the plugin:

- writes a variable only when its value actually changed, floats only past a deadband,
- rate-limits all continuous signals to one pass per `update_interval_ms`, and
- exempts nod, shake and blink from that gate, because they are flagged for a single frame and
  would otherwise be dropped.

Lowering `update_interval_ms` makes the flow more responsive to continuous values and costs
interpreter time. The three events are unaffected by it either way.

When no face is detected the plugin sets `sss_face_visible` to false and holds the last measured
values, rather than writing the defaults that arrive in an invalid frame. Set `write_when_invalid`
to true if a flow would rather see the fallback values.

## Open design question: switching signal groups off

**Not implemented.** Recorded here because the question came up and the answer is not the obvious
one. The motivation was saving resources by starting and stopping cue groups (head movement,
facial expressions and so on) from the flow.

### The cost is in the models, not the cue groups

From the service's own latency budget: FaceLandmarker 10 to 18 ms, PoseLandmarker 8 to 12 ms
concurrent, ONNX emotion 6 to 10 ms when that backend is selected, and **all extractors combined
1 to 3 ms**.

The extractors are the cue groups. Head pose, mouth, gaze, blink, expression, smile, brow and
nod/shake are all arithmetic over one FaceLandmarker result that has already been computed.
Switching off "facial expressions" saves a fraction of a millisecond.

The switches that would save something are at model granularity:

| Switch | Saves | Costs |
|---|---|---|
| PoseLandmarker | 8 to 12 ms | lean, and hand-face, whose wrist gate needs pose |
| HandLandmarker | fires on about 25 % of frames | hand-face self-touch |
| ONNX emotion CNN | 6 to 10 ms | nothing today, `backend` defaults to the cheap blendshape MLP |
| Silero VAD and mic capture | an audio thread | `vad_active`, turn-context fallback |
| FaceLandmarker | 10 to 18 ms | essentially every facial cue |

### It needs work in SocialSignalStream first

The service's `_on_config_patch` only adjusts thresholds. There is no runtime switch to skip an
extractor or a model. If it accepted patch keys such as `pipeline.pose = false`, the VSM side
would be nearly free, since the existing `patch` action already reaches that channel. The plugin
would need only a config default applied on connect plus an `enable`/`disable` action.

### Two traps

**Defaults look like measurements.** `FeatureFrame` is a fixed dataclass, so a disabled extractor
still emits its defaults: `lean_state` becomes `neutral`, `expression` becomes `neutral`. A flow
branching on a switched-off signal would read a plausible value and never know. So the service
would have to report which groups are live, and the plugin would have to stop writing those
variables rather than pass defaults through. Saving 10 ms at the price of silently wrong flow
decisions is a bad trade.

**Detectors need warm-up.** Nod and shake need a 60-frame buffer, about 2 s. Blink rate uses a
60 s window. Gaze and smile use EMA smoothing, and gaze also carries calibration state. A group
switched on mid-interaction is not trustworthy immediately. That argues for changing the profile
at phase boundaries rather than per node, and for exposing readiness rather than pretending a
freshly enabled signal is usable.

### Suggested direction

Four model-level switches in SocialSignalStream, set from the plugin config at launch, with the
active set echoed back so the plugin can gate variables honestly.

Open decisions:

1. Is the goal CPU on the vision machine, or interpreter load in VSM? If the latter, the deadband
   and throttle above already handle it.
2. Model granularity (four switches) or cue granularity (twelve)? Cue granularity saves nothing,
   but it is a reasonable feature for a different purpose, namely bounding which cues a study is
   permitted to react to. That version lives entirely in VSM and costs nothing.
3. Should the switch live in the service's hot-patch channel, a new endpoint, or named profiles in
   the plugin config?
4. Switching at project launch only, at phase boundaries, or freely per node?
