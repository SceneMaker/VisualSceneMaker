# VoiceTTS Plugin — Playback Timing Architecture

This document explains how word/viseme events are time-aligned with audio playback
and what care is needed when modifying the audio or event pipeline.

---

## The Problem: Events Arrive Before Audio Is Audible

The TTS server sends `word.final` and `viseme.frame` WebSocket messages interleaved
with `audio.chunk` messages. All of these arrive over the network at roughly the same
wall-clock time, but the audio is **not played immediately** — it is held in a
client-side prebuffer until enough has accumulated to avoid underruns.

| Mode              | Prebuffer target | Effect on timing offset |
|-------------------|-----------------|------------------------|
| Live generation   | 1000 ms         | Events arrive ~1 s early |
| Cache hit         | 50 ms           | Events arrive ~50 ms early |

Without compensation, `wordVar` and `wordFinalVar` would be written to the VSM
sceneflow roughly 1 second before the corresponding word is actually spoken.

---

## The Anchor-Based Scheduling Solution

The plugin uses the same anchor approach as `avatar.html`.

### Playback anchor

When `audioLine.start()` is called (the prebuffer is full), two values are captured:

```
anchorWallMs      = System.currentTimeMillis()   // wall clock at start()
anchorAccumulatedMs = prebuffer bytes converted to ms  // audio already in hardware buffer
```

These are stored in `playbackAnchorWallMs` and `playbackAnchorAccumulatedMs`
(both `volatile` fields on `VoiceTtsExecutor`).

### Fire-time formula

For any event with an audio-stream position `startMs` (from `StreamClock.startMs()`):

```
fireAtWallMs = anchorWallMs + startMs - anchorAccumulatedMs + hwLatencyMs
```

- `anchorWallMs - anchorAccumulatedMs` is the effective wall-clock "zero" of the
  audio stream (when sample 0 would have been audible if the hardware had zero latency).
- `hwLatencyMs` accounts for the DAC output delay (time between writing bytes and
  the speaker emitting sound).

This is identical to the formula used in `avatar.html`:
```javascript
wallFor(audioMs) = anchor.wallMs + audioMs - anchor.accumulatedMs
```
(`avatar.html` omits `hw_latency_ms` in the formula intentionally — visual mouth
anticipation slightly ahead of audio is perceptually natural.)

### Pending queue for pre-anchor events

Word events that arrive **before** `audioLine.start()` fires (i.e. during prebuffering)
cannot be scheduled immediately because the anchor is not yet known. They are queued
in `pendingWordVarEvents` (a `LinkedList<WordTimingEvent>` guarded by `markerLock`).

When `broadcastPlaybackAnchor()` runs, it:
1. Stores `anchorWallMs` / `anchorAccumulatedMs`
2. Calls `flushPendingWordVarEvents()` which drains the queue and schedules each entry

### Session lifecycle and anchor reset

| Event | Action |
|---|---|
| `onSessionStarted` | `playbackAnchorWallMs = -1`, clear `pendingWordVarEvents` |
| `audioLine.start()` (prebuffer full) | anchor set, pending queue flushed |
| `audioLine.start()` (short utterance drain) | anchor set, pending queue flushed |
| `stopActiveSession` / cancel | anchor reset, queue cleared, scheduler shut down |

---

## Hardware Latency Measurement

`hwLatencyMs` (default 50 ms) is measured once per plugin lifecycle by
`measureHardwareLatencyMs(AudioFormat)`:

- Opens a short-lived **probe** `SourceDataLine` (separate from the playback line)
- Writes ~80 ms of silence, calls `probe.start()`
- Polls `probe.getLongFramePosition()` until the first frame clears, timing the interval
- Result stored in `hwLatencyMs` (volatile); probe line immediately closed

The measurement is launched in a **daemon background thread** by
`maybeStartHwLatencyMeasurement(format)`, called the first time an audio line is
opened. For non-cached sessions the 1 s prebuffer gives ample time for the ~50–200 ms
probe to complete before `audioLine.start()` fires.

For cached sessions (50 ms prebuffer) the measurement may not finish in time; the
default 50 ms fallback is used. This is acceptable because cached playback has
near-zero network and generation latency, making the small timing error negligible.

The measured value is used in:
- `scheduleWordVarAtPlaybackPosition` — the word-var fire-time formula
- `broadcastPlaybackAnchor` — the `hw_latency_ms` field of the SSE event sent to
  `avatar.html` (currently unused by avatar.html but available for future use)

---

## Timemark Scheduling (separate mechanism)

Timemarks (VSM sceneflow markers embedded in speech text) use a **different** timing
mechanism: `scheduleNextTimemarkAtPlaybackPosition(StreamClock)`.

That method uses `audioLine.getMicrosecondPosition()` to read the hardware playback
cursor in real time and computes `delay = targetMs - playbackMs`. This works well
because timemarks are always dispatched **after** `audioLine.start()` has been called
(they arrive late in streaming mode, or are scheduled from `onWordFinal` which itself
fires after the prebuffer phase).

Do not replace this with the anchor formula without careful testing — the two
mechanisms are complementary.

---

## Viseme Events in VSM (`visemeVar`)

`visemeVar` is still written **immediately** in `onViseme` (no scheduling). Viseme
events are used for avatar lip-sync inside VSM scenes but not for precise timing
logic, so the small offset is acceptable. If precise viseme timing in the sceneflow
ever becomes necessary, apply the same `scheduleWordVarAtPlaybackPosition` pattern.

---

## Key Fields and Methods (quick reference)

| Symbol | Type | Purpose |
|---|---|---|
| `playbackAnchorWallMs` | `volatile long` | Wall ms when `audioLine.start()` fired; -1 = not set |
| `playbackAnchorAccumulatedMs` | `volatile double` | Prebuffer size in ms at `start()` time |
| `hwLatencyMs` | `volatile long` | Measured DAC latency in ms (default 50) |
| `hwLatencyMeasured` | `volatile boolean` | Guards single-run measurement |
| `pendingWordVarEvents` | `LinkedList<WordTimingEvent>` | Queue for pre-anchor word events; guarded by `markerLock` |
| `broadcastPlaybackAnchor(wallMs, prebufMs)` | method | Sets anchor, flushes queue, broadcasts SSE event |
| `scheduleWordVarAtPlaybackPosition(event)` | method | Applies anchor formula; queues if anchor not yet set |
| `flushPendingWordVarEvents()` | method | Drains queue once anchor is available |
| `measureHardwareLatencyMs(format)` | static method | Probe-based DAC latency measurement |
| `maybeStartHwLatencyMeasurement(format)` | method | Launches measurement thread once |

---

## Related Components

- **`avatar.html`** (`static/avatar.html` in the TTS server repo) — uses the same
  anchor formula for viseme and word beat-gesture scheduling. `playback.anchor` SSE
  events originate from `broadcastPlaybackAnchor`.
- **`StreamingCliClient.ConsoleListener`** (`stream-client-cli`) — the Java CLI client
  uses the same approach; `measureHardwareLatencyMs` was ported from there.
- **`stream-client-core` event types** — all previously Java records, converted to
  plain classes (`final class` with accessors) for compatibility with the VSM IDE's
  Java 17 project SDK. The JAR is compiled with `options.release = 17`.
