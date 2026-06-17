# VSM Web — Real-Time Capabilities and Resource Requirements

*VisualSceneMaker Web (VSM Web) — June 2026*

---

## Executive Summary — Benchmark Results (June 2026)

First measured benchmark run on Apple Silicon (macOS 15, JVM ms-21.0.8, G1GC, 4 GB heap),
using the `benchmark/minimal` project (no plugins, 2-node self-looping SceneFlow, 200 ms timeout).

### Key findings

**Timing — better than estimated**

| Concurrency | p50 deviation | p99 deviation | max deviation | samples |
|---|---|---|---|---|
| 1 project | 3 ms | 5 ms | 5 ms | 108 |
| 10 projects | 3 ms | 5 ms | 5 ms | 1 090 |
| 50 projects | 3 ms | 5 ms | 6 ms | 5 449 |
| 100 projects | 3 ms | 5 ms | 6 ms | 10 898 |

**How to read the deviation values:**
The benchmark SceneFlow contains a loop node with a 200 ms timeout edge — meaning the
interpreter is told to wait exactly 200 ms before re-entering the node. Each time the
node is entered, the probe records the wall-clock time. The *inter-event interval* is
simply the elapsed time between two consecutive node entries. The *deviation* is how
many milliseconds that interval exceeded the scheduled 200 ms.

*Concrete example:*

```
Node entered at t = 0 ms   → probe records t₀
Timeout scheduled: 200 ms
Node entered at t = 203 ms → probe records t₁

Inter-event interval = t₁ − t₀ = 203 ms
Scheduled timeout    = 200 ms
Deviation            = 203 − 200 = +3 ms  (fired 3 ms late)
```

A deviation of 0 would mean the OS woke the interpreter thread at exactly the right
moment. In practice the OS scheduler always adds a small positive delay.
**p50 = 3 ms** means half of all transitions fired within 3 ms of their scheduled time;
**p99 = 6 ms** means 99 % fired within 6 ms. A negative deviation is theoretically
impossible with a timeout edge and was never observed.

Latency is flat from 1 to 100 concurrent projects — no degradation observed.
This supersedes the prior estimate of 10–50 ms timing granularity.

**Memory — heap is negligible; thread stacks dominate**

| Concurrency | Live heap | Heap overhead | Per-project (live heap) |
|---|---|---|---|
| 1 project | 2 MB | 1 MB | ~1 MB |
| 10 projects | 2 MB | 1 MB | <0.1 MB |
| 50 projects | 3 MB | 2 MB | <0.05 MB |
| 100 projects | 3 MB | 2 MB | <0.03 MB |

*(JVM baseline: 1 MB. These figures cover the pure interpreter with no plugins and a minimal SceneFlow.
Real-world projects with plugins and medium-sized SceneFlows will consume more — see §3.)*

For 100 concurrent projects the dominant cost is **thread stacks** (~50 MB, off-heap),
not live heap. The prior "512 MB heap per 10 projects" rule of thumb is valid only for
plugin-heavy real-world deployments, not for the raw interpreter.

**GC — natural pauses well below timing resolution**

All natural (non-benchmark-induced) GC events at 100 concurrent projects were
Young-generation collections under **2.1 ms**. No natural Full GC was triggered.
The prior estimate of 10–50 ms G1GC pauses applies at much higher project counts
than tested here.

Forced `System.gc()` calls used during memory measurement produced Full GC pauses of
1.7–20 ms — these are benchmark artifacts, not runtime behaviour.

**Practical conclusion**

Up to 100 concurrent no-plugin projects: pure-interpreter overhead is negligible on
any modern JVM. Real-world limits are driven by plugin I/O, network connections, and
thread stack allocation, not by interpreter heap or GC.

---

## 1. What "Real-Time" Means in VSM Web

The VSM runtime is event-driven, not polled. Transitions in the SceneFlow fire as soon
as the triggering condition becomes true — a variable assignment, an event arrival, or a
timeout expiry.

| Timing claim | Basis |
|---|---|
| "Millisecond-granularity" transitions | Events dispatched and transitions evaluated synchronously — no polling cycle; trigger-to-evaluation latency is sub-millisecond |
| Measured timeout deviation (100 concurrent projects, macOS) | **p50 = 3 ms, p99 = 6 ms** |
| Practical end-to-end granularity | **3–6 ms** measured; prior estimate of 10–50 ms was conservative |
| Sensor stream reactivity | Physiological (ECG, HRV), acoustic (ASR), and visual signals enter the same event bus and trigger transitions identically to high-level dialogue events |

VSM is well-suited for time-critical multimodal interaction design — cardiac R-peak
responses, speech VAD crossings, affect-triggered branching — within a 3–10 ms window
on modern hardware. It is **not** a hard-real-time system in the Real Time Operation System (RTOS) sense; timing is
best-effort and subject to OS scheduling jitter and GC pauses.

---

## 2. Runtime Thread Model

Each running VSM project spawns at least one interpreter thread (`Process extends
java.lang.Thread`). While waiting for a transition condition — the vast majority of
execution time — the interpreter blocks on `Condition.awaitUninterruptibly()`.

**Resource consumption of an idle interpreter thread:**

| Resource | Idle consumption |
|---|---|
| CPU | ~0% |
| Stack memory | 512 KB (JVM default; reducible to 256–384 KB with `-Xss`) |
| Heap (live objects while blocked) | negligible |

The limiting factor for parallel project capacity is **thread stack memory** (off-heap)
and **heap allocated by plugins and the SceneFlow model**, not CPU.

---

## 3. Memory Requirements Per Project

Two scenarios must be distinguished:

### 3a. Minimal (no plugins, small SceneFlow) — measured

| Component | Measured at 100 projects |
|---|---|
| Live heap per project | < 0.03 MB |
| Thread stack | 512 KB (off-heap) |
| **Total per project** | **~0.5 MB** (dominated by thread stack) |

### 3b. Real-world (medium project, ~50 nodes, standard plugin set) — estimated

| Component | Estimate |
|---|---|
| JVM interpreter thread stack | 512 KB |
| SceneFlow + SceneScript model (heap) | 5–20 MB |
| Interpreter + Configuration + Environment | 3–10 MB |
| EventDispatcher + listeners | ~0.5 MB |
| **Total per project** | **~10–35 MB** |

These estimates have not yet been verified by benchmark. Projects with large SceneFlows
(hundreds of nodes) or plugin-heavy configurations will sit toward the upper end.

---

## 4. Parallel Project Capacity

### 4a. Measured (no plugins, minimal SceneFlow)

On Apple Silicon macOS, G1GC, 4 GB heap: 100 concurrent projects used 3 MB live heap
and showed no timing degradation. Thread stacks accounted for ~50 MB off-heap.
The measurement did not approach any limit.

### 4b. Estimated (real-world projects with plugins)

The following table uses the 10–35 MB per-project estimate and assumes an 8-core
machine with 8 GB JVM heap. These figures have not yet been validated by benchmark.

| Concurrent projects | Heap used (est.) | Thread stacks | Assessment |
|---|---|---|---|
| 10 | ~200 MB | ~5 MB | No observable impact; no tuning needed |
| 50 | ~750 MB | ~25 MB | No measurable impact |
| 100 | ~1.5 GB | ~50 MB | Well within limits |
| 200 | ~3 GB | ~100 MB | First GC pressure with G1GC; ZGC recommended |
| 400 | ~6 GB | ~200 MB | GC pauses measurable; explicit heap sizing required |
| 500+ | heap pressure | ~250 MB | Requires JVM tuning or project hibernation |

**Rule of thumb for real-world (plugin-heavy) deployments:** allocate at least
**512 MB JVM heap per 10 concurrent projects**.

---

## 5. Garbage Collection Behaviour

**Background — what GC algorithms are and why they matter here**

Java manages memory automatically: the JVM periodically runs a *garbage collector* (GC)
to reclaim heap objects that are no longer reachable. During certain phases of collection
the GC must briefly pause all application threads — a *stop-the-world* (STW) pause. For
a real-time dialogue system, a long STW pause delays the interpreter thread and causes a
transition to fire later than scheduled.

Two GC algorithms are relevant to VSM deployments:

| Algorithm | Flag | Default since | Typical STW pause | Trade-off |
|---|---|---|---|---|
| **G1GC** (Garbage First) | *(none — default)* | Java 9 | 10–200 ms | Best throughput; pauses grow with heap pressure |
| **ZGC** (Z Garbage Collector) | `-XX:+UseZGC` | Java 15 (production) | < 1 ms | Sub-ms pauses at any heap size; slightly higher CPU overhead |

G1GC divides the heap into equal-sized regions and preferentially collects the regions
with the most reclaimable garbage ("Garbage First"). ZGC performs almost all its work
*concurrently* — while application threads continue running — keeping STW pauses
below 1 ms regardless of heap size or project count. For time-sensitive VSM deployments
with many concurrent projects, ZGC is the recommended choice (see §6).

### 5a. Measured (100 concurrent projects, G1GC, macOS)

All naturally occurring GC events during the benchmark run were Young-generation
evacuations. No natural Full GC was observed.

| GC event type | Pause range (measured) |
|---|---|
| Young (G1 Evacuation) — natural | 0.4–2.1 ms |
| Full (System.gc()) — benchmark artifact | 1.7–20.3 ms |
| Natural Full GC | **not observed** at 100 projects |

The maximum natural pause of 2.1 ms is well below the measured p99 timing deviation of
6 ms. GC does not contribute meaningfully to timing imprecision at this project count.

### 5b. Expected at higher project counts (estimated)

| GC algorithm | Typical pause | Impact at ~200 real-world projects |
|---|---|---|
| G1GC (JVM default) | 10–50 ms | Pauses become possible under allocation pressure |
| ZGC (`-XX:+UseZGC`) | < 1 ms | Pauses effectively eliminated |

---

## 6. Practical Deployment Tiers

### Tier 1 — Up to 50 concurrent projects

- No JVM tuning required
- Default G1GC; measured natural GC pauses < 2.1 ms
- Suitable for all current VSM research deployments

```bash
java -jar runtime-server.jar --port=8091
```

### Tier 2 — 50–150 concurrent projects

- Enable ZGC; no other tuning required

```bash
java -XX:+UseZGC -jar runtime-server.jar --port=8091
```

### Tier 3 — 150–300 concurrent projects

- ZGC + explicit heap sizing required
- Thread scheduling jitter becomes measurable but remains below dialogue timing resolution

```bash
java \
  -Xms2g -Xmx8g \
  -XX:+UseZGC \
  -Xss512k \
  -jar runtime-server.jar --port=8091
```

For Java 21 (editor server), add `-XX:+ZGenerational`:

```bash
java \
  -Xms2g -Xmx8g \
  -XX:+UseZGC -XX:+ZGenerational \
  -Xss512k \
  -jar SceneMaker.jar
```

### Tier 4 — 300+ concurrent projects

At this scale, a single JVM process is no longer sufficient. Options:

- **Project hibernation:** serialize idle projects to disk; restore on next activity.
  (Planned feature — not yet implemented.)
- **Process partitioning:** run multiple `runtime-server` JVM instances on distinct ports
  behind a reverse proxy or coordinator.

---

## 7. Real-Time Plugin Capabilities

Several VSM plugins operate on continuous high-frequency sensor streams:

| Plugin | Stream type | Update rate / latency |
|---|---|---|
| **HeartFlow** | Polar H10 ECG — BPM, HRV (RMSSD, SDNN, pNN50), breathing phase, predictive R-peak events | Beat-synchronous; R-peak events within ~1 ms of detection |
| **AffectToolBox** | Multimodal social signals via SSI/SSJ pipeline (TCP/IP bridge); multiple concurrent named pipelines | Configurable; typically 10–25 Hz |
| **SocialSignalStream** *(planned)* | Mac-native vision: head pose, facial expression (7-class), gaze zone, body lean, AU analysis | 30 FPS; ~33 ms frame latency |
| **ASR** | Streaming speech recognition; VAD; turn-end prediction; keyword matching | Partial: real-time; final: end-of-utterance |
| **Timer** | Millisecond-precision elapsed time, time-difference, system time | Sub-millisecond; OS-accurate |

All plugin-produced values enter the same SceneFlow variable space and trigger
transitions with identical dispatch — no architectural distinction between a cardiac
R-peak and a high-level dialogue decision.

---

## 8. Summary

| Characteristic | Measured | Prior estimate |
|---|---|---|
| Transition evaluation latency (software) | Sub-millisecond | Sub-millisecond |
| Timing deviation p50 (100 concurrent projects) | **3 ms** | — |
| Timing deviation p99 (100 concurrent projects) | **6 ms** | ~10–50 ms |
| Natural GC pause, G1GC (100 projects) | **< 2.1 ms** | 10–50 ms |
| Live heap per no-plugin project | **< 0.03 MB** | 10–35 MB* |
| Thread stack per project (off-heap) | 512 KB | 512 KB |
| Latency degradation 1 → 100 projects | **none observed** | — |

*The 10–35 MB estimate remains the working figure for real-world plugin-carrying projects;
it has not yet been measured.*

| Characteristic | Estimated (unverified) |
|---|---|
| Max parallel projects, 8-core, 8 GB, no tuning | ~100 (real-world) |
| Max parallel projects, 8-core, 8 GB, ZGC tuned | ~300–400 (real-world) |
| Architectural limit beyond 300 real-world projects | Process partitioning or hibernation |

---

## 9. Benchmark

Benchmarks live in `benchmark/` at the repository root.

```bash
# Default run: memory + latency, 1/10/50/100 projects, 100 iterations
./gradlew :benchmark:runBenchmark

# Memory only, extended project count range
./gradlew :benchmark:runBenchmark -Pmode=memory -PprojectCounts=1,10,50,100,200

# Latency with ZGC for comparison
./gradlew :benchmark:runBenchmark -Pmode=latency -PjvmArgs="-XX:+UseZGC -Xmx8g"
```

GC log is written to `benchmark/gc.log` on each run.
The `benchmark/minimal` project uses no plugins and a 2-node self-looping SceneFlow —
it isolates pure interpreter overhead. A second benchmark project with a representative
plugin set (Timer, LLM, ASR) would close the gap between the measured and estimated
figures in §3b and §4b.

### Opening benchmark/minimal in VSM Web

The project can be loaded in the VSM Web editor for visual inspection of the running
interpreter (active node highlight, variable panel showing `tick` incrementing).

**Use the full absolute path** when opening it — the server resolves the path relative
to its own working directory, not the repository root:

```
/Users/yourname/Code/Repo/VisualSceneMaker/benchmark/minimal
```
---

*§ Executive Summary and §§ 1, 5, 8 updated with measured data from benchmark run
2026-06-16 (Apple Silicon, macOS 15, JVM ms-21.0.8, G1GC, 4 GB heap, 100 concurrent
projects). Engineering estimates in §§ 3b, 4b remain unverified by benchmark.
Contact: patrick.gebhard@dfki.de*
