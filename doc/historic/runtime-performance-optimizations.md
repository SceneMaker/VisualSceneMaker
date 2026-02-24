# Runtime Engine Performance Optimizations

**Date**: 2026-02-12
**Status**: Priorities 1-8 implemented
**Scope**: `core/` module — Interpreter, Evaluator, Process, Configuration, EventDispatcher

## Overview

The runtime engine uses a poll-based, tree-walking interpreter with fine-grained locking.
Expressions are parsed once from XML into an AST at project load time, then re-interpreted
on every evaluation. The execution model creates one JVM thread per Process (fork), with a
central `ReentrantLock` serializing all access.

This document catalogues the identified performance bottlenecks and the planned optimizations
in priority order.

---

## Identified Bottlenecks

### Critical Severity

| # | Bottleneck | Location | Mechanism |
|---|-----------|----------|-----------|
| C1 | **10ms polling loop** | `Process.java:604-681` | `Thread.sleep(10)` in tight loop checking edge guards. Creates hard 10ms latency floor for every state transition. Lock/unlock cycle 100x/second per waiting process. |
| C2 | **No expression memoization** | `Evaluator.java:169-532`, `Process.java:1056-1078` | Guard conditions fully re-evaluated from AST root on every 10ms poll. 107-branch `instanceof` chain, no visitor pattern. No caching, no dirty-tracking. `&&`/`||` not short-circuited (both operands eagerly evaluated). |
| C3 | **O(n) configuration lookups by String ID** | `Configuration.java:93-102` | Linear scan through `mConfiguration.keySet()` with `String.equals()`. Called from 13+ places in `Interpreter.java` on every variable access and `InStateQuery`. |
| C4 | **Deep environment copy on every fork** | `Environment.java:47-49`, `SymbolTable.java:46-58` | Each `ForkingEdge` deep-copies entire scope chain: all SymbolTables + all SymbolEntry values. O(scopes * variables). |
| C5 | **Full configuration scan in Interruptor** | `Interruptor.java:30-85` | Called after every command execution (`Process.java:580`) and every node start (`:557`). Allocates ArrayList, copies all active states, sorts O(n log n), evaluates all interrupt edge conditions. Single most frequently called expensive operation. |

### High Severity

| # | Bottleneck | Location | Mechanism |
|---|-----------|----------|-----------|
| H1 | **Lock per command** | `Process.java:568-582` | Lock/unlock on every command in a node's command list. |
| H2 | **Lock per getter** | `Interpreter.java:52-130` | 10 getter methods each acquire/release the exclusive `ReentrantLock(fair)`. Fair locks add 10-30% overhead. |
| H3 | **Uncached reflection per function call** | `Evaluator.java:609,656,687` | `Class.forName()` + `getMethod()` + `invoke()` on every user command. No caching of Method handles. |
| H4 | **Thread-per-activity** | `ActivityScheduler.java:54-81` | New `ActivityWorker` thread per activity. No thread pooling. |
| H5 | **Deep copy on every node exit (history)** | `SystemHistory.java:132` | `symbolTable.getCopy()` records full environment snapshot at every node transition. |
| H6 | **O(n) variable lookup through scope chain** | `Environment.java:69-77` | Linear scan through `LinkedList<SymbolTable>` on every variable read/write. |

### Medium Severity

| # | Bottleneck | Location | Mechanism |
|---|-----------|----------|-----------|
| M1 | **Synchronous event dispatch** | `EventDispatcher.java:56-66` | All listeners block the interpreter thread. |
| M2 | **Value object allocation** | `Evaluator.java:508-528` | Every operation allocates new `IntValue`/`BooleanValue` etc. wrappers. GC pressure. |
| M3 | **New Random() per edge check** | `Process.java:1081` | `new Random().nextInt(100)` in `checkPEdgeList()`. |
| M4 | **Sequential XML parsing at startup** | `RunTimeProject.java:196-206` | 6 XML parses sequential, each creating new `DocumentBuilderFactory`. |
| M5 | **O(n^2) child thread cleanup** | `Process.java:842-875` | `ArrayList.remove()` in loop, temp list allocated each time. |
| M6 | **Timeout expression re-evaluation** | `Process.java:1004-1026` | Timeout expressions evaluated on every poll iteration (from line 635). |

---

## Optimization Plan (Priority Order)

### Priority 1: Replace 10ms Polling with Condition Variable Signaling [DONE]

**Addresses**: C1, C2 (partially)
**Effort**: Low
**Expected impact**: Eliminates latency floor, reduces idle CPU by 50%+

**Approach**: Replace `Thread.sleep(10)` in the edge-waiting loop with `Object.wait()` /
`Condition.await()`. Signal (`notifyAll()`) from variable write paths and event arrival
points. Add a short safety-net timeout (e.g., 200ms) for edge cases.

**State of the art**: Reactive state machines (SCXML, XState) use notify-on-change. Variable
writes trigger condition re-evaluation only when relevant state changes.

### Priority 2: Gate Interruptor.update() with Dirty Flag [DONE]

**Addresses**: C5
**Effort**: Low
**Expected impact**: Eliminates largest per-command overhead

**Approach**: Add an `AtomicBoolean mDirty` flag. Set it `true` on variable writes and state
changes. In `Interruptor.update()`, check-and-clear the flag; skip the full scan when
nothing changed. A node with 10 commands that don't modify variables skips 10 unnecessary
full configuration scans.

### Priority 3: Cache Reflection Methods in Evaluator [DONE]

**Addresses**: H3
**Effort**: Low
**Expected impact**: 10-50x faster user function calls

**Approach**: Add a `ConcurrentHashMap<String, MethodHandle>` keyed by
`className + "#" + methodName + "(" + paramTypes + ")"`. On first call, resolve via
`MethodHandles.lookup()` and cache. Subsequent calls invoke the cached handle directly.

**State of the art**: `java.lang.invoke.MethodHandle` (Java 7+) provides near-native call
performance after initial lookup. Standard practice in frameworks (Spring, Hibernate).

### Priority 4: Add String-to-Node Index in Configuration [DONE]

**Addresses**: C3
**Effort**: Low (~10 min)
**Expected impact**: O(1) vs O(n) for all state lookups by ID

**Approach**: Add `HashMap<String, BasicNode> mNodeIndex` alongside existing configuration
map. Populate on `enterState()`, remove on `exitState()`. Replace linear scan in
`getState(String)` and `isInState(String)` with direct lookup.

### Priority 5: Read-Write Lock for Interpreter Getters [DONE]

**Addresses**: H1, H2
**Effort**: Low
**Expected impact**: Eliminates UI polling contention, allows concurrent reads

**Approach**: Replace `ReentrantLock` with `ReentrantReadWriteLock`. Getter methods acquire
read lock (shared). Mutating methods acquire write lock (exclusive). Alternative: make
frequently-read fields `volatile` and remove locking from getters where safe.

### Priority 6: Short-Circuit Evaluation and Switch Dispatch [DONE]

**Addresses**: C2 (partially)
**Effort**: Medium
**Expected impact**: Better branch prediction, cleaner code, enables future optimizations

**Approach**: Extracted binary expression evaluation into `evaluateBinary()` with switch
dispatch on `BinaryOp` enum (jump table instead of if/else chain). Implemented proper
short-circuit evaluation for `&&` and `||` operators — left operand evaluated first, right
operand skipped when result is determined. Ternary expressions also short-circuit: only the
taken branch is evaluated. Extracted helper methods for arithmetic, comparison, add, and
string conversion.

Full visitor pattern was deferred (would touch ~20 model classes for limited additional
benefit at this stage). The switch dispatch captures most of the branch-prediction benefit.

**State of the art**: Standard AST evaluation pattern. Enables constant folding, partial
evaluation, and bytecode compilation as future extensions.

### Priority 7: Copy-on-Write SymbolTable [DONE]

**Addresses**: H5
**Effort**: Low
**Expected impact**: Eliminates deep copies for read-only history snapshots

**Approach**: Added `mShared` flag to `SymbolTable`. `getCopy()` now returns a COW snapshot
that shares the backing HashMap; both original and copy are marked shared. The first mutation
(`create()`/`write()`) calls `ensureExclusive()`, which deep-copies all entries and clears
the flag. Since history entries are read-only, the deep copy is never triggered for them.

Note: `Environment.getCopy()` for fork already used shallow list copies (shared SymbolTable
references) — this was already O(scopes), not O(variables). Fork semantics (shared memory
between parent and child) are preserved unchanged. The COW optimization targets history
recording (`SystemHistory.Entry.setSymbolTable()`), which calls `SymbolTable.getCopy()` on
every node exit.

**State of the art**: Functional language runtimes (Erlang, Clojure) use persistent data
structures or COW semantics. Well-suited for fork-heavy SceneFlow models.

### Priority 8: Guard Evaluation with Dependency Tracking [DONE]

**Addresses**: C1, C2 (fully)
**Effort**: Medium
**Expected impact**: Optimal CPU usage, near-zero idle cost for unchanged guards

**Approach**: Three-part implementation:

1. **GuardDependencyExtractor** — Recursive AST walker that extracts variable names
   referenced by a guard expression. Expressions with opaque dependencies (function calls,
   `InStateQuery`, `RandomQuery`, `TimeoutQuery`, Prolog queries, history lookups) return
   null, meaning "always re-evaluate". Results are cached per expression identity.

2. **Per-variable generation counters** — `Interpreter.mVarGenerations` tracks a generation
   counter per variable name, incremented on every write via `notifyVariableChanged(varName)`.
   `mStateGeneration` increments on every `signalStateChange()` for opaque guard invalidation.
   Write notifications added to: Evaluator (assignment, increment, Prolog), Interpreter
   (external `setVariable()` methods).

3. **Guard result cache in Process** — `CachedGuardResult` per `GuargedEdge`, storing the
   evaluation result plus dependency snapshot (variable generations for known deps, state
   generation for opaque deps). `checkCEdgeList()` checks cache validity before re-evaluating.
   Cache is scoped per node (created before the edge-waiting loop).

Guards with only variable dependencies skip re-evaluation when no referenced variable has
changed. Guards with opaque dependencies skip re-evaluation when no state change has occurred
(e.g., timeout wake-ups that don't involve any actual state mutation).

**State of the art**: Reactive programming (RxJava, Reactor), incremental computation
frameworks (Adapton, Salsa). Transforms polling into push-based propagation.

---

## Stretch Optimizations (Diminishing Returns)

These are worth considering only for very large SceneFlows (100+ active nodes, deep fork
trees, or high-frequency variable updates):

| # | Optimization | Effort | Rationale |
|---|-------------|--------|-----------|
| S1 | **Thread pool for ActivityScheduler** | Medium | Replace thread-per-activity with `ExecutorService`. Bounded concurrency, reduced thread creation overhead. |
| S2 | **ArrayDeque for scope stack** | Low | Replace `LinkedList<SymbolTable>` with `ArrayDeque` for better cache locality. |
| S3 | **Async event dispatch** | Medium | Queue events for delivery on dedicated thread. Decouples interpreter from slow listeners. |
| S4 | **Expression compilation to bytecode** | High | JIT-compile ASTs via ASM/Javassist. Eliminates tree-walking overhead. Only justified if expression evaluation is profiled as dominant cost after priorities 1-8. |
| S5 | **DocumentBuilder caching** | Low | Thread-local `DocumentBuilder` reuse in XML parsing. Reduces startup time. |
| S6 | **Value object pooling / interning** | Low-Med | Pool common values (Boolean TRUE/FALSE, small integers). Reduces GC pressure. |
| S7 | **Static Random instance** | Trivial | Replace `new Random().nextInt(100)` in `checkPEdgeList()` with cached instance. |

---

## Implementation Notes

- **Java 17 constraint**: All changes must remain Java 17 compatible (no Java 18+ APIs).
  `MethodHandle` and `ReentrantReadWriteLock` are available since Java 7.
- **Thread safety**: Any signaling mechanism must account for the existing lock ordering to
  avoid deadlocks. The `ReentrantLock` in `Interpreter` is the primary synchronization
  point.
- **Backward compatibility**: Behavioral semantics of SceneFlow execution must not change.
  Guard evaluation order, event delivery order, and fork semantics must be preserved.
- **Testing**: Each optimization should be validated against the integration test suite
  (`./gradlew test --tests "de.dfki.vsm.runtime.interpreter.InterpreterIntegrationTest"`)
  and manual execution of sample projects.

---

## Test Suite

Integration tests are at `src/test/java/de/dfki/vsm/runtime/interpreter/InterpreterIntegrationTest.java`.

**Correctness tests**:
- `epsilonChainVisitsAllNodesInOrder` — Verifies epsilon-edge transitions: N1 → N2 → N3
- `guardedEdgeFiresWhenConditionIsTrue` — Variable assignment + guarded edge evaluation
- `timeoutEdgeFiresAfterDelay` — 100ms timeout edge accuracy
- `externalVariableChangeTriggerGuard` — External `setVariable()` triggers guard reaction
- `shortCircuitEvaluation` — `&&` fails (short-circuits), `||` succeeds (short-circuits)

**Performance benchmarks** (output via `[PERF]` log lines):
- `benchmarkEpsilonChainThroughput` — 200-node epsilon chain, measures transitions/sec
- `benchmarkTimeoutAccuracy` — 10×50ms timeout chain, measures overhead vs expected 500ms

**Baseline results** (after priorities 1–4):
- Epsilon chain: ~62,000 transitions/sec
- Guard reaction time: <1ms (was up to 10ms with polling)
- Timeout overhead: ~38ms over 10×50ms chain (was up to 100ms with 10ms polling)
