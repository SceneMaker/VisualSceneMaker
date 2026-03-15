# SceneFlow Join Semantics — Proposed Model Improvement

**Date:** 2026-03-14
**Status:** Proposal / Future Work

---

## Problem: Implicit Thread Accumulation at Shared Targets

VSM's concurrent execution model launches multiple threads via the `start` attribute of a SceneFlow or SuperNode (e.g. `start="S3;N10;S1;N12;"`). Each thread evolves independently. When two independent threads both transition to the **same** SuperNode via their respective IEdges, the runtime creates two active threads inside that node.

This is semantically correct for truly independent concurrent branches. However, it breaks down for the common **fork-join synchronization pattern**, where multiple branches are two halves of a single synchronization condition and are expected to **merge into one thread** at the meeting point.

### Concrete Example

```
start = "S1; S3; N10; N12;"

S1 (waits for a=true)  ──IEdge(a)──┐
                                    ├──► S4 ("A and B")  ──IEdge(a&&b)──► N5 (cnt = cnt+1)
S3 (waits for b=true)  ──IEdge(b)──┘
```

**Observed result:** `cnt = 2` — both threads independently reach S4, both satisfy `a && b`, both execute N5.
**Intended result:** `cnt = 1` — S4 is a synchronization barrier; both branches must have completed before execution continues once.

This is the fundamental difference between **independent parallel branches** and a **fork-join**.

---

## Proposed Solution

### Option A: `join` Attribute on SuperNode (Recommended)

Add a `join` attribute to the `SuperNode` element specifying how many incoming threads must arrive before the node becomes active:

```xml
<SuperNode id="S4" name="A and B" join="2" start="N8;">
  <IEdge target="N5" start="">
    <AndAnd>
      <SimpleVariable name="a"/>
      <SimpleVariable name="b"/>
    </AndAnd>
  </IEdge>
  ...
</SuperNode>
```

**Semantics:**
- The first thread to arrive is **parked** (suspended) inside S4's waiting area.
- When the second thread arrives, the two threads **collapse into one** and S4's normal execution begins at `start="N8"`.
- The `a && b` guard on the IEdge becomes structurally redundant (both conditions are guaranteed true once both branches have arrived), but can be retained as a defensive check.

This option is the most compatible with the existing SuperNode structure and requires no new XML elements.

### Option B: Dedicated `JoinEdge` Type

Alternatively, introduce a `JoinEdge` element that references a shared `joinId`, eliminating the intermediate SuperNode entirely:

```xml
<!-- In S1 -->
<JoinEdge target="N5" joinId="J1" joinCount="2">
  <SimpleVariable name="a"/>
</JoinEdge>

<!-- In S3 -->
<JoinEdge target="N5" joinId="J1" joinCount="2">
  <SimpleVariable name="b"/>
</JoinEdge>
```

**Semantics:** When `joinCount` distinct threads fire a `JoinEdge` sharing the same `joinId`, they merge and exactly one thread proceeds to `target`.

This approach is more explicit and removes the need for an intermediate synchronization node, but requires a new edge type and cross-node state management (`joinId` must be resolved globally).

---

## Runtime Impact

### Interpreter Changes

The `Configuration` (the interpreter's active-node set) requires two additions:

| Addition | Description |
|---|---|
| `Map<joinNodeId, int>` | Arrival counter per join node |
| `Map<joinNodeId, List<Thread>>` | Parked threads awaiting the join to complete |

Because VSM's interpreter is **single-threaded**, no concurrency primitives (locks, semaphores) are needed. This is pure bookkeeping.

**Step execution change:**
Before activating a node marked as join, check its counter. If `arrivedCount < joinCount`, park the arriving thread and skip. If `arrivedCount == joinCount`, collapse to one thread, clear the parked set, and proceed normally.

**Asymptotic cost:** Unchanged — O(|active nodes| × |edges per node|) per step, plus O(|join nodes|) counter checks.

### Memory

Parked threads occupy a small, bounded amount of memory (proportional to the number of concurrent threads in the system, which is typically small).

### Edge Cases and Risks

**Deadlock:** If the expected `joinCount` is never reached (a branch terminates early, takes an unexpected exit, or an error occurs), parked threads block permanently. Mitigation strategies:

1. **Timeout:** A join that does not complete within a configurable interval releases with however many threads arrived (lossy but safe).
2. **Load-time validation:** Statically verify that the number of threads that can reach a join node equals `joinCount`. Feasible for static fork counts; does not generalize to dynamic forks.
3. **Runtime diagnostic:** Expose permanently parked join nodes as a runtime warning in the debugger/Web UI. Simplest to implement.

**History nodes:** The interaction between join semantics and `history="true"` nodes inside a joining SuperNode needs careful specification. On re-entry via history, does the join counter reset? Likely yes — each new activation should require a fresh quorum.

**Dynamic fork count:** If the number of threads that can reach a join node varies at runtime (e.g. a loop creates variable-count forks), a static `joinCount` attribute is insufficient. A future extension could support `join="all"` (wait for all threads that entered the corresponding fork) by tracking fork provenance, but this is significantly more complex.

---

## Relationship to Existing Semantics

This proposal does not change the behavior of any existing SceneFlow — the `join` attribute defaults to absent/disabled, preserving full backward compatibility. Nodes without `join` continue to accumulate threads as today.

The proposed semantics align with the **UML State Machine join pseudostate** and the **Statecharts AND-state synchronization** concept, both well-established in the reactive systems literature.

---

## Files to Modify

| File | Change |
|---|---|
| `core/src/main/java/de/dfki/vsm/runtime/interpreter/Interpreter.java` | Add join counter/parked-thread tracking to the step computation |
| `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/SuperNode.java` | Add `joinCount` field and XML binding |
| `core/src/main/java/de/dfki/vsm/model/sceneflow/chart/edge/` | Add `JoinEdge` class (Option B only) |
| `core/src/main/resources/res/xsd/sceneflow.xsd` | Extend schema with `join` attribute / `JoinEdge` element |
| `editor/web-ui/src/SceneFlowView.svelte` | Visual representation of join nodes/edges in the graph editor |
