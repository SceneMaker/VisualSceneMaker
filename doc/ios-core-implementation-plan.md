# VSM iOS Core Implementation Plan (Future Work)

Date: February 19, 2026
Status: Planning notes only (no implementation started)

## Context
VisualSceneMaker currently has a Java runtime and an Android-native runtime integration path. A future goal is to add a native iOS runtime stack while keeping the Java runtime as the canonical reference implementation.

## Key Decisions
- Keep Java runtime as the functional oracle and compatibility baseline.
- Build an iOS-native runtime in Swift (recommended over Objective-C).
- Preserve protocol compatibility so the existing desktop/web tooling can interoperate with iOS runtime.
- Treat plugin migration as a separate stream from core runtime parity.

## Language Positioning (iOS)
- Swift: recommended target for native iOS runtime implementation.
- Objective-C: technically possible, but not recommended for new core work.
- C#/.NET for iOS: viable for app development, but not the selected direction for VSM core parity.

## Feasibility Summary
A Swift port of core runtime modules is feasible, but it is a reimplementation effort (not direct source translation). The cleaner the portable core boundaries remain, the lower the porting risk.

## Scope Candidates for Swift Port
Primary:
- Runtime model loading and project config handling
- Interpreter/evaluator execution loop
- Runtime state machine and event dispatching
- Variable/runtime snapshots

Secondary/later:
- Full plugin parity
- Advanced dynamic behavior currently based on Java reflection/classpath assumptions

## Main Technical Risks
- Behavioral parity of interpreter/evaluator semantics
- Concurrency/timing equivalence across platforms
- Parser/model compatibility for existing project artifacts
- Plugin architecture differences (Java dynamic loading vs iOS-safe static/registry model)

## Performance Expectation
No intrinsic performance degradation is expected from architecture split alone. Runtime performance impact will mostly depend on implementation details in scheduling, event dispatch, and snapshot/event emission frequency.

## Effort Estimate (Order of Magnitude)
1. Minimal runtime parity:
- 3-5 person-months
- Typical timeline: 2-4 months (1-2 engineers)

2. Strong parity with conformance harness:
- 6-10 person-months
- Typical timeline: 4-7 months (2 engineers)

3. Near-full parity including plugin strategy:
- 10-18+ person-months
- Typical timeline: 7-12+ months (2-3 engineers)

## Proposed Phased Plan
### Phase 0: Contracts and Parity Definition
- Freeze protocol and compatibility targets.
- Define must-pass parity scenarios.

### Phase 1: Conformance Harness
- Build black-box comparison harness against Java runtime.
- Capture golden traces for key scenarios.

### Phase 2: Swift Models + Parsing
- Implement project/sceneflow/runtime models in Swift.
- Validate loading compatibility on representative projects.

### Phase 3: Interpreter/Evaluator MVP
- Implement execution semantics and runtime states.
- Pass MVP conformance scenarios.

### Phase 4: Protocol Server Parity
- Expose equivalent runtime control + events via HTTP/WS.
- Verify desktop web UI compatibility without UI rewrites.

### Phase 5: iOS Plugin/Extension Architecture
- Replace Java dynamic loading with explicit iOS-safe registry.
- Migrate/stub representative plugins.

### Phase 6: Performance and Stability Hardening
- Profile runtime latency/memory.
- Add soak tests and failure-mode hardening.

### Phase 7: Beta Integration and Release Readiness
- Packaging, diagnostics, and developer-facing integration docs.

## Recommended Delivery Strategy
- Keep Java runtime as ongoing reference oracle.
- Prioritize protocol parity early to reuse current tooling.
- Defer full plugin parity until core semantics are stable.

## Out of Scope for This Note
- No code changes for iOS runtime yet.
- No immediate backlog breakdown into tickets.
