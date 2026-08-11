# SceneFlow Edge Routing Implementation Preparation

This document converts the design in `sceneflow-edge-routing-plan.md` into an implementation-ready contract. It deliberately contains no application-code changes and can be used as the handover checklist after concurrent editor work is complete.

> **Rollback note:** The experimental automatic router was rejected and removed. The constraints under “Constraints for a future attempt” in `sceneflow-edge-routing-plan.md` supersede the original implementation contract below. In particular, the next milestone is a non-mutating collision detector, followed by an explicit per-edge suggestion preview. Automatic correction is not currently authorized.

## Frozen version-one decisions

- Route ordinary, non-self-loop edges in the web SceneFlow view.
- Preserve a stored route exactly when it passes the collision criteria.
- When a stored route collides, calculate a derived display route without changing `sceneflow.xml`.
- Continue to represent a route as one cubic Bezier with two endpoint/control records.
- Treat node bodies and action badges as hard obstacles.
- Include the curve stroke, arrowhead, and edge-label rectangle in collision measurement.
- Treat edge touching and shared paths as more harmful than a short, clean crossing.
- Prefer the eight primary docking positions at 45-degree intervals when safety is equal.
- Route reciprocal and parallel edges as a group.
- Keep the current specialized self-loop routing in version one.
- Do not run the full router on animation frames or on every pointer movement.
- Persist an automatically selected route only through a later explicit command such as Normalize Edge.

## Route-selection state policy

Every displayed ordinary edge follows this deterministic policy:

```text
edge is being created, retargeted, or control-dragged
    -> display interactive geometry; do not auto-route

otherwise measure stored geometry
    -> preservation-clear: display stored geometry exactly
    -> conflicting: generate and select a derived automatic route

automatic route selected
    -> use for display, hit testing, labels, animation, export, and bounds
    -> do not call SceneFlow.Edge.Update
    -> do not modify project history or dirty state
```

After an interactive edit ends, the resulting stored geometry re-enters the same measurement step. It is preserved when clear and may be replaced for display when conflicting. Version one has no permanent manual lock.

### Definition of preservation-clear

A stored route is preservation-clear only when its complete footprint:

- Does not touch or intersect any action obstacle.
- Does not touch or intersect any unrelated node body.
- Does not cause its own label to touch an action or unrelated node.
- Does not touch, overlap, or share a significant path with another fixed edge outside endpoint exclusion zones.
- Has no avoidable proper crossing identified during its edge-group evaluation.

The source and target node bodies are exempt only where the edge connects to their boundaries. Their action badges are never exempt.

## Pure router boundary

The router should be a pure module, provisionally `editor/web-ui/src/sceneFlowEdgeRouter.js`. It receives already resolved world-coordinate geometry and returns routes. It must not read Svelte state, DOM elements, XML, or transport objects directly.

Suggested input:

```ts
type RoutingInput = {
    nodes: RoutingNode[];
    edges: RoutingEdge[];
    actionObstacles: RectObstacle[];
    typeBadgeObstacles: RectObstacle[];
    startMarkerObstacles: ShapeObstacle[];
    settings: RoutingSettings;
};

type RoutingEdge = {
    id: string;
    sourceId: string;
    targetId: string;
    kind: string;
    stored: CubicRoute;
    strokeWidth: number;
    label: LabelMetrics;
    interactive: boolean;
    selfLoop: boolean;
};

type CubicRoute = {
    start: Point;
    control1: Point;
    control2: Point;
    end: Point;
};
```

Suggested output shared by every geometry consumer:

```ts
type RoutedEdge = CubicRoute & {
    edgeId: string;
    provenance: "stored" | "automatic" | "interactive" | "self-loop";
    visibleEnd: Point;
    visibleControl2: Point;
    path: string;
    arrow: Point[];
    midpoint: Point;
    length: number;
    bounds: Rect;
    flattenedSegments: Segment[];
    labelPosition: Point;
    labelBounds: Rect;
    measurements: RouteMeasurements;
};
```

The visible target/control values account for arrow trimming. Measurements and rendering must refer to the same selected cubic; there must not be separate route calculations for the path, arrow, midpoint, label, selection bounds, and animation.

## Collision measurement contract

All collision geometry uses SVG world coordinates. Zoom changes must not alter the chosen route.

Initial configurable values:

```text
actionClearance = max(6, 0.35 * fontSize)
edgeClearance   = max(4, 0.25 * fontSize)
flattenError    = 0.5 world units
```

The effective curve-to-obstacle threshold is:

```text
edgeStrokeWidth / 2 + obstacleClearance + flattenError
```

For two curves it is:

```text
(firstStrokeWidth + secondStrokeWidth) / 2
    + edgeClearance
    + 2 * flattenError
```

Equality is a collision. A curve that only touches an inflated obstacle or another buffered edge is not clear.

Curve flattening continues until both conditions hold:

- Maximum control-polygon/chord deviation is at most `flattenError`.
- Segment length is at most a configured maximum segment length.

Required measurements:

```ts
type RouteMeasurements = {
    actionBodyConflicts: number;
    actionArrowConflicts: number;
    actionLabelConflicts: number;
    unrelatedNodeConflicts: number;
    otherLabelConflicts: number;
    edgeTouchCount: number;
    sharedPathLength: number;
    crossingCount: number;
    clearanceShortfall: number;
    minimumActionClearance: number;
    minimumEdgeClearance: number;
    preferredDockPenalty: number;
    dockCongestionPenalty: number;
    directionPenalty: number;
    tangentPenalty: number;
    pairSymmetryPenalty: number;
    handleBalancePenalty: number;
    length: number;
    curvaturePenalty: number;
    storedDifference: number;
};
```

## Deterministic candidate ordering

Compare routes or route-group combinations with this lexicographic tuple:

```text
1.  action body + arrow + label conflicts
2.  unrelated-node body + label conflicts
3.  edge-touch count
4.  shared-path length
5.  crossing count
6.  total clearance shortfall
7.  other edge-label conflicts
8.  primary/secondary dock penalty and congestion
9.  dock-direction and tangent penalties
10. reciprocal-pair symmetry and handle balance
11. route length
12. curvature penalty
13. difference from stored route
14. stable edge/group key
```

The stored route is evaluated first and returned immediately when the complete affected group is preservation-clear. Candidate generation only occurs for conflicting groups.

Candidate enumeration must itself be stable:

1. Primary docking positions `0, 3, 6, 9, 12, 15, 18, 21`.
2. Positions ordered by angular relevance to the other node.
3. Symmetric lane offsets in the order center, negative one, positive one, negative two, positive two.
4. Handle lengths in a fixed order around the default length.
5. Secondary docking positions only if the primary search has no hard-conflict-free result.

## Boundary and tangent rules

Docking positions are directions, not rectangle coordinates. Project each direction onto the visual node boundary used by the web renderer.

- Circular/elliptical nodes use the ellipse intersection.
- Supernodes and aliases use the existing power-five superellipse boundary.
- The start-marker-reserved dock remains unavailable where the existing layout reserves it.

The default control direction is the local outward normal:

```text
ellipse normal proportional to (dx / rx^2, dy / ry^2)
power-five superellipse normal from its normalized gradient
```

At the source, `control1` extends outward from the source. At the target, `control2` extends outward from the target, so the forward cubic derivative approaches the target inward. Tangent alignment is a soft rule and can be relaxed to obtain clearance.

## Candidate-search bounds

The first implementation should keep search work bounded:

- Measure the stored route before generating anything.
- Search primary docks first.
- Retain only the best few source and target docks by direction before forming all pairs.
- Test a small fixed set of handle-length factors and lane offsets.
- Expand to secondary docks only after primary candidates fail hard constraints.
- Use bounding boxes or a simple spatial grid before exact flattened-segment checks.
- Route reciprocal/parallel groups together.
- Use only a small fixed number of global improvement passes.

The router cache must not depend on `timeoutNow`, activity animation, selection state, hover state, or viewport zoom. Those values can change every frame and must not trigger route generation.

## Concrete SIA Demo regression fixture

Source project used to derive the fixture:

```text
/Users/gebhard/Code/Temp/SIA Demo/sceneflow.xml
```

Relevant stored model data:

```yaml
nodes:
  - id: S2
    kind: super
    name: controls
    position: { x: 20, y: 440 }
  - id: N11
    kind: basic
    name: do emotion
    position: { x: 200, y: 320 }
    actions:
      - kind: PlayAction
        displayPrefix: "[Xenia emotion type='"
      - kind: Assignment
        displayText: "emo_type = ''"

edges:
  - direction: S2 -> N11
    kind: IEdge
    condition: "emo_type != ''"
    stored:
      start:    { x: 90,  y: 455 }
      control1: { x: 146, y: 423 }
      control2: { x: 170, y: 420 }
      end:      { x: 210, y: 380 }
  - direction: N11 -> S2
    kind: EEdge
    stored:
      start:    { x: 205, y: 372 }
      control1: { x: 157, y: 399 }
      control2: { x: 146, y: 384 }
      end:      { x: 90,  y: 440 }
```

The action rectangles must be obtained from the same badge-layout function used for rendering at test font size; they must not be copied from a screenshot. This keeps the fixture valid if text measurement is deliberately adjusted.

Required assertions:

- The two stored routes are detected as conflicting with the N11 action area.
- Both selected routes have zero action-body, action-arrow, and action-label conflicts.
- Neither selected route has an unrelated-node conflict.
- The pair has zero edge touches and zero shared-path length outside endpoint exclusion zones.
- The pair has zero crossings.
- Each route meets the configured action and edge clearances.
- Primary docks are used unless the measured geometry proves that only secondary docks are clear.
- Repeating the calculation produces byte-for-byte equivalent control coordinates and provenance.
- Changing selection, activity progress, timeout progress, or viewport zoom does not change the routes.

## Geometry test catalogue

These test names can be created before Svelte integration:

```text
cubicRectangleDistance_rejectsIntersection
cubicRectangleDistance_rejectsTangentialTouch
cubicRectangleDistance_acceptsClearanceAboveThreshold
arrowRectangleIntersection_countsArrowConflict
curvePairMeasurement_detectsProperCrossing
curvePairMeasurement_detectsNearParallelTouch
curvePairMeasurement_measuresSharedPathLength
curvePairMeasurement_ignoresAllowedEndpointZone
dockProjection_usesEllipseBoundary
dockProjection_usesPowerFiveSuperellipseBoundary
candidateSelection_preservesClearStoredRoute
candidateSelection_prefersSafetyOverPrimaryDock
candidateSelection_prefersPrimaryDockWhenEquallySafe
candidateSelection_usesSecondaryDockWhenPrimaryDocksFail
candidateSelection_isDeterministic
reciprocalRouting_separatesDirections
siaDemoS2N11_avoidsActionsAndPairConflicts
```

The web package currently has no dedicated test framework. Geometry tests can initially use Node's built-in `node:test` and strict assertions without adding a runtime dependency. Test integration should be decided after checking any concurrent changes to `package.json`.

## SceneFlowView integration inventory

After the handover, confirm the current names and signatures before editing. The previously identified consumers that must use `RoutedEdge` together are:

- Edge SVG path generation.
- Arrowhead geometry and target trimming.
- End tangent and activity animation.
- Visual length and midpoint.
- Edge label layout, position, and bounds.
- Selection bounds and hit path.
- Bend/control handle positions.
- Overall SceneFlow world bounds and export.

Command badge rectangles should come from the existing computed badge layout rather than DOM queries. Type badges are obstacles only while rendered. Drag markers, selection glows, control handles, and transparent hit strokes are transient UI and are not routing obstacles.

The minimap may keep its simplified center-to-center lines in version one, but its world box must include the selected routed-edge bounds.

## Interaction invariants

- Edge control dragging displays direct interactive geometry and remains responsive.
- Node dragging may use lightweight translated geometry; run full routing after release.
- Edge creation and retarget previews may remain simple during the gesture.
- Selection, hover, activity, and timeout animation never recalculate routes.
- Labels and arrowheads always follow the selected route.
- SVG export uses the selected displayed route automatically.
- No automatic display calculation calls an update command or changes undo/redo history.

## Performance instrumentation to add with implementation

Record in development builds or a temporary debug mode:

- Number of stored routes accepted without search.
- Number of candidate routes generated and exactly measured.
- Number rejected by broad-phase bounds.
- Time per affected edge group and total routing pass.
- Number and type of remaining conflicts.
- Route recalculation reason.

Before setting a hard time budget, capture baselines for a small, medium, and large real SceneFlow. The non-negotiable performance assertions are that animation frames do not invoke the router and that candidate enumeration is bounded.

## Post-handover start procedure

When concurrent editor work is finished:

1. Record the exact commit and `git status` supplied by the other worker.
2. Review their diff for changes to `SceneFlowView.svelte`, `App.svelte`, snapshot JSON, edge IDs, edge update commands, node/action layout, and web test tooling.
3. Reconfirm the integration inventory above against the new source.
4. Run the existing baseline web build and applicable tests before routing edits.
5. Open the SIA Demo and capture a fresh baseline screenshot.
6. Add the pure geometry module and geometry tests first.
7. Integrate obstacle extraction and current-route measurement.
8. Add candidate generation and reciprocal-group selection.
9. Replace all edge geometry consumers with the shared `RoutedEdge` cache in one integration step.
10. Verify interactions, bounds, minimap, and export.
11. Run the SIA fixture and visual acceptance check.

Suggested commit boundaries:

```text
add edge-routing geometry primitives and tests
measure sceneflow edge obstacles
route conflicting sceneflow edge pairs
use routed geometry throughout sceneflow view
add SIA edge-routing regression coverage
```

## Definition of ready

Implementation can begin immediately when:

- Concurrent changes have a known handover commit or diff.
- The current web UI builds before routing edits.
- The SIA Demo still reproduces the collision.
- The route-selection state policy above remains agreed.
- Any concurrent changes to badge layout, edge identity, or edge rendering have been reconciled with this contract.
