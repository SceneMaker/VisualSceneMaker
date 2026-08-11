# SceneFlow Automatic Edge Routing Plan

## Goal

Improve automatic edge routing in the web SceneFlow editor so that edges remain readable and do not touch, cover, or pass through node actions. The router should also reduce crossings, touching, and shared paths between edges while preserving existing routes that are already clear.

The motivating case is the pair of reciprocal edges between supernode `S2` (`controls`) and node `N11` (`do emotion`) in the SIA Demo project. The current curves pass through or touch the action badges rendered below `N11`. A better route approaches `N11` from the side and keeps the two directions visually separated.

The first implementation will be local to the web UI and will preserve the established SceneFlow representation of one cubic Bézier per edge.

The companion implementation contract and handover checklist is in `sceneflow-edge-routing-implementation-preparation.md`.

## Status: experimental implementation rolled back

The first automatic-routing implementation was rejected and removed after testing with the complete SIA Demo. It produced false-positive collisions, changed unrelated authored routes, delayed project display, and degraded node-dragging feedback. The web UI currently uses the previous stored-route renderer.

The next attempt must not start with automatic routing. It must pass the constraints and staged gates below first.

## Constraints for a future attempt

### 1. Stored geometry is authoritative

- Opening a project must display its stored edge anchors and controls exactly.
- Moving a node must continue to use the existing lightweight endpoint/control adjustment.
- No collision detector or candidate search runs on project load or pointer movement.
- Unrelated edges must remain geometrically identical after an edit.
- A suggested route is display-only until the user explicitly accepts it.

### 2. Detector before router

Build and validate a collision detector as a separate first milestone. It may report or highlight collisions but must not change a route.

For the first milestone, a hard collision means only one of these:

- The visible stroked curve intersects or touches a rendered node-action badge.
- The visible arrowhead intersects or touches a rendered node-action badge.

Edge labels, type badges, start markers, nearby edges, crossings, and endpoint-node proximity must not initiate a correction in this milestone. They can be measured later as diagnostics or used to rank an explicitly requested suggestion.

The detector and renderer must consume the same action rectangles, curve, arrowhead, stroke width, and coordinate transformation. A browser-level test must compare the calculated rectangles with the rendered SVG/DOM bounds. Separate approximations are not acceptable.

### 3. Explicit per-edge suggestion

Only after the detector has no false positives on the reference fixtures should routing be exposed as an explicit action for a selected edge or reciprocal pair, for example `Suggest route`.

- Route only the selected unordered node pair.
- Never perform a graph-wide improvement pass.
- Existing edge-edge crossings or proximity do not trigger the operation.
- Other edges are read-only obstacles or soft scoring inputs.
- Show a preview and require explicit acceptance before changing stored controls.
- Canceling the preview restores the exact previous geometry.

### 4. Reference invariants

The complete SIA Demo is a required golden fixture:

- `S1`–`N1`–`N2` remains exactly unchanged.
- `S2`–`N12` remains exactly unchanged.
- `S3`–`N15` remains exactly unchanged.
- The detector reports the intended curve/arrow collisions for `S2`–`N11` only.
- A requested suggestion for `S2`–`N11` avoids the action badges and keeps its reciprocal edges readable.

These assertions must use the project’s actual `70 × 70` node configuration and real rendered command layouts. Screenshot-derived or guessed obstacle rectangles are insufficient.

### 5. Performance gates

- Feature idle: no measurable project-load regression and no work on pointer-move frames.
- Collision detection for the complete SIA graph: at most `16 ms` at the 95th percentile after warm-up.
- Selected-pair suggestion: at most `50 ms` at the 95th percentile, or it must run outside the main thread with a non-blocking preview.
- Node dragging must maintain a 60 Hz frame budget; the routing feature cannot participate in drag-frame computation.

### 6. Promotion gate

Automatic correction on load or after node movement remains out of scope until explicit suggestions have been used successfully on multiple real projects, golden browser screenshots are stable, there are zero false-positive route changes, and the performance gates pass. Even then it should be opt-in before becoming a default.

## Current implementation

The relevant rendering code is in `editor/web-ui/src/SceneFlowView.svelte`:

- `nodeCommandBadgesLayout()` and `commandBadgesWorld()` calculate the positions and dimensions of node action badges.
- `edgePoints()` resolves the stored edge points and adjusts endpoints during interaction.
- `edgeCurveControls()` resolves the two Bézier controls.
- `edgePath()` renders a normal edge as one cubic Bézier.
- Arrowheads, labels, activity animation, selection, and hit testing derive from that path.

The server-side `core-webserver/.../EdgeLayoutService.java` allocates docking points and normalizes control handles based on node positions and the source-target direction. It does not know the dimensions of action badges rendered by the web UI, so it cannot currently avoid them.

## 1. Extract routing geometry

Create a pure JavaScript module, provisionally:

```text
editor/web-ui/src/sceneFlowEdgeRouter.js
```

The module should contain:

- Cubic Bézier evaluation and adaptive subdivision
- Rectangle inflation
- Point-to-rectangle and segment-to-rectangle distance
- Segment-rectangle intersection
- Segment-segment intersection and distance
- Route candidate generation
- Candidate measurement and deterministic selection

Keeping this logic outside `SceneFlowView.svelte` makes it independently testable and avoids adding more geometry code to an already large component.

## 2. Represent obstacles and edge footprints

### Node and action obstacles

For every displayed node, construct:

- Its visual node body
- A rectangle for every action badge returned by `commandBadgesWorld()`
- Optionally, a union rectangle for a tightly packed row of action badges
- An inflated rectangle around every action obstacle to provide visual clearance

The source and target node bodies are exempt when checking their connecting edge because the edge must attach to them. Their action badges are not exempt. For example, an edge may terminate at `N11`, but it may not pass through the actions displayed beneath `N11`.

Every unrelated node body is an obstacle.

### Edge footprint

An edge's visual footprint consists of:

- Its stroked centerline
- Its arrowhead triangle
- Its condition, probability, or timeout label rectangle

Collision checks must use the complete footprint. Testing only the mathematical centerline could still allow the stroke, arrowhead, or label to cover an action.

All geometry is measured in SVG world coordinates so that routing results do not change with viewport zoom.

## 3. Collision and clearance criteria

An inflated obstacle turns touching into a collision, not merely an overlap. Initial clearance values should be derived from the displayed font size and then tuned with visual tests:

```text
action clearance = max(6, 0.35 * fontSize)
edge clearance   = max(4, 0.25 * fontSize)
```

The following criteria define conflicts:

| Conflict | Measurable criterion |
| --- | --- |
| Edge versus action | The minimum distance from the edge centerline to the action rectangle is less than or equal to half the edge stroke width plus the action clearance. |
| Arrow versus action | The arrowhead triangle touches or intersects the action rectangle inflated by the action clearance. |
| Edge label versus action | The edge-label rectangle touches or intersects the action rectangle inflated by the action clearance. |
| Edge versus unrelated node | The buffered edge centerline, arrowhead, or edge label touches or intersects the unrelated node's visual body. |
| Edge crossing | Flattened segments of two edges have a proper intersection outside an allowed shared-endpoint area. |
| Edge touching another edge | The minimum centerline distance is less than or equal to half the combined stroke widths plus the edge clearance. |
| Edge overlap/shared path | Two edges remain within the minimum edge-separation distance for a significant continuous arc length. |
| Edge versus another edge's label | The buffered centerline or arrowhead intersects the other edge's label rectangle. |

### Curve approximation tolerance

Collision measurement will flatten each cubic Bézier by recursive subdivision. Subdivision continues until:

- The maximum chord approximation error is at most approximately `0.5` world units; and
- No resulting line segment exceeds a configured maximum length.

The obstacle inflation includes the approximation tolerance. This makes a reported non-collision conservative: approximation error should not hide a visual touch.

### Definition of action-clear

A route is clear of an action only when all of the following hold:

```text
minimum curve-to-action distance
    > edgeStrokeWidth / 2 + actionClearance

arrowhead does not intersect inflated action rectangle

edge label does not intersect inflated action rectangle
```

Equality is a collision. An edge that just touches an action is therefore rejected.

### Definition of edge-edge clear

Two edges are clear of one another outside allowed endpoint areas when:

- They have no proper segment intersections.
- Their minimum centerline distance exceeds half their combined stroke widths plus the edge clearance.
- They do not run within that minimum distance for a measurable continuous length.
- Neither edge's curve or arrowhead intersects the other edge's label.

Edges sharing a source or target may meet within a small endpoint exclusion zone because some convergence can be unavoidable. After leaving that zone they must satisfy normal separation criteria. Reciprocal edges should preferably use different docking points and should separate immediately.

## 4. Measure the existing route first

The stored route is always the first candidate:

1. Resolve its displayed endpoints and control handles.
2. Flatten the cubic with the defined tolerance.
3. Measure action, unrelated-node, edge-label, crossing, touching, and overlap conflicts.
4. Keep it unchanged if it has no hard curve/arrow collision. Existing label or edge-edge proximity alone does not invalidate authored geometry.

This minimizes visual churn and preserves authored layouts that are already readable.

## 5. Generate alternative single-cubic routes

When the current route collides, generate alternatives from combinations of:

- Source docking points
- Target docking points
- Outward control directions at both endpoints
- Several control-handle lengths relative to node size and source-target distance
- Left/right lane offsets around the source-target axis
- Progressive adjustments of the existing controls

The 24 docking directions already used by `EdgeLayoutService` provide a useful basis. Candidate generation should prioritize faces oriented toward the other node but must consider side and upper approaches whenever the direct face is obstructed by actions.

In the S2/N11 case, candidates that approach the lower side of `N11` should collide with the action obstacle and be rejected. A left-side approach similar to the reference image should receive a better score.

### Preferred connection points and visual aesthetics

The automatic router should favor a small set of visually meaningful connection points instead of treating all positions on a node boundary as equally attractive.

The preferred set consists of eight evenly distributed positions:

- The center of the top side
- The center of the right side
- The center of the bottom side
- The center of the left side
- The four intermediate boundary positions halfway between adjacent side centers

For circular and elliptical nodes, these are the eight compass directions at 45-degree intervals. For supernodes and aliases, each direction is projected onto the actual rounded/superellipse boundary, rather than onto an enclosing rectangle. The current 24-point docking model already contains these directions. With its current indexing, the preferred points are `0`, `3`, `6`, `9`, `12`, `15`, `18`, and `21`.

These eight points form the primary docking tier. The remaining 16 existing docking points form a secondary tier and are considered when:

- Every suitable preferred point creates an action or node collision.
- Preferred points cannot provide the required edge-edge clearance.
- Several edges would otherwise use the same point or become visually congested.
- A manual route already uses a non-preferred point and remains collision-free.

Within the preferred tier, first consider points whose outward direction is closest to the source-target direction. Side or diagonal points that produce a clearer route may win when the most direct point is obstructed.

The following aesthetic measurements should be included after collision measurements:

| Aesthetic property | Measurement |
| --- | --- |
| Preferred docking | Zero penalty for one of the eight preferred points; a configurable penalty for a secondary point. |
| Direction relevance | Angular difference between the docking point's outward normal and the direction toward the other node. |
| Tangent alignment | Angular difference between the endpoint control handle and the outward normal at the docking point. |
| Balanced handles | Difference between source and target handle lengths after accounting for different node sizes. |
| Unnecessary curvature | Excess curve length and total tangent rotation relative to a direct connection. |
| Pair symmetry | Difference from the mirrored or intentionally offset geometry selected for a reciprocal/parallel edge pair. |
| Dock congestion | Number and proximity of other endpoints already using the same preferred region. |

Control handles should normally leave and enter a node along the local outward normal. This produces deliberate-looking curves and avoids an edge appearing to slide tangentially along the node boundary. The router may relax tangent alignment when required to clear actions or other edges.

When multiple edges need the same boundary region, distribute them symmetrically around the closest preferred point. Prefer a sequence such as center, equal offset on one side, equal offset on the other side, rather than filling points in one direction. Reciprocal edges should use separate but visually related docking points.

Aesthetic preferences are soft constraints. They may decide between routes with equal readability, but they must never cause an action collision, node collision, edge overlap, or avoidable edge crossing.

## 6. Route reciprocal and parallel edges together

Edges connecting the same unordered node pair must be treated as a group. Optimizing them independently could make both choose the same safe curve.

For a reciprocal pair such as S2 to N11 and N11 to S2:

- Assign deterministic opposite lane preferences around the node-pair axis.
- Prefer different source and target docking points.
- Score the candidate pair for crossings, touching, and shared-path length.
- Place labels using the selected curves and include their rectangles in the group score.

The group router should select the best combination, not just the independently best route for each edge.

## 7. Score candidates deterministically

Use a lexicographic score rather than a single loosely weighted sum. Compare candidates in this order:

1. Action conflicts from the complete edge footprint: curve, arrowhead, and label
2. Unrelated-node conflicts from the complete edge footprint
3. Edge touching outside allowed endpoint areas
4. Edge shared-path/overlap length
5. Number of proper edge crossings
6. Total clearance shortfall
7. Conflicts with other edge labels
8. Preferred docking tier and dock congestion
9. Dock-direction and tangent-alignment penalties
10. Pair symmetry and handle balance
11. Route length
12. Curvature complexity
13. Difference from the previous route

This ordering guarantees that a shorter path cannot win simply because it passes through an action. When scores are otherwise equal, prefer:

1. The existing route
2. The shorter route
3. The smoother route
4. A stable edge-ID-based tie-breaker

The final tie-breaker makes results repeatable and prevents routes from switching between equivalent candidates after unrelated UI updates.

## 8. Handle unavoidable conflicts

Some dense graphs cannot be drawn without any edge crossings. The router should distinguish hard readability obstacles from softer graph conflicts:

- Action and unrelated-node collisions are hard failures for a candidate.
- Edge overlap is more damaging than one short, clear crossing.
- Crossings and insufficient edge-edge clearance are minimized globally when they cannot be eliminated.

If no tested single cubic satisfies every constraint, choose the deterministic route with the lowest lexicographic conflict score and retain internal measurements of the remaining conflicts for debugging. An edge crossing is preferable to covering a node action.

The candidate search may expand its control-handle range and docking choices before accepting a conflicting fallback.

## 9. Multi-curve fallback: compatibility finding and deferred work

A general multi-curve fallback is **not part of the first implementation**.

### What the current model permits mechanically

`EdgeArrow` stores an `ArrayList<EdgePoint>`. Its XML parser accepts every `<ControlPoint>` child of `<Connection>`, and its XML writer serializes every point in that list. The web snapshot builders and `SceneFlow.Edge.Update` also preserve arbitrary point arrays.

This means that XML parsing and serialization do not enforce a two-point count in the current Java implementation.

### Why support is not established

Despite the unrestricted list:

- Every checked `sceneflow.xml` connection in the repository contains exactly two `<ControlPoint>` elements.
- No SceneFlow XSD defining a multi-point interpretation is present in the repository.
- `SceneFlowView.svelte` currently renders only the first and last point as one cubic Bézier.
- Server normalization and node-move logic treat the first point as the source endpoint and the last point as the target endpoint; intermediate points have no routing semantics there.
- An `EdgePoint` contains one anchor and one control coordinate. A general smooth join between two cubic segments normally needs independent incoming and outgoing control handles at the waypoint. The current structure does not express those handles unambiguously.

Therefore, the fact that extra XML elements survive a round trip is not sufficient evidence that they represent a portable or compatible multi-curve edge.

### Required compatibility spike before multi-curve support

Before adding a multi-curve fallback, complete a separate design and compatibility task:

1. Define the exact meaning of three or more `ControlPoint` elements.
2. Decide how independent incoming and outgoing waypoint handles are represented.
3. Create a fixture containing a multi-segment edge.
4. Verify load-save-load preservation.
5. Verify web snapshot and update round trips.
6. Verify normalization, node movement, retargeting, copy/paste, undo/redo, Android snapshots, and any legacy editor consumers.
7. Add renderer, arrow, label-position, hit-test, animation, selection, and manual-edit support for segmented routes.
8. Decide whether the established XML format can be extended compatibly or needs an explicit new representation/version.

Until that work is complete, automatic routing must generate one cubic represented by the established two endpoint/control records. If no collision-free single cubic exists, the router uses the least-conflicting deterministic single-cubic fallback described in section 8.

## 10. Integrate one shared routed-path model

`SceneFlowView.svelte` currently derives the path, arrow direction, label position, hit target, activity animation, and handles through several functions. They must all consume the same selected route object so that visual and interactive geometry cannot disagree.

The route object should provide at least:

```text
edge ID
source and target anchors
first and second Bézier controls
SVG path data
flattened measurement segments
arrow tangent
label anchor and bounds
collision measurements
route provenance (existing or automatic candidate)
```

Cache routes for the current snapshot and invalidate only affected routes where practical.

## 11. Recalculation and stability

Recalculate routing when:

- A SceneFlow level is loaded
- A node moves or changes size
- Actions are added, removed, reordered, or renamed
- An edge is created, deleted, retargeted, or normalized
- Font or node display sizing changes

During node dragging, the displayed route is deformed with the moving endpoint in constant time. A bounded local calculation runs when dragging ends; the full graph is not globally optimized.

Candidate ordering and tie-breaking must remain deterministic. Edge-edge conflicts influence a replacement candidate but do not trigger global improvement passes.

## 12. Preserve clear stored routing

Automatic routing must not fight useful stored geometry:

- The stored route is always measured before alternatives are generated.
- A collision-free stored or manually edited route remains displayed unchanged.
- Dragging an edge control temporarily bypasses automatic routing and displays the interactive geometry directly.
- After editing ends, the edited route is measured by the same criteria as every other stored route. It remains unchanged when clear; when it still collides, the UI may display a derived automatic route.
- Loading or rerouting a project must not silently write derived display geometry back to the project.
- A later persistence step may store the chosen automatic controls only through an explicit operation such as Normalize Edge.

The first implementation therefore does not require a durable manual/automatic flag. Route preservation is determined from the stored geometry and measurable collision state. A strict permanent manual lock can be added later if the model gains stable edge identity or explicit routing metadata.

## 13. Verification and acceptance criteria

### SIA Demo S2/N11 acceptance case

The result is accepted when:

- S2 to N11 has zero action collisions.
- N11 to S2 has zero action collisions.
- Neither arrowhead touches an action badge.
- Neither edge label touches an action badge.
- The two edges do not cross.
- The two edges do not touch or share a path outside endpoint exclusion zones.
- Both routes maintain the configured minimum clearance.
- Collision-free routes use the preferred eight connection regions unless congestion or clearance requires a secondary point.
- Endpoint tangents are aligned with the local node-boundary normals unless relaxing them measurably improves clearance.
- The result is identical across repeated loads.
- Moving either node produces a deterministic recalculation.

### Regression cases

Tests should also verify:

- A straight edge without nearby obstacles remains unchanged.
- Existing collision-free curves remain unchanged.
- Multiple actions arranged across several rows are avoided.
- Reciprocal and parallel edges remain separated.
- Unobstructed edges select the expected preferred connection points.
- Multiple edges near one preferred point are distributed symmetrically.
- Self-loops retain their specialized routing.
- Unavoidable crossings are minimized consistently.
- Manual control-point dragging remains usable.
- Selection, hit testing, arrow direction, activity animation, timeout progress, and edge-label placement use the selected routed path.

### Unit-level measurements

The pure routing module should have deterministic fixtures for:

- Curve-rectangle intersection and tangential touching
- Curve clearance just below, equal to, and above the configured threshold
- Arrow-triangle intersection
- Proper edge crossings
- Near-touching parallel curves
- Shared-path length
- Allowed shared endpoint areas
- Candidate ordering and stable tie-breaking
- Preferred-versus-secondary docking selection
- Outward-normal tangent alignment
- Symmetric dock distribution under congestion
- Reciprocal-edge group selection

## 14. Suggested implementation order

1. Extract and unit-test geometry primitives.
2. Build action and node obstacles from the existing display layout.
3. Measure and optionally visualize collisions for current routes.
4. Add single-edge single-cubic candidate generation, including preferred docking points and boundary normals.
5. Add deterministic lexicographic scoring, including the soft aesthetic measurements.
6. Integrate a shared routed-path object into rendering and interaction.
7. Add reciprocal/parallel edge group routing.
8. Add bounded global edge-edge improvement passes.
9. Add manual-route locking and Normalize integration.
10. Validate against the SIA Demo and regression fixtures.
11. Consider the separate multi-curve compatibility spike only if single-cubic routing proves insufficient in real projects.
