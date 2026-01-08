# SceneMaker Web UI Plan

This plan documents the migration from the Swing editor to the web-based editor. It captures the agreed architecture, what is already delivered, and the remaining work so we can resume later without re-deriving context.

Legend:
- Done: implemented and working in the web UI.
- Next: planned and not yet implemented.

## Decisions and constraints
- Web UI stack: Svelte + Vite.
- Server: Javalin in core (`de.dfki.vsm.web`), started from `SceneMaker3`.
- LAN access: token enforced for `/api` and `/ws` only.
- Startup: binds to `127.0.0.1` by default; `--allow-lan` enables `0.0.0.0`. Auto-opens the browser unless `--no-browser`.
- Graph editing: custom SVG/Canvas (no external graph lib).
- Code editor: CodeMirror 6.
- Swing editor remains for reference only (no sync work).

## Phase 0: Infrastructure and delivery
Done
- Javalin server bootstrapped in `SceneMaker3`, token logging, LAN reachability.
- Static web assets served from classpath (`/web-ui`).
- WebSocket command framework with request/response + events.
- Gradle tasks for `webUiInstall` and `webUiBuild`.

## Phase 1: Web UI shell and layout
Done
- Svelte app shell with three main regions: left building blocks + scenes, center SceneFlow canvas, right inspector.
- Scene Script section positioned below SceneFlow and aligned to width.
- Project dialogs + preferences reachable from web UI.

## Phase 2: SceneFlow canvas (core editing)
Done
- Render nodes, edges, comments with Swing-aligned styling.
- Grid dots, snap-to-grid, zoom/pan/fit, minimap, and viewport constraints.
- Node/edge creation via block panel and edge creation flow.
- Edge routing, arrowheads, control-point updates, and probability manager (sum=100).
- Comment editing, moving, resizing, and styling (banana handle).
- Command visualization as text or dots, toggleable.
- Start node indicators and supernode sizing by child count.

## Phase 3: Inspector and definitions
Done
- Node, edge, comment inspection/editing.
- Type definitions, variable definitions, command executions (node + sceneflow).
- Command execution modal dialog (inline edit).
- Start-node toggle in inspector; history node protected.

## Phase 4: Scenes and script data
Done
- Scene list, filtering, drag handle icon.
- Drag scene onto node to add command.
- Drag scene onto canvas to create a node with PlayScene command.
- Script editor with CodeMirror 6 plus diagnostics/highlighting.

## Phase 5: Selection and clipboard
Done
- Box selection and multi-select (nodes + comments).
- Group drag and local selection state.
- Deep-copy clipboard on server (supernode contents included).
- Copy, paste, cut, duplicate.
- Start-node flags preserved on paste/cut/undo.

## Phase 6: Multi-selection inspector (summary)
Done
- Summary view in inspector for multi-selection with counts and preview lists.
- Quick actions: copy, cut, duplicate, delete.

## Phase 7: Runtime and variable display
Done
- Runtime controls (start/pause/stop) in SceneFlow + Scene Script toolbars.
- Variable badges for local/global variables in the SceneFlow canvas (toggleable).

## Next steps
Next
1) Bulk actions for multi-selection.
   - Align/distribute nodes.
   - Mass toggles (start node, delete, etc.).
2) Expand edge editing.
   - More control-point tools and UX affordances.
   - Better edge selection handles.
3) SceneFlow preferences and project configuration.
   - Expose SceneFlow preference handling (grid, sizes, etc.).
   - Project configuration editing in the web UI.
4) Remaining Swing parity checks.
   - Verify all Swing dialogs/features are represented.
   - Validate any missing inspector fields or metadata.
5) Hardening & QA.
   - Error handling in WebSocket flows.
   - Validate undo/redo coverage for complex operations.
6) Final web UI / app design pass.
   - Visual polish, spacing, iconography, and consistency sweep.
7) Cutover prep.
   - Swing editor deprecation toggle.
   - Documentation + release checklist.
8) Android headless hosting feasibility.
   - Refactor web server to depend only on core/runtime (no Swing/editor classes).
   - Identify Android-compatible HTTP/WebSocket server alternative to Javalin/Jetty.
   - Package web UI assets for Android (classpath/assets) and define launch flags.
9) Agents palette follow-ups.
   - Read agent types (input/processing/output) from project.xml instead of defaulting to processing.
   - Map agents to their configured commands in project.xml (replace PlayAction placeholder).
   - Expand agent drag/drop to respect per-agent type + command metadata.
