<script>
  import { onDestroy, onMount, tick } from "svelte";
  export let snapshot = null;
  export let onNavigate = null;
  export let onNodeMove = null;
  export let onNodeGroupMove = null;
  export let onCommentUpdate = null;
  export let onEdgeControlUpdate = null;
  export let onEdgeRetarget = null;
  export let onDeleteSelection = null;
  export let onUndo = null;
  export let onRedo = null;
  export let edgeCreateMode = false;
  export let edgeCreateSourceId = "";
  export let edgeCreateType = "EEDGE";
  export let onEdgePick = null;
  export let onSceneDrop = null;
  export let sceneDragType = "application/x-vsm-scene";
  export let onAgentDrop = null;
  export let agentDragType = "application/x-vsm-agent";
  export let onBlockDrop = null;
  export let blockDragType = "application/x-vsm-block";
  export let showCommandText = true;
  export let onCommandOpen = null;
  export let worldBox = null;
  export let viewBoxState = null;
  export let config = null;
  export let selection = null;
  export let multiSelection = [];
  export let nodeSnapToGrid = true;
  export let onCopySelection = null;
  export let onPasteSelection = null;
  export let onCutSelection = null;
  export let onDuplicateSelection = null;
  export let onTimeoutEdgeUpdate = null;
  export let activityNodes = [];
  export let activityEdges = [];
  export let timeoutEdges = [];
  export let runtimeValues = {};
  export let runtimeState = "stopped";

  const DEFAULT_NODE_SIZE = 90;
  const DEFAULT_FONT_SIZE = 16;
  const MIN_WORLD_COORD = 1;
  const COLORS = {
    node: "#7a7d81",
    history: "#ffffff",
    text: "#ffffff",
    textHistory: "#000000",
    startSign: "#e84a4f",
    altStartSign: "#c0c0c0",
    selected: "#5b8fdc",
    commentFill: "rgba(232, 232, 232, 0.9)",
    commentText: "#4f5864",
    edges: {
      eedge: "#7a7d81",
      fedge: "#5b8edc",
      tedge: "#a06a4b",
      cedge: "#ffc857",
      pedge: "#5bae7a",
      iedge: "#e26d5a"
    }
  };

  const padding = 80;
  const minCanvasWidth = 680;
  const minCanvasHeight = 420;
  const minZoom = 0.3;
  const maxZoom = 3.5;
  const zoomStep = 1.12;
  const SUPER_NODE_SHAPE_POWER = 5;
  const TIMEOUT_INLINE_LEFT_MAX_MS = 5000;
  const TIMEOUT_INLINE_MAX_MS = 60000;
  const TIMEOUT_INLINE_SLIDER_MAX = 1000;
  const TIMEOUT_INLINE_HALF = TIMEOUT_INLINE_SLIDER_MAX / 2;
  const COMMAND_FONT_FAMILY = '"SansSerif", "Helvetica Neue", Arial, sans-serif';
  const EXPORT_STYLE_PROPS = [
    "fill",
    "fill-opacity",
    "stroke",
    "stroke-width",
    "stroke-linecap",
    "stroke-linejoin",
    "stroke-dasharray",
    "stroke-dashoffset",
    "stroke-opacity",
    "opacity",
    "font-family",
    "font-size",
    "font-style",
    "font-weight",
    "letter-spacing",
    "text-anchor",
    "dominant-baseline",
    "paint-order",
    "filter",
    "mix-blend-mode",
    "background-color",
    "background-image",
    "background-size",
    "background-position"
  ];

  function edgePairKey(edge) {
    const a = (edge?.sourceId || "").trim();
    const b = (edge?.targetId || "").trim();
    if (!a || !b) return "";
    return a < b ? `${a}|${b}` : `${b}|${a}`;
  }

  function buildEdgeLabelOffsets(list) {
    const map = new Map();
    if (!Array.isArray(list)) return map;
    const groups = new Map();
    for (const edge of list) {
      const key = edgePairKey(edge);
      if (!key) continue;
      if (!groups.has(key)) {
        groups.set(key, []);
      }
      groups.get(key).push(edge);
    }
    for (const group of groups.values()) {
      if (!group.length) continue;
      if (group.length === 1) {
        map.set(group[0].id, 0);
        continue;
      }
      const sorted = [...group].sort((a, b) => String(a.id || "").localeCompare(String(b.id || "")));
      sorted.forEach((edge, idx) => {
        const step = Math.floor(idx / 2) + 1;
        const sign = idx % 2 === 0 ? 1 : -1;
        map.set(edge.id, sign * step);
      });
    }
    return map;
  }

  $: nodes = snapshot?.nodes || [];
  $: edges = snapshot?.edges || [];
  $: comments = snapshot?.comments || [];
  $: nodeMap = new Map(nodes.map((node) => [node.id, node]));
  $: outgoing = new Set(edges.map((edge) => edge.sourceId).filter(Boolean));

  $: nodeWidth = readNumber(config?.node_width ?? config?.["node_width"], null);
  $: nodeHeight = readNumber(config?.node_height ?? config?.["node_height"], nodeWidth);
  $: workspaceFontSize = readNumber(
    config?.workspace_fontsize ?? config?.["workspace_fontsize"],
    null
  );
  $: gridScaleX = readNumber(config?.grid_x ?? config?.["grid_x"], 1);
  $: gridScaleY = readNumber(config?.grid_y ?? config?.["grid_y"], 1);
  $: gridEnabled = readBoolean(config?.grid ?? config?.["grid"], true);
  $: activityEnabled = readBoolean(config?.visualization ?? config?.["visualization"], true);
  $: activityNodeSet = activityEnabled ? new Set(activityNodes || []) : new Set();
  $: activityEdgeMap = activityEnabled
    ? new Map(
        (activityEdges || [])
          .filter((entry) => entry && entry.id)
          .map((entry) => [entry.id, entry])
      )
    : new Map();
  $: timeoutEdgeMap = activityEnabled
    ? new Map(
        (timeoutEdges || [])
          .filter((entry) => entry && entry.id)
          .map((entry) => [entry.id, entry])
      )
    : new Map();
  $: edgeLabelOffsets = buildEdgeLabelOffsets(edges);
  $: baseNodeSize = Number.isFinite(nodeWidth)
    ? nodeWidth
    : (guessNodeSize(nodes) ?? DEFAULT_NODE_SIZE);
  $: nodeStrokeWidth = Math.max(1, baseNodeSize / 25);
  $: edgeStrokeWidth = Math.max(1, baseNodeSize / 30) * 1.34;
  $: fontSize = Number.isFinite(workspaceFontSize) && workspaceFontSize > 0
    ? workspaceFontSize
    : Math.max(10, Math.round(baseNodeSize * 0.18));
  $: labelLineHeight = Math.max(10, Math.round(fontSize * 1.2));
  $: commandLineHeight = labelLineHeight;
  $: labelVerticalOffset = Math.round(labelLineHeight * 0.12) + 2;
  $: commandPaddingX = Math.max(6, Math.round(fontSize * 0.5));
  $: commandPaddingY = Math.max(4, Math.round(fontSize * 0.35));
  $: commandGap = Math.max(4, Math.round(fontSize * 0.5));
  $: commandCornerRadius = Math.max(4, Math.round(fontSize * 0.6));
  $: commentMinSize = Math.max(50, Math.round(baseNodeSize * 0.5));
  $: showNodeIds = readBoolean(config?.shownodeid ?? config?.["shownodeid"], true);
  $: gridNodeWidth = nodeWidth || baseNodeSize;
  $: gridNodeHeight = nodeHeight || baseNodeSize;
  $: gridX = Math.max(8, readNumber(config?.grid_space_x ?? config?.["grid_space_x"], baseNodeSize * gridScaleX));
  $: gridY = Math.max(8, readNumber(config?.grid_space_y ?? config?.["grid_space_y"], baseNodeSize * gridScaleY));
  $: gridOriginX = gridNodeWidth / 2 + gridNodeWidth / 3;
  $: gridOriginY = gridNodeHeight / 2 + gridNodeHeight / 3;
  // With 1:1 pixel mapping at zoom=1, uniformScale equals zoomLevel
  $: uniformScale = zoomLevel;
  $: gridScreenX = gridX * uniformScale;
  $: gridScreenY = gridY * uniformScale;
  $: viewOriginX = panX;
  $: viewOriginY = panY;
  // No offset needed since viewBox matches canvas dimensions
  $: viewOffsetX = 0;
  $: viewOffsetY = 0;
  $: gridOffsetX = (gridOriginX - viewOriginX - gridX / 2) * uniformScale + viewOffsetX;
  $: gridOffsetY = (gridOriginY - viewOriginY - gridY / 2) * uniformScale + viewOffsetY;
  $: svgStyle = [
    `--sf-node-stroke:${nodeStrokeWidth.toFixed(2)}px`,
    `--sf-edge-stroke:${edgeStrokeWidth.toFixed(2)}px`,
    `--sf-font-size:${fontSize}px`,
    `--sf-grid-x:${gridScreenX}px`,
    `--sf-grid-y:${gridScreenY}px`,
    `--sf-grid-offset-x:${gridOffsetX.toFixed(2)}px`,
    `--sf-grid-offset-y:${gridOffsetY.toFixed(2)}px`,
    `--sf-grid-color:${gridEnabled ? "rgba(120, 120, 120, 0.7)" : "transparent"}`,
    `--sf-comment-fill:${COLORS.commentFill}`,
    `--sf-comment-text:${COLORS.commentText}`
  ].join(";");

  let viewportSize = { width: 0, height: 0 };
  let viewportObserver = null;

  $: bounds = computeBounds(nodes, edges, comments, showCommandText);
  $: baseBox = bounds.box;
  $: canvasWidth = Math.max(minCanvasWidth, bounds.width, viewportSize.width || 0);
  $: canvasHeight = Math.max(minCanvasHeight, bounds.height, viewportSize.height || 0);
  // Use canvas dimensions for viewBox to achieve 1:1 pixel mapping at zoom=1
  $: viewBox = viewBoxString(canvasWidth, canvasHeight, zoomLevel, panX, panY);
  $: if (baseBox) {
    clampPanToNonNegative();
  }

  let svgEl;
  let stageEl;
  export let zoomLevel = 1;
  let panX = 0;
  let panY = 0;
  let isPanning = false;
  let shiftDown = false;
  let selectedNodeId = null;
  let selectedEdgeId = null;
  let selectedCommentId = null;
  let activityNodeSet = new Set();
  let activityEdgeMap = new Map();
  let timeoutEdgeMap = new Map();
  let edgeLabelOffsets = new Map();
  let activityEnabled = true;
  let timeoutNow = Date.now();
  let timeoutFrame = null;
  let selectedNodeIds = new Set();
  let selectedEdgeIds = new Set();
  let selectedCommentIds = new Set();
  let selectionBox = null;
  let suppressStageClick = false;
  let editingCommentId = null;
  let editingCommentDraft = "";
  let editingCommentOriginal = "";
  let commentEditorEl = null;
  let hoveredCommentId = null;
  let hoveredEdgeId = null;
  let edgeCreateHoverId = null;
  let edgeRetargetHoverId = null;
  let edgeCreateCursor = null;
  let dragState = null;
  let timeoutInlineDragEdgeId = "";
  let timeoutSliderSuppressedEdgeId = "";
  let selfLoopManualControlIds = new Set();
  let panStart = { x: 0, y: 0 };
  let panOrigin = { x: 0, y: 0 };
  let lastSnapshotKey = "";
  const commentCornerRadius = 12;
  let commentMinSize = 50;
  const dragThreshold = 3;
  let textMeasureCtx = null;
  let lastTextMeasureSize = null;
  let timeoutInlineDrafts = new Map();

  $: worldBox = baseBox;
  $: viewBoxState = currentViewBox();

  $: if (snapshot) {
    const key = `${snapshot.projectId || ""}:${snapshot.superNodeId || ""}`;
    if (key !== lastSnapshotKey) {
      lastSnapshotKey = key;
      resetView();
      clearSelection();
      timeoutInlineDrafts = new Map();
    }
  }

  $: if (selection) {
    if (selection.type === "node") {
      selectedNodeId = selection.id;
      selectedEdgeId = null;
      selectedCommentId = null;
    } else if (selection.type === "edge") {
      selectedEdgeId = selection.id;
      selectedNodeId = null;
      selectedCommentId = null;
    } else if (selection.type === "comment") {
      selectedCommentId = selection.id;
      selectedNodeId = null;
      selectedEdgeId = null;
    }
  } else if (selection === null) {
    selectedNodeId = null;
    selectedEdgeId = null;
    selectedCommentId = null;
  }

  $: {
    const nodes = new Set();
    const comments = new Set();
    const edges = new Set();
    if (Array.isArray(multiSelection)) {
      for (const entry of multiSelection) {
        if (!entry || !entry.id) continue;
        if (entry.type === "node") {
          nodes.add(entry.id);
        } else if (entry.type === "edge") {
          edges.add(entry.id);
        } else if (entry.type === "comment") {
          comments.add(entry.id);
        }
      }
    }
    if (selection?.type === "node") {
      nodes.add(selection.id);
    } else if (selection?.type === "edge") {
      edges.add(selection.id);
    } else if (selection?.type === "comment") {
      comments.add(selection.id);
    }
    selectedNodeIds = nodes;
    selectedEdgeIds = edges;
    selectedCommentIds = comments;
  }

  $: if (selection) {
    const inMulti = Array.isArray(multiSelection)
      ? multiSelection.some((entry) => entry.type === selection.type && entry.id === selection.id)
      : false;
    if (!inMulti) {
      multiSelection = [{ type: selection.type, id: selection.id }];
    }
  } else if (selection === null && Array.isArray(multiSelection) && multiSelection.length) {
    multiSelection = [];
  }

  $: editingComment = editingCommentId ? findCommentById(editingCommentId) : null;
  $: editingCommentRect = editingComment ? commentRect(editingComment, dragState) : null;
  $: editingCommentScreenRect = editingCommentRect
    ? worldRectToScreenRect(editingCommentRect)
    : null;
  $: commentEditorStyle = editingCommentScreenRect
    ? `left:${editingCommentScreenRect.x}px; top:${editingCommentScreenRect.y}px; width:${editingCommentScreenRect.w}px; height:${editingCommentScreenRect.h}px;`
    : "";
  $: edgeCreateSource = edgeCreateMode && edgeCreateSourceId ? nodeMap.get(edgeCreateSourceId) : null;
  $: edgeCreateHover = edgeCreateMode && edgeCreateHoverId ? nodeMap.get(edgeCreateHoverId) : null;
  $: edgeCreatePreview = buildEdgeCreatePreview(edgeCreateSource, edgeCreateHover, edgeCreateCursor);
  $: edgeCreatePreviewColor = edgeColor({ type: edgeCreateType || "EEDGE" });

  $: if (!edgeCreateMode) {
    edgeCreateHoverId = null;
    edgeCreateCursor = null;
  }

  function updateViewportSize() {
    const host = stageEl?.parentElement;
    if (!host) return;
    const rect = host.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    viewportSize = { width: rect.width, height: rect.height };
  }

  onMount(async () => {
    await tick();
    updateViewportSize();
    const host = stageEl?.parentElement;
    if (!host || typeof ResizeObserver === "undefined") return;
    viewportObserver = new ResizeObserver(() => {
      updateViewportSize();
    });
    viewportObserver.observe(host);
  });

  onDestroy(() => {
    if (viewportObserver) {
      viewportObserver.disconnect();
      viewportObserver = null;
    }
    stopTimeoutTicker();
  });

  function startTimeoutTicker() {
    if (timeoutFrame) return;
    const tick = () => {
      timeoutNow = Date.now();
      timeoutFrame = requestAnimationFrame(tick);
    };
    timeoutFrame = requestAnimationFrame(tick);
  }

  function stopTimeoutTicker() {
    if (!timeoutFrame) return;
    cancelAnimationFrame(timeoutFrame);
    timeoutFrame = null;
  }

  $: if (activityEnabled && timeoutEdges && timeoutEdges.length > 0) {
    startTimeoutTicker();
  } else {
    stopTimeoutTicker();
  }

  $: if (selection && snapshot) {
    const exists =
      selection.type === "node"
        ? nodes.some((node) => node.id === selection.id)
        : selection.type === "edge"
          ? edges.some((edge) => edge.id === selection.id)
          : selection.type === "comment"
            ? comments.some((comment) => comment.id === selection.id)
            : false;
    if (!exists) {
      clearSelection();
    }
  }

  function computeBounds(nodesList, edgesList, commentList, showText) {
    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;

    const expand = (x, y) => {
      if (!Number.isFinite(x) || !Number.isFinite(y)) return;
      minX = Math.min(minX, x);
      minY = Math.min(minY, y);
      maxX = Math.max(maxX, x);
      maxY = Math.max(maxY, y);
    };

    nodesList.forEach((node) => {
      const pos = nodePosition(node, null);
      const x = pos.x;
      const y = pos.y;
      const { w, h } = nodeSize(node);
      expand(x, y);
      expand(x + w, y + h);
      const cmdLayout = showText
        ? nodeCommandLayout(node, w, h)
        : nodeCommandDotsLayout(node, w, h);
      if (cmdLayout) {
        expand(x + cmdLayout.x, y + cmdLayout.y);
        expand(x + cmdLayout.x + cmdLayout.width, y + cmdLayout.y + cmdLayout.height);
      }
    });

    commentList.forEach((comment) => {
      const x = toFinite(comment.rect?.x, 0);
      const y = toFinite(comment.rect?.y, 0);
      const w = toFinite(comment.rect?.w, 0);
      const h = toFinite(comment.rect?.h, 0);
      expand(x, y);
      expand(x + w, y + h);
    });

    edgesList.forEach((edge) => {
      const pts = edgePoints(edge, null) || [];
      pts.forEach((pt) => {
        expand(pt.x, pt.y);
        expand(pt.cx, pt.cy);
      });
    });

    if (!Number.isFinite(minX) || !Number.isFinite(minY)) {
      minX = 0;
      minY = 0;
      maxX = 400;
      maxY = 300;
    }

    const width = maxX - minX;
    const height = maxY - minY;
    const boxX = 0;
    const boxY = 0;
    const boxW = Math.max(200, maxX + padding - boxX);
    const boxH = Math.max(200, maxY + padding - boxY);
    return {
      box: {
        x: boxX,
        y: boxY,
        width: boxW,
        height: boxH
      },
      width: boxW,
      height: boxH
    };
  }

  function readNumber(value, fallback) {
    const parsed = Number.parseFloat(value);
    return Number.isFinite(parsed) ? parsed : fallback;
  }

  function readBoolean(value, fallback) {
    if (value === undefined || value === null) {
      return fallback;
    }
    if (typeof value === "boolean") {
      return value;
    }
    const normalized = String(value).toLowerCase().trim();
    if (normalized === "true" || normalized === "1") {
      return true;
    }
    if (normalized === "false" || normalized === "0") {
      return false;
    }
    return fallback;
  }

  function superNodeScale(node) {
    const count = Number.isFinite(node?.childCount) ? node.childCount : 0;
    const steps = Math.max(0, Math.floor(count / 5));
    return 1 + steps * 0.05;
  }

  function nodeBaseSize(node) {
    return {
      w: toFinite(node?.size?.w, baseNodeSize),
      h: toFinite(node?.size?.h, nodeHeight ?? baseNodeSize)
    };
  }

  function nodeSize(node) {
    const base = nodeBaseSize(node);
    if (node?.type !== "Super") {
      return base;
    }
    const scale = superNodeScale(node);
    return { w: base.w * scale, h: base.h * scale };
  }

  function nodeVisualOffset(node) {
    if (node?.type !== "Super") {
      return { x: 0, y: 0 };
    }
    const base = nodeBaseSize(node);
    const scaled = nodeSize(node);
    return {
      x: (base.w - scaled.w) / 2,
      y: (base.h - scaled.h) / 2
    };
  }

  function nodeRenderPosition(node, baseX, baseY) {
    const offset = nodeVisualOffset(node);
    return {
      x: (baseX ?? 0) + offset.x,
      y: (baseY ?? 0) + offset.y
    };
  }

  function superNodePath(w, h) {
    const power = SUPER_NODE_SHAPE_POWER;
    const steps = 32;
    const a = w / 2;
    const b = h / 2;
    const cx = a;
    const cy = b;
    const points = [];
    for (let i = 0; i <= steps; i += 1) {
      const theta = (Math.PI * 2 * i) / steps;
      const cos = Math.cos(theta);
      const sin = Math.sin(theta);
      const x = cx + a * Math.sign(cos) * Math.pow(Math.abs(cos), 2 / power);
      const y = cy + b * Math.sign(sin) * Math.pow(Math.abs(sin), 2 / power);
      points.push({ x, y });
    }
    return points
      .map((pt, idx) => `${idx === 0 ? "M" : "L"} ${pt.x} ${pt.y}`)
      .concat("Z")
      .join(" ");
  }

  function guessNodeSize(nodesList) {
    for (const node of nodesList) {
      const size = node.size?.w ?? node.size?.h;
      if (Number.isFinite(size)) {
        return size;
      }
    }
    return null;
  }

  function viewBoxString(canvasW, canvasH, zoomLevel, offsetX, offsetY) {
    // At zoom=1, viewBox matches canvas for 1:1 pixel mapping
    // Zooming changes viewBox size inversely (smaller viewBox = magnified content)
    const width = canvasW / zoomLevel;
    const height = canvasH / zoomLevel;
    const x = offsetX;
    const y = offsetY;
    return `${x} ${y} ${width} ${height}`;
  }

  function currentViewBox() {
    // Use canvas dimensions for 1:1 pixel mapping at zoom=1
    return {
      x: panX,
      y: panY,
      width: canvasWidth / zoomLevel,
      height: canvasHeight / zoomLevel
    };
  }

  function clamp(value, min, max) {
    return Math.min(max, Math.max(min, value));
  }

  function setZoom(nextZoom, anchor = null) {
    const view = currentViewBox();
    const clamped = clamp(nextZoom, minZoom, maxZoom);
    if (!anchor) {
      // Zoom centered on current view center
      const centerX = view.x + view.width / 2;
      const centerY = view.y + view.height / 2;
      zoomLevel = clamped;
      const width = canvasWidth / zoomLevel;
      const height = canvasHeight / zoomLevel;
      panX = centerX - width / 2;
      panY = centerY - height / 2;
      clampPanToNonNegative();
      return;
    }
    // Zoom anchored at specific point (e.g., mouse wheel zoom)
    const relX = clamp(anchor.relX ?? 0.5, 0, 1);
    const relY = clamp(anchor.relY ?? 0.5, 0, 1);
    zoomLevel = clamped;
    const width = canvasWidth / zoomLevel;
    const height = canvasHeight / zoomLevel;
    const newX = anchor.x - relX * width;
    const newY = anchor.y - relY * height;
    panX = newX;
    panY = newY;
    clampPanToNonNegative();
  }

  export function zoomIn() {
    setZoom(zoomLevel * zoomStep);
  }

  export function zoomOut() {
    setZoom(zoomLevel / zoomStep);
  }

  function resetView() {
    zoomLevel = clamp(zoomLevel, minZoom, maxZoom);
    panX = 0;
    panY = 0;
    clampPanToNonNegative();
  }

  export function fitToView() {
    zoomLevel = 1;
    panX = 0;
    panY = 0;
    clampPanToNonNegative();
  }

  export function centerOn(x, y) {
    if (!Number.isFinite(x) || !Number.isFinite(y)) return;
    const width = canvasWidth / zoomLevel;
    const height = canvasHeight / zoomLevel;
    panX = x - width / 2;
    panY = y - height / 2;
    clampPanToNonNegative();
  }

  function inlineSvgStyles(sourceSvg, targetSvg) {
    if (typeof window === "undefined" || !sourceSvg || !targetSvg) return;
    const copyStyles = (source, target) => {
      const computed = window.getComputedStyle(source);
      for (const prop of EXPORT_STYLE_PROPS) {
        const value = computed.getPropertyValue(prop);
        if (value) {
          target.style.setProperty(prop, value);
        }
      }
    };
    copyStyles(sourceSvg, targetSvg);
    const sourceEls = sourceSvg.querySelectorAll("*");
    const targetEls = targetSvg.querySelectorAll("*");
    sourceEls.forEach((el, idx) => {
      const target = targetEls[idx];
      if (!target) return;
      copyStyles(el, target);
    });
  }

  export async function exportPng(options = {}) {
    if (!svgEl) return null;
    const scale = Number.isFinite(options.scale) ? options.scale : 1;
    const background = options.background || "#ffffff";
    const clone = svgEl.cloneNode(true);
    inlineSvgStyles(svgEl, clone);
    const width = Number.parseFloat(svgEl.getAttribute("width")) || canvasWidth || 0;
    const height = Number.parseFloat(svgEl.getAttribute("height")) || canvasHeight || 0;
    clone.setAttribute("width", String(width));
    clone.setAttribute("height", String(height));
    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    clone.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
    const serialized = new XMLSerializer().serializeToString(clone);
    const blob = new Blob([serialized], { type: "image/svg+xml;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    return new Promise((resolve) => {
      const image = new Image();
      image.onload = () => {
        const canvas = document.createElement("canvas");
        canvas.width = Math.max(1, Math.round(width * scale));
        canvas.height = Math.max(1, Math.round(height * scale));
        const ctx = canvas.getContext("2d");
        if (!ctx) {
          URL.revokeObjectURL(url);
          resolve(null);
          return;
        }
        ctx.fillStyle = background;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(image, 0, 0, canvas.width, canvas.height);
        URL.revokeObjectURL(url);
        resolve(canvas.toDataURL("image/png"));
      };
      image.onerror = () => {
        URL.revokeObjectURL(url);
        resolve(null);
      };
      image.src = url;
    });
  }

  function clampPanToNonNegative() {
    if (!baseBox) return;
    const minX = -baseBox.x;
    const minY = -baseBox.y;
    if (Number.isFinite(minX)) {
      panX = Math.max(panX, minX);
    }
    if (Number.isFinite(minY)) {
      panY = Math.max(panY, minY);
    }
  }

  function focusStage() {
    if (!stageEl || typeof stageEl.focus !== "function") return;
    stageEl.focus({ preventScroll: true });
  }

  function isTypingTarget(event) {
    const target = event?.target;
    if (!target) return false;
    if (target.isContentEditable) return true;
    const tag = target.tagName ? target.tagName.toLowerCase() : "";
    return tag === "input" || tag === "textarea" || tag === "select";
  }

  function handleStageKeydown(event) {
    if (isTypingTarget(event)) return;
    const key = event.key;
    const mod = event.metaKey || event.ctrlKey;
    if (mod && (key === "c" || key === "C")) {
      if (typeof onCopySelection === "function") {
        event.preventDefault();
        onCopySelection();
      }
      return;
    }
    if (mod && (key === "x" || key === "X")) {
      if (typeof onCutSelection === "function") {
        event.preventDefault();
        onCutSelection();
      }
      return;
    }
    if (mod && (key === "d" || key === "D")) {
      if (typeof onDuplicateSelection === "function") {
        event.preventDefault();
        onDuplicateSelection();
      }
      return;
    }
    if (mod && (key === "v" || key === "V")) {
      if (typeof onPasteSelection === "function") {
        event.preventDefault();
        onPasteSelection();
      }
      return;
    }
    if (key === "Escape") {
      event.preventDefault();
      clearSelection();
      return;
    }
    if (key === "Delete" || key === "Backspace") {
      if (typeof onDeleteSelection === "function") {
        event.preventDefault();
        onDeleteSelection();
      }
      return;
    }
    if (mod && (key === "z" || key === "Z")) {
      event.preventDefault();
      if (event.shiftKey) {
        if (typeof onRedo === "function") {
          onRedo();
        }
      } else if (typeof onUndo === "function") {
        onUndo();
      }
      return;
    }
    if (mod && (key === "y" || key === "Y")) {
      event.preventDefault();
      if (typeof onRedo === "function") {
        onRedo();
      }
    }
    if (key === "Shift") {
      shiftDown = true;
    }
  }

  function handleStageKeyup(event) {
    if (event.key === "Shift") {
      shiftDown = false;
    }
  }

  function handleStageClick() {
    if (suppressStageClick) {
      suppressStageClick = false;
      return;
    }
    clearSelection();
  }

  function selectionRect(box) {
    if (!box) return null;
    return { x: box.x, y: box.y, w: box.w, h: box.h };
  }

  function rectsIntersect(a, b) {
    if (!a || !b) return false;
    return !(a.x + a.w < b.x || a.x > b.x + b.w || a.y + a.h < b.y || a.y > b.y + b.h);
  }

  function edgeBounds(edge, drag) {
    if (!edge) return null;
    const pts = edgePoints(edge, drag);
    if (!pts || pts.length === 0) return null;
    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;
    for (const pt of pts) {
      if (!pt) continue;
      const values = [
        { x: pt.x, y: pt.y },
        { x: pt.cx, y: pt.cy }
      ];
      for (const value of values) {
        if (!Number.isFinite(value.x) || !Number.isFinite(value.y)) continue;
        minX = Math.min(minX, value.x);
        minY = Math.min(minY, value.y);
        maxX = Math.max(maxX, value.x);
        maxY = Math.max(maxY, value.y);
      }
    }
    if (!Number.isFinite(minX) || !Number.isFinite(minY) || !Number.isFinite(maxX) || !Number.isFinite(maxY)) {
      return null;
    }
    return { x: minX, y: minY, w: maxX - minX, h: maxY - minY };
  }

  function startSelectionBox(event) {
    if (event.button !== 0 || !svgEl) return;
    event.preventDefault();
    focusStage();
    const world = eventToWorld(event);
    selectionBox = {
      startX: world.x,
      startY: world.y,
      x: world.x,
      y: world.y,
      w: 0,
      h: 0,
      moved: false,
      additive: isMultiModifier(event),
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function updateSelectionBox(event) {
    if (!selectionBox || selectionBox.pointerId !== event.pointerId) return;
    const world = eventToWorld(event);
    const dx = world.x - selectionBox.startX;
    const dy = world.y - selectionBox.startY;
    const x = Math.min(selectionBox.startX, world.x);
    const y = Math.min(selectionBox.startY, world.y);
    const w = Math.abs(dx);
    const h = Math.abs(dy);
    selectionBox = {
      ...selectionBox,
      x,
      y,
      w,
      h,
      moved: selectionBox.moved || Math.hypot(dx, dy) > dragThreshold
    };
  }

  function endSelectionBox(event) {
    if (!selectionBox || selectionBox.pointerId !== event.pointerId) return;
    const finished = selectionBox;
    selectionBox = null;
    const captureEl = stageEl || svgEl;
    if (captureEl && captureEl.hasPointerCapture(event.pointerId)) {
      captureEl.releasePointerCapture(event.pointerId);
    }
    if (!finished.moved) {
      suppressStageClick = false;
      return;
    }
    suppressStageClick = true;
    const rect = selectionRect(finished);
    const nextNodeIds = finished.additive ? new Set(selectedNodeIds) : new Set();
    const nextEdgeIds = finished.additive ? new Set(selectedEdgeIds) : new Set();
    const nextCommentIds = finished.additive ? new Set(selectedCommentIds) : new Set();
    if (rect) {
      for (const node of nodes) {
        const pos = nodePosition(node, null);
        const size = nodeSize(node);
        const nodeRect = { x: pos.x, y: pos.y, w: size.w, h: size.h };
        if (rectsIntersect(rect, nodeRect)) {
          nextNodeIds.add(node.id);
        }
      }
      for (const comment of comments) {
        const cRect = commentRect(comment, null);
        if (rectsIntersect(rect, cRect)) {
          nextCommentIds.add(comment.id);
        }
      }
      for (const edge of edges) {
        const eRect = edgeBounds(edge, null);
        if (eRect && rectsIntersect(rect, eRect)) {
          nextEdgeIds.add(edge.id);
        }
      }
    }
    if (nextNodeIds.size || nextEdgeIds.size || nextCommentIds.size) {
      updateMultiSelection(nextNodeIds, nextCommentIds, nextEdgeIds);
      return;
    }
    if (!finished.additive) {
      clearSelection();
    }
  }

  function updateEdgeCreateCursor(event) {
    if (!edgeCreateMode || !svgEl) return;
    const world = eventToWorld(event);
    edgeCreateCursor = clampWorldPoint(world);
  }

  function startPan(event) {
    if (event.button !== 0 || !svgEl) return;
    focusStage();
    if (editingCommentId && !event.target?.closest?.(".comment-editor")) {
      commitCommentEdit();
    }
    if (event.target?.closest?.(".node, .edge-group, .comment")) return;
    if (dragState) return;
    if (!event.shiftKey) {
      startSelectionBox(event);
      return;
    }
    event.preventDefault();
    isPanning = true;
    panStart = { x: event.clientX, y: event.clientY };
    panOrigin = { x: panX, y: panY };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function movePan(event) {
    updateEdgeCreateCursor(event);
    if (dragState) {
      updateDrag(event);
      return;
    }
    if (selectionBox) {
      updateSelectionBox(event);
      return;
    }
    if (!isPanning || !svgEl) return;
    const rect = svgEl.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    const scaleX = baseBox.width / (rect.width * zoomLevel);
    const scaleY = baseBox.height / (rect.height * zoomLevel);
    const dx = (event.clientX - panStart.x) * scaleX;
    const dy = (event.clientY - panStart.y) * scaleY;
    panX = panOrigin.x - dx;
    panY = panOrigin.y - dy;
    clampPanToNonNegative();
  }

  function endPan(event) {
    updateEdgeCreateCursor(event);
    if (dragState) {
      endDrag(event);
      return;
    }
    if (selectionBox) {
      endSelectionBox(event);
      return;
    }
    if (!isPanning || !svgEl) return;
    isPanning = false;
    const captureEl = stageEl || svgEl;
    if (captureEl && captureEl.hasPointerCapture(event.pointerId)) {
      captureEl.releasePointerCapture(event.pointerId);
    }
  }

  function handleWheel(event) {
    if (editingCommentId) {
      commitCommentEdit();
    }
    if (!event.shiftKey) {
      return;
    }
    event.preventDefault();
    if (!svgEl) {
      setZoom(zoomLevel);
      return;
    }
    const rect = svgEl.getBoundingClientRect();
    if (!rect.width || !rect.height) {
      setZoom(zoomLevel);
      return;
    }
    const view = currentViewBox();
    const relX = clamp((event.clientX - rect.left) / rect.width, 0, 1);
    const relY = clamp((event.clientY - rect.top) / rect.height, 0, 1);
    const anchorX = view.x + relX * view.width;
    const anchorY = view.y + relY * view.height;
    const factor = event.deltaY > 0 ? 1 / zoomStep : zoomStep;
    setZoom(zoomLevel * factor, { x: anchorX, y: anchorY, relX, relY });
  }

  function isMultiModifier(event) {
    return event?.metaKey || event?.ctrlKey;
  }

  function selectionEntry(type, id) {
    return { type, id };
  }

  function isSelectionEntry(entry, target) {
    return entry && target && entry.type === target.type && entry.id === target.id;
  }

  function buildSelectionList(nodeIds, commentIds, edgeIds) {
    const list = [];
    nodeIds.forEach((id) => list.push(selectionEntry("node", id)));
    edgeIds.forEach((id) => list.push(selectionEntry("edge", id)));
    commentIds.forEach((id) => list.push(selectionEntry("comment", id)));
    return list;
  }

  function updateMultiSelection(nextNodeIds, nextCommentIds, nextEdgeIds, primary = null) {
    const list = buildSelectionList(nextNodeIds, nextCommentIds, nextEdgeIds);
    multiSelection = list;
    if (primary) {
      selection = primary;
      return;
    }
    if (selection && list.some((entry) => isSelectionEntry(entry, selection))) {
      return;
    }
    selection = list.length ? list[0] : null;
  }

  function toggleSelection(type, id) {
    const nextNodeIds = new Set(selectedNodeIds);
    const nextEdgeIds = new Set(selectedEdgeIds);
    const nextCommentIds = new Set(selectedCommentIds);
    if (type === "node") {
      if (nextNodeIds.has(id)) {
        nextNodeIds.delete(id);
      } else {
        nextNodeIds.add(id);
      }
    }
    if (type === "edge") {
      if (nextEdgeIds.has(id)) {
        nextEdgeIds.delete(id);
      } else {
        nextEdgeIds.add(id);
      }
    }
    if (type === "comment") {
      if (nextCommentIds.has(id)) {
        nextCommentIds.delete(id);
      } else {
        nextCommentIds.add(id);
      }
    }
    const list = buildSelectionList(nextNodeIds, nextCommentIds, nextEdgeIds);
    multiSelection = list;
    if (list.length === 0) {
      selection = null;
      return;
    }
    const toggled = selectionEntry(type, id);
    if (list.some((entry) => isSelectionEntry(entry, toggled))) {
      selection = toggled;
      return;
    }
    selection = list[0];
  }

  function clearSelection() {
    if (editingCommentId) {
      commitCommentEdit();
    }
    selectedNodeId = null;
    selectedEdgeId = null;
    selectedCommentId = null;
    selection = null;
    multiSelection = [];
    timeoutSliderSuppressedEdgeId = "";
  }

  function selectNode(nodeId, options = {}) {
    if (editingCommentId) {
      commitCommentEdit();
    }
    focusStage();
    const isMulti = options.multi;
    if (isMulti) {
      toggleSelection("node", nodeId);
      return;
    }
    selectedNodeId = nodeId;
    selectedEdgeId = null;
    selectedCommentId = null;
    updateMultiSelection(new Set([nodeId]), new Set(), new Set(), { type: "node", id: nodeId });
  }

  function selectEdge(edgeId, options = {}) {
    if (editingCommentId) {
      commitCommentEdit();
    }
    focusStage();
    const isMulti = options.multi;
    if (isMulti) {
      toggleSelection("edge", edgeId);
      return;
    }
    if (timeoutSliderSuppressedEdgeId === edgeId) {
      timeoutSliderSuppressedEdgeId = "";
    }
    selectedEdgeId = edgeId;
    selectedNodeId = null;
    selectedCommentId = null;
    updateMultiSelection(new Set(), new Set(), new Set([edgeId]), { type: "edge", id: edgeId });
  }

  function selectComment(commentId, options = {}) {
    if (editingCommentId && editingCommentId !== commentId) {
      commitCommentEdit();
    }
    focusStage();
    const isMulti = options.multi;
    if (isMulti) {
      toggleSelection("comment", commentId);
      return;
    }
    selectedCommentId = commentId;
    selectedNodeId = null;
    selectedEdgeId = null;
    updateMultiSelection(new Set(), new Set([commentId]), new Set(), { type: "comment", id: commentId });
  }

  function edgeColor(edge) {
    const key = (edge?.type || "").toLowerCase();
    return COLORS.edges[key] || COLORS.edges.eedge;
  }

  function nodeFill(node) {
    if (node?.isHistory) {
      return COLORS.history;
    }
    const flavour = (node?.flavour || "").toLowerCase();
    if (flavour === "enode") return COLORS.edges.eedge;
    if (flavour === "fnode") return COLORS.edges.fedge;
    if (flavour === "tnode") return COLORS.edges.tedge;
    if (flavour === "cnode") return COLORS.edges.cedge;
    if (flavour === "pnode") return COLORS.edges.pedge;
    if (flavour === "inode") return COLORS.edges.iedge;
    return COLORS.node;
  }

  function nodeTextColor(node) {
    return node?.isHistory ? COLORS.textHistory : COLORS.text;
  }

  function nodeLines(node, w) {
    if (!node?.name) return [];
    const raw = node.name.trim();
    if (!raw) return [];
    if (raw.includes(";")) {
      return raw.split(";").map((line) => line.trim()).filter((line) => line.length > 0);
    }
    const size = fontSize || 12;
    const paddingX = Math.max(8, Math.round(size * 0.6));
    const maxWidth = Math.max(20, toFinite(w, 0) - paddingX * 2);
    if (!Number.isFinite(maxWidth) || maxWidth <= 0) {
      return [raw];
    }
    const words = raw.split(/\s+/).filter((word) => word.length > 0);
    if (!words.length) return [raw];
    const lines = [];
    let current = "";
    for (const word of words) {
      const candidate = current ? `${current} ${word}` : word;
      if (!current || measureTextMetrics(candidate, size).width <= maxWidth) {
        current = candidate;
        continue;
      }
      lines.push(current);
      current = word;
    }
    if (current) {
      lines.push(current);
    }
    return lines;
  }

  function nodeCommandLines(node) {
    const list = Array.isArray(node?.commands) ? node.commands : [];
    return list
      .map((cmd) => (cmd?.text ?? cmd?.syntax ?? "").trim())
      .filter((line) => line.length > 0);
  }

  function nodeCommandDotsLayout(node, w, h) {
    const safeW = toFinite(w, 0);
    const safeH = toFinite(h, 0);
    const count = nodeCommandLines(node).length;
    if (!count) return null;
    const size = fontSize || 12;
    const radius = Math.max(3, Math.round(size * 0.33)) * 2;
    const gap = Math.max(4, Math.round(radius * 0.9));
    const totalWidth = count * radius * 2 + (count - 1) * gap;
    const startX = (safeW - totalWidth) / 2;
    const rx = Math.max(1, safeW / 2);
    const ry = Math.max(1, safeH / 2);
    const isSuper = node?.type === "Super";
    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;
    const dots = Array.from({ length: count }, (_, idx) => {
      const cx = startX + radius + idx * (radius * 2 + gap);
      let cy = h - radius;
      if (!isSuper) {
        const dx = cx - rx;
        const normalized = Math.max(0, 1 - (dx * dx) / (rx * rx));
        const boundaryY = ry + ry * Math.sqrt(normalized);
        cy = boundaryY - radius;
      }
      minX = Math.min(minX, cx - radius);
      maxX = Math.max(maxX, cx + radius);
      minY = Math.min(minY, cy - radius);
      maxY = Math.max(maxY, cy + radius);
      return { cx, cy, r: radius };
    });
    if (!Number.isFinite(minX)) {
      return null;
    }
    return {
      dots,
      x: minX,
      y: minY,
      width: maxX - minX,
      height: maxY - minY
    };
  }

  function measureTextMetrics(text, size) {
    const fontSize = size || 12;
    if (!text) {
      return {
        width: 0,
        ascent: fontSize * 0.8,
        descent: fontSize * 0.2
      };
    }
    if (typeof document === "undefined") {
      return {
        width: text.length * fontSize * 0.6,
        ascent: fontSize * 0.8,
        descent: fontSize * 0.2
      };
    }
    if (!textMeasureCtx) {
      const canvas = document.createElement("canvas");
      textMeasureCtx = canvas.getContext("2d");
    }
    if (!textMeasureCtx) {
      return {
        width: text.length * fontSize * 0.6,
        ascent: fontSize * 0.8,
        descent: fontSize * 0.2
      };
    }
    if (lastTextMeasureSize !== fontSize) {
      textMeasureCtx.font = `600 ${fontSize}px ${COMMAND_FONT_FAMILY}`;
      lastTextMeasureSize = fontSize;
    }
    const metrics = textMeasureCtx.measureText(text);
    const ascent = Number.isFinite(metrics.actualBoundingBoxAscent)
      ? metrics.actualBoundingBoxAscent
      : fontSize * 0.8;
    const descent = Number.isFinite(metrics.actualBoundingBoxDescent)
      ? metrics.actualBoundingBoxDescent
      : fontSize * 0.2;
    return {
      width: metrics.width || 0,
      ascent,
      descent
    };
  }

  function nodeCommandLayout(node, w, h) {
    const safeW = toFinite(w, 0);
    const safeH = toFinite(h, 0);
    const lines = nodeCommandLines(node);
    if (!lines.length) return null;
    const size = fontSize || 12;
    const padY = commandPaddingY || 4;
    const padX = commandPaddingX || 6;
    const gap = commandGap || 4;
    const metrics = lines.map((line) => measureTextMetrics(line, size));
    const maxTextWidth = metrics.reduce((max, metric) => Math.max(max, metric.width), 0);
    const maxAscent = metrics.reduce((max, metric) => Math.max(max, metric.ascent), 0);
    const maxDescent = metrics.reduce((max, metric) => Math.max(max, metric.descent), 0);
    const lineHeight = Math.max(1, maxAscent + maxDescent);
    const width = Math.max(1, maxTextWidth + padX * 2);
    if (!Number.isFinite(width) || !Number.isFinite(lineHeight)) {
      return null;
    }
    return {
      lines,
      x: (safeW - width) / 2,
      y: safeH + gap,
      width,
      height: lines.length * lineHeight + padY * 2,
      textX: (safeW - width) / 2 + padX,
      textStartY: padY + maxAscent,
      lineHeight,
      fontSize: size
    };
  }

  function nodeLabelLayout(node, w, h) {
    const lines = nodeLines(node, w);
    if (!lines.length) return null;
    const idLine = showNodeIds && node?.id ? `[${node.id}]` : "";
    const total = lines.length + (idLine ? 1 : 0);
    const startY = Math.round(h / 2 - ((total - 1) * labelLineHeight) / 2 + labelVerticalOffset);
    return {
      lines,
      idLine,
      startY,
      lineHeight: labelLineHeight
    };
  }

  function darkenColor(hexColor, factor = 0.25) {
    if (!hexColor || hexColor[0] !== "#") {
      return hexColor;
    }
    const rgb = hexToRgb(hexColor);
    if (!rgb) {
      return hexColor;
    }
    const scale = 1 - factor;
    const r = Math.round(rgb.r * scale);
    const g = Math.round(rgb.g * scale);
    const b = Math.round(rgb.b * scale);
    return `rgb(${r}, ${g}, ${b})`;
  }

  function hexToRgb(hexColor) {
    const hex = hexColor.replace("#", "");
    if (hex.length !== 6) return null;
    const value = Number.parseInt(hex, 16);
    if (!Number.isFinite(value)) return null;
    return {
      r: (value >> 16) & 0xff,
      g: (value >> 8) & 0xff,
      b: value & 0xff
    };
  }

  function startSignMetrics(size) {
    const halfHeight = size / 6;
    const width = size / 8;
    const stroke = Math.max(2, size / 50);
    const points = [
      [2 * stroke, 2 * stroke],
      [width + 2 * stroke, halfHeight + 2 * stroke],
      [2 * stroke, halfHeight * 2 + 2 * stroke],
      [width / 2 + stroke, halfHeight + 2 * stroke]
    ]
      .map((point) => point.join(","))
      .join(" ");
    return { width, halfHeight, stroke, points };
  }

  function edgePath(edge, drag, arrow) {
    const pts = edgePoints(edge, drag);
    if (pts.length >= 2) {
      const start = pts[0];
      let end = pts[pts.length - 1];
      const controls = edgeCurveControls(edge, start, end, drag);
      const ctrl1 = controls.ctrl1;
      let ctrl2 = controls.ctrl2;
      const trimmed = trimEdgeEnd(start, end, ctrl2, arrow?.trim);
      if (trimmed) {
        end = trimmed.end;
        ctrl2 = adjustedEdgeEndCtrl(edge, start, end, trimmed.ctrl2);
      }
      return `M ${start.x} ${start.y} C ${ctrl1.x} ${ctrl1.y} ${ctrl2.x} ${ctrl2.y} ${end.x} ${end.y}`;
    }
    const fallback = fallbackEdgeEndpoints(edge, drag);
    if (fallback) {
      const s = fallback.start;
      let t = fallback.end;
      if (arrow?.trim) {
        const trimmed = trimEdgeEnd(s, t, s, arrow.trim);
        if (trimmed) {
          t = trimmed.end;
        }
      }
      return `M ${s.x} ${s.y} L ${t.x} ${t.y}`;
    }
    return "";
  }

  function edgeArrow(edge, drag) {
    const vector = edgeEndVector(edge, drag);
    if (!vector) return null;
    const selfLoop = isSelfLoopEdge(edge);
    const { length, trim } = edgeArrowMetrics();
    const width = length * 0.7;
    const inset = Math.max(0, edgeStrokeWidth * 0.6);
    const gap = selfLoop
      ? Math.max(2, Math.round(edgeStrokeWidth * 1.2))
      : Math.max(3, Math.round(edgeStrokeWidth * 2));
    const magnitude = Math.hypot(vector.dx, vector.dy);
    if (!Number.isFinite(magnitude) || magnitude < 0.01) return null;
    const ux = vector.dx / magnitude;
    const uy = vector.dy / magnitude;
    const tipOffset = inset - gap;
    const tipX = vector.x + ux * tipOffset;
    const tipY = vector.y + uy * tipOffset;
    const baseX = tipX - ux * length;
    const baseY = tipY - uy * length;
    const half = width / 2;
    const perpX = -uy;
    const perpY = ux;
    const leftX = baseX + perpX * half;
    const leftY = baseY + perpY * half;
    const rightX = baseX - perpX * half;
    const rightY = baseY - perpY * half;
    const arrowTrim = selfLoop
      ? Math.max(0, gap - inset + Math.max(1, edgeStrokeWidth * 0.9))
      : trim;
    return { tipX, tipY, leftX, leftY, rightX, rightY, trim: arrowTrim };
  }

  function edgeEndVector(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length >= 2) {
      const start = pts[0];
      const end = pts[pts.length - 1];
      if (isSelfLoopEdge(edge)) {
        const loopNode = nodeMap.get(edge.sourceId);
        const center = loopNode ? nodeCenter(loopNode, drag) : null;
        if (center) {
          const cdx = center.x - end.x;
          const cdy = center.y - end.y;
          if (Number.isFinite(cdx) && Number.isFinite(cdy) && Math.hypot(cdx, cdy) >= 0.01) {
            return { x: end.x, y: end.y, dx: cdx, dy: cdy };
          }
        }
      }
      const ctrl2 = adjustedEdgeEndCtrl(edge, start, end, safeCtrl(end));
      let dx = end.x - ctrl2.x;
      let dy = end.y - ctrl2.y;
      if (!Number.isFinite(dx) || !Number.isFinite(dy) || Math.hypot(dx, dy) < 0.01) {
        dx = end.x - start.x;
        dy = end.y - start.y;
      }
      if (!Number.isFinite(dx) || !Number.isFinite(dy) || Math.hypot(dx, dy) < 0.01) {
        return null;
      }
      return { x: end.x, y: end.y, dx, dy };
    }
    const fallback = fallbackEdgeEndpoints(edge, drag);
    if (fallback) {
      const s = fallback.start;
      const t = fallback.end;
      const dx = t.x - s.x;
      const dy = t.y - s.y;
      if (!Number.isFinite(dx) || !Number.isFinite(dy) || Math.hypot(dx, dy) < 0.01) {
        return null;
      }
      return { x: t.x, y: t.y, dx, dy };
    }
    return null;
  }

  function arrowPath(arrow) {
    return `M ${arrow.tipX} ${arrow.tipY} L ${arrow.leftX} ${arrow.leftY} L ${arrow.rightX} ${arrow.rightY} Z`;
  }

  function edgeArrowMetrics() {
    const length = Math.max(9, baseNodeSize * 0.13, edgeStrokeWidth * 4);
    const inset = Math.max(0, edgeStrokeWidth * 0.6);
    const gap = Math.max(3, Math.round(edgeStrokeWidth * 2));
    const trim = Math.max(0, length - inset + gap);
    return { length, trim };
  }

  function isSelfLoopEdge(edge) {
    const sourceId = (edge?.sourceId || "").trim();
    const targetId = (edge?.targetId || "").trim();
    return !!sourceId && sourceId === targetId;
  }

  function edgeCurveControls(edge, start, end, drag) {
    const ctrl1 = safeCtrl(start);
    const ctrl2 = safeCtrl(end);
    if (isSelfLoopEdge(edge)) {
      if (selfLoopManualControlIds.has(edge?.id)) {
        return { ctrl1, ctrl2 };
      }
      const loop = mirroredSelfLoopControls(edge, start, end, drag);
      if (loop) {
        return loop;
      }
      return { ctrl1, ctrl2 };
    }
    return {
      ctrl1,
      ctrl2: adjustedEdgeEndCtrl(edge, start, end, ctrl2)
    };
  }

  function mirroredSelfLoopControls(edge, start, end, drag) {
    const loopNode = nodeMap.get(edge?.sourceId);
    if (!loopNode) return null;
    const center = nodeCenter(loopNode, drag);
    const mid = { x: (start.x + end.x) / 2, y: (start.y + end.y) / 2 };
    let axisX = mid.x - center.x;
    let axisY = mid.y - center.y;
    let axisLen = Math.hypot(axisX, axisY);
    if (!Number.isFinite(axisLen) || axisLen < 0.01) {
      axisX = 0;
      axisY = -1;
      axisLen = 1;
    }
    const ux = axisX / axisLen;
    const uy = axisY / axisLen;
    const nx = -uy;
    const ny = ux;

    const startSide = (start.x - mid.x) * nx + (start.y - mid.y) * ny;
    const sideSign = startSide >= 0 ? 1 : -1;
    const span = Math.hypot(end.x - start.x, end.y - start.y);
    const outward = Math.max(baseNodeSize * 1.17, axisLen * 1.17);
    const lateral = Math.max(baseNodeSize * 0.62, span * 0.95);
    const baseCtrl1 = {
      x: mid.x + ux * outward + nx * lateral * sideSign,
      y: mid.y + uy * outward + ny * lateral * sideSign
    };
    const baseCtrl2 = reflectPointAcrossAxis(baseCtrl1, mid, { x: ux, y: uy });
    const ctrl1Side = (baseCtrl1.x - mid.x) * nx + (baseCtrl1.y - mid.y) * ny;
    const ctrl2Side = (baseCtrl2.x - mid.x) * nx + (baseCtrl2.y - mid.y) * ny;
    if (Math.sign(ctrl1Side) === Math.sign(ctrl2Side)) {
      return {
        ctrl1: baseCtrl1,
        ctrl2: {
          x: mid.x + ux * outward - nx * lateral * sideSign,
          y: mid.y + uy * outward - ny * lateral * sideSign
        }
      };
    }
    return {
      ctrl1: baseCtrl1,
      ctrl2: baseCtrl2
    };
  }

  function reflectPointAcrossAxis(point, axisOrigin, axisUnit) {
    const ux = axisUnit?.x ?? 0;
    const uy = axisUnit?.y ?? 0;
    const len = Math.hypot(ux, uy);
    if (!Number.isFinite(len) || len < 0.01) {
      return { ...point };
    }
    const nx = ux / len;
    const ny = uy / len;
    const vx = point.x - axisOrigin.x;
    const vy = point.y - axisOrigin.y;
    const parallel = vx * nx + vy * ny;
    const px = nx * parallel;
    const py = ny * parallel;
    const perpX = vx - px;
    const perpY = vy - py;
    return {
      x: axisOrigin.x + px - perpX,
      y: axisOrigin.y + py - perpY
    };
  }

  function rotateVectorClockwise(x, y, degrees) {
    const theta = (Number(degrees) * Math.PI) / 180;
    const cos = Math.cos(theta);
    const sin = Math.sin(theta);
    return {
      // SVG/screen coordinates use a downward-positive y axis.
      x: x * cos - y * sin,
      y: x * sin + y * cos
    };
  }

  function adjustedEdgeEndCtrl(edge, start, end, ctrl2) {
    const candidate = safeCtrl(ctrl2);
    if (isSelfLoopEdge(edge)) {
      return candidate;
    }
    const minTangent = Math.max(10, edgeArrowMetrics().trim + 2);
    let dx = end.x - candidate.x;
    let dy = end.y - candidate.y;
    let magnitude = Math.hypot(dx, dy);
    if (Number.isFinite(magnitude) && magnitude >= minTangent) {
      return candidate;
    }
    dx = end.x - start.x;
    dy = end.y - start.y;
    magnitude = Math.hypot(dx, dy);
    if (!Number.isFinite(magnitude) || magnitude < 0.01) {
      return candidate;
    }
    const ux = dx / magnitude;
    const uy = dy / magnitude;
    return {
      x: end.x - ux * minTangent,
      y: end.y - uy * minTangent
    };
  }

  function trimEdgeEnd(start, end, ctrl2, trim) {
    if (!trim || trim <= 0) return null;
    let dx = end.x - ctrl2.x;
    let dy = end.y - ctrl2.y;
    if (!Number.isFinite(dx) || !Number.isFinite(dy) || Math.hypot(dx, dy) < 0.01) {
      dx = end.x - start.x;
      dy = end.y - start.y;
    }
    const magnitude = Math.hypot(dx, dy);
    if (!Number.isFinite(magnitude) || magnitude < trim + 0.5) {
      return null;
    }
    const ux = dx / magnitude;
    const uy = dy / magnitude;
    const offsetX = ux * trim;
    const offsetY = uy * trim;
    return {
      end: { ...end, x: end.x - offsetX, y: end.y - offsetY },
      ctrl2: { x: ctrl2.x - offsetX, y: ctrl2.y - offsetY }
    };
  }

  function safeCtrl(point) {
    const cx = normalizeCoord(point?.cx, point?.x);
    const cy = normalizeCoord(point?.cy, point?.y);
    return { x: cx, y: cy };
  }

  function normalizeCoord(value, fallback) {
    if (!Number.isFinite(value) || Math.abs(value) > 1_000_000) {
      return fallback ?? 0;
    }
    return value;
  }

  function fallbackEdgeEndpoints(edge, drag) {
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    if (!source || !target) return null;
    const sourceCenter = nodeCenter(source, drag);
    const targetCenter = nodeCenter(target, drag);
    if (edge.sourceId && edge.sourceId === edge.targetId) {
      return { start: sourceCenter, end: targetCenter };
    }
    const start = nodeBoundaryPoint(source, targetCenter, drag);
    const end = nodeBoundaryPoint(target, sourceCenter, drag);
    return { start, end };
  }

  function nodeBoundaryPoint(node, toward, drag) {
    const pos = nodePosition(node, drag);
    const { w, h } = nodeSize(node);
    const cx = pos.x + w / 2;
    const cy = pos.y + h / 2;
    const dx = toward.x - cx;
    const dy = toward.y - cy;
    if (!Number.isFinite(dx) || !Number.isFinite(dy) || (!dx && !dy)) {
      return { x: cx, y: cy };
    }
    if (node.type === "Super") {
      const halfW = w / 2;
      const halfH = h / 2;
      const absDx = Math.abs(dx);
      const absDy = Math.abs(dy);
      const nx = halfW > 0 ? absDx / halfW : 0;
      const ny = halfH > 0 ? absDy / halfH : 0;
      const denom = Math.pow(nx, SUPER_NODE_SHAPE_POWER) + Math.pow(ny, SUPER_NODE_SHAPE_POWER);
      const scale = denom > 0 ? 1 / Math.pow(denom, 1 / SUPER_NODE_SHAPE_POWER) : 0;
      return { x: cx + dx * scale, y: cy + dy * scale };
    }
    const rx = w / 2;
    const ry = h / 2;
    const denom = (dx * dx) / (rx * rx) + (dy * dy) / (ry * ry);
    if (!Number.isFinite(denom) || denom <= 0) {
      return { x: cx, y: cy };
    }
    const t = 1 / Math.sqrt(denom);
    return { x: cx + dx * t, y: cy + dy * t };
  }

  function nodeCenter(node, drag) {
    const pos = nodePosition(node, drag);
    const x = pos.x;
    const y = pos.y;
    const { w, h } = nodeSize(node);
    return { x: x + w / 2, y: y + h / 2 };
  }

  function nodeContainsPoint(node, point, drag) {
    if (!node || !point) return false;
    const pos = nodePosition(node, drag);
    const { w, h } = nodeSize(node);
    return point.x >= pos.x && point.x <= pos.x + w && point.y >= pos.y && point.y <= pos.y + h;
  }

  function findNodeAtPoint(point, drag) {
    if (!point) return null;
    for (let i = nodes.length - 1; i >= 0; i -= 1) {
      const node = nodes[i];
      if (nodeContainsPoint(node, point, drag)) {
        return node;
      }
    }
    return null;
  }

  function edgeLabel(edge, values, state) {
    if (edge.condition) return edge.condition;
    if (edge.probability !== undefined && edge.probability !== null) {
      return `${edge.probability}%`;
    }
    if (edge.timeoutExpr) {
      if (state === "running") {
        const runtimeValue = runtimeNumericValue(edge.timeoutExpr, values);
        if (runtimeValue !== null) {
          const expr = edge.timeoutExpr.trim();
          return `${runtimeValue}ms (${expr})`;
        }
      }
      return `${edge.timeoutExpr}(ms)`;
    }
    if (edge.timeoutMs !== undefined && edge.timeoutMs !== null) {
      return `${edge.timeoutMs}ms`;
    }
    return "";
  }

  function timeoutEdgeProgress(entry, now) {
    if (!entry || !Number.isFinite(entry.timeoutMs) || entry.timeoutMs <= 0) {
      return null;
    }
    const startedAt = Number(entry.startedAt);
    if (!Number.isFinite(startedAt)) {
      return null;
    }
    const progress = (now - startedAt) / entry.timeoutMs;
    if (progress <= 0 || progress >= 1) {
      return null;
    }
    return Math.min(1, Math.max(0, progress));
  }

  function edgeLabelPos(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length >= 2) {
      const start = pts[0];
      const end = pts[pts.length - 1];
      const controls = edgeCurveControls(edge, start, end, drag);
      const ctrl1 = controls.ctrl1;
      const ctrl2 = controls.ctrl2;
      const mid = cubicPointAt(start, ctrl1, ctrl2, end, 0.5);
      const lineMid = { x: (start.x + end.x) / 2, y: (start.y + end.y) / 2 };
      const dx = mid.x - lineMid.x;
      const dy = mid.y - lineMid.y;
      const boost = isSelfLoopEdge(edge) ? 0.10 : 0.5;
      let x = mid.x + dx * boost;
      let y = mid.y + dy * boost;
      const offsetUnits = edgeLabelOffsets.get(edge.id) || 0;
      if (offsetUnits) {
        const source = nodeMap.get(edge.sourceId);
        const target = nodeMap.get(edge.targetId);
        let baseDx = end.x - start.x;
        let baseDy = end.y - start.y;
        if (source && target) {
          const aId = edge.sourceId || "";
          const bId = edge.targetId || "";
          const a = aId < bId ? source : target;
          const b = aId < bId ? target : source;
          const aCenter = nodeCenter(a, drag);
          const bCenter = nodeCenter(b, drag);
          baseDx = bCenter.x - aCenter.x;
          baseDy = bCenter.y - aCenter.y;
        }
        const length = Math.hypot(baseDx, baseDy);
        if (length > 0) {
          const spacing = Math.max(8, Math.round(labelLineHeight * 0.9));
          const curvedOffset = Math.hypot(dx, dy);
          const offsetThreshold = Math.max(6, Math.round(labelLineHeight * 0.6));
          if (curvedOffset >= offsetThreshold) {
            return { x, y };
          }
          if (Math.abs(baseDy) > Math.abs(baseDx)) {
            y += offsetUnits * spacing;
          } else {
            const perpX = -baseDy / length;
            const perpY = baseDx / length;
            x += perpX * offsetUnits * spacing;
            y += perpY * offsetUnits * spacing;
          }
        }
      }
      return { x, y };
    }
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    if (source && target) {
      const s = nodeCenter(source, drag);
      const t = nodeCenter(target, drag);
      return { x: (s.x + t.x) / 2, y: (s.y + t.y) / 2 };
    }
    return { x: 0, y: 0 };
  }

  function timeoutLiteralMs(edge) {
    if (!edge || edge.type !== "TEDGE") return null;
    if ((edge.timeoutExpr || "").trim()) return null;
    const raw = edge.timeoutMs;
    if (Number.isFinite(raw) && raw >= 0) {
      return Math.floor(raw);
    }
    const parsed = Number.parseInt(String(raw ?? "").trim(), 10);
    return Number.isFinite(parsed) && parsed >= 0 ? parsed : null;
  }

  function timeoutSliderPosFromMs(ms) {
    const clamped = Math.max(0, Math.min(TIMEOUT_INLINE_MAX_MS, Math.floor(ms)));
    if (clamped <= TIMEOUT_INLINE_LEFT_MAX_MS) {
      return Math.round((clamped / TIMEOUT_INLINE_LEFT_MAX_MS) * TIMEOUT_INLINE_HALF);
    }
    return Math.round(
      TIMEOUT_INLINE_HALF +
      ((clamped - TIMEOUT_INLINE_LEFT_MAX_MS) / (TIMEOUT_INLINE_MAX_MS - TIMEOUT_INLINE_LEFT_MAX_MS)) * TIMEOUT_INLINE_HALF
    );
  }

  function timeoutMsFromSliderPos(pos) {
    const clamped = Math.max(0, Math.min(TIMEOUT_INLINE_SLIDER_MAX, Math.floor(pos)));
    if (clamped <= TIMEOUT_INLINE_HALF) {
      return Math.round((clamped / TIMEOUT_INLINE_HALF) * TIMEOUT_INLINE_LEFT_MAX_MS);
    }
    const value = TIMEOUT_INLINE_LEFT_MAX_MS +
      ((clamped - TIMEOUT_INLINE_HALF) / TIMEOUT_INLINE_HALF) * (TIMEOUT_INLINE_MAX_MS - TIMEOUT_INLINE_LEFT_MAX_MS);
    return Math.min(TIMEOUT_INLINE_MAX_MS, Math.max(TIMEOUT_INLINE_LEFT_MAX_MS + 1, Math.round(value)));
  }

  function edgeSliderBackground(color) {
    if (typeof color !== "string" || !color) return "rgba(255, 255, 255, 0.94)";
    if (color.startsWith("#")) {
      const rgb = hexToRgb(color);
      if (rgb) {
        return `rgba(${rgb.r}, ${rgb.g}, ${rgb.b}, 0.16)`;
      }
    }
    return "rgba(255, 255, 255, 0.94)";
  }

  function timeoutSliderModel(edge) {
    const base = timeoutLiteralMs(edge);
    if (!Number.isFinite(base)) return null;
    if (base > TIMEOUT_INLINE_MAX_MS) return null;
    const draft = timeoutInlineDrafts.get(edge.id);
    const valueMs = Math.max(0, Math.min(TIMEOUT_INLINE_MAX_MS, Number.isFinite(draft) ? draft : base));
    return {
      valueMs,
      value: timeoutSliderPosFromMs(valueMs),
      min: 0,
      max: TIMEOUT_INLINE_SLIDER_MAX,
      step: 1
    };
  }

  function handleTimeoutInlineSliderInput(edge, event) {
    const sliderPos = Number.parseInt(String(event?.currentTarget?.value ?? "").trim(), 10);
    if (!edge?.id || !Number.isFinite(sliderPos)) return;
    const value = timeoutMsFromSliderPos(sliderPos);
    const next = new Map(timeoutInlineDrafts);
    next.set(edge.id, value);
    timeoutInlineDrafts = next;
    if (typeof onTimeoutEdgeUpdate === "function") {
      onTimeoutEdgeUpdate(edge.id, value);
    }
  }

  function startTimeoutInlineDrag(edge, event) {
    if (!edge?.id) return;
    event?.stopPropagation?.();
    timeoutInlineDragEdgeId = edge.id;
    clearSelection();
  }

  function endTimeoutInlineDrag(event) {
    event?.stopPropagation?.();
    timeoutInlineDragEdgeId = "";
  }

  function edgeMidPoint(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length >= 2) {
      const start = pts[0];
      const end = pts[pts.length - 1];
      const ctrl1 = safeCtrl(start);
      const ctrl2 = safeCtrl(end);
      return cubicPointAt(start, ctrl1, ctrl2, end, 0.5);
    }
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    if (source && target) {
      const s = nodeCenter(source, drag);
      const t = nodeCenter(target, drag);
      return { x: (s.x + t.x) / 2, y: (s.y + t.y) / 2 };
    }
    return null;
  }

  function edgeBendHandlePos(edge, drag, label) {
    const pos = edgeMidPoint(edge, drag);
    if (!pos) return null;
    if (!label) return pos;
    const offset = Math.max(10, Math.round(labelLineHeight * 0.9));
    return { x: pos.x, y: pos.y - offset };
  }

  function edgeControlPoints(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length < 2) return null;
    const start = pts[0];
    const end = pts[pts.length - 1];
    const controls = edgeCurveControls(edge, start, end, drag);
    return {
      start,
      end,
      ctrl1: controls.ctrl1,
      ctrl2: controls.ctrl2
    };
  }

  function cubicPointAt(start, ctrl1, ctrl2, end, t) {
    const clamped = Math.min(1, Math.max(0, t));
    const inv = 1 - clamped;
    const inv2 = inv * inv;
    const inv3 = inv2 * inv;
    const t2 = clamped * clamped;
    const t3 = t2 * clamped;
    const x =
      inv3 * start.x +
      3 * inv2 * clamped * ctrl1.x +
      3 * inv * t2 * ctrl2.x +
      t3 * end.x;
    const y =
      inv3 * start.y +
      3 * inv2 * clamped * ctrl1.y +
      3 * inv * t2 * ctrl2.y +
      t3 * end.y;
    return { x, y };
  }

  function nodeTooltip(node) {
    if (!node) return "";
    const lines = [];
    const name = node.name || "(unnamed)";
    lines.push(`${node.type === "Super" ? "Super node" : "Node"}: ${name}`);
    if (node.flavour) {
      lines.push(`Flavour: ${node.flavour}`);
    }
    if (node.isStart) lines.push("Start node");
    if (node.isAltStart) lines.push("Alt start");
    if (node.isHistory) lines.push("History node");
    if (node.comment) {
      lines.push(`Comment: ${node.comment}`);
    }
    return lines.join("\n");
  }

  function runtimeNumericValue(expr, values) {
    if (!expr) return null;
    const name = expr.trim();
    if (!name) return null;
    const raw = values?.[name];
    if (Number.isFinite(raw)) return raw;
    if (raw === null || raw === undefined) return null;
    const text = String(raw).trim();
    if (!text) return null;
    if (!/^-?\d+(\.\d+)?$/.test(text)) return null;
    const parsed = Number.parseFloat(text);
    return Number.isFinite(parsed) ? parsed : null;
  }

  function edgeTooltip(edge, values) {
    if (!edge) return "";
    const lines = [];
    if (edge.type) {
      lines.push(`Edge: ${edge.type}`);
    }
    if (edge.condition) {
      lines.push(`Condition: ${edge.condition}`);
    }
    if (edge.probability !== undefined && edge.probability !== null) {
      lines.push(`Probability: ${edge.probability}`);
    }
    if (edge.timeoutExpr) {
      const expr = edge.timeoutExpr.trim();
      const runtimeValue = runtimeNumericValue(expr, values);
      if (runtimeValue !== null) {
        lines.push(`Timeout: ${runtimeValue}ms (${expr})`);
      } else {
        lines.push(`Timeout expr: ${edge.timeoutExpr}`);
      }
      return lines.join("\n");
    }
    if (edge.timeoutMs !== undefined && edge.timeoutMs !== null) {
      lines.push(`Timeout: ${edge.timeoutMs}ms`);
    }
    return lines.join("\n");
  }

  function commentTooltip(comment) {
    const text = stripHtml(comment?.text).trim();
    if (!text) return "";
    return `Comment: ${text}`;
  }

  function stripHtml(text) {
    if (!text) return "";
    const normalized = text.replace(/<br\s*\/?>/gi, "\n");
    const stripped = normalized.replace(/<[^>]*>/g, "");
    return stripped
      .replace(/&nbsp;/gi, " ")
      .replace(/&#160;/gi, " ")
      .replace(/&#xA0;/gi, " ")
      .replace(/&amp;/gi, "&")
      .replace(/&lt;/gi, "<")
      .replace(/&gt;/gi, ">")
      .replace(/&quot;/gi, "\"")
      .replace(/&#39;/gi, "'");
  }

  function normalizeCommentText(text) {
    return (text ?? "").replace(/\r\n/g, "\n");
  }

  function commentLines(comment) {
    const text = stripHtml(comment?.text);
    if (!text) return [];
    return text.split(/\r?\n/).map(preserveSpaces);
  }

  function preserveSpaces(line) {
    return line.replace(/ /g, "\u00A0");
  }

  function handleCommentKeydown(comment, event) {
    if (editingCommentId === comment.id) {
      return;
    }
    if (event.target?.closest?.(".comment-editor")) {
      return;
    }
    if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") {
      return;
    }
    event.preventDefault();
    selectComment(comment.id);
  }

  function handleEdgeKeydown(edge, event) {
    if (event.key === "r" || event.key === "R") {
      event.preventDefault();
      if (typeof onEdgeControlUpdate === "function") {
        onEdgeControlUpdate(edge.id, "reset", 0, 0);
      }
      return;
    }
    if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") {
      return;
    }
    event.preventDefault();
    selectEdge(edge.id);
  }

  function commentAriaLabel(comment) {
    const text = stripHtml(comment?.text).trim();
    if (!text) return "Comment";
    return `Comment: ${text}`;
  }

  function findCommentById(commentId) {
    if (!commentId) return null;
    return comments.find((comment) => comment.id === commentId) || null;
  }

  async function startCommentEdit(comment) {
    if (!comment) return;
    if (editingCommentId && editingCommentId !== comment.id) {
      commitCommentEdit();
    }
    editingCommentId = comment.id;
    editingCommentDraft = stripHtml(comment.text || "").replace(/\u00a0/g, " ");
    editingCommentOriginal = editingCommentDraft;
    await tick();
    if (commentEditorEl) {
      if (typeof commentEditorEl.focus === "function") {
        try {
          commentEditorEl.focus({ preventScroll: true });
        } catch (err) {
          commentEditorEl.focus();
        }
      }
      if (typeof commentEditorEl.setSelectionRange === "function") {
        const end = commentEditorEl.value.length;
        commentEditorEl.setSelectionRange(end, end);
      }
    }
  }

  function commitCommentEdit() {
    if (!editingCommentId) return;
    const comment = findCommentById(editingCommentId);
    if (!comment) {
      editingCommentId = null;
      return;
    }
    const rect = commentRect(comment, dragState);
    if (typeof onCommentUpdate === "function") {
      const nextText = normalizeCommentText(editingCommentDraft ?? "");
      onCommentUpdate(comment.id, rect.x, rect.y, rect.w, rect.h, nextText);
    }
    editingCommentId = null;
    editingCommentOriginal = "";
  }

  function cancelCommentEdit() {
    editingCommentId = null;
    editingCommentDraft = editingCommentOriginal;
    editingCommentOriginal = "";
  }

  function handleCommentEditorKeydown(event) {
    const isEnter =
      event.key === "Enter" ||
      event.key === "NumpadEnter" ||
      event.key === "Return" ||
      event.code === "Enter" ||
      event.code === "NumpadEnter" ||
      event.keyCode === 13;
    if (event.key === "Escape") {
      event.preventDefault();
      cancelCommentEdit();
      return;
    }
    if ((event.ctrlKey || event.metaKey) && isEnter) {
      event.preventDefault();
      commitCommentEdit();
      return;
    }
  }

  function edgeAriaLabel(edge, label) {
    if (!edge) return "Edge";
    if (label) {
      return `Edge ${edge.type || ""}: ${label}`;
    }
    return edge.type ? `Edge ${edge.type}` : "Edge";
  }

  function handleNodeClick(node, event) {
    if (!node) return;
    if (edgeCreateMode && typeof onEdgePick === "function") {
      onEdgePick(node.id);
      return;
    }
    selectNode(node.id, { multi: isMultiModifier(event) });
  }

  function handleNodeDoubleClick(node) {
    if (!node) return;
    selectNode(node.id);
    if (node.type !== "Super") return;
    if (typeof onNavigate === "function") {
      onNavigate(node.id);
    }
  }

  function handleCommandOpen(node) {
    if (!node || typeof onCommandOpen !== "function") return;
    onCommandOpen(node.id);
  }

  function handleNodeKeydown(node, event) {
    if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") {
      return;
    }
    event.preventDefault();
    if (edgeCreateMode && typeof onEdgePick === "function") {
      onEdgePick(node.id);
      return;
    }
    selectNode(node.id);
    if (node.type === "Super" && event.key === "Enter") {
      handleNodeDoubleClick(node);
    }
  }

  function parseSceneDrop(event) {
    const data = event?.dataTransfer;
    if (!data) return null;
    const raw = data.getData(sceneDragType);
    if (raw) {
      try {
        return JSON.parse(raw);
      } catch (err) {
        return null;
      }
    }
    const text = data.getData("text/plain");
    if (!text) return null;
    try {
      const parsed = JSON.parse(text);
      if (parsed?.name) {
        return { name: parsed.name, language: parsed.language || "" };
      }
    } catch (err) {
      return { name: text, language: "" };
    }
    return null;
  }

  function parseAgentDrop(event) {
    const data = event?.dataTransfer;
    if (!data) return null;
    const raw = data.getData(agentDragType);
    if (raw) {
      try {
        return JSON.parse(raw);
      } catch (err) {
        return null;
      }
    }
    const text = data.getData("text/plain");
    if (!text) return null;
    try {
      const parsed = JSON.parse(text);
      if (parsed?.kind === "agent" && parsed?.name) {
        return parsed;
      }
    } catch (err) {
      return null;
    }
    return null;
  }

  function parseBlockDrop(event) {
    const data = event?.dataTransfer;
    if (!data) return null;
    const raw = data.getData(blockDragType);
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch (err) {
      return null;
    }
  }

  function findNodeAt(world) {
    if (!world) return null;
    for (let i = nodes.length - 1; i >= 0; i -= 1) {
      const node = nodes[i];
      const pos = nodePosition(node, dragState);
      const { w, h } = nodeSize(node);
      if (
        world.x >= pos.x &&
        world.x <= pos.x + w &&
        world.y >= pos.y &&
        world.y <= pos.y + h
      ) {
        return node;
      }
    }
    return null;
  }

  function isSceneDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(sceneDragType) || (types.includes("text/plain") && !types.includes(agentDragType));
  }

  function isAgentDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(agentDragType);
  }

  function isBlockDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(blockDragType);
  }

  function handleSceneDragOver(event) {
    if (!isSceneDrag(event) && !isBlockDrag(event) && !isAgentDrag(event)) return;
    event.preventDefault();
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = "copy";
    }
  }

  function handleSceneDrop(event) {
    if (isBlockDrag(event)) {
      const payload = parseBlockDrop(event);
      if (!payload) return;
      event.preventDefault();
      const world = eventToWorld(event);
      const clamped = clampWorldPoint(world);
      const target = findNodeAt(world);
      if (typeof onBlockDrop === "function") {
        onBlockDrop({
          ...payload,
          x: clamped.x,
          y: clamped.y,
          targetNodeId: target?.id || ""
        });
      }
      return;
    }
    if (isAgentDrag(event)) {
      const payload = parseAgentDrop(event);
      if (!payload?.name) return;
      event.preventDefault();
      const world = eventToWorld(event);
      const clamped = clampWorldPoint(world);
      const target = findNodeAt(world);
      if (typeof onAgentDrop === "function") {
        onAgentDrop({
          name: payload.name,
          type: payload.type || "processing",
          x: clamped.x,
          y: clamped.y,
          targetNodeId: target?.id || ""
        });
      }
      return;
    }
    const payload = parseSceneDrop(event);
    if (!payload || !payload.name) return;
    event.preventDefault();
    const world = eventToWorld(event);
    const clamped = clampWorldPoint(world);
    const target = findNodeAt(world);
    if (typeof onSceneDrop === "function") {
      onSceneDrop({
        name: payload.name,
        language: payload.language || "",
        x: clamped.x,
        y: clamped.y,
        targetNodeId: target?.id || ""
      });
    }
  }

  function nodePosition(node, drag) {
    const activeDrag = drag || dragState;
    if (activeDrag?.type === "group" && activeDrag.nodeOrigins?.[node.id]) {
      const origin = activeDrag.nodeOrigins[node.id];
      const nextX = origin.x + (activeDrag.dx ?? 0);
      const nextY = origin.y + (activeDrag.dy ?? 0);
      const clamped = clampNodePoint(node, { x: nextX, y: nextY });
      return nodeRenderPosition(node, clamped.x, clamped.y);
    }
    if (activeDrag?.type === "node" && activeDrag.id === node.id) {
      const baseX = activeDrag.x ?? node.graphics?.x ?? 0;
      const baseY = activeDrag.y ?? node.graphics?.y ?? 0;
      return nodeRenderPosition(node, baseX, baseY);
    }
    const baseX = node.graphics?.x ?? 0;
    const baseY = node.graphics?.y ?? 0;
    return nodeRenderPosition(node, baseX, baseY);
  }

  function commentPosition(comment, drag) {
    const rect = commentRect(comment, drag);
    return { x: rect.x, y: rect.y };
  }

  function toFinite(value, fallback = 0) {
    const num = Number(value);
    return Number.isFinite(num) ? num : fallback;
  }

  function safeSvgNumber(value) {
    return Number.isFinite(value) ? value : 0;
  }

  function commentRect(comment, drag) {
    const base = {
      x: toFinite(comment.rect?.x, 0),
      y: toFinite(comment.rect?.y, 0),
      w: Math.max(commentMinSize, toFinite(comment.rect?.w, 0)),
      h: Math.max(commentMinSize, toFinite(comment.rect?.h, 0))
    };
    const activeDrag = drag || dragState;
    if (activeDrag?.type === "group" && activeDrag.commentOrigins?.[comment.id]) {
      const origin = activeDrag.commentOrigins[comment.id];
      const nextX = origin.x + (activeDrag.dx ?? 0);
      const nextY = origin.y + (activeDrag.dy ?? 0);
      const clamped = clampWorldPoint({ x: nextX, y: nextY });
      return {
        x: clamped.x,
        y: clamped.y,
        w: origin.w,
        h: origin.h
      };
    }
    if (!activeDrag || activeDrag.id !== comment.id) {
      return base;
    }
    if (activeDrag.type === "comment") {
      return {
        x: toFinite(activeDrag.x, base.x),
        y: toFinite(activeDrag.y, base.y),
        w: Math.max(commentMinSize, toFinite(activeDrag.width, base.w)),
        h: Math.max(commentMinSize, toFinite(activeDrag.height, base.h))
      };
    }
    if (activeDrag.type === "comment-resize") {
      return {
        x: base.x,
        y: base.y,
        w: Math.max(commentMinSize, toFinite(activeDrag.width, base.w)),
        h: Math.max(commentMinSize, toFinite(activeDrag.height, base.h))
      };
    }
    return base;
  }

  function eventToWorld(event) {
    if (!svgEl) return { x: 0, y: 0 };
    const rect = svgEl.getBoundingClientRect();
    if (!rect.width || !rect.height) return { x: 0, y: 0 };
    const view = currentViewBox();
    const relX = clamp((event.clientX - rect.left) / rect.width, 0, 1);
    const relY = clamp((event.clientY - rect.top) / rect.height, 0, 1);
    return {
      x: view.x + relX * view.width,
      y: view.y + relY * view.height
    };
  }

  function clampWorldPoint(point) {
    if (!point) return { x: MIN_WORLD_COORD, y: MIN_WORLD_COORD };
    return {
      x: Math.max(MIN_WORLD_COORD, point.x ?? 0),
      y: Math.max(MIN_WORLD_COORD, point.y ?? 0)
    };
  }

  function clampNodePoint(node, point) {
    if (!point) return { x: MIN_WORLD_COORD, y: MIN_WORLD_COORD };
    if (!node || node.type !== "Super") {
      return clampWorldPoint(point);
    }
    const offset = nodeVisualOffset(node);
    return {
      x: Math.max(MIN_WORLD_COORD - offset.x, point.x ?? 0),
      y: Math.max(MIN_WORLD_COORD - offset.y, point.y ?? 0)
    };
  }

  function nodePositionKey(x, y) {
    return `${Math.round(x)}|${Math.round(y)}`;
  }

  function buildNodePositionSet(ignoreId) {
    const items = [];
    for (const entry of nodes || []) {
      if (!entry || entry.id === ignoreId) continue;
      const nxRaw = entry?.graphics?.x ?? entry?.x;
      const nyRaw = entry?.graphics?.y ?? entry?.y;
      const nx = Number(nxRaw);
      const ny = Number(nyRaw);
      if (!Number.isFinite(nx) || !Number.isFinite(ny)) continue;
      const size = nodeSize(entry);
      items.push({ x: nx, y: ny, w: size.w, h: size.h });
    }
    return items;
  }

  function avoidNodeOverlap(node, point) {
    if (!node || !point) return point;
    const occupied = buildNodePositionSet(node.id);
    const { w, h } = nodeSize(node);
    const overlaps = (pos) => {
      const rect = { x: pos.x, y: pos.y, w, h };
      for (const other of occupied) {
        if (
          rect.x < other.x + other.w &&
          rect.x + rect.w > other.x &&
          rect.y < other.y + other.h &&
          rect.y + rect.h > other.y
        ) {
          return true;
        }
      }
      return false;
    };
    if (!overlaps(point)) return point;
    const stepX = Number.isFinite(gridX) && gridX > 0 ? gridX : 10;
    const stepY = Number.isFinite(gridY) && gridY > 0 ? gridY : 10;
    const maxRadius = Math.max(20, Math.ceil(Math.sqrt(occupied.length || 0)) + 10);
    for (let radius = 1; radius <= maxRadius; radius += 1) {
      for (let dx = -radius; dx <= radius; dx += 1) {
        for (let dy = -radius; dy <= radius; dy += 1) {
          if (dx === 0 && dy === 0) continue;
          if (Math.abs(dx) !== radius && Math.abs(dy) !== radius) continue;
          const candidate = {
            x: Math.max(MIN_WORLD_COORD, point.x + dx * stepX),
            y: Math.max(MIN_WORLD_COORD, point.y + dy * stepY)
          };
          if (!overlaps(candidate)) {
            return candidate;
          }
        }
      }
    }
    const scanRadius = Math.max(40, maxRadius * 2);
    for (let dy = -scanRadius; dy <= scanRadius; dy += 1) {
      for (let dx = -scanRadius; dx <= scanRadius; dx += 1) {
        if (dx === 0 && dy === 0) continue;
        const candidate = {
          x: Math.max(MIN_WORLD_COORD, point.x + dx * stepX),
          y: Math.max(MIN_WORLD_COORD, point.y + dy * stepY)
        };
        if (!overlaps(candidate)) {
          return candidate;
        }
      }
    }
    return point;
  }

  function snapNodePoint(node, point) {
    if (!node || !point) return point;
    let nextPoint = point;
    if (nodeSnapToGrid && Number.isFinite(gridX) && Number.isFinite(gridY) && gridX > 0 && gridY > 0) {
      const { w, h } = nodeSize(node);
      const centerX = point.x + w / 2;
      const centerY = point.y + h / 2;
      const snappedCenterX = gridOriginX + Math.round((centerX - gridOriginX) / gridX) * gridX;
      const snappedCenterY = gridOriginY + Math.round((centerY - gridOriginY) / gridY) * gridY;
      nextPoint = { x: snappedCenterX - w / 2, y: snappedCenterY - h / 2 };
    }
    return avoidNodeOverlap(node, nextPoint);
  }

  function worldRectToScreenRect(rect) {
    if (!svgEl || !rect) return null;
    const view = currentViewBox();
    const bounds = svgEl.getBoundingClientRect();
    const width = bounds.width;
    const height = bounds.height;
    if (!width || !height || !view.width || !view.height) {
      return null;
    }
    const scaleX = width / view.width;
    const scaleY = height / view.height;
    return {
      x: (rect.x - view.x) * scaleX,
      y: (rect.y - view.y) * scaleY,
      w: rect.w * scaleX,
      h: rect.h * scaleY
    };
  }

  function startGroupDrag(event) {
    if (!event || event.button !== 0) return;
    const nodeIds = Array.from(selectedNodeIds);
    const commentIds = Array.from(selectedCommentIds);
    if (!nodeIds.length && !commentIds.length) {
      return;
    }
    const world = eventToWorld(event);
    const nodeOrigins = {};
    const commentOrigins = {};
    let minDx = -Infinity;
    let minDy = -Infinity;
    for (const nodeId of nodeIds) {
      const node = nodeMap.get(nodeId);
      if (!node) continue;
      const originX = node.graphics?.x ?? 0;
      const originY = node.graphics?.y ?? 0;
      nodeOrigins[nodeId] = { x: originX, y: originY };
      const offset = nodeVisualOffset(node);
      const minX = MIN_WORLD_COORD - offset.x;
      const minY = MIN_WORLD_COORD - offset.y;
      minDx = Math.max(minDx, minX - originX);
      minDy = Math.max(minDy, minY - originY);
    }
    for (const commentId of commentIds) {
      const comment = findCommentById(commentId);
      if (!comment) continue;
      const rect = commentRect(comment, null);
      commentOrigins[commentId] = { x: rect.x, y: rect.y, w: rect.w, h: rect.h };
      minDx = Math.max(minDx, MIN_WORLD_COORD - rect.x);
      minDy = Math.max(minDy, MIN_WORLD_COORD - rect.y);
    }
    if (!Number.isFinite(minDx)) minDx = 0;
    if (!Number.isFinite(minDy)) minDy = 0;
    dragState = {
      type: "group",
      nodeIds,
      commentIds,
      nodeOrigins,
      commentOrigins,
      minDx,
      minDy,
      dx: 0,
      dy: 0,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startNodeDrag(event, node) {
    if (!node || event.button !== 0) return;
    if (edgeCreateMode) return;
    if (isMultiModifier(event)) return;
    event.preventDefault();
    focusStage();
    if (selectedNodeIds.has(node.id) && (selectedNodeIds.size + selectedCommentIds.size > 1)) {
      startGroupDrag(event);
      return;
    }
    selectNode(node.id);
    const pos = { x: node.graphics?.x ?? 0, y: node.graphics?.y ?? 0 };
    const world = eventToWorld(event);
    dragState = {
      type: "node",
      id: node.id,
      originX: pos.x,
      originY: pos.y,
      x: pos.x,
      y: pos.y,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startCommentDrag(event, comment) {
    if (!comment || event.button !== 0) return;
    if (editingCommentId === comment.id) return;
    if (isMultiModifier(event)) return;
    event.preventDefault();
    focusStage();
    if (selectedCommentIds.has(comment.id) && (selectedNodeIds.size + selectedCommentIds.size > 1)) {
      startGroupDrag(event);
      return;
    }
    selectComment(comment.id);
    const rect = commentRect(comment, null);
    const world = eventToWorld(event);
    dragState = {
      type: "comment",
      id: comment.id,
      originX: rect.x,
      originY: rect.y,
      x: rect.x,
      y: rect.y,
      width: rect.w,
      height: rect.h,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startCommentResize(event, comment) {
    if (!comment || event.button !== 0) return;
    if (editingCommentId === comment.id) return;
    event.preventDefault();
    focusStage();
    selectComment(comment.id);
    const rect = commentRect(comment, null);
    const world = eventToWorld(event);
    dragState = {
      type: "comment-resize",
      id: comment.id,
      originX: rect.x,
      originY: rect.y,
      originWidth: rect.w,
      originHeight: rect.h,
      x: rect.x,
      y: rect.y,
      width: rect.w,
      height: rect.h,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startEdgeControlDrag(event, edge, handle, point) {
    if (!edge || event.button !== 0) return;
    if (edgeCreateMode) return;
    event.preventDefault();
    focusStage();
    selectEdge(edge.id);
    if (isSelfLoopEdge(edge)) {
      selfLoopManualControlIds = new Set(selfLoopManualControlIds).add(edge.id);
    }
    const world = eventToWorld(event);
    dragState = {
      type: "edge-control",
      id: edge.id,
      handle,
      originX: point?.x ?? world.x,
      originY: point?.y ?? world.y,
      cx: point?.x ?? world.x,
      cy: point?.y ?? world.y,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startEdgeTargetDrag(event, edge, point) {
    if (!edge || event.button !== 0) return;
    if (edgeCreateMode) return;
    event.preventDefault();
    focusStage();
    selectEdge(edge.id);
    const world = eventToWorld(event);
    dragState = {
      type: "edge-target",
      id: edge.id,
      x: point?.x ?? world.x,
      y: point?.y ?? world.y,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function startEdgeBendDrag(event, edge, controls) {
    if (!edge || event.button !== 0) return;
    if (edgeCreateMode) return;
    if (!controls) return;
    event.preventDefault();
    focusStage();
    selectEdge(edge.id);
    if (isSelfLoopEdge(edge)) {
      selfLoopManualControlIds = new Set(selfLoopManualControlIds).add(edge.id);
    }
    const world = eventToWorld(event);
    dragState = {
      type: "edge-bend",
      id: edge.id,
      originCtrl1: { x: controls.ctrl1.x, y: controls.ctrl1.y },
      originCtrl2: { x: controls.ctrl2.x, y: controls.ctrl2.y },
      dx: 0,
      dy: 0,
      startX: world.x,
      startY: world.y,
      moved: false,
      pointerId: event.pointerId
    };
    const captureEl = stageEl || svgEl;
    if (captureEl) {
      captureEl.setPointerCapture(event.pointerId);
    }
  }

  function updateDrag(event) {
    if (!dragState || dragState.pointerId !== event.pointerId) return;
    const world = eventToWorld(event);
    const dragPoint = world;
    const dx = world.x - dragState.startX;
    const dy = world.y - dragState.startY;
    if (dragState.type === "group") {
      const clampedDx = Math.max(dx, dragState.minDx ?? dx);
      const clampedDy = Math.max(dy, dragState.minDy ?? dy);
      dragState = {
        ...dragState,
        dx: clampedDx,
        dy: clampedDy,
        moved: dragState.moved || Math.hypot(clampedDx, clampedDy) > dragThreshold
      };
      return;
    }
    if (dragState.type === "edge-control") {
      dragState = {
        ...dragState,
        cx: dragPoint.x,
        cy: dragPoint.y,
        moved: dragState.moved || Math.hypot(dx, dy) > dragThreshold
      };
      return;
    }
    if (dragState.type === "edge-target") {
      dragState = {
        ...dragState,
        x: dragPoint.x,
        y: dragPoint.y,
        moved: dragState.moved || Math.hypot(dx, dy) > dragThreshold
      };
      const hoveredNode = findNodeAtPoint(dragPoint, null);
      edgeRetargetHoverId = hoveredNode ? hoveredNode.id : null;
      return;
    }
    if (dragState.type === "edge-bend") {
      const bendDx = dragPoint.x - dragState.startX;
      const bendDy = dragPoint.y - dragState.startY;
      dragState = {
        ...dragState,
        dx: bendDx,
        dy: bendDy,
        moved: dragState.moved || Math.hypot(bendDx, bendDy) > dragThreshold
      };
      return;
    }
    if (dragState.type === "comment-resize") {
      const nextWidth = Math.max(commentMinSize, (dragState.originWidth ?? 0) + dx);
      const nextHeight = Math.max(commentMinSize, (dragState.originHeight ?? 0) + dy);
      dragState = {
        ...dragState,
        width: nextWidth,
        height: nextHeight,
        moved: dragState.moved || Math.hypot(dx, dy) > dragThreshold
      };
      return;
    }
    const nextX = dragState.originX + dx;
    const nextY = dragState.originY + dy;
    let clampedX = nextX;
    let clampedY = nextY;
    if (dragState.type === "node") {
      const node = nodeMap.get(dragState.id);
      const clamped = clampNodePoint(node, { x: nextX, y: nextY });
      clampedX = clamped.x;
      clampedY = clamped.y;
    }
    if (dragState.type === "comment") {
      const clamped = clampWorldPoint({ x: nextX, y: nextY });
      clampedX = clamped.x;
      clampedY = clamped.y;
    }
    dragState = {
      ...dragState,
      x: clampedX,
      y: clampedY,
      moved: dragState.moved || Math.hypot(dx, dy) > dragThreshold
    };
  }

  async function endDrag(event) {
    if (!dragState || dragState.pointerId !== event.pointerId) return;
    const finished = dragState;
    dragState = null;
    timeoutInlineDragEdgeId = "";
    edgeRetargetHoverId = null;
    const captureEl = stageEl || svgEl;
    if (captureEl && captureEl.hasPointerCapture(event.pointerId)) {
      captureEl.releasePointerCapture(event.pointerId);
    }
    if (!finished.moved) {
      return;
    }
    const finalX = finished.x ?? finished.originX;
    const finalY = finished.y ?? finished.originY;
    if (finished.type === "group") {
      const dx = finished.dx ?? 0;
      const dy = finished.dy ?? 0;
      const nodeMoves = [];
      if (finished.nodeIds?.length) {
        for (const nodeId of finished.nodeIds || []) {
          const origin = finished.nodeOrigins?.[nodeId];
          if (!origin) continue;
          const node = nodeMap.get(nodeId);
          const nextX = origin.x + dx;
          const nextY = origin.y + dy;
          const clamped = clampNodePoint(node, { x: nextX, y: nextY });
          const snapped = snapNodePoint(node, clamped);
          nodeMoves.push({ id: nodeId, x: snapped.x, y: snapped.y });
        }
      }
      if (nodeMoves.length > 1 && typeof onNodeGroupMove === "function") {
        await Promise.resolve(onNodeGroupMove(nodeMoves, nodeSnapToGrid));
      } else if (typeof onNodeMove === "function") {
        for (const move of nodeMoves) {
          await Promise.resolve(onNodeMove(move.id, move.x, move.y, nodeSnapToGrid));
        }
      }
      if (typeof onCommentUpdate === "function") {
        for (const commentId of finished.commentIds || []) {
          const origin = finished.commentOrigins?.[commentId];
          if (!origin) continue;
          const nextX = origin.x + dx;
          const nextY = origin.y + dy;
          const clamped = clampWorldPoint({ x: nextX, y: nextY });
          await Promise.resolve(onCommentUpdate(commentId, clamped.x, clamped.y, origin.w, origin.h));
        }
      }
      return;
    }
    if (finished.type === "node" && typeof onNodeMove === "function") {
      const node = nodeMap.get(finished.id);
      const clamped = clampNodePoint(node, { x: finalX, y: finalY });
      const snapped = snapNodePoint(node, clamped);
      onNodeMove(finished.id, snapped.x, snapped.y, nodeSnapToGrid);
    }
    if ((finished.type === "comment" || finished.type === "comment-resize") && typeof onCommentUpdate === "function") {
      onCommentUpdate(finished.id, finalX, finalY, finished.width, finished.height);
    }
    if (finished.type === "edge-control" && typeof onEdgeControlUpdate === "function") {
      onEdgeControlUpdate(finished.id, finished.handle, finished.cx, finished.cy);
    }
    if (finished.type === "edge-target") {
      const point = { x: finalX, y: finalY };
      const target = findNodeAtPoint(point, null);
      if (target && typeof onEdgeRetarget === "function") {
        timeoutSliderSuppressedEdgeId = finished.id;
        onEdgeRetarget(finished.id, target.id, point.x, point.y);
      }
      suppressStageClick = true;
      return;
    }
    if (finished.type === "edge-bend" && typeof onEdgeControlUpdate === "function") {
      const dx = finished.dx ?? 0;
      const dy = finished.dy ?? 0;
      onEdgeControlUpdate(finished.id, "bend", dx, dy);
    }
  }

  function edgePoints(edge, drag) {
    const pts = edge.graphics?.points || [];
    if (!pts.length) return pts;
    const activeDrag = drag || dragState;
    let nextPoints = pts;
    if (activeDrag?.type === "group") {
      const movedNodes = new Set(activeDrag.nodeIds || []);
      if (movedNodes.size && (movedNodes.has(edge.sourceId) || movedNodes.has(edge.targetId))) {
        const dx = activeDrag.dx ?? 0;
        const dy = activeDrag.dy ?? 0;
        const movedSource = movedNodes.has(edge.sourceId);
        const movedTarget = movedNodes.has(edge.targetId);
        if (movedSource && movedTarget) {
          nextPoints = nextPoints.map((pt) => ({
            ...pt,
            x: pt.x + dx,
            y: pt.y + dy,
            cx: Number.isFinite(pt.cx) ? pt.cx + dx : pt.cx,
            cy: Number.isFinite(pt.cy) ? pt.cy + dy : pt.cy
          }));
        } else {
          const lastIdx = nextPoints.length - 1;
          nextPoints = nextPoints.map((pt, idx) => {
            const isStart = idx === 0;
            const isEnd = idx === lastIdx;
            const adjustSource = isStart && movedSource;
            const adjustTarget = isEnd && movedTarget;
            if (!adjustSource && !adjustTarget) {
              return pt;
            }
            const adjX = pt.x + dx;
            const adjY = pt.y + dy;
            const adjCx = Number.isFinite(pt.cx) ? pt.cx + dx : pt.cx;
            const adjCy = Number.isFinite(pt.cy) ? pt.cy + dy : pt.cy;
            return { ...pt, x: adjX, y: adjY, cx: adjCx, cy: adjCy };
          });
        }
      }
    }
    if (activeDrag?.type === "node") {
      const movedId = activeDrag.id;
      if (movedId && (edge.sourceId === movedId || edge.targetId === movedId)) {
        const movedNode = nodeMap.get(movedId);
        if (movedNode) {
          const origin = movedNode.graphics || {};
          const originX = Number.isFinite(origin.x) ? origin.x : 0;
          const originY = Number.isFinite(origin.y) ? origin.y : 0;
          const nextX = Number.isFinite(activeDrag.x) ? activeDrag.x : originX;
          const nextY = Number.isFinite(activeDrag.y) ? activeDrag.y : originY;
          const dx = nextX - originX;
          const dy = nextY - originY;
          if (Number.isFinite(dx) && Number.isFinite(dy) && (dx || dy)) {
            const isSelfLoop = edge.sourceId === movedId && edge.targetId === movedId;
            if (isSelfLoop) {
              nextPoints = nextPoints.map((pt) => ({
                ...pt,
                x: pt.x + dx,
                y: pt.y + dy,
                cx: Number.isFinite(pt.cx) ? pt.cx + dx : pt.cx,
                cy: Number.isFinite(pt.cy) ? pt.cy + dy : pt.cy
              }));
            } else {
            nextPoints = nextPoints.map((pt, idx) => {
              const isStart = idx === 0;
              const isEnd = idx === nextPoints.length - 1;
              const adjustSource = edge.sourceId === movedId && isStart;
              const adjustTarget = edge.targetId === movedId && isEnd;
              if (!adjustSource && !adjustTarget) {
                return pt;
              }
              const adjX = pt.x + dx;
              const adjY = pt.y + dy;
              const adjCx = Number.isFinite(pt.cx) ? pt.cx + dx : pt.cx;
              const adjCy = Number.isFinite(pt.cy) ? pt.cy + dy : pt.cy;
              return { ...pt, x: adjX, y: adjY, cx: adjCx, cy: adjCy };
            });
            }
          }
        }
      }
    }
    if (activeDrag?.type === "edge-control" && activeDrag.id === edge.id) {
      const idx = activeDrag.handle === "ctrl1" ? 0 : nextPoints.length - 1;
      if (nextPoints[idx]) {
        const next = { ...nextPoints[idx], cx: activeDrag.cx, cy: activeDrag.cy };
        nextPoints = nextPoints.map((pt, index) => (index === idx ? next : pt));
      }
    }
    if (activeDrag?.type === "edge-target" && activeDrag.id === edge.id) {
      const lastIdx = nextPoints.length - 1;
      const end = nextPoints[lastIdx];
      if (end) {
        const dx = (activeDrag.x ?? end.x) - end.x;
        const dy = (activeDrag.y ?? end.y) - end.y;
        const next = {
          ...end,
          x: activeDrag.x ?? end.x,
          y: activeDrag.y ?? end.y,
          cx: Number.isFinite(end.cx) ? end.cx + dx : end.cx,
          cy: Number.isFinite(end.cy) ? end.cy + dy : end.cy
        };
        nextPoints = nextPoints.map((pt, index) => (index === lastIdx ? next : pt));
      }
    }
    if (activeDrag?.type === "edge-bend" && activeDrag.id === edge.id) {
      const dx = activeDrag.dx ?? 0;
      const dy = activeDrag.dy ?? 0;
      const originCtrl1 = activeDrag.originCtrl1;
      const originCtrl2 = activeDrag.originCtrl2;
      const lastIdx = nextPoints.length - 1;
      nextPoints = nextPoints.map((pt, index) => {
        if (index !== 0 && index !== lastIdx) return pt;
        const origin = index === 0 ? originCtrl1 : originCtrl2;
        if (!origin) return pt;
        return { ...pt, cx: origin.x + dx, cy: origin.y + dy };
      });
    }
    const adjusted = applyEdgeOffsets(nextPoints, edge);
    if (activeDrag?.type === "edge-target" && activeDrag.id === edge.id) {
      return adjusted;
    }
    return adjustEdgeEndpoints(adjusted, edge, drag);
  }

  function applyEdgeOffsets(points, edge) {
    if (!points?.length) return points;
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    const sourceOffset = nodeVisualOffset(source);
    const targetOffset = nodeVisualOffset(target);
    const lastIndex = points.length - 1;
    return points.map((pt, idx) => {
      const offset =
        idx === 0
          ? sourceOffset
          : idx === lastIndex
            ? targetOffset
            : null;
      if (!offset || (!offset.x && !offset.y)) {
        return pt;
      }
      const next = {
        ...pt,
        x: pt.x + offset.x,
        y: pt.y + offset.y
      };
      if (Number.isFinite(pt.cx)) {
        next.cx = pt.cx + offset.x;
      }
      if (Number.isFinite(pt.cy)) {
        next.cy = pt.cy + offset.y;
      }
      return next;
    });
  }

  function adjustEdgeEndpoints(points, edge, drag) {
    if (!points?.length) return points;
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    if (!source || !target) return points;
    const lastIndex = points.length - 1;
    const start = points[0];
    const end = points[lastIndex];
    if (isSelfLoopEdge(edge)) {
      const selfLoopBoundaries = selfLoopDockBoundaries(source, start, end, drag);
      if (selfLoopBoundaries) {
        const { startBoundary, endBoundary } = selfLoopBoundaries;
        return points.map((pt, idx) => {
          const boundary = idx === 0 ? startBoundary : idx === lastIndex ? endBoundary : null;
          if (!boundary || !Number.isFinite(boundary.x) || !Number.isFinite(boundary.y)) {
            return pt;
          }
          const dx = boundary.x - pt.x;
          const dy = boundary.y - pt.y;
          if (!Number.isFinite(dx) || !Number.isFinite(dy) || (!dx && !dy)) {
            return pt;
          }
          const next = { ...pt, x: boundary.x, y: boundary.y };
          if (Number.isFinite(pt.cx)) {
            next.cx = pt.cx + dx;
          }
          if (Number.isFinite(pt.cy)) {
            next.cy = pt.cy + dy;
          }
          return next;
        });
      }
    }
    if (edge?.graphics?.docked) {
      return points;
    }
    const startGuide = edgeGuidePoint(start, end);
    const endGuide = edgeGuidePoint(end, start);
    const startBoundary = nodeBoundaryPoint(source, startGuide, drag);
    const endBoundary = nodeBoundaryPoint(target, endGuide, drag);
    return points.map((pt, idx) => {
      const boundary = idx === 0 ? startBoundary : idx === lastIndex ? endBoundary : null;
      if (!boundary || !Number.isFinite(boundary.x) || !Number.isFinite(boundary.y)) {
        return pt;
      }
      const dx = boundary.x - pt.x;
      const dy = boundary.y - pt.y;
      if (!Number.isFinite(dx) || !Number.isFinite(dy) || (!dx && !dy)) {
        return pt;
      }
      const next = { ...pt, x: boundary.x, y: boundary.y };
      if (Number.isFinite(pt.cx)) {
        next.cx = pt.cx + dx;
      }
      if (Number.isFinite(pt.cy)) {
        next.cy = pt.cy + dy;
      }
      return next;
    });
  }

  function selfLoopDockBoundaries(node, start, end, drag) {
    if (!node || !start || !end) return null;
    const center = nodeCenter(node, drag);
    const mid = { x: (start.x + end.x) / 2, y: (start.y + end.y) / 2 };
    let axisX = mid.x - center.x;
    let axisY = mid.y - center.y;
    ({ x: axisX, y: axisY } = rotateVectorClockwise(axisX, axisY, 45));
    let axisLen = Math.hypot(axisX, axisY);
    if (!Number.isFinite(axisLen) || axisLen < 0.01) {
      axisX = 0;
      axisY = -1;
      axisLen = 1;
    }
    const ux = axisX / axisLen;
    const uy = axisY / axisLen;
    const nx = -uy;
    const ny = ux;
    const startSide = (start.x - mid.x) * nx + (start.y - mid.y) * ny;
    const sideSign = startSide >= 0 ? 1 : -1;
    const { w, h } = nodeSize(node);
    const radius = Math.max(10, Math.min(w, h) / 2);
    const axisReach = radius * 3;
    const currentSpread = Math.abs(startSide);
    const spread = Math.max(radius * 0.24, currentSpread * 2.0);
    const startToward = {
      x: center.x + ux * axisReach + nx * spread * sideSign,
      y: center.y + uy * axisReach + ny * spread * sideSign
    };
    const endToward = {
      x: center.x + ux * axisReach - nx * spread * sideSign,
      y: center.y + uy * axisReach - ny * spread * sideSign
    };
    return {
      startBoundary: nodeBoundaryPoint(node, startToward, drag),
      endBoundary: nodeBoundaryPoint(node, endToward, drag)
    };
  }

  function edgeGuidePoint(primary, fallback) {
    if (Number.isFinite(primary?.cx) && Number.isFinite(primary?.cy)) {
      if (primary.cx !== primary.x || primary.cy !== primary.y) {
        return { x: primary.cx, y: primary.cy };
      }
    }
    if (fallback && Number.isFinite(fallback.x) && Number.isFinite(fallback.y)) {
      return { x: fallback.x, y: fallback.y };
    }
    return { x: primary?.x ?? 0, y: primary?.y ?? 0 };
  }

  function buildEdgeCreatePreview(source, hover, cursor) {
    if (!edgeCreateMode || !source) return null;
    const targetNode = hover || null;
    const targetPoint = targetNode ? nodeCenter(targetNode, null) : cursor;
    if (!targetPoint) return null;
    const sourceCenter = nodeCenter(source, null);
    const start = nodeBoundaryPoint(source, targetPoint, null);
    const end = targetNode ? nodeBoundaryPoint(targetNode, sourceCenter, null) : targetPoint;
    if (!Number.isFinite(start.x) || !Number.isFinite(start.y) || !Number.isFinite(end.x) || !Number.isFinite(end.y)) {
      return null;
    }
    const path = `M ${start.x} ${start.y} L ${end.x} ${end.y}`;
    const arrow = previewArrow(start, end);
    return {
      path,
      arrowPath: arrow ? arrowPath(arrow) : "",
      invalid: false
    };
  }

  function previewArrow(start, end) {
    if (!start || !end) return null;
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    const magnitude = Math.hypot(dx, dy);
    if (!Number.isFinite(magnitude) || magnitude < 0.01) return null;
    const length = Math.max(9, baseNodeSize * 0.13, edgeStrokeWidth * 4);
    const width = length * 0.7;
    const inset = Math.max(0, edgeStrokeWidth * 0.6);
    const gap = Math.max(3, Math.round(edgeStrokeWidth * 2));
    const ux = dx / magnitude;
    const uy = dy / magnitude;
    const tipOffset = inset - gap;
    const tipX = end.x + ux * tipOffset;
    const tipY = end.y + uy * tipOffset;
    const baseX = tipX - ux * length;
    const baseY = tipY - uy * length;
    const half = width / 2;
    const perpX = -uy;
    const perpY = ux;
    const leftX = baseX + perpX * half;
    const leftY = baseY + perpY * half;
    const rightX = baseX - perpX * half;
    const rightY = baseY - perpY * half;
    return { tipX, tipY, leftX, leftY, rightX, rightY };
  }
</script>

  <div
    class="sceneflow-stage"
    bind:this={stageEl}
    on:pointerdown={startPan}
    on:pointermove={movePan}
    on:pointerup={endPan}
    on:pointercancel={endPan}
    on:dragover={handleSceneDragOver}
    on:drop={handleSceneDrop}
    on:wheel={handleWheel}
    on:click={handleStageClick}
    on:keydown={handleStageKeydown}
    on:keyup={handleStageKeyup}
    on:blur={() => (shiftDown = false)}
  tabindex="-1"
  role="presentation"
>
  <svg
    class="sceneflow-canvas"
    class:panning={isPanning}
    class:shift-pan={shiftDown && !isPanning}
    class:dragging={dragState}
    viewBox={viewBox}
    width={canvasWidth}
    height={canvasHeight}
    style={svgStyle}
    bind:this={svgEl}
    aria-hidden="true"
  >
  <defs>
    <filter
      id="sf-selected-glow"
      filterUnits="userSpaceOnUse"
      x={baseBox ? baseBox.x - 300 : -300}
      y={baseBox ? baseBox.y - 300 : -300}
      width={baseBox ? baseBox.width + 600 : 1200}
      height={baseBox ? baseBox.height + 600 : 1200}
      color-interpolation-filters="sRGB"
    >
      <feDropShadow dx="0" dy="0" stdDeviation="5" flood-color={COLORS.selected} flood-opacity="0.8" />
    </filter>
    {#each comments as comment (comment.id)}
      {@const rect = commentRect(comment, dragState)}
      <clipPath id={`comment-clip-${comment.id}`} clipPathUnits="userSpaceOnUse">
        <rect
          x={safeSvgNumber(rect.x)}
          y={safeSvgNumber(rect.y)}
          width={safeSvgNumber(rect.w)}
          height={safeSvgNumber(rect.h)}
          rx={commentCornerRadius}
          ry={commentCornerRadius}
        />
      </clipPath>
    {/each}
  </defs>
  {#if selectionBox && selectionBox.moved && Number.isFinite(selectionBox.w) && Number.isFinite(selectionBox.h)}
    <rect
      class="selection-box"
      x={safeSvgNumber(selectionBox.x)}
      y={safeSvgNumber(selectionBox.y)}
      width={safeSvgNumber(selectionBox.w)}
      height={safeSvgNumber(selectionBox.h)}
    />
  {/if}
  <g class="comments">
    {#each comments as comment (comment.id)}
      {@const tooltip = commentTooltip(comment)}
      {@const rect = commentRect(comment, dragState)}
      {@const lines = commentLines(comment)}
      {@const textX = rect.x + 12}
      {@const textY = rect.y + Math.max(16, fontSize + 4)}
      {@const isEditing = editingCommentId === comment.id}
      {@const isSelected = selectedCommentIds.has(comment.id)}
      {@const clipId = `comment-clip-${comment.id}`}
      <g
        class="comment"
        class:selected={isSelected}
        on:click|stopPropagation={(event) => selectComment(comment.id, { multi: isMultiModifier(event) })}
        on:pointerdown|stopPropagation={(event) => startCommentDrag(event, comment)}
        on:dblclick|stopPropagation={() => startCommentEdit(comment)}
        on:keydown={(event) => handleCommentKeydown(comment, event)}
        on:mouseenter={() => (hoveredCommentId = comment.id)}
        on:mouseleave={() => {
          if (hoveredCommentId === comment.id) hoveredCommentId = null;
        }}
        role="button"
        tabindex={isEditing ? -1 : 0}
        aria-disabled={isEditing ? "true" : "false"}
        aria-label={commentAriaLabel(comment)}
      >
        {#if tooltip}
          <title>{tooltip}</title>
        {/if}
        <rect
          class="comment-rect"
          x={safeSvgNumber(rect.x)}
          y={safeSvgNumber(rect.y)}
          width={safeSvgNumber(rect.w)}
          height={safeSvgNumber(rect.h)}
          rx={commentCornerRadius}
          ry={commentCornerRadius}
          filter={isSelected ? "url(#sf-selected-glow)" : null}
        />
        {#if !isEditing && lines.length}
          <text class="comment-text" x={textX} y={textY} clip-path={`url(#${clipId})`} xml:space="preserve">
            {#each lines as line, idx}
              <tspan x={textX} dy={idx === 0 ? 0 : labelLineHeight}>{line}</tspan>
            {/each}
          </text>
        {/if}
        {#if (isSelected || hoveredCommentId === comment.id) && !isEditing}
          {@const handleSize = Math.min(commentCornerRadius, rect.w / 3, rect.h / 3)}
          {@const handleX = rect.x + rect.w - handleSize}
          {@const handleY = rect.y + rect.h - handleSize}
          {@const outerRadius = Math.max(4, handleSize - 0.5)}
          {@const thickness = Math.max(2, Math.min(3, outerRadius * 0.22))}
          {@const innerRadius = outerRadius - thickness * 3}
          {@const outerStartX = handleSize}
          {@const outerStartY = handleSize - outerRadius}
          {@const outerEndX = handleSize - outerRadius}
          {@const outerEndY = handleSize}
          {@const innerStartX = handleSize - innerRadius}
          {@const innerStartY = handleSize}
          {@const innerEndX = handleSize}
          {@const innerEndY = handleSize - innerRadius}
          <g
            class="comment-resize-handle"
            transform={`translate(${handleX}, ${handleY})`}
            on:pointerdown|stopPropagation={(event) => startCommentResize(event, comment)}
          >
            <path
              class="comment-resize-fill"
              d={`M ${outerStartX} ${outerStartY} A ${outerRadius} ${outerRadius} 0 0 1 ${outerEndX} ${outerEndY} L ${innerStartX} ${innerStartY} A ${innerRadius} ${innerRadius} 0 0 0 ${innerEndX} ${innerEndY} Z`}
            />
          </g>
        {/if}
      </g>
    {/each}
  </g>

  {#if edgeCreatePreview}
    <g
      class="edge-preview"
      class:invalid={edgeCreatePreview.invalid}
      style={`--edge-color:${edgeCreatePreviewColor}`}
    >
      <path class="edge-preview-path" d={edgeCreatePreview.path} />
      {#if edgeCreatePreview.arrowPath}
        <path class="edge-preview-head" d={edgeCreatePreview.arrowPath} />
      {/if}
    </g>
  {/if}

  <g class="edges">
    {#each edges as edge (edge.id)}
      {@const label = edgeLabel(edge, runtimeValues, runtimeState)}
      {@const tooltip = edgeTooltip(edge, runtimeValues)}
      {@const baseColor = edgeColor(edge)}
      {@const isHovered = hoveredEdgeId === edge.id}
      {@const isSelected = selectedEdgeIds.has(edge.id)}
      {@const color = isSelected ? COLORS.selected : baseColor}
      {@const arrow = edgeArrow(edge, dragState)}
      {@const arrowPathData = arrow ? arrowPath(arrow) : ""}
      {@const path = edgePath(edge, dragState, arrow)}
      {@const activity = activityEdgeMap.get(edge.id)}
      {@const timeoutEntry = timeoutEdgeMap.get(edge.id)}
      {@const timeoutProgress = edge.type === "TEDGE" ? timeoutEdgeProgress(timeoutEntry, timeoutNow) : null}
      {@const isHandleDrag = !!dragState && dragState.id === edge.id && (dragState.type === "edge-control" || dragState.type === "edge-target" || dragState.type === "edge-bend")}
      {@const isTimeoutSliderDrag = timeoutInlineDragEdgeId === edge.id}
      {@const controls = isSelected || isHovered ? edgeControlPoints(edge, dragState) : null}
      {@const showHandles = (isSelected || isHovered) && !isTimeoutSliderDrag}
      {@const baseHandleRadius = Math.max(5, Math.round(baseNodeSize * 0.08))}
      {@const handleRadius = showHandles ? (isSelected ? baseHandleRadius : Math.max(4, Math.round(baseHandleRadius * 0.8))) : 0}
      {@const handleHitRadius = showHandles ? Math.max(12, handleRadius + 8) : 0}
      {@const anchorRadius = showHandles ? Math.max(3, Math.round(handleRadius * 0.55)) : 0}
      {@const bendRadius = showHandles ? Math.max(5, Math.round(handleRadius * 0.9)) : 0}
      {@const bendHitRadius = showHandles ? Math.max(14, bendRadius + 10) : 0}
      {@const bendPos = controls ? edgeBendHandlePos(edge, dragState, label) : null}
        {@const labelPos = edgeLabelPos(edge, dragState)}
      {@const timeoutSlider = (isSelected || isTimeoutSliderDrag) && !isHandleDrag && timeoutSliderSuppressedEdgeId !== edge.id && edge.type === "TEDGE" ? timeoutSliderModel(edge) : null}
      {@const timeoutSliderBg = timeoutSlider ? edgeSliderBackground(baseColor) : null}
      <g
        class="edge-group"
        class:selected={isSelected}
        on:click|stopPropagation={(event) => selectEdge(edge.id, { multi: isMultiModifier(event) })}
        on:keydown={(event) => handleEdgeKeydown(edge, event)}
        on:mouseenter={() => (hoveredEdgeId = edge.id)}
        on:mouseleave={() => {
          if (hoveredEdgeId === edge.id) hoveredEdgeId = null;
        }}
        role="button"
        tabindex="0"
        aria-label={edgeAriaLabel(edge, label)}
      >
        {#if tooltip}
          <title>{tooltip}</title>
        {/if}
        <path class="edge-hit" d={path} />
        {#if isSelected}
          <path class="edge-glow" d={path} filter="url(#sf-selected-glow)" />
        {/if}
        <path
          class={`edge edge-${(edge.type || "").toLowerCase()}`}
          style={`--edge-color:${color}`}
          d={path}
        />
        {#if timeoutProgress !== null}
          <path
            class="edge-timeout-progress"
            style={`--edge-color:${baseColor}`}
            d={path}
            pathLength="1"
            stroke-dasharray="1"
            stroke-dashoffset={1 - timeoutProgress}
          />
        {/if}
        {#if arrow}
          {#if isSelected}
            <path class="edge-head-glow" d={arrowPathData} filter="url(#sf-selected-glow)" />
          {/if}
          <path class="edge-head" style={`--edge-color:${color}`} d={arrowPathData} />
        {/if}
        {#if activity}
          {#key activity.ts}
            <path class="edge-activity" d={path} />
            {#if arrow}
              <path class="edge-activity-head" d={arrowPathData} />
            {/if}
          {/key}
        {/if}
        {#if showHandles}
          {#if controls}
            <circle
              class="edge-anchor"
              class:muted={!isSelected}
              cx={controls.start.x}
              cy={controls.start.y}
              r={anchorRadius}
              style={`--edge-color:${color}`}
            />
            <circle
              class="edge-anchor"
              class:muted={!isSelected}
              cx={controls.end.x}
              cy={controls.end.y}
              r={anchorRadius}
              style={`--edge-color:${color}`}
            />
            <circle
              class="edge-anchor-hit"
              cx={controls.end.x}
              cy={controls.end.y}
              r={handleHitRadius}
              on:pointerdown|stopPropagation={(event) => startEdgeTargetDrag(event, edge, controls.end)}
            />
            <path
              class="edge-control-line"
              class:muted={!isSelected}
              d={`M ${controls.start.x} ${controls.start.y} L ${controls.ctrl1.x} ${controls.ctrl1.y}`}
            />
            <path
              class="edge-control-line"
              class:muted={!isSelected}
              d={`M ${controls.end.x} ${controls.end.y} L ${controls.ctrl2.x} ${controls.ctrl2.y}`}
            />
            <circle
              class="edge-control-hit"
              cx={controls.ctrl1.x}
              cy={controls.ctrl1.y}
              r={handleHitRadius}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl1", controls.ctrl1)}
            />
            <circle
              class="edge-control-handle"
              class:muted={!isSelected}
              cx={controls.ctrl1.x}
              cy={controls.ctrl1.y}
              r={handleRadius}
              style={`--edge-color:${color}`}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl1", controls.ctrl1)}
            />
            <circle
              class="edge-control-hit"
              cx={controls.ctrl2.x}
              cy={controls.ctrl2.y}
              r={handleHitRadius}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl2", controls.ctrl2)}
            />
            <circle
              class="edge-control-handle"
              class:muted={!isSelected}
              cx={controls.ctrl2.x}
              cy={controls.ctrl2.y}
              r={handleRadius}
              style={`--edge-color:${color}`}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl2", controls.ctrl2)}
            />
          {/if}
        {/if}
        {#if label}
          <text
            class="edge-label"
            x={labelPos.x}
            y={labelPos.y}
            text-anchor="middle"
            dominant-baseline="middle"
            style={`--edge-color:${color}`}
          >
            {label}
          </text>
        {/if}
        {#if timeoutSlider}
          {@const sliderWidth = Math.max(110, Math.round(fontSize * 8.2))}
          {@const sliderHeight = Math.max(18, Math.round(fontSize * 1.2))}
          {@const sliderPadX = Math.max(10, Math.round(fontSize * 0.75))}
          {@const sliderPadY = Math.max(8, Math.round(fontSize * 0.65))}
          {@const sliderY = labelPos.y + Math.max(20, Math.round(labelLineHeight * 1.45)) + sliderHeight / 2}
          <foreignObject
            class="edge-timeout-inline-fo"
            x={labelPos.x - sliderWidth / 2 - sliderPadX}
            y={sliderY - sliderHeight / 2 - sliderPadY}
            width={sliderWidth + sliderPadX * 2}
            height={sliderHeight + sliderPadY * 2}
            on:pointerdown|stopPropagation
          >
            <div
              xmlns="http://www.w3.org/1999/xhtml"
              class="edge-timeout-inline"
              style={`--edge-color:${baseColor}; --edge-bg:${timeoutSliderBg};`}
            >
              <input
                class="edge-timeout-inline-slider"
                type="range"
                min={timeoutSlider.min}
                max={timeoutSlider.max}
                step={timeoutSlider.step}
                value={timeoutSlider.value}
                on:pointerdown={(event) => startTimeoutInlineDrag(edge, event)}
                on:pointerup={endTimeoutInlineDrag}
                on:pointercancel={endTimeoutInlineDrag}
                on:blur={endTimeoutInlineDrag}
                on:input={(event) => handleTimeoutInlineSliderInput(edge, event)}
              />
            </div>
          </foreignObject>
        {/if}
        {#if controls && bendPos && (isSelected || isHovered)}
          <circle
            class="edge-bend-hit"
            cx={bendPos.x}
            cy={bendPos.y}
            r={bendHitRadius}
            on:pointerdown|stopPropagation={(event) => startEdgeBendDrag(event, edge, controls)}
          />
          <circle
            class="edge-bend-handle"
            class:muted={!isSelected}
            cx={bendPos.x}
            cy={bendPos.y}
            r={bendRadius}
            style={`--edge-color:${color}`}
            on:pointerdown|stopPropagation={(event) => startEdgeBendDrag(event, edge, controls)}
          />
        {/if}
      </g>
    {/each}
  </g>

  <g class="nodes">
    {#each nodes as node (node.id)}
      {@const tooltip = nodeTooltip(node)}
      {@const pos = nodePosition(node, dragState)}
      {@const x = pos.x}
      {@const y = pos.y}
      {@const size = nodeSize(node)}
      {@const w = size.w}
      {@const h = size.h}
      {@const flavour = (node.flavour || "").toLowerCase()}
      {@const fill = nodeFill(node)}
      {@const textColor = nodeTextColor(node)}
      {@const stroke = darkenColor(fill, 0.25)}
      {@const isEnd = !outgoing.has(node.id)}
      {@const isSelected = selectedNodeIds.has(node.id)}
      {@const isActive = activityNodeSet.has(node.id)}
      {@const sign = startSignMetrics(w)}
      {@const signGap = Math.max(4, Math.round(w * 0.06))}
      {@const signX = -sign.width - sign.stroke * 2 - signGap}
      {@const signY = h / 2 - sign.halfHeight - sign.stroke * 2}
      {@const label = nodeLabelLayout(node, w, h)}
      {@const cmdLayout = showCommandText ? nodeCommandLayout(node, w, h) : nodeCommandDotsLayout(node, w, h)}
      <g
        class={`node node-${node.type === "Super" ? "super" : "basic"} ${node.isStart ? "start" : ""} ${
          node.isAltStart ? "alt-start" : ""
        } ${node.isHistory ? "history" : ""} ${isEnd ? "end" : ""} ${
          flavour ? `flavour-${flavour}` : ""
        }`}
        transform={`translate(${x}, ${y})`}
        on:click|stopPropagation={(event) => handleNodeClick(node, event)}
        on:dblclick|stopPropagation={() => handleNodeDoubleClick(node)}
        on:keydown={(event) => handleNodeKeydown(node, event)}
        on:pointerdown|stopPropagation={(event) => startNodeDrag(event, node)}
        on:mouseenter={() => {
          if (edgeCreateMode) edgeCreateHoverId = node.id;
        }}
        on:mouseleave={() => {
          if (edgeCreateHoverId === node.id) edgeCreateHoverId = null;
        }}
        role="button"
        tabindex="0"
        style={`--node-fill:${fill}; --node-text:${textColor}; --node-stroke:${stroke}`}
        class:selected={isSelected}
        class:active={isActive}
        class:edge-source={edgeCreateMode && edgeCreateSourceId === node.id}
        class:edge-target={
          (edgeCreateMode && edgeCreateHoverId === node.id && edgeCreateHoverId !== edgeCreateSourceId) ||
          edgeRetargetHoverId === node.id
        }
      >
        {#if tooltip}
          <title>{tooltip}</title>
        {/if}
        {#if node.isStart}
          <g class="node-start-sign" transform={`translate(${signX}, ${signY})`}>
            <polygon points={sign.points} style={`stroke-width:${sign.stroke}px`} />
          </g>
        {/if}
        {#if node.isAltStart}
          <g class="node-alt-start-sign" transform={`translate(${signX}, ${signY})`}>
            <polygon points={sign.points} style={`stroke-width:${sign.stroke}px`} />
          </g>
        {/if}
        {#if node.type === "Super"}
          <path
            class="node-shape node-super-shape"
            d={superNodePath(w, h)}
            filter={isSelected ? "url(#sf-selected-glow)" : null}
          />
        {:else}
          <ellipse
            class="node-shape node-ellipse"
            cx={w / 2}
            cy={h / 2}
            rx={w / 2}
            ry={h / 2}
            filter={isSelected ? "url(#sf-selected-glow)" : null}
          />
        {/if}
        {#if isActive}
          {#if node.type === "Super"}
            <path class="node-activity" d={superNodePath(w, h)} />
          {:else}
            <ellipse class="node-activity" cx={w / 2} cy={h / 2} rx={w / 2} ry={h / 2} />
          {/if}
        {/if}
        {#if label}
          <text class="node-title">
            {#each label.lines as line, idx}
              <tspan x={w / 2} y={label.startY + idx * label.lineHeight}>{line}</tspan>
            {/each}
            {#if label.idLine}
              <tspan class="node-id" x={w / 2} y={label.startY + label.lines.length * label.lineHeight}>
                {label.idLine}
              </tspan>
            {/if}
          </text>
        {/if}
          {#if cmdLayout}
          {#if showCommandText}
            <g
              class="node-commands"
              role="button"
              tabindex="0"
              aria-label={`Edit commands for ${node?.name || node?.id || "node"}`}
              on:dblclick|stopPropagation={() => handleCommandOpen(node)}
            >
              <rect
                class="node-command-box"
                x={safeSvgNumber(cmdLayout.x)}
                y={safeSvgNumber(cmdLayout.y)}
                width={safeSvgNumber(cmdLayout.width)}
                height={safeSvgNumber(cmdLayout.height)}
                rx={commandCornerRadius}
                ry={commandCornerRadius}
              />
              <text
                class="node-command-text"
                text-anchor="start"
                dominant-baseline="alphabetic"
                style={`font-size:${cmdLayout.fontSize}px`}
              >
                {#each cmdLayout.lines as line, idx}
                  <tspan x={cmdLayout.textX} y={cmdLayout.y + cmdLayout.textStartY + idx * cmdLayout.lineHeight}>
                    {line}
                  </tspan>
                {/each}
              </text>
            </g>
          {:else}
            <g
              class="node-command-dots"
              role="button"
              tabindex="0"
              aria-label={`Edit commands for ${node?.name || node?.id || "node"}`}
              on:dblclick|stopPropagation={() => handleCommandOpen(node)}
            >
              {#each cmdLayout.dots as dot}
                <circle class="node-command-dot" cx={dot.cx} cy={dot.cy} r={dot.r} />
              {/each}
            </g>
          {/if}
        {/if}
      </g>
    {/each}
  </g>
  </svg>
  {#if editingCommentId && editingCommentRect && editingCommentScreenRect}
    <div
      class="comment-editor-overlay"
      style={commentEditorStyle}
      on:pointerdown|stopPropagation
    >
      <textarea
        class="comment-editor"
        bind:this={commentEditorEl}
        bind:value={editingCommentDraft}
        on:keydown|stopPropagation={handleCommentEditorKeydown}
        on:keyup|stopPropagation
        on:keypress|stopPropagation
        on:wheel|stopPropagation
        on:pointerdown|stopPropagation
        on:click|stopPropagation
        on:blur={commitCommentEdit}
      />
    </div>
  {/if}
</div>
