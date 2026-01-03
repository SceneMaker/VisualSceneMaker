<script>
  import { onDestroy, onMount, tick } from "svelte";
  export let snapshot = null;
  export let onNavigate = null;
  export let onNodeMove = null;
  export let onCommentUpdate = null;
  export let onEdgeControlUpdate = null;
  export let onDeleteSelection = null;
  export let onUndo = null;
  export let onRedo = null;
  export let edgeCreateMode = false;
  export let onEdgePick = null;
  export let onSceneDrop = null;
  export let sceneDragType = "application/x-vsm-scene";
  export let onBlockDrop = null;
  export let blockDragType = "application/x-vsm-block";
  export let showCommandText = true;
  export let onCommandOpen = null;
  export let worldBox = null;
  export let viewBoxState = null;
  export let config = null;
  export let selection = null;
  export let snapToGrid = true;

  const DEFAULT_NODE_SIZE = 90;
  const DEFAULT_FONT_SIZE = 16;
  const MIN_WORLD_COORD = 1;
  const COLORS = {
    node: "#7d7d7d",
    history: "#ffffff",
    text: "#ffffff",
    textHistory: "#000000",
    startSign: "#b52d0d",
    altStartSign: "#c0c0c0",
    selected: "#b52d0d",
    commentFill: "rgba(200, 200, 200, 0.78)",
    commentText: "rgba(75, 75, 75, 0.5)",
    edges: {
      eedge: "#827d78",
      fedge: "#234d67",
      tedge: "#543f1d",
      cedge: "#988e34",
      pedge: "#2a6723",
      iedge: "#983434"
    }
  };

  const padding = 80;
  const minCanvasWidth = 680;
  const minCanvasHeight = 420;
  const minZoom = 0.3;
  const maxZoom = 3.5;
  const zoomStep = 1.12;
  const COMMAND_FONT_FAMILY = '"SansSerif", "Helvetica Neue", Arial, sans-serif';

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
  $: baseNodeSize = nodeWidth || guessNodeSize(nodes) || DEFAULT_NODE_SIZE;
  $: nodeStrokeWidth = Math.max(1, baseNodeSize / 25);
  $: edgeStrokeWidth = Math.max(1, baseNodeSize / 30) * 2;
  $: fontSize = workspaceFontSize || Math.max(10, Math.round(baseNodeSize * 0.18));
  $: labelLineHeight = Math.max(10, Math.round(fontSize * 1.15));
  $: commandFontSize = Math.max(9, Math.round(fontSize * 0.85));
  $: commandLineHeight = Math.max(10, Math.round(commandFontSize * 1.25));
  $: commandPaddingX = Math.max(6, Math.round(commandFontSize * 0.5));
  $: commandPaddingY = Math.max(4, Math.round(commandFontSize * 0.35));
  $: commandGap = Math.max(4, Math.round(commandFontSize * 0.5));
  $: commandCornerRadius = Math.max(4, Math.round(commandFontSize * 0.6));
  $: commentMinSize = Math.max(50, Math.round(baseNodeSize * 0.5));
  $: showNodeIds = readBoolean(config?.shownodeid ?? config?.["shownodeid"], true);
  $: gridNodeWidth = nodeWidth || baseNodeSize;
  $: gridNodeHeight = nodeHeight || baseNodeSize;
  $: gridX = Math.max(8, baseNodeSize * gridScaleX);
  $: gridY = Math.max(8, baseNodeSize * gridScaleY);
  $: gridOriginX = gridNodeWidth / 2 + gridNodeWidth / 3;
  $: gridOriginY = gridNodeHeight / 2 + gridNodeHeight / 3;
  $: viewWidth = baseBox ? baseBox.width / zoomLevel : 1;
  $: viewHeight = baseBox ? baseBox.height / zoomLevel : 1;
  $: scaleX = viewWidth ? canvasWidth / viewWidth : zoomLevel;
  $: scaleY = viewHeight ? canvasHeight / viewHeight : zoomLevel;
  $: uniformScale = Math.min(scaleX, scaleY);
  $: viewOffsetX = (canvasWidth - viewWidth * uniformScale) / 2;
  $: viewOffsetY = (canvasHeight - viewHeight * uniformScale) / 2;
  $: gridScreenX = gridX * uniformScale;
  $: gridScreenY = gridY * uniformScale;
  $: viewOriginX = ((baseBox && Number.isFinite(baseBox.x)) ? baseBox.x : 0) + panX;
  $: viewOriginY = ((baseBox && Number.isFinite(baseBox.y)) ? baseBox.y : 0) + panY;
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
  $: viewBox = viewBoxString(baseBox, zoomLevel, panX, panY);
  $: canvasWidth = Math.max(minCanvasWidth, bounds.width, viewportSize.width || 0);
  $: canvasHeight = Math.max(minCanvasHeight, bounds.height, viewportSize.height || 0);
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
  let editingCommentId = null;
  let editingCommentDraft = "";
  let editingCommentOriginal = "";
  let commentEditorEl = null;
  let hoveredCommentId = null;
  let dragState = null;
  let panStart = { x: 0, y: 0 };
  let panOrigin = { x: 0, y: 0 };
  let lastSnapshotKey = "";
  const commentCornerRadius = 12;
  let commentMinSize = 50;
  const dragThreshold = 3;
  let textMeasureCtx = null;
  let lastTextMeasureSize = null;

  $: worldBox = baseBox;
  $: viewBoxState = currentViewBox();

  $: if (snapshot) {
    const key = `${snapshot.projectId || ""}:${snapshot.superNodeId || ""}`;
    if (key !== lastSnapshotKey) {
      lastSnapshotKey = key;
      fitToView();
      clearSelection();
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

  $: editingComment = editingCommentId ? findCommentById(editingCommentId) : null;
  $: editingCommentRect = editingComment ? commentRect(editingComment, dragState) : null;
  $: editingCommentScreenRect = editingCommentRect
    ? worldRectToScreenRect(editingCommentRect)
    : null;
  $: commentEditorStyle = editingCommentScreenRect
    ? `left:${editingCommentScreenRect.x}px; top:${editingCommentScreenRect.y}px; width:${editingCommentScreenRect.w}px; height:${editingCommentScreenRect.h}px;`
    : "";

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
  });

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
      const x = node.graphics?.x ?? 0;
      const y = node.graphics?.y ?? 0;
      const w = node.size?.w ?? 160;
      const h = node.size?.h ?? 60;
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
      const x = comment.rect?.x ?? 0;
      const y = comment.rect?.y ?? 0;
      const w = comment.rect?.w ?? 0;
      const h = comment.rect?.h ?? 0;
      expand(x, y);
      expand(x + w, y + h);
    });

    edgesList.forEach((edge) => {
      const pts = edge.graphics?.points || [];
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

  function guessNodeSize(nodesList) {
    for (const node of nodesList) {
      const size = node.size?.w ?? node.size?.h;
      if (Number.isFinite(size)) {
        return size;
      }
    }
    return null;
  }

  function viewBoxString(box, zoomLevel, offsetX, offsetY) {
    if (!box) return "0 0 200 200";
    const width = box.width / zoomLevel;
    const height = box.height / zoomLevel;
    const x = box.x + offsetX;
    const y = box.y + offsetY;
    return `${x} ${y} ${width} ${height}`;
  }

  function currentViewBox() {
    if (!baseBox) {
      return { x: 0, y: 0, width: 200, height: 200 };
    }
    return {
      x: baseBox.x + panX,
      y: baseBox.y + panY,
      width: baseBox.width / zoomLevel,
      height: baseBox.height / zoomLevel
    };
  }

  function clamp(value, min, max) {
    return Math.min(max, Math.max(min, value));
  }

  function setZoom(nextZoom, anchor = null) {
    const view = currentViewBox();
    const clamped = clamp(nextZoom, minZoom, maxZoom);
    if (!anchor) {
      const centerX = view.x + view.width / 2;
      const centerY = view.y + view.height / 2;
      zoomLevel = clamped;
      const width = baseBox.width / zoomLevel;
      const height = baseBox.height / zoomLevel;
      panX = centerX - width / 2 - baseBox.x;
      panY = centerY - height / 2 - baseBox.y;
      clampPanToNonNegative();
      return;
    }
    const relX = clamp(anchor.relX ?? 0.5, 0, 1);
    const relY = clamp(anchor.relY ?? 0.5, 0, 1);
    zoomLevel = clamped;
    const width = baseBox.width / zoomLevel;
    const height = baseBox.height / zoomLevel;
    const newX = anchor.x - relX * width;
    const newY = anchor.y - relY * height;
    panX = newX - baseBox.x;
    panY = newY - baseBox.y;
    clampPanToNonNegative();
  }

  export function zoomIn() {
    setZoom(zoomLevel * zoomStep);
  }

  export function zoomOut() {
    setZoom(zoomLevel / zoomStep);
  }

  export function fitToView() {
    zoomLevel = 1;
    panX = 0;
    panY = 0;
    clampPanToNonNegative();
  }

  export function centerOn(x, y) {
    if (!baseBox || !Number.isFinite(x) || !Number.isFinite(y)) return;
    const width = baseBox.width / zoomLevel;
    const height = baseBox.height / zoomLevel;
    panX = x - width / 2 - baseBox.x;
    panY = y - height / 2 - baseBox.y;
    clampPanToNonNegative();
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
    const mod = event.metaKey || event.ctrlKey;
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

  function startPan(event) {
    if (event.button !== 0 || !svgEl) return;
    if (!event.shiftKey) return;
    focusStage();
    if (editingCommentId && !event.target?.closest?.(".comment-editor")) {
      commitCommentEdit();
    }
    if (event.target?.closest?.(".node, .edge-group, .comment")) return;
    if (dragState) return;
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
    if (dragState) {
      updateDrag(event);
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
    if (dragState) {
      endDrag(event);
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

  function clearSelection() {
    if (editingCommentId) {
      commitCommentEdit();
    }
    selectedNodeId = null;
    selectedEdgeId = null;
    selectedCommentId = null;
    selection = null;
  }

  function selectNode(nodeId) {
    if (editingCommentId) {
      commitCommentEdit();
    }
    focusStage();
    selectedNodeId = nodeId;
    selectedEdgeId = null;
    selectedCommentId = null;
    selection = { type: "node", id: nodeId };
  }

  function selectEdge(edgeId) {
    if (editingCommentId) {
      commitCommentEdit();
    }
    focusStage();
    selectedEdgeId = edgeId;
    selectedNodeId = null;
    selectedCommentId = null;
    selection = { type: "edge", id: edgeId };
  }

  function selectComment(commentId) {
    if (editingCommentId && editingCommentId !== commentId) {
      commitCommentEdit();
    }
    focusStage();
    selectedCommentId = commentId;
    selectedNodeId = null;
    selectedEdgeId = null;
    selection = { type: "comment", id: commentId };
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

  function nodeLines(node) {
    if (!node?.name) return [];
    return node.name.split(";").filter((line) => line.trim().length > 0);
  }

  function nodeCommandLines(node) {
    const list = Array.isArray(node?.commands) ? node.commands : [];
    return list
      .map((cmd) => (cmd?.text ?? cmd?.syntax ?? "").trim())
      .filter((line) => line.length > 0);
  }

  function nodeCommandDotsLayout(node, w, h) {
    const count = nodeCommandLines(node).length;
    if (!count) return null;
    const size = commandFontSize || fontSize || 12;
    const radius = Math.max(3, Math.round(size * 0.33)) * 2;
    const gap = Math.max(4, Math.round(radius * 0.9));
    const totalWidth = count * radius * 2 + (count - 1) * gap;
    const startX = (w - totalWidth) / 2;
    const rx = Math.max(1, w / 2);
    const ry = Math.max(1, h / 2);
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
    const lines = nodeCommandLines(node);
    if (!lines.length) return null;
    const size = commandFontSize || fontSize || 12;
    const padY = commandPaddingY || 4;
    const padX = commandPaddingX || 6;
    const gap = commandGap || 4;
    const metrics = lines.map((line) => measureTextMetrics(line, size));
    const maxTextWidth = metrics.reduce((max, metric) => Math.max(max, metric.width), 0);
    const maxAscent = metrics.reduce((max, metric) => Math.max(max, metric.ascent), 0);
    const maxDescent = metrics.reduce((max, metric) => Math.max(max, metric.descent), 0);
    const lineHeight = Math.max(1, maxAscent + maxDescent);
    const width = Math.max(1, maxTextWidth + padX * 2);
    return {
      lines,
      x: (w - width) / 2,
      y: h + gap,
      width,
      height: lines.length * lineHeight + padY * 2,
      textX: (w - width) / 2 + padX,
      textStartY: padY + maxAscent,
      lineHeight,
      fontSize: size
    };
  }

  function nodeLabelLayout(node, w, h) {
    const lines = nodeLines(node);
    if (!lines.length) return null;
    const idLine = showNodeIds && node?.id ? `[${node.id}]` : "";
    const total = lines.length + (idLine ? 1 : 0);
    const startY = h / 2 - ((total - 1) * labelLineHeight) / 2;
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
      const ctrl1 = safeCtrl(start);
      let ctrl2 = safeCtrl(end);
      const trimmed = trimEdgeEnd(start, end, ctrl2, arrow?.trim);
      if (trimmed) {
        end = trimmed.end;
        ctrl2 = trimmed.ctrl2;
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
    const length = Math.max(9, baseNodeSize * 0.13, edgeStrokeWidth * 4);
    const width = length * 0.7;
    const inset = Math.max(0, edgeStrokeWidth * 0.6);
    const gap = Math.max(2, Math.round(edgeStrokeWidth * 1.4));
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
    const trim = Math.max(0, length - inset + gap);
    return { tipX, tipY, leftX, leftY, rightX, rightY, trim };
  }

  function edgeEndVector(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length >= 2) {
      const start = pts[0];
      const end = pts[pts.length - 1];
      const ctrl2 = safeCtrl(end);
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
    const w = node.size?.w ?? baseNodeSize;
    const h = node.size?.h ?? nodeHeight ?? baseNodeSize;
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
      const tx = dx !== 0 ? halfW / Math.abs(dx) : Infinity;
      const ty = dy !== 0 ? halfH / Math.abs(dy) : Infinity;
      const t = Math.min(tx, ty);
      return { x: cx + dx * t, y: cy + dy * t };
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
    const w = node.size?.w ?? 160;
    const h = node.size?.h ?? 60;
    return { x: x + w / 2, y: y + h / 2 };
  }

  function edgeLabel(edge) {
    if (edge.condition) return edge.condition;
    if (edge.probability !== undefined && edge.probability !== null) {
      return `p=${edge.probability}`;
    }
    if (edge.timeoutExpr) return `t=${edge.timeoutExpr}`;
    if (edge.timeoutMs !== undefined && edge.timeoutMs !== null) {
      return `t=${edge.timeoutMs}ms`;
    }
    return "";
  }

  function edgeLabelPos(edge, drag) {
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
    return { x: 0, y: 0 };
  }

  function edgeControlPoints(edge, drag) {
    const pts = edgePoints(edge, drag);
    if (pts.length < 2) return null;
    const start = pts[0];
    const end = pts[pts.length - 1];
    return {
      start,
      end,
      ctrl1: safeCtrl(start),
      ctrl2: safeCtrl(end)
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

  function edgeTooltip(edge) {
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
      lines.push(`Timeout expr: ${edge.timeoutExpr}`);
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

  function edgeAriaLabel(edge) {
    if (!edge) return "Edge";
    const label = edgeLabel(edge);
    if (label) {
      return `Edge ${edge.type || ""}: ${label}`;
    }
    return edge.type ? `Edge ${edge.type}` : "Edge";
  }

  function handleNodeClick(node) {
    if (!node) return;
    if (edgeCreateMode && typeof onEdgePick === "function") {
      onEdgePick(node.id);
      return;
    }
    selectNode(node.id);
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
      const w = node.size?.w ?? baseNodeSize;
      const h = node.size?.h ?? nodeHeight ?? baseNodeSize;
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
    return types.includes(sceneDragType) || types.includes("text/plain");
  }

  function isBlockDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(blockDragType);
  }

  function handleSceneDragOver(event) {
    if (!isSceneDrag(event) && !isBlockDrag(event)) return;
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
    if (activeDrag?.type === "node" && activeDrag.id === node.id) {
      return {
        x: activeDrag.x ?? node.graphics?.x ?? 0,
        y: activeDrag.y ?? node.graphics?.y ?? 0
      };
    }
    return { x: node.graphics?.x ?? 0, y: node.graphics?.y ?? 0 };
  }

  function commentPosition(comment, drag) {
    const rect = commentRect(comment, drag);
    return { x: rect.x, y: rect.y };
  }

  function commentRect(comment, drag) {
    const base = {
      x: comment.rect?.x ?? 0,
      y: comment.rect?.y ?? 0,
      w: Math.max(commentMinSize, comment.rect?.w ?? 0),
      h: Math.max(commentMinSize, comment.rect?.h ?? 0)
    };
    const activeDrag = drag || dragState;
    if (!activeDrag || activeDrag.id !== comment.id) {
      return base;
    }
    if (activeDrag.type === "comment") {
      return {
        x: activeDrag.x ?? base.x,
        y: activeDrag.y ?? base.y,
        w: activeDrag.width ?? base.w,
        h: activeDrag.height ?? base.h
      };
    }
    if (activeDrag.type === "comment-resize") {
      return {
        x: base.x,
        y: base.y,
        w: activeDrag.width ?? base.w,
        h: activeDrag.height ?? base.h
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

  function startNodeDrag(event, node) {
    if (!node || event.button !== 0) return;
    if (edgeCreateMode) return;
    event.preventDefault();
    focusStage();
    selectNode(node.id);
    const pos = nodePosition(node, null);
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
    event.preventDefault();
    focusStage();
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

  function updateDrag(event) {
    if (!dragState || dragState.pointerId !== event.pointerId) return;
    const world = eventToWorld(event);
    const dx = world.x - dragState.startX;
    const dy = world.y - dragState.startY;
    if (dragState.type === "edge-control") {
      dragState = {
        ...dragState,
        cx: world.x,
        cy: world.y,
        moved: dragState.moved || Math.hypot(dx, dy) > dragThreshold
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
    if (dragState.type === "node" || dragState.type === "comment") {
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

  function endDrag(event) {
    if (!dragState || dragState.pointerId !== event.pointerId) return;
    const finished = dragState;
    dragState = null;
    const captureEl = stageEl || svgEl;
    if (captureEl && captureEl.hasPointerCapture(event.pointerId)) {
      captureEl.releasePointerCapture(event.pointerId);
    }
    if (!finished.moved) {
      return;
    }
    const finalX = finished.x ?? finished.originX;
    const finalY = finished.y ?? finished.originY;
    if (finished.type === "node" && typeof onNodeMove === "function") {
      onNodeMove(finished.id, finalX, finalY, snapToGrid);
    }
    if ((finished.type === "comment" || finished.type === "comment-resize") && typeof onCommentUpdate === "function") {
      onCommentUpdate(finished.id, finalX, finalY, finished.width, finished.height);
    }
    if (finished.type === "edge-control" && typeof onEdgeControlUpdate === "function") {
      onEdgeControlUpdate(finished.id, finished.handle, finished.cx, finished.cy);
    }
  }

  function edgePoints(edge, drag) {
    const pts = edge.graphics?.points || [];
    if (!pts.length) return pts;
    const activeDrag = drag || dragState;
    let nextPoints = pts;
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
    if (activeDrag?.type === "edge-control" && activeDrag.id === edge.id) {
      const idx = activeDrag.handle === "ctrl1" ? 0 : nextPoints.length - 1;
      if (nextPoints[idx]) {
        const next = { ...nextPoints[idx], cx: activeDrag.cx, cy: activeDrag.cy };
        nextPoints = nextPoints.map((pt, index) => (index === idx ? next : pt));
      }
    }
    return nextPoints;
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
    on:click={clearSelection}
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
    {#each comments as comment (comment.id)}
      {@const rect = commentRect(comment, dragState)}
      <clipPath id={`comment-clip-${comment.id}`} clipPathUnits="userSpaceOnUse">
        <rect x={rect.x} y={rect.y} width={rect.w} height={rect.h} rx={commentCornerRadius} ry={commentCornerRadius} />
      </clipPath>
    {/each}
  </defs>
  <g class="comments">
    {#each comments as comment (comment.id)}
      {@const tooltip = commentTooltip(comment)}
      {@const rect = commentRect(comment, dragState)}
      {@const lines = commentLines(comment)}
      {@const textX = rect.x + 12}
      {@const textY = rect.y + Math.max(16, fontSize + 4)}
      {@const isEditing = editingCommentId === comment.id}
      {@const clipId = `comment-clip-${comment.id}`}
      <g
        class="comment"
        class:selected={selectedCommentId === comment.id}
        on:click|stopPropagation={() => selectComment(comment.id)}
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
          x={rect.x}
          y={rect.y}
          width={rect.w}
          height={rect.h}
          rx={commentCornerRadius}
          ry={commentCornerRadius}
        />
        {#if !isEditing && lines.length}
          <text class="comment-text" x={textX} y={textY} clip-path={`url(#${clipId})`} xml:space="preserve">
            {#each lines as line, idx}
              <tspan x={textX} dy={idx === 0 ? 0 : labelLineHeight}>{line}</tspan>
            {/each}
          </text>
        {/if}
        {#if (selectedCommentId === comment.id || hoveredCommentId === comment.id) && !isEditing}
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

  <g class="edges">
    {#each edges as edge (edge.id)}
      {@const label = edgeLabel(edge)}
      {@const tooltip = edgeTooltip(edge)}
      {@const baseColor = edgeColor(edge)}
      {@const isSelected = selectedEdgeId === edge.id}
      {@const color = isSelected ? COLORS.selected : baseColor}
      {@const arrow = edgeArrow(edge, dragState)}
      <g
        class="edge-group"
        class:selected={isSelected}
        on:click|stopPropagation={() => selectEdge(edge.id)}
        on:keydown={(event) => handleEdgeKeydown(edge, event)}
        role="button"
        tabindex="0"
        aria-label={edgeAriaLabel(edge)}
      >
        {#if tooltip}
          <title>{tooltip}</title>
        {/if}
        <path class="edge-hit" d={edgePath(edge, dragState, arrow)} />
        <path
          class={`edge edge-${(edge.type || "").toLowerCase()}`}
          style={`--edge-color:${color}`}
          d={edgePath(edge, dragState, arrow)}
        />
        {#if arrow}
          <path class="edge-head" style={`--edge-color:${color}`} d={arrowPath(arrow)} />
        {/if}
        {#if isSelected}
          {@const controls = edgeControlPoints(edge, dragState)}
          {@const handleRadius = Math.max(5, Math.round(baseNodeSize * 0.08))}
          {#if controls}
            <path
              class="edge-control-line"
              d={`M ${controls.start.x} ${controls.start.y} L ${controls.ctrl1.x} ${controls.ctrl1.y}`}
            />
            <path
              class="edge-control-line"
              d={`M ${controls.end.x} ${controls.end.y} L ${controls.ctrl2.x} ${controls.ctrl2.y}`}
            />
            <circle
              class="edge-control-handle"
              cx={controls.ctrl1.x}
              cy={controls.ctrl1.y}
              r={handleRadius}
              style={`--edge-color:${color}`}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl1", controls.ctrl1)}
            />
            <circle
              class="edge-control-handle"
              cx={controls.ctrl2.x}
              cy={controls.ctrl2.y}
              r={handleRadius}
              style={`--edge-color:${color}`}
              on:pointerdown|stopPropagation={(event) => startEdgeControlDrag(event, edge, "ctrl2", controls.ctrl2)}
            />
          {/if}
        {/if}
        {#if label}
          {@const pos = edgeLabelPos(edge, dragState)}
          <text
            class="edge-label"
            x={pos.x}
            y={pos.y}
            text-anchor="middle"
            dominant-baseline="middle"
            style={`--edge-color:${color}`}
          >
            {label}
          </text>
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
      {@const w = node.size?.w ?? baseNodeSize}
      {@const h = node.size?.h ?? nodeHeight ?? baseNodeSize}
      {@const flavour = (node.flavour || "").toLowerCase()}
      {@const fill = nodeFill(node)}
      {@const textColor = nodeTextColor(node)}
      {@const stroke = darkenColor(fill, 0.25)}
      {@const isEnd = !outgoing.has(node.id)}
      {@const sign = startSignMetrics(w)}
      {@const signX = -sign.width - sign.stroke * 2}
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
        on:click|stopPropagation={() => handleNodeClick(node)}
        on:dblclick|stopPropagation={() => handleNodeDoubleClick(node)}
        on:keydown={(event) => handleNodeKeydown(node, event)}
        on:pointerdown|stopPropagation={(event) => startNodeDrag(event, node)}
        role="button"
        tabindex="0"
        style={`--node-fill:${fill}; --node-text:${textColor}; --node-stroke:${stroke}`}
        class:selected={selectedNodeId === node.id}
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
          <rect class="node-shape node-rect" width={w} height={h} />
        {:else}
          <ellipse class="node-shape node-ellipse" cx={w / 2} cy={h / 2} rx={w / 2} ry={h / 2} />
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
                x={cmdLayout.x}
                y={cmdLayout.y}
                width={cmdLayout.width}
                height={cmdLayout.height}
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
