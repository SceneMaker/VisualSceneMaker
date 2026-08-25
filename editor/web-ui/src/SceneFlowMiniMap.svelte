<script>
  export let snapshot = null;
  export let worldBox = null;
  export let viewBox = null;
  export let onCenter = null;
  /** Array of peer presence objects { color, viewport: {x,y,width,height} } */
  export let peers = [];

  const COLORS = {
    node: "#7a7d81",
    history: "#dcdcdc",
    edges: {
      eedge: "#7a7d81",
      fedge: "#5b8edc",
      tedge: "#a06a4b",
      cedge: "#ffc857",
      pedge: "#5bae7a",
      iedge: "#e26d5a"
    },
    comment: "rgba(232, 232, 232, 0.9)"
  };

  $: nodes = snapshot?.nodes || [];
  $: edges = snapshot?.edges || [];
  $: comments = snapshot?.comments || [];
  $: nodeMap = new Map(nodes.map((node) => [node.id, node]));
  $: miniViewBox = worldBox
    ? `${toFinite(worldBox.x)} ${toFinite(worldBox.y)} ${Math.max(0, toFinite(worldBox.width))} ${Math.max(
        0,
        toFinite(worldBox.height)
      )}`
    : "0 0 200 200";
  $: viewport = computeViewport(worldBox, viewBox);

  const MINIMAP_WIDTH = 192;
  const MINIMAP_MIN_HEIGHT = 70;
  const MINIMAP_MAX_HEIGHT = 192;

  $: minimapHeight = (() => {
    const w = toFinite(worldBox?.width);
    const h = toFinite(worldBox?.height);
    if (w <= 0 || h <= 0) return 160;
    const derived = Math.round(MINIMAP_WIDTH * (h / w));
    return Math.min(MINIMAP_MAX_HEIGHT, Math.max(MINIMAP_MIN_HEIGHT, derived));
  })();
  $: minimapStyle = `height: ${minimapHeight}px;`;

  let dragging = false;

  function toFinite(value, fallback = 0) {
    const num = Number(value);
    return Number.isFinite(num) ? num : fallback;
  }

  function safeSvgNumber(value, fallback = 0) {
    return toFinite(value, fallback);
  }

  function worldCoordsFromEvent(event, element) {
    const rect = element.getBoundingClientRect();
    if (!rect.width || !rect.height || !worldBox) return null;
    const relX = (event.clientX - rect.left) / rect.width;
    const relY = (event.clientY - rect.top) / rect.height;
    return {
      x: worldBox.x + relX * worldBox.width,
      y: worldBox.y + relY * worldBox.height
    };
  }

  function handlePointerDown(event) {
    if (!worldBox || typeof onCenter !== "function") return;
    event.currentTarget.setPointerCapture(event.pointerId);
    dragging = true;
    const coords = worldCoordsFromEvent(event, event.currentTarget);
    if (coords) onCenter(coords.x, coords.y);
  }

  function handlePointerMove(event) {
    if (!dragging || !worldBox || typeof onCenter !== "function") return;
    const coords = worldCoordsFromEvent(event, event.currentTarget);
    if (coords) onCenter(coords.x, coords.y);
  }

  function handlePointerUp(event) {
    dragging = false;
    try { event.currentTarget.releasePointerCapture(event.pointerId); } catch (_) {}
  }

  function handleKeydown(event) {
    if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") return;
    event.preventDefault();
    if (!worldBox || typeof onCenter !== "function") return;
    onCenter(worldBox.x + worldBox.width / 2, worldBox.y + worldBox.height / 2);
  }

  function superNodeScale(node) {
    const count = Number.isFinite(node?.childCount) ? node.childCount : 0;
    const steps = Math.max(0, Math.floor(count / 5));
    return 1 + steps * 0.05;
  }

  function nodeSize(node) {
    const width = toFinite(node?.size?.w, 160);
    const height = toFinite(node?.size?.h, 60);
    if (node?.type !== "Super") {
      return { w: width, h: height };
    }
    const scale = superNodeScale(node);
    return { w: width * scale, h: height * scale };
  }

  function nodeVisualOffset(node) {
    if (node?.type !== "Super") {
      return { x: 0, y: 0 };
    }
    const baseWidth = toFinite(node?.size?.w, 160);
    const baseHeight = toFinite(node?.size?.h, 60);
    const scaled = nodeSize(node);
    return {
      x: (baseWidth - scaled.w) / 2,
      y: (baseHeight - scaled.h) / 2
    };
  }

  function nodeRenderPosition(node) {
    const x = toFinite(node?.graphics?.x, 0);
    const y = toFinite(node?.graphics?.y, 0);
    const offset = nodeVisualOffset(node);
    return { x: x + offset.x, y: y + offset.y };
  }

  function nodeCenter(node) {
    const pos = nodeRenderPosition(node);
    const { w, h } = nodeSize(node);
    return { x: pos.x + w / 2, y: pos.y + h / 2 };
  }

  function edgeLine(edge) {
    const source = nodeMap.get(edge.sourceId);
    const target = nodeMap.get(edge.targetId);
    if (!source || !target) return null;
    const s = nodeCenter(source);
    const t = nodeCenter(target);
    return { x1: s.x, y1: s.y, x2: t.x, y2: t.y };
  }

  function edgeColor(edge) {
    const key = (edge?.type || "").toLowerCase();
    return COLORS.edges[key] || COLORS.edges.eedge;
  }

  function nodeFill(node) {
    if (node?.isHistory) return COLORS.history;
    const flavour = (node?.flavour || "").toLowerCase();
    if (flavour === "enode") return COLORS.edges.eedge;
    if (flavour === "fnode") return COLORS.edges.fedge;
    if (flavour === "tnode") return COLORS.edges.tedge;
    if (flavour === "cnode") return COLORS.edges.cedge;
    if (flavour === "pnode") return COLORS.edges.pedge;
    if (flavour === "inode") return COLORS.edges.iedge;
    return COLORS.node;
  }

  function computeViewport(world, view) {
    if (!world || !view) return null;
    const worldX = toFinite(world.x);
    const worldY = toFinite(world.y);
    const worldW = toFinite(world.width);
    const worldH = toFinite(world.height);
    const viewX = toFinite(view.x);
    const viewY = toFinite(view.y);
    const viewW = toFinite(view.width);
    const viewH = toFinite(view.height);
    const x1 = Math.max(worldX, viewX);
    const y1 = Math.max(worldY, viewY);
    const x2 = Math.min(worldX + worldW, viewX + viewW);
    const y2 = Math.min(worldY + worldH, viewY + viewH);
    const width = Math.max(0, x2 - x1);
    const height = Math.max(0, y2 - y1);
    return { x: x1, y: y1, width, height };
  }
</script>

<div
  class="sceneflow-minimap"
  class:dragging
  role="button"
  tabindex="0"
  aria-label="SceneFlow minimap"
  style={minimapStyle}
  on:pointerdown={handlePointerDown}
  on:pointermove={handlePointerMove}
  on:pointerup={handlePointerUp}
  on:pointercancel={handlePointerUp}
  on:keydown={handleKeydown}
>
  <svg class="sceneflow-minimap-canvas" viewBox={miniViewBox} aria-hidden="true">
    <g class="mini-comments">
      {#each comments as comment}
        <rect
          class="mini-comment"
          fill={COLORS.comment}
          x={safeSvgNumber(comment?.rect?.x ?? 0)}
          y={safeSvgNumber(comment?.rect?.y ?? 0)}
          width={safeSvgNumber(comment?.rect?.w ?? 0)}
          height={safeSvgNumber(comment?.rect?.h ?? 0)}
          rx="8"
        />
      {/each}
    </g>
    <g class="mini-edges">
      {#each edges as edge (edge.id)}
        {@const line = edgeLine(edge)}
        {#if line}
          <line
            class="mini-edge"
            x1={line.x1}
            y1={line.y1}
            x2={line.x2}
            y2={line.y2}
            stroke={edgeColor(edge)}
          />
        {/if}
      {/each}
    </g>
    <g class="mini-nodes">
      {#each nodes as node (node.id)}
        {@const pos = nodeRenderPosition(node)}
        {@const x = pos.x}
        {@const y = pos.y}
        {@const size = nodeSize(node)}
        {@const w = size.w}
        {@const h = size.h}
        {#if node.type === "Super"}
          <rect
            class="mini-node"
            x={safeSvgNumber(x)}
            y={safeSvgNumber(y)}
            width={safeSvgNumber(w)}
            height={safeSvgNumber(h)}
            fill={nodeFill(node)}
            rx="6"
          />
        {:else}
          <ellipse
            class="mini-node"
            cx={safeSvgNumber(x + w / 2)}
            cy={safeSvgNumber(y + h / 2)}
            rx={safeSvgNumber(w / 2)}
            ry={safeSvgNumber(h / 2)}
            fill={nodeFill(node)}
          />
        {/if}
      {/each}
    </g>
    {#if viewport}
      <rect
        class="mini-viewport"
        x={safeSvgNumber(viewport.x)}
        y={safeSvgNumber(viewport.y)}
        width={safeSvgNumber(viewport.width)}
        height={safeSvgNumber(viewport.height)}
        rx="8"
      />
    {/if}
    {#each peers as peer (peer.userId)}
      {@const pv = peer.viewport ? computeViewport(worldBox, peer.viewport) : null}
      {#if pv && pv.width > 0 && pv.height > 0}
        <rect
          class="mini-peer-viewport"
          x={safeSvgNumber(pv.x)}
          y={safeSvgNumber(pv.y)}
          width={safeSvgNumber(pv.width)}
          height={safeSvgNumber(pv.height)}
          rx="8"
          stroke={peer.color}
          fill={peer.color}
          fill-opacity="0.08"
          stroke-width="2"
        />
      {/if}
    {/each}
  </svg>
</div>
