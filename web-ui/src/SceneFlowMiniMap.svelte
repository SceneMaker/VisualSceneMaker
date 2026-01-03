<script>
  export let snapshot = null;
  export let worldBox = null;
  export let viewBox = null;
  export let onCenter = null;

  const COLORS = {
    node: "#7d7d7d",
    history: "#ffffff",
    edges: {
      eedge: "#827d78",
      fedge: "#234d67",
      tedge: "#543f1d",
      cedge: "#988e34",
      pedge: "#2a6723",
      iedge: "#983434"
    },
    comment: "rgba(200, 200, 200, 0.6)"
  };

  $: nodes = snapshot?.nodes || [];
  $: edges = snapshot?.edges || [];
  $: comments = snapshot?.comments || [];
  $: nodeMap = new Map(nodes.map((node) => [node.id, node]));
  $: miniViewBox = worldBox
    ? `${worldBox.x} ${worldBox.y} ${worldBox.width} ${worldBox.height}`
    : "0 0 200 200";
  $: viewport = computeViewport(worldBox, viewBox);

  function nodeCenter(node) {
    const x = node.graphics?.x ?? 0;
    const y = node.graphics?.y ?? 0;
    const w = node.size?.w ?? 160;
    const h = node.size?.h ?? 60;
    return { x: x + w / 2, y: y + h / 2 };
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
    const x1 = Math.max(world.x, view.x);
    const y1 = Math.max(world.y, view.y);
    const x2 = Math.min(world.x + world.width, view.x + view.width);
    const y2 = Math.min(world.y + world.height, view.y + view.height);
    const width = Math.max(0, x2 - x1);
    const height = Math.max(0, y2 - y1);
    return { x: x1, y: y1, width, height };
  }

  function handleClick(event) {
    if (!worldBox || typeof onCenter !== "function") return;
    const rect = event.currentTarget.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    const relX = (event.clientX - rect.left) / rect.width;
    const relY = (event.clientY - rect.top) / rect.height;
    const x = worldBox.x + relX * worldBox.width;
    const y = worldBox.y + relY * worldBox.height;
    onCenter(x, y);
  }

  function handleKeydown(event) {
    if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") return;
    event.preventDefault();
    if (!worldBox || typeof onCenter !== "function") return;
    onCenter(worldBox.x + worldBox.width / 2, worldBox.y + worldBox.height / 2);
  }
</script>

<div
  class="sceneflow-minimap"
  role="button"
  tabindex="0"
  aria-label="SceneFlow minimap"
  on:click={handleClick}
  on:keydown={handleKeydown}
>
  <svg class="sceneflow-minimap-canvas" viewBox={miniViewBox} aria-hidden="true">
    <g class="mini-comments">
      {#each comments as comment}
        <rect
          class="mini-comment"
          fill={COLORS.comment}
          x={comment.rect?.x ?? 0}
          y={comment.rect?.y ?? 0}
          width={comment.rect?.w ?? 0}
          height={comment.rect?.h ?? 0}
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
        {@const x = node.graphics?.x ?? 0}
        {@const y = node.graphics?.y ?? 0}
        {@const w = node.size?.w ?? 160}
        {@const h = node.size?.h ?? 60}
        {#if node.type === "Super"}
          <rect class="mini-node" x={x} y={y} width={w} height={h} fill={nodeFill(node)} rx="6" />
        {:else}
          <ellipse class="mini-node" cx={x + w / 2} cy={y + h / 2} rx={w / 2} ry={h / 2} fill={nodeFill(node)} />
        {/if}
      {/each}
    </g>
    {#if viewport}
      <rect class="mini-viewport" x={viewport.x} y={viewport.y} width={viewport.width} height={viewport.height} rx="8" />
    {/if}
  </svg>
</div>
