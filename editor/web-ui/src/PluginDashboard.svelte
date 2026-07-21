<script>
  import IconBlocks from "./icons/IconBlocks.svelte";
  import ScreenEditor from "./ScreenEditor.svelte";

  export let open = false;
  export let projectId = null;
  export let wsConnected = false;
  export let serverMode = "FULL_EDITOR";
  export let onClose = () => {};
  export let apiGet;
  export let apiPost;
  export let apiPut;
  export let projectName = "";
  export let sendCommand = null;      // WS command dispatcher from App.svelte
  export let onOpenPreview = () => {}; // loads this plugin's agent into the SIA panel in App.svelte (survives this modal closing)

  // ── screen editor state ────────────────────────────────────────────────────
  let screenEditorPlugin = null;

  function openScreenEditor(plugin) { screenEditorPlugin = plugin; }
  function closeScreenEditor()      { screenEditorPlugin = null; }

  // ── state ──────────────────────────────────────────────────────────────────
  let plugins = [];
  let loading = false;
  let loadError = "";
  let viewMode = "grid"; // "grid" | "flow"
  let filterTypes = new Set(); // active type filters (empty = show all)
  let filterServiceModel = ""; // "" | "service" | "self-contained"
  let healthResults = {};   // instanceName → { status, message, checkedAt }
  let healthChecking = {};  // instanceName → true while in-flight
  let selectedPlugin = null; // plugin entry being edited
  let editDraft = null;     // array of { key, value } being edited
  let editError = "";
  let editBusy = false;
  let expandedChips = new Set(); // instanceName → expanded chips

  // ── plugin update checks ───────────────────────────────────────────────────
  // className → { newVars: [{name, type}], specVersion: string, checking: bool }
  let updateChecks = {};
  let updateDialogPlugin = null;  // plugin whose update dialog is open
  let updateApplying = false;
  let updateApplyError = "";

  // ── derived ────────────────────────────────────────────────────────────────
  $: displayed = plugins.filter((p) => {
    const meta = p.meta || {};
    if (filterServiceModel && meta.serviceModel !== filterServiceModel) return false;
    if (filterTypes.size > 0) {
      const types = allTypes(meta);
      if (!types.some((t) => filterTypes.has(t))) return false;
    }
    return true;
  });

  $: flowLayout = buildFlowLayout(displayed, healthResults, healthChecking, projectName);
  let selectedFlowNodeId = null;

  // ── flow layout constants ───────────────────────────────────────────────────
  const FL_NODE_W = 184;
  const FL_NODE_H = 58;
  const FL_NODE_GAP = 14;
  const FL_COL_EDGE_GAP = 80;   // horizontal space reserved between columns for bezier routing
  const FL_PAD_LEFT = 20;
  const FL_PAD_TOP = 36;         // space below column header labels
  const FL_PAD_BOT = 32;
  const FL_COL_ORDER = ["input", "processing", "output", "other"];
  const FL_COL_LABELS = { input: "Input", processing: "Processing", output: "Output", other: "Other" };
  const FL_COL_ACCENT = { input: "#3b82f6", processing: "#f59e0b", output: "#10b981", other: "#9ca3af" };

  // ── helpers ────────────────────────────────────────────────────────────────
  function allTypes(meta) {
    const out = [meta.categories?.primary || "other"];
    if (Array.isArray(meta.categories?.secondary)) out.push(...meta.categories.secondary);
    return out;
  }

  function typeLabel(t) {
    return t ? t.charAt(0).toUpperCase() + t.slice(1) : t;
  }

  function healthDot(instanceName) {
    if (healthChecking[instanceName]) return "checking";
    const h = healthResults[instanceName];
    if (!h) return "unknown";
    return h.status;
  }

  function healthLabel(instanceName) {
    const h = healthResults[instanceName];
    if (!h) return "Not checked";
    if (h.status === "loaded") return "Plugin loaded — service not yet verified";
    return h.message || h.status;
  }

  function healthTooltip(instanceName) {
    const label = healthLabel(instanceName);
    const h = healthResults[instanceName];
    if (!h || h.status === "unknown") return "Not checked — click to check";
    if (h.status === "loaded") return `${label} — click to check`;
    return label;
  }

  // Build radial flow layout: director (project runtime) at centre-left,
  // plugin agents arranged in a right-facing half-circle around it.
  // _h / _hc / _pn are passed so Svelte tracks all reactive deps.
  function buildFlowLayout(ps, _h, _hc, _pn) {
    const DIR_SIZE = 72;
    const DIR_R    = DIR_SIZE / 2;
    const PAD      = 36;
    const LABEL_H  = 24;

    // Sort: input (top) → processing (mid) → output (bottom) → other
    const typePriority = { input: 0, processing: 1, output: 2, other: 3 };
    const sorted = [...ps].sort((a, b) => {
      const pa = typePriority[a.meta?.categories?.primary || "other"] ?? 3;
      const pb = typePriority[b.meta?.categories?.primary || "other"] ?? 3;
      return pa - pb;
    });
    const n = sorted.length;

    // ── Radius ────────────────────────────────────────────────────────────────
    // n+1 equal segments → plugins never touch the endpoints of the arc.
    // Arc spacing per slot = π·R/(n+1) ≥ FL_NODE_H + gap
    const minSpacing = FL_NODE_H + FL_NODE_GAP + 8;
    const radius = Math.round(Math.max(300, Math.ceil((minSpacing * (n + 1)) / Math.PI)) * 1.25);

    // ── Director centre ───────────────────────────────────────────────────────
    // dirCY chosen so topmost node's top edge sits at PAD:
    //   dirCY − radius − FL_NODE_H/2 = PAD  →  dirCY = PAD + radius + FL_NODE_H/2
    const dirCX = PAD + DIR_R;
    const dirCY = PAD + radius + FL_NODE_H / 2;

    // ── Canvas size ───────────────────────────────────────────────────────────
    let svgWidth  = dirCX + radius + FL_NODE_W / 2 + PAD + 20;
    const svgHeight = dirCY + radius + FL_NODE_H / 2 + PAD + LABEL_H;

    const director = {
      id: "__director__",
      x: dirCX - DIR_R, y: dirCY - DIR_R,
      w: DIR_SIZE, h: DIR_SIZE,
      isDirector: true,
      label: _pn || "SceneMaker",
    };

    // ── Plugin nodes ──────────────────────────────────────────────────────────
    // θ_i = -π/2 + π·(i+1)/(n+1)  — equidistant, endpoints excluded.
    const pluginNodes = sorted.map((p, i) => {
      const theta = n === 1 ? 0 : -Math.PI / 2 + (Math.PI * (i + 1)) / (n + 1);
      const pcx = dirCX + radius * Math.cos(theta);
      const pcy = dirCY + radius * Math.sin(theta);
      return {
        id: p.instanceName,
        x: pcx - FL_NODE_W / 2,
        y: pcy - FL_NODE_H / 2,
        w: FL_NODE_W, h: FL_NODE_H,
        col: p.meta?.categories?.primary || "other",
        label: p.instanceName,
        types: allTypes(p.meta || {}),
        serviceModel: p.meta?.serviceModel || "self-contained",
        health: healthDot(p.instanceName),
        theta,
        writes: (p.meta?.writes || []).map(v => v.var || v),
        reads:  (p.meta?.reads  || []).map(v => v.var || v),
      };
    });

    const nodes = [director, ...pluginNodes];

    // ── Service boxes ──────────────────────────────────────────────────────────
    // For service plugins, place a small endpoint box to the right.
    const SVC_GAP = 44;
    const SVC_W   = 250;
    const SVC_H   = 36;
    for (const node of pluginNodes) {
      if (node.serviceModel !== "service") continue;
      const sp = sorted.find(s => s.instanceName === node.id);
      const features = sp?.features || [];
      let endpoint = "";
      let host = null, portStr = null;
      for (const f of features) {
        const k = (f.key || "").toLowerCase();
        const v = (f.value || "").trim();
        if (!v) continue;
        if (k.includes("url") || k.startsWith("ws")) { endpoint = v; break; }
        else if (k.includes("host")) host = v;
        else if (k.includes("port") && host) portStr = v;
      }
      if (!endpoint && host) endpoint = portStr ? `${host}:${portStr}` : host;
      node.serviceBox = {
        x: node.x + node.w + SVC_GAP,
        y: node.y + (node.h - SVC_H) / 2,
        w: SVC_W, h: SVC_H,
        endpoint,
        latency: _h[node.id]?.latency ?? null,
      };
      svgWidth = Math.max(svgWidth, node.serviceBox.x + node.serviceBox.w + PAD);
    }

    // ── Dangling-variable detection ───────────────────────────────────────────
    const allWriteVars = new Map();
    const allReadVars  = new Map();
    for (const nd of pluginNodes) {
      for (const v of nd.writes) {
        if (!allWriteVars.has(v)) allWriteVars.set(v, new Set());
        allWriteVars.get(v).add(nd.id);
      }
      for (const v of nd.reads) {
        if (!allReadVars.has(v)) allReadVars.set(v, new Set());
        allReadVars.get(v).add(nd.id);
      }
    }
    function getDanglingWrites(pluginId, varList) {
      return varList.filter(v => {
        const r = allReadVars.get(v);
        return !r || ![...r].some(id => id !== pluginId);
      });
    }
    function getDanglingReads(pluginId, varList) {
      return varList.filter(v => {
        const w = allWriteVars.get(v);
        return !w || ![...w].some(id => id !== pluginId);
      });
    }

    // ── Port helpers ──────────────────────────────────────────────────────────
    // Plugin port: always the LEFT edge centre (user-facing "left docking point").
    function plugLeftPort(nd) {
      return { x: nd.x, y: nd.y + nd.h / 2 };
    }

    // Point on the director circle edge facing towards (tx, ty).
    function dirPort(tx, ty) {
      const dx = tx - dirCX, dy = ty - dirCY;
      const len = Math.sqrt(dx * dx + dy * dy) || 1;
      return { x: dirCX + DIR_R * dx / len, y: dirCY + DIR_R * dy / len };
    }

    // ── Edges ─────────────────────────────────────────────────────────────────
    // ONE straight line per plugin.
    //   input     : plugin → director, arrowhead at director
    //   output/other: director → plugin, arrowhead at plugin
    //   processing: director ↔ plugin, arrowhead at BOTH ends
    // Ports are precomputed and stored on each edge for rendering docking dots.
    const edges = [];
    for (const node of pluginNodes) {
      const primary  = node.col;
      const isInput  = primary === "input";
      const isOutput = primary === "output";
      const isProc   = primary === "processing";
      // True when the plugin is both input AND output (regardless of which is primary).
      const isInputOutput = (isInput && node.types.includes("output"))
                         || (isOutput && node.types.includes("input"));

      if (isInputOutput) {
        // ── Two separate edges, each with its own docking point ───────────────
        // Upper-left port: output connection (director → plugin)
        // Lower-left port: input  connection (plugin → director)
        const ppOut = { x: node.x, y: node.y + node.h / 3 };
        const ppIn  = { x: node.x, y: node.y + node.h * 2 / 3 };
        const dpOut = dirPort(ppOut.x, ppOut.y);
        const dpIn  = dirPort(ppIn.x,  ppIn.y);
        edges.push({
          id:           `edge_${node.id}_out`,
          from:         "__director__", to: node.id,
          pluginPort:   ppOut, directorPort: dpOut,
          bothEnds:     false,
          varNames:     node.reads,
          hasDangling:  getDanglingReads(node.id, node.reads).length > 0,
          style:        "solid",
          col:          "output",
          color:        FL_COL_ACCENT["output"],
          pluginId:     node.id,
        });
        edges.push({
          id:           `edge_${node.id}_in`,
          from:         node.id, to: "__director__",
          pluginPort:   ppIn, directorPort: dpIn,
          bothEnds:     false,
          varNames:     node.writes,
          hasDangling:  getDanglingWrites(node.id, node.writes).length > 0,
          style:        "solid",
          col:          "input",
          color:        FL_COL_ACCENT["input"],
          pluginId:     node.id,
        });
      } else {
        // ── Single edge for pure input / output / processing / other ──────────
        const varNames = isProc
          ? [...new Set([...node.reads, ...node.writes])]
          : isInput ? node.writes : node.reads;
        const dang = isProc
          ? [...getDanglingReads(node.id, node.reads), ...getDanglingWrites(node.id, node.writes)]
          : isInput
            ? getDanglingWrites(node.id, node.writes)
            : getDanglingReads(node.id, node.reads);
        const pp = plugLeftPort(node);
        const dp = dirPort(pp.x, pp.y);
        const fromDir = !isInput && !isProc;
        // Color priority across all types: input (blue) > output (green) > primary
        const edgeCol = node.types.includes("input")  ? "input"
                      : node.types.includes("output") ? "output"
                      : primary;
        edges.push({
          id:           `edge_${node.id}`,
          from:         fromDir ? "__director__" : node.id,
          to:           fromDir ? node.id        : "__director__",
          pluginPort:   pp,
          directorPort: dp,
          bothEnds:     isProc,
          varNames,
          hasDangling:  dang.length > 0,
          style:        isProc ? "dashed" : "solid",
          col:          edgeCol,
          color:        FL_COL_ACCENT[edgeCol] || FL_COL_ACCENT["other"],
          pluginId:     node.id,
        });
      }
    }

    // ── Guide arc ─────────────────────────────────────────────────────────────
    // Dotted right-facing semicircle centred on director.
    // M top  A r,r 0 0,1 bottom  (sweep=1, large-arc=0 → right half) ✓
    const guideArcPath =
      `M ${dirCX},${dirCY - radius} A ${radius},${radius} 0 0,1 ${dirCX},${dirCY + radius}`;

    // ── Path / label helpers ──────────────────────────────────────────────────
    function nodeById(id) { return nodes.find(nd => nd.id === id); }

    // Straight line between the precomputed port positions.
    function edgePath(edge) {
      const { pluginPort: pp, directorPort: dp, from } = edge;
      if (!pp || !dp) return "";
      const fromDir = nodeById(from)?.isDirector;
      const x1 = fromDir ? dp.x : pp.x, y1 = fromDir ? dp.y : pp.y;
      const x2 = fromDir ? pp.x : dp.x, y2 = fromDir ? pp.y : dp.y;
      return `M ${x1} ${y1} L ${x2} ${y2}`;
    }

    // Midpoint between the two ports, shifted slightly perpendicular for label clearance.
    function edgeMid(edge) {
      const { pluginPort: pp, directorPort: dp } = edge;
      if (!pp || !dp) return { x: 0, y: 0 };
      const mx = (pp.x + dp.x) / 2;
      const my = (pp.y + dp.y) / 2;
      // Perpendicular offset so label doesn't sit exactly on the line
      const dist = Math.sqrt((dp.x - pp.x) ** 2 + (dp.y - pp.y) ** 2) || 1;
      return {
        x: mx - ((dp.y - pp.y) / dist) * 10,
        y: my + ((dp.x - pp.x) / dist) * 10 - 4,
      };
    }

    return {
      nodes, director, edges, guideArcPath, edgePath, edgeMid, nodeById,
      colX: {}, occupiedCols: [],
      svgWidth, svgHeight,
    };
  }

  // ── flow view helpers ──────────────────────────────────────────────────────
  function isEdgeHighlighted(edge) {
    if (!selectedFlowNodeId) return true;
    return edge.from === selectedFlowNodeId || edge.to === selectedFlowNodeId;
  }

  function isNodeHighlighted(nodeId) {
    if (!selectedFlowNodeId) return true;
    if (nodeId === selectedFlowNodeId) return true;
    return flowLayout.edges.some(
      (e) =>
        (e.from === selectedFlowNodeId && e.to === nodeId) ||
        (e.to === selectedFlowNodeId && e.from === nodeId)
    );
  }

  function toggleFlowNode(id) {
    selectedFlowNodeId = selectedFlowNodeId === id ? null : id;
  }

  // ── data loading ───────────────────────────────────────────────────────────
  async function loadDashboard() {
    if (!projectId) return;
    loading = true;
    loadError = "";
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/plugins/dashboard`);
      plugins = Array.isArray(data.plugins) ? data.plugins : [];
      // Seed health dots from the dashboard's runtime status field so that
      // self-contained plugins show ok/not_loaded immediately without a manual check.
      const seeded = {};
      for (const p of plugins) {
        if (p.status && p.status !== "unknown") {
          const msg =
            p.status === "ok"     ? "Plugin loaded and available" :
            p.status === "loaded" ? "Plugin loaded — service not yet verified" :
                                    "Plugin not loaded";
          seeded[p.instanceName] = { status: p.status, message: msg, checkedAt: Date.now() };
        }
      }
      healthResults = { ...healthResults, ...seeded };
      // Auto-check all loaded plugins so dots resolve to real status without user interaction.
      // Fire in background (no await) — dots show amber while checks are in-flight.
      const toAutoCheck = plugins.filter((p) => p.status === "ok" || p.status === "loaded");
      if (toAutoCheck.length > 0) {
        Promise.all(toAutoCheck.map((p) => checkHealth(p.instanceName)));
      }
      // Check for plugin spec updates in the background.
      checkPluginUpdates();
    } catch (err) {
      loadError = err.message || "Failed to load plugin dashboard.";
    } finally {
      loading = false;
    }
  }

  // ── health ─────────────────────────────────────────────────────────────────
  async function checkHealth(instanceName) {
    if (healthChecking[instanceName]) return;
    healthChecking = { ...healthChecking, [instanceName]: true };
    try {
      const data = await apiPost(`/api/v1/projects/${projectId}/plugins/${encodeURIComponent(instanceName)}/health`, {});
      healthResults = { ...healthResults, [instanceName]: data.health };
    } catch (err) {
      healthResults = { ...healthResults, [instanceName]: { status: "error", message: err.message, checkedAt: Date.now() } };
    } finally {
      healthChecking = { ...healthChecking, [instanceName]: false };
    }
  }

  async function checkAllHealth() {
    await Promise.all(displayed.map((p) => checkHealth(p.instanceName)));
  }

  // ── param editing ──────────────────────────────────────────────────────────
  function openEdit(plugin) {
    selectedPlugin = plugin;
    // Build a value map from current instance features
    const featureMap = {};
    for (const f of plugin.features || []) featureMap[f.key] = f.value;

    // Merge schema entries (meta.config) with current values.
    // Schema entries come first; then any extra feature keys not in schema.
    const schema = plugin.meta?.config || [];
    const schemaKeys = new Set(schema.map((s) => s.key));

    editDraft = [
      ...schema.map((s) => ({
        key: s.key,
        type: s.type || "string",
        required: s.required ?? false,
        default: String(s.default ?? ""),
        description: s.description || "",
        value: featureMap[s.key] ?? String(s.default ?? ""),
      })),
      // Extra keys not in schema (custom / legacy) — skip internal system keys (_*)
      ...(plugin.features || [])
        .filter((f) => !schemaKeys.has(f.key) && !f.key.startsWith("_"))
        .map((f) => ({
          key: f.key,
          type: "string",
          required: false,
          default: "",
          description: "",
          value: f.value,
        })),
    ];
    editError = "";
    editBusy = false;
  }

  function closeEdit() {
    selectedPlugin = null;
    editDraft = null;
    editError = "";
  }

  async function saveEdit() {
    if (!selectedPlugin || editBusy) return;
    editBusy = true;
    editError = "";
    try {
      await apiPost(
        `/api/v1/projects/${projectId}/plugins/${encodeURIComponent(selectedPlugin.instanceName)}/params`,
        { features: editDraft.map(({ key, value }) => ({ key, value })) }
      );
      // Refresh dashboard
      await loadDashboard();
      closeEdit();
    } catch (err) {
      editError = err.message || "Failed to save parameters.";
    } finally {
      editBusy = false;
    }
  }

  // ── update checks ─────────────────────────────────────────────────────────
  async function checkPluginUpdates() {
    if (!sendCommand || !projectId || plugins.length === 0) return;
    for (const p of plugins) {
      const className = p.meta?.plugin?.className || p.className;
      if (!className) continue;
      updateChecks = { ...updateChecks, [className]: { ...(updateChecks[className] || {}), checking: true } };
      try {
        // No knownVarNames: let the server query its own SceneFlow (authoritative source)
        const result = await sendCommand("ProjectConfig.Plugin.GetUpdate", {
          projectId,
          className,
        });
        updateChecks = {
          ...updateChecks,
          [className]: {
            checking: false,
            newVars: result?.newVars || [],
            specVersion: result?.specVersion || "",
            upToDate: result?.upToDate ?? true,
          },
        };
      } catch (_) {
        updateChecks = { ...updateChecks, [className]: { checking: false, newVars: [], upToDate: true } };
      }
    }
  }

  async function applyUpdate() {
    if (!updateDialogPlugin || !sendCommand || updateApplying) return;
    const className = updateDialogPlugin.meta?.plugin?.className || updateDialogPlugin.className;
    const check = updateChecks[className];
    if (!check?.newVars?.length) return;
    updateApplying = true;
    updateApplyError = "";
    try {
      for (const v of check.newVars) {
        await sendCommand("SceneFlow.Node.VarDef.Add", {
          projectId,
          nodeId: "",
          superNodeId: "",
          varDef: { name: v.name, type: v.type },
        });
      }
      updateDialogPlugin = null;
      // Re-check after a short delay (server broadcasts snapshot update)
      setTimeout(() => checkPluginUpdates(), 600);
    } catch (err) {
      updateApplyError = err.message || "Failed to apply update.";
    } finally {
      updateApplying = false;
    }
  }

  function toggleChips(name) {
    const next = new Set(expandedChips);
    if (next.has(name)) next.delete(name);
    else next.add(name);
    expandedChips = next;
  }

  function displayName(plugin) {
    const n = plugin.meta?.plugin?.name;
    return n && n !== plugin.instanceName ? n : plugin.instanceName;
  }
  function instanceLabel(plugin) {
    const n = plugin.meta?.plugin?.name;
    return n && n !== plugin.instanceName ? plugin.instanceName : null;
  }

  // ── filter toggles ─────────────────────────────────────────────────────────
  function toggleTypeFilter(t) {
    const next = new Set(filterTypes);
    if (next.has(t)) next.delete(t);
    else next.add(t);
    filterTypes = next;
  }

  // ── lifecycle ──────────────────────────────────────────────────────────────
  $: if (open && projectId) {
    loadDashboard();
  }

  function handleBackdropClick(e) {
    if (e.target === e.currentTarget) onClose();
  }

  function handleKeydown(e) {
    if (e.key === "Escape") onClose();
  }


</script>

{#if open}
  <!-- svelte-ignore a11y-click-events-have-key-events -->
  <!-- svelte-ignore a11y-no-static-element-interactions -->
  <div
    class="modal-backdrop plugin-dashboard-backdrop"
    role="presentation"
    on:click={handleBackdropClick}
  >
    <!-- svelte-ignore a11y-no-noninteractive-element-interactions -->
    <div
      class="modal plugin-dashboard-modal"
      role="dialog"
      aria-modal="true"
      aria-labelledby="plugin-dashboard-title"
      tabindex="-1"
      on:keydown={handleKeydown}
    >
      <!-- Header -->
      <div class="pd-header">
        <div class="pd-title">
          <span class="pd-title-icon"><IconBlocks className="icon" /></span>
          <div>
            <h3 id="plugin-dashboard-title">Plugin Dashboard</h3>
          </div>
        </div>
        <div class="pd-header-actions">
          <div class="pd-view-toggle" role="group" aria-label="View mode">
            <button
              type="button"
              class="pd-view-btn"
              class:active={viewMode === "grid"}
              on:click={() => (viewMode = "grid")}
              title="Grid view"
            >Grid</button>
            <button
              type="button"
              class="pd-view-btn"
              class:active={viewMode === "flow"}
              on:click={() => (viewMode = "flow")}
              title="Connection flow view"
            >Flow</button>
          </div>
          <button
            type="button"
            class="ghost icon-button pd-close"
            on:click={onClose}
            aria-label="Close plugin dashboard"
            title="Close"
          >×</button>
        </div>
      </div>

      <!-- Body -->
      <div class="pd-body">
        <!-- Sidebar -->
        <aside class="pd-sidebar">
          <div class="pd-sidebar-section">
            <div class="pd-sidebar-heading">Type</div>
            {#each [["input","Input"],["processing","Processing"],["output","Output"]] as [val, label]}
              <label class="pd-filter-row">
                <input
                  type="checkbox"
                  checked={filterTypes.has(val)}
                  on:change={() => toggleTypeFilter(val)}
                />
                <span class="pd-filter-dot pd-dot-{val}"></span>
                <span class="pd-filter-label">{label}</span>
              </label>
            {/each}
          </div>
          <div class="pd-sidebar-section">
            <div class="pd-sidebar-heading">Category</div>
            <label class="pd-filter-row">
              <input type="radio" name="serviceModel" value="" bind:group={filterServiceModel} />
              <span class="pd-filter-label">All</span>
            </label>
            <label class="pd-filter-row">
              <input type="radio" name="serviceModel" value="service" bind:group={filterServiceModel} />
              <span class="pd-filter-dot pd-dot-service"></span>
              <span class="pd-filter-label">Service</span>
            </label>
            <label class="pd-filter-row">
              <input type="radio" name="serviceModel" value="self-contained" bind:group={filterServiceModel} />
              <span class="pd-filter-dot pd-dot-self"></span>
              <span class="pd-filter-label">Self-contained</span>
            </label>
          </div>
          {#if plugins.length > 0}
            <div class="pd-sidebar-stats">
              {displayed.length} of {plugins.length} plugin{plugins.length !== 1 ? "s" : ""}
            </div>
          {/if}
          <div class="pd-sidebar-section pd-sidebar-bottom">
            <button
              type="button"
              class="pd-check-all-btn"
              disabled={!wsConnected || displayed.length === 0}
              on:click={checkAllHealth}
            >↻ Check all health</button>
          </div>
        </aside>

        <!-- Main content -->
        <main class="pd-main">
          {#if loading}
            <div class="pd-status">Loading plugins…</div>
          {:else if loadError}
            <div class="pd-status" style="color: var(--danger)">{loadError}</div>
          {:else if displayed.length === 0 && plugins.length === 0}
            <div class="pd-status">No plugins configured in this project.</div>
          {:else if displayed.length === 0}
            <div class="pd-status">No plugins match the current filter.</div>
          {:else if viewMode === "flow"}
            <!-- Flow / connection diagram (radial, director at centre-left) -->
            <div class="pd-flow-wrap">
              <!-- svelte-ignore a11y-click-events-have-key-events -->
              <svg
                class="pd-flow-svg"
                width={flowLayout.svgWidth}
                height={flowLayout.svgHeight}
                aria-label="Plugin connection flow diagram"
              >
                <defs>
                  <!-- Per-type arrowheads matching FL_COL_ACCENT -->
                  <marker id="pd-arrowhead-input" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto-start-reverse">
                    <polygon points="0 0, 8 3, 0 6" fill="#3b82f6" opacity="0.8" />
                  </marker>
                  <marker id="pd-arrowhead-processing" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto-start-reverse">
                    <polygon points="0 0, 8 3, 0 6" fill="#f59e0b" opacity="0.8" />
                  </marker>
                  <marker id="pd-arrowhead-output" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto-start-reverse">
                    <polygon points="0 0, 8 3, 0 6" fill="#10b981" opacity="0.8" />
                  </marker>
                  <marker id="pd-arrowhead-other" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto-start-reverse">
                    <polygon points="0 0, 8 3, 0 6" fill="#9ca3af" opacity="0.8" />
                  </marker>
                  <!-- Amber marker for edges with dangling (unmatched) variables -->
                  <marker id="pd-arrowhead-warn" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto-start-reverse">
                    <polygon points="0 0, 8 3, 0 6" fill="#f59e0b" opacity="0.85" />
                  </marker>
                  <!-- Purple marker for service connections -->
                  <marker id="pd-arrowhead-svc" markerWidth="8" markerHeight="6"
                          refX="7" refY="3" orient="auto">
                    <polygon points="0 0, 8 3, 0 6" fill="#8b5cf6" opacity="0.8" />
                  </marker>
                </defs>

                <!-- Guideline arc — dotted right semicircle centred on director -->
                <path
                  d={flowLayout.guideArcPath}
                  fill="none"
                  stroke="var(--accent)"
                  stroke-width="1"
                  stroke-dasharray="3 5"
                  opacity="0.18"
                  pointer-events="none"
                />

                <!-- Edge lines (drawn before nodes so nodes render on top) -->
                {#each flowLayout.edges as edge}
                  {@const highlighted = isEdgeHighlighted(edge)}
                  {@const arrowId = `url(#pd-arrowhead-${edge.col})`}
                  <line
                    x1={edge.from === "__director__" ? edge.directorPort.x : edge.pluginPort.x}
                    y1={edge.from === "__director__" ? edge.directorPort.y : edge.pluginPort.y}
                    x2={edge.to   === "__director__" ? edge.directorPort.x : edge.pluginPort.x}
                    y2={edge.to   === "__director__" ? edge.directorPort.y : edge.pluginPort.y}
                    stroke={edge.color}
                    stroke-width={highlighted ? 2 : 1.5}
                    stroke-dasharray={edge.style === "dashed" ? "8 4" : "none"}
                    opacity={highlighted ? 0.85 : 0.28}
                    marker-end={arrowId}
                    marker-start={edge.bothEnds ? arrowId : "none"}
                  />
                  <!-- Variable name annotation at line midpoint; amber when dangling -->
                  {#if edge.varNames?.length > 0}
                    {@const mid = flowLayout.edgeMid(edge)}
                    {@const label = edge.varNames.slice(0, 2).join(", ") + (edge.varNames.length > 2 ? ` +${edge.varNames.length - 2}` : "")}
                    <text
                      x={mid.x} y={mid.y}
                      text-anchor="middle"
                      class="pd-flow-edge-label"
                      style="fill:{edge.hasDangling ? '#f59e0b' : edge.color}"
                      opacity={highlighted ? 0.85 : 0.25}
                    >{label}</text>
                  {/if}
                {/each}

                <!-- Nodes -->
                {#each flowLayout.nodes as node}
                  {@const dimmed = !isNodeHighlighted(node.id)}
                  {@const selected = node.id === selectedFlowNodeId}

                  {#if node.isDirector}
                    <!-- ── Director node ─────────────────────────────────────── -->
                    <!-- svelte-ignore a11y-no-static-element-interactions -->
                    <g
                      class="pd-flow-node pd-director-node"
                      class:pd-flow-node-dimmed={dimmed}
                      transform="translate({node.x},{node.y})"
                      on:click={() => toggleFlowNode(node.id)}
                    >
                      <title>{node.label} — Director{selected ? " (click to deselect)" : " (click to highlight)"}</title>
                      {#if selected}
                        <circle cx={node.w/2} cy={node.h/2} r={node.w/2 + 5}
                          fill="none" stroke="var(--accent)" stroke-width="2" opacity="0.35" />
                      {/if}
                      <circle cx={node.w/2} cy={node.h/2} r={node.w/2}
                        fill="var(--panel)" stroke="var(--accent)" stroke-width={selected ? 3 : 2} />
                      <g transform="translate({node.w/2 - 19},{node.h/2 - 19})">
                        <svg width="38" height="38" viewBox="0 0 256 256">
                          <path fill="var(--accent)"
                            d="M229.11,70.82A16,16,0,0,0,216,64H136V32h16a8,8,0,0,0,0-16H104a8,8,0,0,0,0,16h16V64H40A16,16,0,0,0,25,85.47l26.19,72a16,16,0,0,0,15,10.53H96v64a8,8,0,0,0,16,0V168h32v64a8,8,0,0,0,16,0V168h29.82a16,16,0,0,0,15-10.53l26.19-72A16,16,0,0,0,229.11,70.82ZM110.68,152,97.58,80h60.84l-13.1,72ZM40,80H81.32l13.09,72H66.18Zm149.82,72H161.59l13.09-72H216Z"
                          />
                        </svg>
                      </g>
                      <text x={node.w/2} y={node.h + 17} text-anchor="middle"
                        class="pd-director-label">{node.label}</text>
                    </g>

                  {:else}
                    <!-- ── Plugin agent node ──────────────────────────────────── -->
                    <!-- svelte-ignore a11y-no-static-element-interactions -->
                    <g
                      class="pd-flow-node"
                      class:pd-flow-node-dimmed={dimmed}
                      class:pd-flow-node-selected={selected}
                      transform="translate({node.x},{node.y})"
                      on:click={() => toggleFlowNode(node.id)}
                    >
                      <title>{node.label} ({node.types.join(", ") || node.serviceModel}) — {healthLabel(node.id)}</title>
                      <rect width={node.w} height={node.h} rx="6"
                        fill="var(--panel)"
                        stroke={selected ? "var(--accent)" : (FL_COL_ACCENT[node.col] || "var(--stroke)")}
                        stroke-width={selected ? 2 : 1.5}
                      />
                      <!-- Type accent bar (left edge colour) -->
                      <rect x="0" y="0" width="4" height={node.h} rx="3"
                        fill={FL_COL_ACCENT[node.col] || "var(--stroke)"} opacity="0.7" />
                      <!-- Health dot -->
                      <circle cx={node.w - 10} cy="12" r="5"
                        class="pd-health-dot pd-health-{node.health}" />
                      <text x="14" y="26" class="pd-flow-node-name">{node.label}</text>
                      <text x="14" y="42" class="pd-flow-node-types">{node.types.join(" · ")}</text>
                    </g>
                  {/if}
                {/each}

                <!-- Docking dots — rendered last so they sit on top of node borders -->
                {#each flowLayout.edges as edge}
                  {@const highlighted = isEdgeHighlighted(edge)}
                  {@const dotFill = edge.color}
                  {@const op = highlighted ? 0.9 : 0.35}
                  <!-- Director-side docking dot -->
                  <circle
                    cx={edge.directorPort.x} cy={edge.directorPort.y} r="4"
                    fill={dotFill} opacity={op} pointer-events="none"
                  />
                  <!-- Plugin-side docking dot -->
                  <circle
                    cx={edge.pluginPort.x} cy={edge.pluginPort.y} r="4"
                    fill={dotFill} opacity={op} pointer-events="none"
                  />
                {/each}

                <!-- Service boxes — external service endpoint visualisation -->
                {#each flowLayout.nodes as node}
                  {#if !node.isDirector && node.serviceBox}
                    {@const sb = node.serviceBox}
                    {@const px = node.x + node.w}
                    {@const py = node.y + node.h / 2}
                    {@const sx = sb.x}
                    {@const sy = sb.y + sb.h / 2}
                    <!-- Service connection line (plugin right → service box left) -->
                    <line
                      x1={px} y1={py} x2={sx} y2={sy}
                      stroke="#8b5cf6" stroke-width="1.5" stroke-dasharray="5 3"
                      opacity="0.65" marker-end="url(#pd-arrowhead-svc)"
                    />
                    <!-- Latency label above the connection midpoint -->
                    {#if sb.latency != null}
                      <text
                        x={(px + sx) / 2} y={py - 7}
                        text-anchor="middle"
                        class="pd-flow-svc-label"
                      >{sb.latency}ms</text>
                    {/if}
                    <!-- Service box: dashed purple border -->
                    <rect
                      x={sb.x} y={sb.y} width={sb.w} height={sb.h}
                      rx="5" fill="var(--panel)" stroke="#8b5cf6"
                      stroke-width="1.5" stroke-dasharray="4 2"
                    />
                    <!-- Accent bar -->
                    <rect x={sb.x} y={sb.y} width="4" height={sb.h}
                      rx="3" fill="#8b5cf6" opacity="0.5" />
                    <!-- Endpoint label -->
                    <text
                      x={sb.x + sb.w / 2 + 2} y={sb.y + sb.h / 2 + 4}
                      text-anchor="middle"
                      class="pd-flow-svc-label"
                    >{sb.endpoint || "service"}</text>
                  {/if}
                {/each}
              </svg>
            </div>
          {:else}
            <!-- Grid view -->
            <div class="pd-grid-scroll">
            <div class="pd-grid">
              {#each displayed as plugin (plugin.instanceName)}
                {@const meta = plugin.meta || {}}
                {@const types = allTypes(meta)}
                {@const health = healthDot(plugin.instanceName)}
                {@const checking = healthChecking[plugin.instanceName]}
                {@const pluginClassName = meta.plugin?.className || plugin.className || ""}
                {@const updateCheck = updateChecks[pluginClassName]}
                {@const hasUpdate = (updateCheck?.newVars?.length ?? 0) > 0}
                <div class="pd-card" class:pd-card-editing={selectedPlugin === plugin}>
                  <div class="pd-card-header">
                    <div class="pd-card-name">
                      <button
                        type="button"
                        class="pd-health-btn pd-health-dot pd-health-{checking ? 'checking' : health}"
                        title={healthTooltip(plugin.instanceName)}
                        disabled={!wsConnected || checking}
                        on:click={() => checkHealth(plugin.instanceName)}
                      ></button>
                      <div class="pd-card-name-stack">
                        <span class="pd-card-title">{displayName(plugin)}</span>
                        {#if instanceLabel(plugin)}
                          <span class="pd-card-subtitle">{instanceLabel(plugin)}</span>
                        {/if}
                      </div>
                    </div>
                    <div class="pd-card-badges">
                      {#each types as t}
                        <span class="pd-type-badge pd-type-{t}">{typeLabel(t)}</span>
                      {/each}
                      <span class="pd-service-badge {meta.serviceModel || 'self-contained'}">
                        {meta.serviceModel === "service" ? "Service" : "Self-contained"}
                      </span>
                      {#if hasUpdate}
                        <button
                          type="button"
                          class="pd-update-badge"
                          title="{updateCheck.newVars.length} new SceneFlow variable{updateCheck.newVars.length !== 1 ? 's' : ''} available — click to review"
                          on:click={() => { updateDialogPlugin = plugin; updateApplyError = ""; }}
                        >↑ Update</button>
                      {/if}
                    </div>
                  </div>

                  {#if meta.plugin?.description}
                    <p class="pd-card-desc">{meta.plugin.description}</p>
                  {/if}

                  <!-- Variable chips -->
                  {#if (meta.writes?.length || meta.reads?.length)}
                    {@const allChips = [...(meta.writes||[]).map(v=>({dir:"write",v})), ...(meta.reads||[]).map(v=>({dir:"read",v}))]}
                    {@const MAX = 6}
                    {@const expanded = expandedChips.has(plugin.instanceName)}
                    {@const visible = expanded ? allChips : allChips.slice(0, MAX)}
                    <div class="pd-var-chips">
                      {#each visible as chip}
                        <span class="pd-var-chip {chip.dir}" title="{chip.dir === 'write' ? 'writes' : 'reads'} {chip.v.var}">
                          {chip.dir === "write" ? "→" : "←"} {chip.v.var}
                        </span>
                      {/each}
                      {#if allChips.length > MAX}
                        <button type="button" class="pd-chips-more" on:click={() => toggleChips(plugin.instanceName)}>
                          {expanded ? "show less" : `+${allChips.length - MAX} more`}
                        </button>
                      {/if}
                    </div>
                  {/if}

                  <!-- Commands -->
                  {#if meta.commands?.length}
                    <details class="pd-commands">
                      <summary class="pd-commands-summary">
                        <span class="pd-commands-label">Commands</span>
                        <span class="pd-commands-count">{meta.commands.length}</span>
                      </summary>
                      <ul class="pd-commands-list">
                        {#each meta.commands as cmd}
                          <li class="pd-command-row" title={cmd.summary || ""}>
                            <code class="pd-command-sig">{cmd.name}{#if cmd.params?.length}({cmd.params.map(p => p.name).join(", ")}){:else}(){/if}</code>
                            {#if cmd.summary}<span class="pd-command-summary">{cmd.summary}</span>{/if}
                          </li>
                        {/each}
                      </ul>
                    </details>
                  {/if}

                  <!-- Edit panel -->
                  {#if selectedPlugin === plugin && editDraft}
                    <div class="pd-edit-panel">
                      <div class="pd-edit-fields">
                        {#each editDraft as row, i}
                          {@const isModified = row.value !== row.default && row.default !== ""}
                          <div class="pd-edit-row">
                            <div class="pd-edit-label-group">
                              <span class="pd-edit-key" title={row.key}>{row.key}</span>
                              {#if row.required}
                                <span class="pd-edit-badge required">req</span>
                              {:else}
                                <span class="pd-edit-badge optional">opt</span>
                              {/if}
                            </div>
                            <div class="pd-edit-input-wrap">
                              {#if row.type === "int" || row.type === "float"}
                                <input
                                  class="pd-edit-value"
                                  class:modified={isModified}
                                  type="number"
                                  step={row.type === "int" ? "1" : "any"}
                                  bind:value={editDraft[i].value}
                                  disabled={editBusy}
                                  title={row.description || row.key}
                                />
                              {:else if row.type === "bool" || row.type === "boolean"}
                                <label class="pd-edit-toggle">
                                  <input
                                    type="checkbox"
                                    checked={row.value === "true"}
                                    disabled={editBusy}
                                    on:change={(e) => (editDraft[i].value = e.target.checked ? "true" : "false")}
                                  />
                                  <span class="pd-edit-toggle-label">{editDraft[i].value === "true" ? "On" : "Off"}</span>
                                </label>
                              {:else}
                                <input
                                  class="pd-edit-value"
                                  class:modified={isModified}
                                  type="text"
                                  bind:value={editDraft[i].value}
                                  disabled={editBusy}
                                  title={row.description || row.key}
                                  placeholder={row.default || ""}
                                />
                              {/if}
                              {#if row.description}
                                <span class="pd-edit-desc" title={row.description}>?</span>
                              {/if}
                              {#if isModified}
                                <button
                                  type="button"
                                  class="pd-edit-reset"
                                  title="Reset to default: {row.default}"
                                  disabled={editBusy}
                                  on:click={() => (editDraft[i].value = row.default)}
                                >↺</button>
                              {/if}
                            </div>
                          </div>
                        {/each}
                        {#if editDraft.length === 0}
                          <span class="pd-edit-empty">No configurable parameters.</span>
                        {/if}
                      </div>
                      {#if editError}
                        <p class="pd-edit-error">{editError}</p>
                      {/if}
                      <div class="pd-edit-actions">
                        <button
                          type="button"
                          class="primary"
                          on:click={saveEdit}
                          disabled={editBusy || !wsConnected}
                        >{editBusy ? "Saving…" : "Save"}</button>
                        <button type="button" class="ghost" on:click={closeEdit} disabled={editBusy}>
                          Cancel
                        </button>
                      </div>
                    </div>
                  {/if}

                  <!-- Card actions -->
                  {#if serverMode !== "RUNTIME_ONLY"}
                    <div class="pd-card-actions">
                      <button
                        type="button"
                        class="pd-action-btn"
                        disabled={!wsConnected}
                        on:click={() => selectedPlugin === plugin ? closeEdit() : openEdit(plugin)}
                      >
                        {selectedPlugin === plugin ? "Cancel edit" : "Edit parameters"}
                      </button>
                      {#if plugin.meta?.plugin?.id === "htmlgui-ws"}
                        <button
                          type="button"
                          class="pd-action-btn"
                          disabled={!wsConnected}
                          on:click={() => openScreenEditor(plugin)}
                        >
                          Edit screens
                        </button>
                      {/if}
                    </div>
                  {/if}
                  {#if plugin.meta?.previewCapable}
                    <div class="pd-card-actions">
                      <button
                        type="button"
                        class="pd-action-btn"
                        disabled={!wsConnected}
                        on:click={() => onOpenPreview(plugin)}
                      >
                        Preview
                      </button>
                    </div>
                  {/if}
                </div>
              {/each}
            </div><!-- pd-grid -->
            </div><!-- pd-grid-scroll -->
          {/if}
        </main>
      </div>
    </div>
  </div>
{/if}

{#if screenEditorPlugin !== null}
  <ScreenEditor
    {projectId}
    plugin={screenEditorPlugin}
    {apiGet}
    {apiPut}
    onClose={closeScreenEditor}
  />
{/if}

{#if updateDialogPlugin !== null}
  {@const udClassName = updateDialogPlugin.meta?.plugin?.className || updateDialogPlugin.className || ""}
  {@const udCheck = updateChecks[udClassName] || {}}
  <!-- svelte-ignore a11y-click-events-have-key-events -->
  <!-- svelte-ignore a11y-no-static-element-interactions -->
  <div
    class="modal-backdrop update-dialog-backdrop"
    role="presentation"
    on:click={(e) => { if (e.target === e.currentTarget && !updateApplying) updateDialogPlugin = null; }}
  >
    <div class="modal update-dialog" role="dialog" aria-modal="true" aria-labelledby="update-dialog-title">
      <h4 id="update-dialog-title" class="update-dialog-title">
        Plugin update — {displayName(updateDialogPlugin)}
      </h4>
      <p class="update-dialog-sub">
        The following SceneFlow variables are declared by the plugin spec but not yet in your project.
        Click <strong>Add variables</strong> to create them at the root SceneFlow level.
      </p>
      <ul class="update-new-vars">
        {#each udCheck.newVars || [] as v}
          <li>
            <code class="update-var-name">{v.name}</code>
            <span class="pd-type-badge">{v.type}</span>
          </li>
        {/each}
      </ul>
      {#if updateApplyError}
        <p class="pd-edit-error">{updateApplyError}</p>
      {/if}
      <div class="update-dialog-actions">
        <button
          type="button"
          class="primary"
          disabled={updateApplying || !wsConnected}
          on:click={applyUpdate}
        >{updateApplying ? "Applying…" : "Add variables"}</button>
        <button
          type="button"
          class="ghost"
          disabled={updateApplying}
          on:click={() => updateDialogPlugin = null}
        >Cancel</button>
      </div>
    </div>
  </div>
{/if}

<style>
  /* ── Header ────────────────────────────────────────────────────────────── */
  .pd-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.2rem 0.4rem 0.5rem;
    border-bottom: 1px solid var(--stroke);
    flex-shrink: 0;
  }

  .pd-title {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .pd-title h3 {
    margin: 0;
    font-size: 1.15rem;
    font-weight: 600;
    color: var(--ink);
  }

  .pd-title-icon {
    display: flex;
    align-items: center;
    color: var(--accent);
  }

  .pd-title-icon :global(.icon) {
    width: 1.2rem;
    height: 1.2rem;
  }

  .pd-header-actions {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .pd-view-toggle {
    display: flex;
    border: 1px solid var(--stroke);
    border-radius: 4px;
    overflow: hidden;
  }

  .pd-view-btn {
    background: none;
    border: none;
    padding: 0.2rem 0.6rem;
    font-size: 0.95rem;
    cursor: pointer;
    color: #6b7280;
  }

  .pd-view-btn.active {
    background: var(--accent);
    color: #fff;
  }

  .pd-close {
    font-size: 1.35rem;
    line-height: 1;
    padding: 0.1rem 0.4rem;
  }

  /* ── Body layout ───────────────────────────────────────────────────────── */
  .pd-body {
    display: flex;
    flex: 1;
    min-height: 0;
    gap: 0;
  }

  .pd-sidebar {
    width: 164px;
    flex-shrink: 0;
    border-right: 1px solid var(--stroke);
    padding: 0.6rem;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 0.8rem;
    background: var(--panel-soft);
  }

  .pd-sidebar-section {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .pd-sidebar-heading {
    font-size: 1rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: #6b7280;
    margin-bottom: 0.05rem;
  }

  .pd-filter-row {
    display: flex;
    align-items: center;
    gap: 0.35rem;
    font-size: 0.97rem;
    cursor: pointer;
    color: var(--ink);
  }

  /* Reset global input styles that bleed onto checkbox/radio inside flex rows */
  .pd-filter-row input {
    width: auto;
    padding: 0;
    flex-shrink: 0;
    border: none;
    background: none;
    box-shadow: none;
  }

  .pd-check-all-btn {
    font-size: 1.05rem;
    padding: 0.2rem 0;
    background: none;
    border: none;
    color: var(--accent);
    cursor: pointer;
    text-align: left;
    width: 100%;
  }
  .pd-check-all-btn:hover:not(:disabled) { text-decoration: underline; }
  .pd-check-all-btn:disabled { color: #9ca3af; cursor: default; }

  /* Sidebar dot indicators */
  .pd-filter-dot {
    display: inline-block;
    width: 8px;
    height: 8px;
    border-radius: 50%;
    flex-shrink: 0;
  }
  .pd-dot-input       { background: #3b82f6; }
  .pd-dot-processing  { background: #f59e0b; }
  .pd-dot-output      { background: #10b981; }
  .pd-dot-service     { background: #8b5cf6; }
  .pd-dot-self        { background: #9ca3af; }

  .pd-filter-label {
    font-size: 0.97rem;
    color: var(--ink);
  }

  .pd-sidebar-stats {
    font-size: 0.87rem;
    color: #6b7280;
    padding: 0.1rem 0;
  }

  .pd-sidebar-bottom {
    margin-top: auto;
  }

  /* ── Main content ──────────────────────────────────────────────────────── */
  .pd-main {
    flex: 1;
    min-width: 0;
    min-height: 0;
    display: flex;
    flex-direction: column;
  }

  /* Scroll container for the grid view only — keeps pd-main overflow:visible
     so pd-flow-wrap can reliably fill the height for vertical centring. */
  .pd-grid-scroll {
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    padding: 0.7rem;
  }

  .pd-status {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100%;
    min-height: 120px;
    color: #6b7280;
    font-size: 1.05rem;
  }

  /* ── Grid ──────────────────────────────────────────────────────────────── */
  .pd-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
    gap: 0.7rem;
    align-content: start;
  }

  /* ── Card ──────────────────────────────────────────────────────────────── */
  .pd-card {
    border: 1px solid var(--stroke);
    border-radius: 8px;
    padding: 0.65rem;
    background: var(--panel);
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
    transition: border-color 0.15s, box-shadow 0.15s;
    box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  }

  .pd-card.pd-card-editing {
    border-color: var(--accent);
    box-shadow: 0 0 0 2px var(--accent-soft);
  }

  .pd-card-header {
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
  }

  .pd-card-name {
    display: flex;
    align-items: flex-start;
    gap: 0.4rem;
  }

  .pd-card-name-stack {
    display: flex;
    flex-direction: column;
    min-width: 0;
  }

  .pd-card-title {
    font-weight: 600;
    font-size: 1.03rem;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    color: var(--ink);
  }

  .pd-card-subtitle {
    font-size: 0.87rem;
    color: #6b7280;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .pd-card-badges {
    display: flex;
    flex-wrap: wrap;
    gap: 0.25rem;
  }

  .pd-card-desc {
    font-size: 0.93rem;
    color: #4b5563;
    margin: 0;
    line-height: 1.35;
  }

  /* ── Type badges (light theme, color-mixed) ────────────────────────────── */
  .pd-type-badge {
    font-size: 0.8rem;
    font-weight: 700;
    padding: 0.1rem 0.4rem;
    border-radius: 3px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .pd-type-input {
    background: #dbeafe;
    color: #1e40af;
    border: 1px solid #bfdbfe;
  }
  .pd-type-processing {
    background: #fef3c7;
    color: #92400e;
    border: 1px solid #fde68a;
  }
  .pd-type-output {
    background: #d1fae5;
    color: #065f46;
    border: 1px solid #a7f3d0;
  }
  .pd-type-other {
    background: var(--panel-soft);
    color: #4b5563;
    border: 1px solid var(--stroke);
  }

  /* ── Service model badges ───────────────────────────────────────────────── */
  .pd-service-badge {
    font-size: 0.8rem;
    font-weight: 600;
    padding: 0.1rem 0.4rem;
    border-radius: 3px;
  }

  .pd-service-badge.service {
    background: #ede9fe;
    color: #5b21b6;
    border: 1px solid #ddd6fe;
  }
  .pd-service-badge.self-contained {
    background: var(--panel-soft);
    color: #4b5563;
    border: 1px solid var(--stroke);
  }

  /* ── Health dot ─────────────────────────────────────────────────────────── */
  .pd-health-dot {
    display: inline-block;
    width: 8px;
    height: 8px;
    border-radius: 50%;
    flex-shrink: 0;
    margin-top: 3px;
  }

  @keyframes pd-pulse {
    0%, 100% { opacity: 1; }
    50%       { opacity: 0.35; }
  }

  /* Clickable health dot — must come BEFORE color rules so colors win source-order tie */
  .pd-health-btn {
    border: none;
    padding: 0;
    cursor: pointer;
    flex-shrink: 0;
  }
  .pd-health-btn:disabled { cursor: default; }

  /* Color rules come last so they override the button baseline above */
  .pd-health-ok        { background: #16a34a; }
  .pd-health-loaded    { background: #3b82f6; }
  .pd-health-error     { background: #dc2626; }
  .pd-health-not_loaded { background: #9ca3af; }
  .pd-health-checking  { background: #f59e0b; animation: pd-pulse 0.8s infinite; }
  .pd-health-unknown   { background: #d1d5db; border: 1px solid #9ca3af; }

  /* ── Variable chips ─────────────────────────────────────────────────────── */
  .pd-var-chips {
    display: flex;
    flex-wrap: wrap;
    gap: 0.2rem;
  }

  .pd-var-chip {
    font-size: 0.83rem;
    padding: 0.05rem 0.35rem;
    border-radius: 3px;
    font-family: monospace;
  }

  .pd-var-chip.write {
    background: #d1fae5;
    color: #065f46;
    border: 1px solid #a7f3d0;
  }
  .pd-var-chip.read {
    background: #dbeafe;
    color: #1e40af;
    border: 1px solid #bfdbfe;
  }

  /* Chips more button */
  .pd-chips-more {
    font-size: 0.83rem;
    padding: 0.05rem 0.35rem;
    background: none;
    border: 1px dashed var(--stroke);
    border-radius: 3px;
    color: var(--accent);
    cursor: pointer;
  }
  .pd-chips-more:hover { background: var(--accent-soft); }

  /* ── Commands ───────────────────────────────────────────────────────────── */
  .pd-commands {
    margin-top: 0.2rem;
  }

  .pd-commands-summary {
    font-size: 1.05rem;
    color: #6b7280;
    cursor: pointer;
    list-style: none;
    display: flex;
    align-items: center;
    gap: 0.3rem;
    padding: 0.15rem 0;
    user-select: none;
  }
  .pd-commands-summary::-webkit-details-marker { display: none; }
  .pd-commands-label { font-weight: 600; }
  .pd-commands-summary::before {
    content: "▶";
    font-size: 0.7rem;
    color: #9ca3af;
    transition: transform 0.15s;
  }
  details[open] .pd-commands-summary::before { transform: rotate(90deg); }

  .pd-commands-count {
    background: var(--panel-soft);
    border: 1px solid var(--stroke);
    border-radius: 9px;
    font-size: 0.8rem;
    font-weight: 700;
    padding: 0.05rem 0.38rem;
    color: #6b7280;
  }

  .pd-commands-list {
    list-style: none;
    margin: 0.3rem 0 0;
    padding: 0;
    display: flex;
    flex-direction: column;
    gap: 0.18rem;
    max-height: 160px;
    overflow-y: auto;
  }

  .pd-command-row {
    display: flex;
    align-items: baseline;
    gap: 0.4rem;
  }

  .pd-command-sig {
    font-size: 0.87rem;
    font-family: monospace;
    color: var(--ink);
    background: var(--panel-soft);
    border: 1px solid var(--stroke);
    border-radius: 3px;
    padding: 0.05rem 0.3rem;
    white-space: nowrap;
    flex-shrink: 0;
  }

  .pd-command-summary {
    font-size: 1rem;
    color: #6b7280;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  /* ── Param edit panel ───────────────────────────────────────────────────── */
  .pd-edit-panel {
    background: var(--panel-soft);
    border: 1px solid var(--stroke);
    border-radius: 6px;
    padding: 0.5rem;
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
  }

  .pd-edit-fields {
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
    max-height: 200px;
    overflow-y: auto;
  }

  .pd-edit-row {
    display: flex;
    align-items: flex-start;
    gap: 0.4rem;
  }

  .pd-edit-label-group {
    display: flex;
    align-items: center;
    gap: 0.2rem;
    min-width: 100px;
    max-width: 100px;
    flex-shrink: 0;
    padding-top: 0.22rem;
  }

  .pd-edit-key {
    font-size: 0.89rem;
    font-family: monospace;
    color: #374151;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    min-width: 0;
  }

  .pd-edit-badge {
    font-size: 0.75rem;
    font-weight: 700;
    padding: 0.05rem 0.28rem;
    border-radius: 2px;
    flex-shrink: 0;
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }

  .pd-edit-badge.required {
    background: #fee2e2;
    color: #991b1b;
    border: 1px solid #fca5a5;
  }

  .pd-edit-badge.optional {
    background: var(--panel-soft);
    color: #6b7280;
    border: 1px solid var(--stroke);
  }

  .pd-edit-input-wrap {
    flex: 1;
    display: flex;
    align-items: center;
    gap: 0.25rem;
    min-width: 0;
  }

  .pd-edit-value {
    flex: 1;
    font-size: 0.93rem;
    padding: 0.2rem 0.4rem;
    background: var(--panel);
    border: 1px solid var(--stroke);
    border-radius: 4px;
    color: var(--ink);
    min-width: 0;
  }

  .pd-edit-value:focus {
    outline: 2px solid var(--accent);
    outline-offset: 1px;
  }

  .pd-edit-value.modified {
    border-color: var(--accent);
    background: var(--accent-soft);
  }

  .pd-edit-toggle {
    display: flex;
    align-items: center;
    gap: 0.3rem;
    cursor: pointer;
    font-size: 0.93rem;
    color: var(--ink);
  }

  .pd-edit-toggle-label {
    font-size: 0.93rem;
    color: #4b5563;
  }

  .pd-edit-desc {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: var(--panel-soft);
    border: 1px solid var(--stroke);
    font-size: 0.8rem;
    color: #6b7280;
    cursor: help;
    flex-shrink: 0;
  }

  .pd-edit-reset {
    background: none;
    border: none;
    font-size: 1rem;
    cursor: pointer;
    color: #9ca3af;
    padding: 0 0.1rem;
    flex-shrink: 0;
    line-height: 1;
  }

  .pd-edit-reset:hover {
    color: var(--accent);
  }

  .pd-edit-empty {
    font-size: 0.93rem;
    color: #6b7280;
  }

  .pd-edit-error {
    font-size: 0.93rem;
    margin: 0;
    color: var(--danger);
  }

  .pd-edit-actions {
    display: flex;
    gap: 0.4rem;
    justify-content: flex-end;
    border-top: 1px solid var(--stroke);
    padding-top: 0.4rem;
    margin-top: 0.1rem;
  }

  .pd-edit-actions button {
    font-size: 0.93rem;
    padding: 0.2rem 0.6rem;
  }

  /* ── Card actions ───────────────────────────────────────────────────────── */
  .pd-card-actions {
    display: flex;
    justify-content: flex-end;
    margin-top: 0.15rem;
  }

  .pd-action-btn {
    font-size: 1.05rem;
    padding: 0.18rem 0.5rem;
    background: none;
    border: 1px solid var(--stroke);
    border-radius: 4px;
    color: var(--ink);
    cursor: pointer;
  }
  .pd-action-btn:hover:not(:disabled) { background: var(--panel-soft); }
  .pd-action-btn:disabled { color: #9ca3af; cursor: default; }

  /* ── Update badge ───────────────────────────────────────────────────────── */
  .pd-update-badge {
    font-size: 0.72rem;
    font-weight: 600;
    padding: 0.1rem 0.45rem;
    background: #f59e0b;
    color: #1a1400;
    border: none;
    border-radius: 10px;
    cursor: pointer;
    white-space: nowrap;
    line-height: 1.5;
  }
  .pd-update-badge:hover { background: #d97706; }

  /* ── Update dialog ──────────────────────────────────────────────────────── */
  .update-dialog-backdrop {
    z-index: 1100;
  }

  .update-dialog {
    max-width: 440px;
    width: 95%;
    padding: 1.25rem 1.5rem 1rem;
  }

  .update-dialog-title {
    margin: 0 0 0.5rem;
    font-size: 1rem;
    font-weight: 600;
    color: var(--ink);
  }

  .update-dialog-sub {
    margin: 0 0 0.75rem;
    font-size: 0.88rem;
    color: var(--ink-muted, #6b7280);
    line-height: 1.45;
  }

  .update-new-vars {
    list-style: none;
    margin: 0 0 0.75rem;
    padding: 0;
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
  }

  .update-new-vars li {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.88rem;
  }

  .update-var-name {
    font-family: monospace;
    font-size: 0.88rem;
    background: var(--panel-soft, #f3f4f6);
    padding: 0.1rem 0.35rem;
    border-radius: 3px;
    color: var(--ink);
  }

  .update-dialog-actions {
    display: flex;
    gap: 0.5rem;
    justify-content: flex-end;
    margin-top: 0.5rem;
  }

  /* ── Flow view ──────────────────────────────────────────────────────────── */
  .pd-flow-wrap {
    /* Fill the available height so the SVG can be vertically centred */
    flex: 1;
    min-height: 0;
    display: flex;
    align-items: center;       /* vertical centre */
    justify-content: flex-start;
    overflow-x: auto;
    overflow-y: auto;
    padding: 0.5rem 0.75rem;
  }

  .pd-flow-svg {
    display: block;
  }

  .pd-flow-col-label {
    font-size: 13px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.07em;
  }

  .pd-flow-node {
    cursor: pointer;
    transition: opacity 0.12s;
  }

  .pd-flow-node-dimmed {
    opacity: 0.25;
  }

  .pd-flow-node-name {
    font-size: 14px;
    font-weight: 600;
    fill: var(--ink);
  }

  .pd-flow-node-types {
    font-size: 12px;
    fill: #6b7280;
  }

  .pd-flow-edge-label {
    font-size: 12px;
    fill: var(--accent);
    font-family: monospace;
    pointer-events: none;
  }

  .pd-flow-svc-label {
    font-size: 12px;
    font-weight: 600;
    fill: #8b5cf6;
    font-family: monospace;
    pointer-events: none;
  }

  .pd-flow-hint {
    font-size: 0.93rem;
    color: #6b7280;
    margin: 0 0 0.5rem;
    font-style: italic;
  }

  .pd-director-node {
    cursor: pointer;
  }

  .pd-director-label {
    font-size: 14px;
    font-weight: 700;
    fill: var(--accent);
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }

  .pd-flow-arc-label {
    font-size: 12px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    pointer-events: none;
  }

  /* SVG health dot fills */
  :global(circle.pd-health-ok)        { fill: #16a34a; }
  :global(circle.pd-health-loaded)    { fill: #3b82f6; }
  :global(circle.pd-health-error)     { fill: #dc2626; }
  :global(circle.pd-health-not_loaded){ fill: #9ca3af; }
  :global(circle.pd-health-checking)  { fill: #f59e0b; animation: pd-pulse 0.8s infinite; }
  :global(circle.pd-health-unknown)   { fill: #d1d5db; }
</style>
