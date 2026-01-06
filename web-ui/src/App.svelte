<script>
  import { tick, onMount } from "svelte";
  import SceneFlowMiniMap from "./SceneFlowMiniMap.svelte";
  import SceneFlowView from "./SceneFlowView.svelte";
  import ScriptEditor from "./ScriptEditor.svelte";
  import IconChevronDown from "./icons/IconChevronDown.svelte";
  import IconChevronUp from "./icons/IconChevronUp.svelte";
  import IconPencil from "./icons/IconPencil.svelte";
  import IconPlus from "./icons/IconPlus.svelte";
  import IconPause from "./icons/IconPause.svelte";
  import IconStart from "./icons/IconStart.svelte";
  import IconStop from "./icons/IconStop.svelte";
  import IconTrash from "./icons/IconTrash.svelte";

  const clientId = (() => {
    const existing = localStorage.getItem("vsm_client_id");
    if (existing) return existing;
    const generated =
      (window.crypto && window.crypto.randomUUID && window.crypto.randomUUID()) ||
      `client-${Date.now()}`;
    localStorage.setItem("vsm_client_id", generated);
    return generated;
  })();

  let token = localStorage.getItem("vsm_token") || "";
  let info = null;
  let error = "";
  let statusMessage = "";
  let sessionReady = false;

  const SCENE_DRAG_TYPE = "application/x-vsm-scene";
  const BLOCK_DRAG_TYPE = "application/x-vsm-block";
  const SCENE_LANGUAGE_ALL = "__all__";
  const SCENEFLOW_ROOT_ID = "__root__";
  const SCENEFLOW_ZOOM_KEY = "vsm_scene_flow_zoom";
  const SCENEFLOW_ZOOM_MIN = 0.3;
  const SCENEFLOW_ZOOM_MAX = 3.5;
  const DEFAULT_VAR_BADGE_STATE = {
    visible: true,
    global: { x: 16, y: 12, w: 240, h: 150 },
    local: { x: 16, y: 190, w: 240, h: 150 }
  };
  const VAR_BADGE_COOKIE = "vsm_var_badges";
  const VAR_BADGE_MIN_WIDTH = 180;
  const VAR_BADGE_MIN_HEIGHT = 90;
  const VAR_BADGE_HANDLE_SIZE = 18;
  const VAR_BADGE_HANDLE_PATH = buildVarBadgeHandlePath(VAR_BADGE_HANDLE_SIZE);
  const RUNTIME_STATE_LABELS = {
    running: "Running",
    paused: "Paused",
    stopped: "Stopped"
  };
  const EDGE_ACTIVITY_MS = 650;

  function clampSceneFlowZoom(value) {
    return Math.min(SCENEFLOW_ZOOM_MAX, Math.max(SCENEFLOW_ZOOM_MIN, value));
  }

  function buildVarBadgeHandlePath(handleSize) {
    const outerRadius = Math.max(4, handleSize - 0.5);
    const thickness = Math.max(2, Math.min(3, outerRadius * 0.22));
    const innerRadius = outerRadius - thickness * 3;
    const outerStartX = handleSize;
    const outerStartY = handleSize - outerRadius;
    const outerEndX = handleSize - outerRadius;
    const outerEndY = handleSize;
    const innerStartX = handleSize - innerRadius;
    const innerStartY = handleSize;
    const innerEndX = handleSize;
    const innerEndY = handleSize - innerRadius;
    return `M ${outerStartX} ${outerStartY} A ${outerRadius} ${outerRadius} 0 0 1 ${outerEndX} ${outerEndY} L ${innerStartX} ${innerStartY} A ${innerRadius} ${innerRadius} 0 0 0 ${innerEndX} ${innerEndY} Z`;
  }

  function readCookie(name) {
    if (typeof document === "undefined") return "";
    const parts = document.cookie.split(";").map((part) => part.trim());
    for (const part of parts) {
      if (!part.startsWith(`${name}=`)) continue;
      return part.slice(name.length + 1);
    }
    return "";
  }

  function writeCookie(name, value, maxAgeSeconds = 31536000) {
    if (typeof document === "undefined") return;
    document.cookie = `${name}=${value}; path=/; max-age=${maxAgeSeconds}; samesite=lax`;
  }

  function cloneBadgeState(state) {
    return JSON.parse(JSON.stringify(state));
  }

  function normalizeBadgeRect(rect, fallback) {
    const x = Number.isFinite(rect?.x) ? rect.x : fallback.x;
    const y = Number.isFinite(rect?.y) ? rect.y : fallback.y;
    const w = Number.isFinite(rect?.w) ? rect.w : fallback.w;
    const h = Number.isFinite(rect?.h) ? rect.h : fallback.h;
    return {
      x,
      y,
      w: Math.max(VAR_BADGE_MIN_WIDTH, w),
      h: Math.max(VAR_BADGE_MIN_HEIGHT, h)
    };
  }

  function normalizeVarBadgeState(state) {
    const fallback = cloneBadgeState(DEFAULT_VAR_BADGE_STATE);
    return {
      visible: state?.visible !== undefined ? !!state.visible : fallback.visible,
      global: normalizeBadgeRect(state?.global, fallback.global),
      local: normalizeBadgeRect(state?.local, fallback.local)
    };
  }

  function loadVarBadgeState() {
    const raw = readCookie(VAR_BADGE_COOKIE);
    if (!raw) {
      return cloneBadgeState(DEFAULT_VAR_BADGE_STATE);
    }
    try {
      const parsed = JSON.parse(decodeURIComponent(raw));
      return normalizeVarBadgeState(parsed);
    } catch (err) {
      return cloneBadgeState(DEFAULT_VAR_BADGE_STATE);
    }
  }

  function persistVarBadgeState(state) {
    const payload = encodeURIComponent(JSON.stringify(state));
    writeCookie(VAR_BADGE_COOKIE, payload);
  }

  function clampBadgeRect(rect, bounds) {
    if (!bounds) return rect;
    const width = Math.min(Math.max(VAR_BADGE_MIN_WIDTH, rect.w), Math.max(VAR_BADGE_MIN_WIDTH, bounds.width));
    const height = Math.min(Math.max(VAR_BADGE_MIN_HEIGHT, rect.h), Math.max(VAR_BADGE_MIN_HEIGHT, bounds.height));
    const maxX = Math.max(0, bounds.width - width);
    const maxY = Math.max(0, bounds.height - height);
    const x = Math.min(Math.max(0, rect.x), maxX);
    const y = Math.min(Math.max(0, rect.y), maxY);
    return { x, y, w: width, h: height };
  }

  function updateVarBadgeRect(key, rect, commit) {
    if (!key || !varBadgeState[key]) return;
    const next = {
      ...varBadgeState,
      [key]: {
        ...varBadgeState[key],
        ...rect
      }
    };
    varBadgeState = next;
    if (commit) {
      persistVarBadgeState(next);
    }
  }

  function varBadgeStyle(key) {
    const rect = varBadgeState[key];
    if (!rect) return "";
    return `left:${rect.x}px; top:${rect.y}px; width:${rect.w}px; height:${rect.h}px;`;
  }

  function toggleVarBadges() {
    const next = { ...varBadgeState, visible: !varBadgeState.visible };
    varBadgeState = next;
    persistVarBadgeState(next);
  }

  function startVarBadgeMove(event, key) {
    if (!isPrimaryPointer(event) || !sceneFlowContainerEl) return;
    const badge = varBadgeState[key];
    if (!badge) return;
    event.preventDefault();
    event.stopPropagation();
    varBadgeDrag = {
      key,
      mode: "move",
      lastClientX: event.clientX,
      lastClientY: event.clientY
    };
  }

  function handleVarBadgePointerDown(event, key) {
    if (!event) return;
    if (varBadgeDrag) return;
    const target = event.target;
    if (target?.closest?.(".sceneflow-var-content")) return;
    if (target?.closest?.(".var-resize-handle")) return;
    startVarBadgeMove(event, key);
  }

  function isPrimaryPointer(event) {
    if (!event) return false;
    if (event.isPrimary === false) return false;
    if (event.button === undefined) return true;
    return event.button === 0;
  }

  function startVarBadgeResize(event, key) {
    if (!isPrimaryPointer(event) || !sceneFlowContainerEl) return;
    const badge = varBadgeState[key];
    if (!badge) return;
    event.preventDefault();
    event.stopPropagation();
    varBadgeDrag = {
      key,
      mode: "resize",
      lastClientX: event.clientX,
      lastClientY: event.clientY
    };
  }

  function handleVarBadgePointerMove(event) {
    if (!varBadgeDrag || !sceneFlowContainerEl) return;
    event.preventDefault();
    const key = varBadgeDrag.key;
    const current = varBadgeState[key];
    if (!current || !Number.isFinite(event.clientX) || !Number.isFinite(event.clientY)) {
      return;
    }
    const dx = event.clientX - varBadgeDrag.lastClientX;
    const dy = event.clientY - varBadgeDrag.lastClientY;
    varBadgeDrag.lastClientX = event.clientX;
    varBadgeDrag.lastClientY = event.clientY;
    const bounds = sceneFlowContainerEl.getBoundingClientRect();
    if (varBadgeDrag.mode === "move") {
      const next = clampBadgeRect(
        {
          x: current.x + dx,
          y: current.y + dy,
          w: current.w,
          h: current.h
        },
        bounds
      );
      updateVarBadgeRect(key, next, false);
    } else if (varBadgeDrag.mode === "resize") {
      const next = clampBadgeRect(
        {
          x: current.x,
          y: current.y,
          w: current.w + dx,
          h: current.h + dy
        },
        bounds
      );
      updateVarBadgeRect(key, next, false);
    }
  }

  function handleVarBadgePointerUp(event) {
    if (!varBadgeDrag) return;
    varBadgeDrag = null;
    persistVarBadgeState(varBadgeState);
  }

  function badgeKeyFromTarget(target) {
    const badgeEl = target?.closest?.(".sceneflow-var-badge");
    if (!badgeEl) return null;
    const key = badgeEl.dataset?.badge;
    if (key === "global" || key === "local") {
      return key;
    }
    return null;
  }

  function handleVarBadgeDocumentDown(event) {
    if (!varBadgeState?.visible) return;
    const key = badgeKeyFromTarget(event?.target);
    if (!key) return;
    const isHandle = event.target?.closest?.(".var-resize-handle");
    const isContent = event.target?.closest?.(".sceneflow-var-content");
    if (isHandle) {
      startVarBadgeResize(event, key);
    } else if (!isContent) {
      startVarBadgeMove(event, key);
    }
  }

  onMount(() => {
    const downHandler = (event) => handleVarBadgeDocumentDown(event);
    const moveHandler = (event) => handleVarBadgePointerMove(event);
    const upHandler = (event) => handleVarBadgePointerUp(event);
    document.addEventListener("mousedown", downHandler, true);
    document.addEventListener("mousemove", moveHandler, true);
    document.addEventListener("mouseup", upHandler, true);
    document.addEventListener("pointermove", moveHandler, true);
    document.addEventListener("pointerup", upHandler, true);
    document.addEventListener("pointercancel", upHandler, true);
    return () => {
      document.removeEventListener("mousedown", downHandler, true);
      document.removeEventListener("mousemove", moveHandler, true);
      document.removeEventListener("mouseup", upHandler, true);
      document.removeEventListener("pointermove", moveHandler, true);
      document.removeEventListener("pointerup", upHandler, true);
      document.removeEventListener("pointercancel", upHandler, true);
    };
  });

  function readSceneFlowZoom() {
    const raw = localStorage.getItem(SCENEFLOW_ZOOM_KEY);
    const parsed = raw ? Number(raw) : NaN;
    if (!Number.isFinite(parsed)) return 1;
    return clampSceneFlowZoom(parsed);
  }

  let projects = [];
  let selectedProjectId = localStorage.getItem("vsm_project_id") || "";
  let recent = [];
  let samples = [];
  let tutorials = [];

  let openPath = "";
  let newName = "";
  let newBaseDir = "";
  let saveAsPath = "";

  let preferences = {};
  let prefDraft = {};
  let prefFilter = "";

  let config = {};
  let configDraft = {};
  let configFilter = "";
  let configSaved = null;
  let lastConfigProjectId = "";

  let scriptText = "";
  let scriptDraft = "";
  let scriptVersion = null;
  let scriptStatus = "";
  let scriptError = "";
  let scriptParseOk = true;
  let scriptDiagnostics = [];
  let scriptDiagTimer = null;
  let scriptDiagRequestId = 0;
  let lastScriptProjectId = "";
  let scriptEditorRef;
  let scriptScenes = [];
  let scriptScenesFilter = "";
  let scriptScenesLanguage = SCENE_LANGUAGE_ALL;
  let scriptScenesError = "";
  let scriptScenesLoading = false;
  let scriptElements = { acticon: [], gesticon: [], visicon: [] };
  let scriptElementsFilter = "";
  let scriptElementsError = "";
  let scriptElementsLoading = false;
  const SELECTION_PREVIEW_LIMIT = 6;

  let sceneFlow = null;
  let sceneFlowError = "";
  let sceneFlowLoading = false;
  let lastSceneFlowProjectId = "";
  let sceneFlowRef;
  let sceneFlowZoom = readSceneFlowZoom();
  let sceneFlowWorldBox = null;
  let sceneFlowViewBox = null;
  let sceneFlowSelection = null;
  let sceneFlowMultiSelection = [];
  let sceneFlowClipboard = null;
  let sceneFlowPasteIndex = 0;
  let sceneFlowDuplicateIndex = 0;
  let sceneFlowDuplicateKey = "";
  let sceneFlowFrameColor = "#7d7d7d";
  let sceneFlowFrameStyle = "";
  let sceneFlowSnap = true;
  let sceneFlowShowCmdText = true;
  let sceneFlowBusy = false;
  let runtimeInfo = null;
  let runtimeError = "";
  let runtimeLoading = false;
  let lastRuntimeProjectId = "";
  let runtimeValues = {};
  let runtimeInitialValues = {};
  let runtimeInitialProjectId = "";
  let runtimeInitialState = "stopped";
  let activityNodeCounts = new Map();
  let activityEdgeHits = new Map();
  let activityNodeIds = [];
  let activityEdgeList = [];
  let varBadgeState = loadVarBadgeState();
  let varBadgeDrag = null;
  let sceneFlowContainerEl;
  let edgeCreateMode = false;
  let edgeCreateSourceId = "";
  let edgeCreateType = "EEDGE";
  let selectedNode = null;
  let selectedEdge = null;
  let selectedComment = null;
  let nodeDraft = null;
  let edgeDraft = null;
  let nodeDraftId = "";
  let edgeDraftId = "";
  let nodeDirty = false;
  let edgeDirty = false;
  let superNodeDirty = false;
  let superNodeStartLocked = false;
  let nodeEditError = "";
  let edgeEditError = "";
  let pEdgeDrafts = [];
  let pEdgeGroup = [];
  let pEdgeDraftKey = "";
  let pEdgeSourceId = "";
  let pEdgeError = "";
  let pEdgeGroupKey = "";
  let pEdgeSum = 0;
  let pEdgeValid = true;
  let pEdgeDirty = false;
  let superNodeDraft = null;
  let superNodeDraftId = "";
  let superNodeEditError = "";
  let typeDefDraft = null;
  let typeDefEditIndex = null;
  let typeDefError = "";
  let typeDefSelectedIndex = null;
  let varDefDraft = null;
  let varDefEditIndex = null;
  let varDefError = "";
  let varDefSelectedIndex = null;
  let cmdDraft = "";
  let cmdEditIndex = null;
  let cmdError = "";
  let cmdSelectedIndex = null;
  let cmdDialogOpen = false;
  let cmdInlineDrafts = [];
  let cmdDialogNodeId = "";
  let lastNodeDefsId = "";

  const edgeTypeLabels = {
    EEDGE: "Epsilon",
    CEDGE: "Conditional",
    IEDGE: "Interruptive",
    PEDGE: "Probability",
    TEDGE: "Timeout",
    FEDGE: "Fork"
  };
  const SCENEFLOW_FRAME_COLORS = {
    node: "#7d7d7d",
    edges: {
      eedge: "#827d78",
      fedge: "#234d67",
      tedge: "#543f1d",
      cedge: "#988e34",
      pedge: "#2a6723",
      iedge: "#983434"
    }
  };

  let ws = null;
  let wsConnected = false;
  let wsError = "";
  let pending = new Map();

  $: selectedProject = projects.find((p) => p.projectId === selectedProjectId) || null;
  $: filteredPrefs = filterKeyValues(prefDraft, prefFilter);
  $: filteredConfig = filterKeyValues(configDraft, configFilter);
  $: scriptDirty = scriptDraft !== scriptText;
  $: selectedNode = sceneFlowSelection?.type === "node" ? sceneFlow?.nodes?.find((node) => node.id === sceneFlowSelection.id) : null;
  $: selectedEdge = sceneFlowSelection?.type === "edge" ? sceneFlow?.edges?.find((edge) => edge.id === sceneFlowSelection.id) : null;
  $: selectedComment =
    sceneFlowSelection?.type === "comment"
      ? sceneFlow?.comments?.find((comment) => comment.id === sceneFlowSelection.id)
      : null;
  $: selectedEdgeTarget =
    selectedEdge && sceneFlow?.nodes
      ? sceneFlow.nodes.find((node) => node.id === selectedEdge.targetId)
      : null;
  $: edgeAltStartEnabled = selectedEdgeTarget?.type === "Super";
  $: selectionList =
    Array.isArray(sceneFlowMultiSelection) && sceneFlowMultiSelection.length
      ? sceneFlowMultiSelection
      : sceneFlowSelection
        ? [sceneFlowSelection]
        : [];
  $: multiSelectionActive = selectionList.length > 1;
  $: selectionNodeMap = new Map((sceneFlow?.nodes || []).map((node) => [node.id, node]));
  $: selectionEdgeMap = new Map((sceneFlow?.edges || []).map((edge) => [edge.id, edge]));
  $: selectionCommentMap = new Map((sceneFlow?.comments || []).map((comment) => [comment.id, comment]));
  $: selectionNodes = selectionList
    .filter((item) => item.type === "node")
    .map((item) => selectionNodeMap.get(item.id))
    .filter(Boolean);
  $: selectionEdges = selectionList
    .filter((item) => item.type === "edge")
    .map((item) => selectionEdgeMap.get(item.id))
    .filter(Boolean);
  $: selectionComments = selectionList
    .filter((item) => item.type === "comment")
    .map((item) => selectionCommentMap.get(item.id))
    .filter(Boolean);
  $: selectionNodePreview = selectionNodes.slice(0, SELECTION_PREVIEW_LIMIT);
  $: selectionNodeRemaining = Math.max(0, selectionNodes.length - selectionNodePreview.length);
  $: selectionCommentPreview = selectionComments.slice(0, SELECTION_PREVIEW_LIMIT);
  $: selectionCommentRemaining = Math.max(0, selectionComments.length - selectionCommentPreview.length);
  $: selectionNodeSummary = selectionNodes.length ? nodeTypeSummary(selectionNodes) : "";
  $: selectionEdgeSummary = selectionEdges.length ? edgeTypeSummary(selectionEdges) : "";
  $: selectionStartCount = selectionNodes.filter((node) => node.isStart).length;
  $: nodeEditorTarget =
    sceneFlowSelection?.type === "node"
      ? selectedNode
      : sceneFlowSelection
        ? null
        : sceneFlow?.superNodeData || null;
  $: nodeEditorTargetId = nodeEditorTarget?.id || "";
  $: nodeEditorTypeDefs = Array.isArray(nodeEditorTarget?.typeDefs) ? nodeEditorTarget.typeDefs : [];
  $: nodeEditorVarDefs = Array.isArray(nodeEditorTarget?.varDefs) ? nodeEditorTarget.varDefs : [];
  $: nodeEditorCommands = Array.isArray(nodeEditorTarget?.commands) ? nodeEditorTarget.commands : [];
  $: nodeEditorTypeOptions = Array.isArray(nodeEditorTarget?.typeOptions)
    ? nodeEditorTarget.typeOptions
    : ["Int", "Bool", "Float", "String"];
  $: nodeEditorTypeCatalog = Array.isArray(nodeEditorTarget?.typeCatalog) ? nodeEditorTarget.typeCatalog : [];
  $: currentSuperName =
    sceneFlow?.path?.length ? sceneFlow.path[sceneFlow.path.length - 1] : sceneFlow?.superNodeId || "SceneFlow";
  $: sceneFlowPathNodes = Array.isArray(sceneFlow?.pathNodes) ? sceneFlow.pathNodes : [];
  $: startNodes = sceneFlow?.nodes ? sceneFlow.nodes.filter((node) => node.isStart && !node.isHistory) : [];
  $: sceneFlowFrameColor = superNodeFrameColor(sceneFlow);
  $: sceneFlowFrameStyle = `--sf-frame-color:${sceneFlowFrameColor};`;
  $: activePathNode = sceneFlowPathNodes.length ? sceneFlowPathNodes[sceneFlowPathNodes.length - 1] : null;
  $: isSceneFlowRoot =
    activePathNode?.isRoot === true ||
    sceneFlow?.superNodeData?.isRoot === true ||
    sceneFlowPathNodes.length === 1;
  $: showLocalVarBadge = !!sceneFlow && !isSceneFlowRoot;
  $: runtimeState = selectedProject?.runtimeState || runtimeInfo?.state || "stopped";
  $: runtimeStateLabel = RUNTIME_STATE_LABELS[runtimeState] || runtimeState;
  $: runtimeGlobals = Array.isArray(runtimeInfo?.globalVariables) ? runtimeInfo.globalVariables : [];
  $: runtimeLocals = Array.isArray(runtimeInfo?.localVariables) ? runtimeInfo.localVariables : [];
  $: runtimeRootVars = runtimeGlobals.length ? runtimeGlobals : runtimeLocals;
  $: runtimeDisplayGlobals = isSceneFlowRoot ? runtimeRootVars : runtimeGlobals;
  $: activityNodeIds = Array.from(activityNodeCounts.keys());
  $: activityEdgeList = Array.from(activityEdgeHits.values());
  $: runtimeCanPlay = wsConnected && !!selectedProjectId && (runtimeState === "stopped" || runtimeState === "paused");
  $: runtimeCanPause = wsConnected && !!selectedProjectId && runtimeState === "running";
  $: runtimeCanStop = wsConnected && !!selectedProjectId && runtimeState !== "stopped";
  $: runtimePlayLabel = runtimeState === "paused" ? "Resume" : "Start";
  $: infoRevision = info?.revision || info?.buildRevision || info?.build || info?.version || "unknown";
  $: infoBuildDate = info?.buildDate || info?.buildTime || "unknown";
  $: filteredScriptScenes = filterSceneLanguages(scriptScenes, scriptScenesFilter, scriptScenesLanguage);
  $: sceneLanguageOptions = sceneLanguageOptionList(scriptScenes);
  $: filteredScriptElements = filterScriptElements(scriptElements, scriptElementsFilter);

  $: if (selectedNode && selectedNode.id !== nodeDraftId) {
    nodeDraftId = selectedNode.id;
    nodeDraft = {
      name: selectedNode.name ?? "",
      comment: selectedNode.comment ?? "",
      isStart: !!selectedNode.isStart
    };
    nodeEditError = "";
  } else if (!selectedNode) {
    nodeDraftId = "";
    nodeDraft = null;
    nodeEditError = "";
  }

  $: if (!sceneFlowSelection && nodeEditorTarget) {
    const draftKey = nodeEditorTarget.id || "__root__";
    if (draftKey !== superNodeDraftId) {
      const isRoot = !!nodeEditorTarget.isRoot;
      superNodeDraftId = draftKey;
      superNodeDraft = {
        name: nodeEditorTarget.name ?? "",
        isStart: isRoot ? true : !!nodeEditorTarget.isStart
      };
      superNodeEditError = "";
    }
  } else if (superNodeDraft) {
    superNodeDraftId = "";
    superNodeDraft = null;
    superNodeEditError = "";
  }

  $: if (nodeEditorTargetId && nodeEditorTargetId !== lastNodeDefsId) {
    lastNodeDefsId = nodeEditorTargetId;
    resetTypeDefEditor();
    resetVarDefEditor();
    if (cmdDialogOpen) {
      syncCmdInlineDrafts();
      cmdSelectedIndex = null;
    } else {
      resetCmdEditor();
    }
  } else if (!nodeEditorTargetId && lastNodeDefsId) {
    lastNodeDefsId = "";
    resetTypeDefEditor();
    resetVarDefEditor();
    if (cmdDialogOpen) {
      syncCmdInlineDrafts();
      cmdSelectedIndex = null;
    } else {
      resetCmdEditor();
    }
  }

  $: if (varDefSelectedIndex !== null && varDefSelectedIndex >= nodeEditorVarDefs.length) {
    varDefSelectedIndex = null;
  }

  $: if (typeDefSelectedIndex !== null && typeDefSelectedIndex >= nodeEditorTypeDefs.length) {
    typeDefSelectedIndex = null;
  }

  $: if (cmdSelectedIndex !== null) {
    const maxIndex = cmdDialogOpen ? cmdInlineDrafts.length : nodeEditorCommands.length;
    if (cmdSelectedIndex >= maxIndex) {
      cmdSelectedIndex = null;
    }
  }

  $: if (cmdDialogOpen) {
    const targetId = nodeEditorTarget?.id || "";
    if (targetId !== cmdDialogNodeId) {
      syncCmdInlineDrafts();
    } else if (
      cmdInlineDrafts.length <= nodeEditorCommands.length &&
      cmdInlineDrafts.length !== nodeEditorCommands.length
    ) {
      syncCmdInlineDrafts();
    }
  }

  $: if (selectedEdge && selectedEdge.id !== edgeDraftId) {
    edgeDraftId = selectedEdge.id;
    edgeDraft = {
      condition: selectedEdge.condition ?? "",
      probability: selectedEdge.probability !== undefined ? String(selectedEdge.probability) : "",
      timeoutMs: selectedEdge.timeoutMs !== undefined ? String(selectedEdge.timeoutMs) : "",
      altStartText: formatAltStartMap(selectedEdge)
    };
    edgeEditError = "";
  } else if (!selectedEdge) {
    edgeDraftId = "";
    edgeDraft = null;
    edgeEditError = "";
  }

  $: nodeDirty =
    !!selectedNode &&
    !!nodeDraft &&
    ((!selectedNode.isHistory && nodeDraft.name !== (selectedNode.name ?? "")) ||
      nodeDraft.comment !== (selectedNode.comment ?? "") ||
      !!nodeDraft.isStart !== !!selectedNode.isStart);
  $: superNodeStartLocked = !!(superNodeDraft && nodeEditorTarget?.isRoot);
  $: superNodeDirty =
    !sceneFlowSelection &&
    !!superNodeDraft &&
    !!nodeEditorTarget &&
    (superNodeDraft.name !== (nodeEditorTarget.name ?? "") ||
      (!superNodeStartLocked && !!superNodeDraft.isStart !== !!nodeEditorTarget.isStart));
  $: edgeDirty = (() => {
    if (!selectedEdge || !edgeDraft) return false;
    const altDirty =
      edgeAltStartEnabled &&
      normalizeAltStartText(edgeDraft.altStartText) !== normalizeAltStartText(formatAltStartMap(selectedEdge));
    if (selectedEdge.type === "CEDGE" || selectedEdge.type === "IEDGE") {
      return (edgeDraft.condition ?? "") !== (selectedEdge.condition ?? "") || altDirty;
    }
    if (selectedEdge.type === "PEDGE") {
      return altDirty;
    }
    if (selectedEdge.type === "TEDGE") {
      return String(edgeDraft.timeoutMs ?? "") !== String(selectedEdge.timeoutMs ?? "") || altDirty;
    }
    return altDirty;
  })();

  $: pEdgeGroup =
    selectedEdge?.type === "PEDGE" && selectedEdge.sourceId
      ? (sceneFlow?.edges || []).filter(
          (edge) => edge.type === "PEDGE" && edge.sourceId === selectedEdge.sourceId
        )
      : [];
  $: pEdgeGroupKey = selectedEdge?.type === "PEDGE" ? pEdgeGroup.map((edge) => edge.id).join("|") : "";
  $: if (selectedEdge?.type === "PEDGE") {
    const sourceId = selectedEdge.sourceId || "";
    if (sourceId !== pEdgeSourceId || pEdgeGroupKey !== pEdgeDraftKey) {
      syncPEdgeDrafts();
    }
  } else if (pEdgeDrafts.length || pEdgeSourceId) {
    resetPEdgeDrafts();
  }
  $: {
    const validation = validatePEdgeDrafts(pEdgeDrafts);
    pEdgeSum = validation.sum;
    pEdgeValid = validation.valid;
  }
  $: pEdgeDirty = isPEdgeDirty(pEdgeDrafts, pEdgeGroup);

  $: if (Number.isFinite(sceneFlowZoom)) {
    const clamped = clampSceneFlowZoom(sceneFlowZoom);
    if (clamped !== sceneFlowZoom) {
      sceneFlowZoom = clamped;
    }
    localStorage.setItem(SCENEFLOW_ZOOM_KEY, clamped.toFixed(3));
  }

  $: if (selectedProjectId && selectedProjectId !== localStorage.getItem("vsm_project_id")) {
    localStorage.setItem("vsm_project_id", selectedProjectId);
  }

  $: if (!sessionReady) {
    lastConfigProjectId = "";
    lastScriptProjectId = "";
    lastSceneFlowProjectId = "";
    lastRuntimeProjectId = "";
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastConfigProjectId) {
    lastConfigProjectId = selectedProjectId;
    loadConfig(selectedProjectId);
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastScriptProjectId) {
    lastScriptProjectId = selectedProjectId;
    scriptDiagRequestId += 1;
    if (scriptDiagTimer) {
      clearTimeout(scriptDiagTimer);
      scriptDiagTimer = null;
    }
    loadScript(selectedProjectId);
    loadScriptScenes(selectedProjectId);
    loadScriptElements(selectedProjectId);
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastSceneFlowProjectId) {
    lastSceneFlowProjectId = selectedProjectId;
    loadSceneFlow(selectedProjectId);
  }
  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastRuntimeProjectId) {
    lastRuntimeProjectId = selectedProjectId;
    runtimeValues = {};
    loadRuntime(selectedProjectId);
  }

  $: if (!selectedProjectId) {
    scriptText = "";
    scriptDraft = "";
    scriptVersion = null;
    scriptStatus = "";
    scriptError = "";
    scriptParseOk = true;
    scriptDiagnostics = [];
    scriptScenes = [];
    scriptScenesError = "";
    scriptScenesLoading = false;
    scriptElements = { acticon: [], gesticon: [], visicon: [] };
    scriptElementsError = "";
    scriptElementsLoading = false;
    if (scriptDiagTimer) {
      clearTimeout(scriptDiagTimer);
      scriptDiagTimer = null;
    }
    sceneFlow = null;
    sceneFlowError = "";
    sceneFlowLoading = false;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    edgeCreateMode = false;
    edgeCreateSourceId = "";
    runtimeInfo = null;
    runtimeError = "";
    runtimeLoading = false;
    lastRuntimeProjectId = "";
    runtimeValues = {};
    runtimeInitialValues = {};
    runtimeInitialProjectId = "";
    runtimeInitialState = "stopped";
    activityNodeCounts = new Map();
    activityEdgeHits = new Map();
  }

  async function connectAll() {
    error = "";
    statusMessage = "";
    sessionReady = false;
    try {
      await loadInfo();
      await Promise.all([loadProjects(), loadPreferences()]);
      const wsOk = await connectWs();
      if (!wsOk) {
        error = wsError || "WebSocket connection failed.";
        return;
      }
      sessionReady = true;
      if (selectedProjectId) {
        lastConfigProjectId = selectedProjectId;
        lastScriptProjectId = selectedProjectId;
        lastSceneFlowProjectId = selectedProjectId;
        lastRuntimeProjectId = selectedProjectId;
        await Promise.all([
          loadConfig(selectedProjectId),
          loadScript(selectedProjectId),
          loadScriptScenes(selectedProjectId),
          loadScriptElements(selectedProjectId),
          loadSceneFlow(selectedProjectId),
          loadRuntime(selectedProjectId)
        ]);
      }
    } catch (err) {
      error = err.message || "Failed to connect.";
    }
  }

  async function loadInfo() {
    info = await apiGet("/api/v1/info");
    localStorage.setItem("vsm_token", token);
  }

  async function refreshInfo() {
    error = "";
    try {
      await loadInfo();
    } catch (err) {
      error = err.message || "Failed to load server info.";
    }
  }

  async function loadProjects() {
    const data = await apiGet("/api/v1/projects");
    projects = data.projects || [];
    if (projects.length && !projects.some((p) => p.projectId === selectedProjectId)) {
      selectedProjectId = projects[0].projectId;
    }
    if (!projects.length) {
      selectedProjectId = "";
    }
  }

  async function loadRecent() {
    const data = await apiGet("/api/v1/projects/recent");
    recent = data.projects || [];
  }

  async function loadSamples() {
    const data = await apiGet("/api/v1/projects/samples");
    samples = data.projects || [];
  }

  async function loadTutorials() {
    const data = await apiGet("/api/v1/projects/tutorials");
    tutorials = data.projects || [];
  }

  async function openProject(path) {
    if (!path) return;
    await apiPost("/api/v1/projects/open", { path });
    openPath = "";
    await loadProjects();
  }

  async function createProject() {
    if (!newName) return;
    const payload = { name: newName };
    if (newBaseDir) {
      payload.baseDir = newBaseDir;
    }
    await apiPost("/api/v1/projects", payload);
    newName = "";
    newBaseDir = "";
    await loadProjects();
  }

  async function saveProject(projectId) {
    if (!projectId) return;
    await apiPost(`/api/v1/projects/${projectId}/save`, {});
    await loadProjects();
  }

  async function saveAsProject(projectId) {
    if (!projectId || !saveAsPath) return;
    await apiPost(`/api/v1/projects/${projectId}/save-as`, { path: saveAsPath });
    saveAsPath = "";
    await loadProjects();
  }

  async function closeProject(projectId) {
    if (!projectId) return;
    await apiPost(`/api/v1/projects/${projectId}/close`, {});
    if (projectId === selectedProjectId) {
      selectedProjectId = "";
    }
    await loadProjects();
  }

  async function activateProject(projectId) {
    if (!projectId) return;
    await sendCommand("Project.Activate", { projectId });
    await loadProjects();
  }

  async function loadPreferences() {
    const data = await apiGet("/api/v1/preferences");
    preferences = data.preferences || {};
    prefDraft = { ...preferences };
  }

  async function applyPreferences() {
    const values = diffValues(preferences, prefDraft);
    if (!Object.keys(values).length) {
      statusMessage = "No preference changes to apply.";
      return;
    }
    const response = await sendCommand("Preferences.Update", { values });
    preferences = response.preferences || {};
    prefDraft = { ...preferences };
    statusMessage = "Preferences updated.";
  }

  async function loadConfig(projectId) {
    if (!projectId) return;
    const data = await apiGet(`/api/v1/projects/${projectId}/config`);
    config = data.config || {};
    configDraft = { ...config };
    configSaved = null;
  }

  async function applyConfig() {
    if (!selectedProjectId) return;
    const values = diffValues(config, configDraft);
    if (!Object.keys(values).length) {
      statusMessage = "No config changes to apply.";
      return;
    }
    const response = await sendCommand("Config.Update", {
      projectId: selectedProjectId,
      values
    });
    config = response.config || {};
    configDraft = { ...config };
    configSaved = response.saved === true;
    statusMessage = response.pending ? "Config stored; save the project to persist." : "Config updated.";
  }

  async function loadScript(projectId) {
    if (!projectId) return;
    scriptError = "";
    scriptStatus = "";
    scriptParseOk = true;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script`);
      scriptText = data.text || "";
      scriptDraft = scriptText;
      scriptVersion = data.version ?? null;
      scriptDiagnostics = data.parseErrors || [];
      scriptParseOk = data.parseOk !== false;
    } catch (err) {
      scriptError = err.message || "Failed to load script.";
    }
  }

  async function loadScriptScenes(projectId) {
    if (!projectId) return;
    scriptScenesError = "";
    scriptScenesLoading = true;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script/scenes`);
      scriptScenes = Array.isArray(data.languages) ? data.languages : [];
    } catch (err) {
      scriptScenesError = err.message || "Failed to load scenes.";
      scriptScenes = [];
    } finally {
      scriptScenesLoading = false;
    }
  }

  async function loadScriptElements(projectId) {
    if (!projectId) return;
    scriptElementsError = "";
    scriptElementsLoading = true;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script/elements`);
      scriptElements = {
        acticon: Array.isArray(data.acticon) ? data.acticon : [],
        gesticon: Array.isArray(data.gesticon) ? data.gesticon : [],
        visicon: Array.isArray(data.visicon) ? data.visicon : []
      };
    } catch (err) {
      scriptElementsError = err.message || "Failed to load script elements.";
      scriptElements = { acticon: [], gesticon: [], visicon: [] };
    } finally {
      scriptElementsLoading = false;
    }
  }

  function insertScriptSnippet(snippet) {
    if (!snippet) return;
    scriptEditorRef?.insertText(snippet);
  }

  async function applyScript() {
    if (!selectedProjectId) return;
    scriptError = "";
    scriptStatus = "";
    try {
      const payload = {
        projectId: selectedProjectId,
        text: scriptDraft
      };
      if (scriptVersion !== null) {
        payload.version = scriptVersion;
      }
      const response = await sendCommand("Script.Update", payload);
      if (response.applied) {
        scriptText = response.text ?? scriptDraft;
        scriptDraft = scriptText;
        scriptVersion = response.version ?? scriptVersion;
        scriptParseOk = response.parseOk !== false;
        scriptDiagnostics = response.parseErrors || [];
        scriptStatus = "Script updated.";
        loadScriptScenes(selectedProjectId);
        return;
      }
      if (response.reason === "VERSION_MISMATCH") {
        scriptParseOk = true;
        scriptStatus = "Script changed on server. Reload to sync.";
        return;
      }
      if (response.reason === "PARSE_FAILED") {
        scriptParseOk = false;
        scriptDiagnostics = response.parseErrors || [];
        scriptError = "Script parse failed. Check syntax.";
        return;
      }
      scriptParseOk = response.parseOk !== false;
      scriptDiagnostics = response.parseErrors || [];
      scriptStatus = "Script update not applied.";
    } catch (err) {
      scriptError = err.message || "Failed to update script.";
    }
  }

  async function loadSceneFlow(projectId, superNodeId = "") {
    if (!projectId) return;
    sceneFlowError = "";
    sceneFlowLoading = true;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    edgeCreateSourceId = "";
    clearSceneFlowActivity();
    try {
      const query = superNodeId ? `?superNodeId=${encodeURIComponent(superNodeId)}` : "";
      const data = await apiGet(`/api/v1/projects/${projectId}/sceneflow${query}`);
      sceneFlow = data;
      loadRuntime(projectId);
    } catch (err) {
      sceneFlowError = err.message || "Failed to load SceneFlow.";
      sceneFlow = null;
    } finally {
      sceneFlowLoading = false;
    }
  }

  async function loadRuntime(projectId) {
    if (!projectId) return;
    runtimeError = "";
    runtimeLoading = true;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/runtime`);
      runtimeInfo = data;
      applyRuntimeValuesFromData(data);
      captureRuntimeInitialValues(data, projectId);
    } catch (err) {
      runtimeError = err.message || "Failed to load runtime.";
      runtimeInfo = null;
    } finally {
      runtimeLoading = false;
    }
  }

  function captureRuntimeInitialValues(data, projectId) {
    if (!data || !projectId) return;
    if (runtimeInitialProjectId !== projectId) {
      runtimeInitialProjectId = projectId;
      runtimeInitialState = "stopped";
      runtimeInitialValues = {};
    }
    const state = data.state || "stopped";
    if (state === "stopped") {
      runtimeInitialState = state;
      runtimeInitialValues = {};
      return;
    }
    if (runtimeInitialState !== "stopped" && Object.keys(runtimeInitialValues).length) {
      runtimeInitialState = state;
      return;
    }
    const updates = {};
    const globals = Array.isArray(data.globalVariables) ? data.globalVariables : [];
    const locals = Array.isArray(data.localVariables) ? data.localVariables : [];
    for (const entry of [...globals, ...locals]) {
      const name = (entry?.name || "").trim();
      if (!name || entry?.value === undefined || entry?.value === null) continue;
      updates[name] = normalizeRuntimeValue(entry.value);
    }
    runtimeInitialValues = updates;
    runtimeInitialState = state;
  }

  function applyRuntimeValuesFromData(data) {
    if (!data) return;
    const updates = {};
    const globals = Array.isArray(data.globalVariables) ? data.globalVariables : [];
    const locals = Array.isArray(data.localVariables) ? data.localVariables : [];
    for (const entry of [...globals, ...locals]) {
      const name = (entry?.name || "").trim();
      if (!name || entry?.value === undefined || entry?.value === null) continue;
      updates[name] = normalizeRuntimeValue(entry.value);
    }
    if (Object.keys(updates).length) {
      runtimeValues = { ...runtimeValues, ...updates };
    }
  }

  async function runRuntimeCommand(command) {
    if (!selectedProjectId) return;
    runtimeError = "";
    try {
      await sendCommand(command, { projectId: selectedProjectId });
      await loadProjects();
      loadRuntime(selectedProjectId);
    } catch (err) {
      runtimeError = err.message || "Failed to update runtime.";
    }
  }

  function clearSceneFlowActivity() {
    activityNodeCounts = new Map();
    activityEdgeHits = new Map();
  }

  function activityProjectMatches(payload) {
    const projectId = payload?.projectId;
    return !projectId || projectId === selectedProjectId;
  }

  function resolveActivityNodeId(payload) {
    if (!sceneFlow?.nodes) return "";
    const nodeId = (payload?.nodeId || "").trim();
    const parentId = (payload?.parentId || "").trim();
    const visible = new Set(sceneFlow.nodes.map((node) => node.id));
    if (nodeId && visible.has(nodeId)) return nodeId;
    if (parentId && visible.has(parentId)) return parentId;
    return "";
  }

  function resolveActivityEdgeId(payload) {
    if (!sceneFlow?.edges) return "";
    const edgeId = (payload?.edgeId || "").trim();
    if (edgeId && sceneFlow.edges.some((edge) => edge.id === edgeId)) {
      return edgeId;
    }
    const sourceId = (payload?.sourceId || "").trim();
    const targetId = (payload?.targetId || "").trim();
    const edgeType = (payload?.edgeType || "").trim();
    if (!sourceId || !targetId) return "";
    const match = sceneFlow.edges.find((edge) => {
      if (edge.sourceId !== sourceId || edge.targetId !== targetId) {
        return false;
      }
      if (!edgeType) return true;
      return (edge.type || "") === edgeType;
    });
    return match?.id || "";
  }

  function incrementActivityNode(nodeId) {
    if (!nodeId) return;
    const next = new Map(activityNodeCounts);
    const count = next.get(nodeId) || 0;
    next.set(nodeId, count + 1);
    activityNodeCounts = next;
  }

  function decrementActivityNode(nodeId) {
    if (!nodeId) return;
    const next = new Map(activityNodeCounts);
    const count = next.get(nodeId);
    if (!count) return;
    if (count <= 1) {
      next.delete(nodeId);
    } else {
      next.set(nodeId, count - 1);
    }
    activityNodeCounts = next;
  }

  function registerEdgeActivity(edgeId) {
    if (!edgeId) return;
    const ts = Date.now();
    const next = new Map(activityEdgeHits);
    next.set(edgeId, { id: edgeId, ts });
    activityEdgeHits = next;
    setTimeout(() => {
      const current = activityEdgeHits.get(edgeId);
      if (!current || current.ts !== ts) return;
      const updated = new Map(activityEdgeHits);
      updated.delete(edgeId);
      activityEdgeHits = updated;
    }, EDGE_ACTIVITY_MS);
  }

  function refreshRuntimeVars(target) {
    if (!selectedProjectId) return;
    if (target?.type === "Super") {
      loadRuntime(selectedProjectId);
    }
  }

  async function navigateSceneFlow(superNodeId) {
    if (!selectedProjectId) return;
    const targetId = superNodeId && superNodeId.trim() ? superNodeId : SCENEFLOW_ROOT_ID;
    const currentId =
      sceneFlow?.superNodeId && sceneFlow.superNodeId.trim() ? sceneFlow.superNodeId : SCENEFLOW_ROOT_ID;
    if (currentId === targetId) return;
    sceneFlowError = "";
    sceneFlowLoading = true;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    edgeCreateSourceId = "";
    clearSceneFlowActivity();
    try {
      const data = await apiPost(`/api/v1/projects/${selectedProjectId}/sceneflow/navigate`, {
        superNodeId: targetId
      });
      sceneFlow = data;
      loadRuntime(selectedProjectId);
    } catch (err) {
      sceneFlowError = err.message || "Failed to navigate SceneFlow.";
    } finally {
      sceneFlowLoading = false;
    }
  }

  function scheduleScriptDiagnostics() {
    if (!sessionReady || !selectedProjectId || !token) return;
    if (scriptDiagTimer) {
      clearTimeout(scriptDiagTimer);
    }
    const requestId = ++scriptDiagRequestId;
    scriptDiagTimer = setTimeout(() => runScriptDiagnostics(requestId), 600);
  }

  async function runScriptDiagnostics(requestId) {
    if (!selectedProjectId || requestId !== scriptDiagRequestId) {
      return;
    }
    try {
      const data = await apiPost(`/api/v1/projects/${selectedProjectId}/script/diagnostics`, {
        text: scriptDraft
      });
      if (requestId !== scriptDiagRequestId) {
        return;
      }
      scriptDiagnostics = data.parseErrors || [];
      scriptParseOk = data.parseOk !== false;
    } catch (err) {
      scriptError = err.message || "Failed to analyze script.";
    }
  }

  function connectWs() {
    wsError = "";
    if (ws) {
      ws.close();
    }
    const needsToken = info?.tokenRequired === true;
    if (!token && needsToken) {
      wsConnected = false;
      wsError = "Missing or invalid token.";
      return Promise.resolve(false);
    }
    return new Promise((resolve) => {
      let settled = false;
      const finish = (ok) => {
        if (settled) return;
        settled = true;
        resolve(ok);
      };
      const protocol = location.protocol === "https:" ? "wss" : "ws";
      const baseUrl = `${protocol}://${location.host}/ws`;
      const url = token ? `${baseUrl}?token=${encodeURIComponent(token)}` : baseUrl;
      ws = new WebSocket(url);
      ws.onopen = () => {
        wsConnected = true;
        finish(true);
      };
      ws.onclose = (event) => {
        wsConnected = false;
        const reason = (event?.reason || "").toLowerCase();
        if (event?.code === 1008 || reason.includes("unauthorized")) {
          const message = "Missing or invalid token.";
          wsError = message;
          error = message;
          sessionReady = false;
          if (token) {
            token = "";
            localStorage.removeItem("vsm_token");
          }
        }
        finish(false);
      };
      ws.onerror = () => {
        wsError = "WebSocket connection failed.";
        finish(false);
      };
      ws.onmessage = (event) => handleWsMessage(event.data);
    });
  }

  function handleWsMessage(data) {
    let message;
    try {
      message = JSON.parse(data);
    } catch (err) {
      return;
    }
    if (message.type === "response" || message.type === "error") {
      const entry = pending.get(message.id);
      if (!entry) return;
      pending.delete(message.id);
      if (message.type === "error") {
        const detail = message.payload?.message;
        entry.reject(new Error(detail || message.name || "Request failed"));
      } else {
        entry.resolve(message.payload || {});
      }
      return;
    }
    if (message.type === "event") {
      if (
        ["Project.Opened", "Project.Closed", "Project.Activated", "Project.DirtyChanged"].includes(message.name)
      ) {
        loadProjects();
      }
      if (message.name === "Preferences.Changed" && message.payload?.preferences) {
        preferences = message.payload.preferences;
        prefDraft = { ...preferences };
      }
      if (message.name === "Config.Changed" && message.payload?.config) {
        config = message.payload.config;
        configDraft = { ...config };
      }
      if (message.name === "Script.Changed" && message.payload?.projectId === selectedProjectId) {
        if (message.sourceClientId && message.sourceClientId === clientId) {
          return;
        }
        if (scriptDirty) {
          scriptStatus = "Script changed on server. Reload to sync.";
          return;
        }
        scriptText = message.payload.text || "";
        scriptDraft = scriptText;
        if (message.payload.version !== undefined) {
          scriptVersion = message.payload.version;
        }
        scriptParseOk = true;
        scriptStatus = "Script updated from another editor.";
        loadScriptScenes(selectedProjectId);
      }
      if (message.name === "Runtime.StateChanged") {
        loadProjects();
        if (message.payload?.projectId === selectedProjectId) {
          loadRuntime(selectedProjectId);
        }
      }
      if (message.name === "Runtime.VariableChanged") {
        const payload = message.payload || {};
        const name = (payload.name || "").trim();
        if (!name || payload.value === undefined || payload.value === null) {
          return;
        }
        const value = normalizeRuntimeValue(payload.value);
        runtimeValues = { ...runtimeValues, [name]: value };
        if (runtimeInfo) {
          const updateList = (list) => {
            if (!Array.isArray(list)) return list;
            let updated = false;
            const next = list.map((entry) => {
              if (!entry || entry.name !== name) return entry;
              updated = true;
              return { ...entry, value };
            });
            return updated ? next : list;
          };
          const globals = updateList(runtimeInfo.globalVariables);
          const locals = updateList(runtimeInfo.localVariables);
          if (globals !== runtimeInfo.globalVariables || locals !== runtimeInfo.localVariables) {
            runtimeInfo = { ...runtimeInfo, globalVariables: globals, localVariables: locals };
          }
        }
      }
      if (message.name === "SceneFlow.Node.Started") {
        if (!activityProjectMatches(message.payload)) return;
        const nodeId = resolveActivityNodeId(message.payload);
        if (nodeId) {
          incrementActivityNode(nodeId);
        }
      }
      if (message.name === "SceneFlow.Node.Stopped") {
        if (!activityProjectMatches(message.payload)) return;
        const nodeId = resolveActivityNodeId(message.payload);
        if (nodeId) {
          decrementActivityNode(nodeId);
        }
      }
      if (message.name === "SceneFlow.Edge.Executed") {
        if (!activityProjectMatches(message.payload)) return;
        const superNodeId = (message.payload?.superNodeId || "").trim();
        if (superNodeId && superNodeId !== (sceneFlow?.superNodeId || "")) {
          return;
        }
        const edgeId = resolveActivityEdgeId(message.payload);
        if (edgeId) {
          registerEdgeActivity(edgeId);
        }
      }
      if (message.name === "SceneFlow.Runtime.Stopped") {
        if (!activityProjectMatches(message.payload)) return;
        clearSceneFlowActivity();
      }
      if (message.name === "SceneFlow.PathChanged" && message.payload?.projectId === selectedProjectId) {
        loadSceneFlow(selectedProjectId, message.payload?.superNodeId || "");
      }
    }
  }

  function sendCommand(name, payload) {
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      return Promise.reject(new Error("WebSocket not connected."));
    }
    const id = `req-${Date.now()}-${Math.random().toString(16).slice(2)}`;
    const message = {
      type: "cmd",
      id,
      name,
      sourceClientId: clientId,
      payload
    };
    return new Promise((resolve, reject) => {
      pending.set(id, { resolve, reject });
      ws.send(JSON.stringify(message));
    });
  }

  async function apiGet(path) {
    return apiFetch(path, { method: "GET" });
  }

  async function apiPost(path, body) {
    return apiFetch(path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body || {})
    });
  }

  async function apiFetch(path, options) {
    const headers = {
      ...(options?.headers || {})
    };
    if (token) {
      headers.Authorization = `Bearer ${token}`;
    }
    const response = await fetch(path, {
      ...options,
      headers
    });
    if (response.status === 401) {
      const text = await response.text();
      const message = text || "Missing or invalid token";
      error = message;
      wsError = message;
      sessionReady = false;
      if (ws) {
        ws.close();
      }
      wsConnected = false;
      if (token) {
        token = "";
        localStorage.removeItem("vsm_token");
      }
      throw new Error(message);
    }
    if (!response.ok) {
      const text = await response.text();
      throw new Error(text || response.statusText);
    }
    return response.json();
  }

  function diffValues(original, draft) {
    const diff = {};
    for (const [key, value] of Object.entries(draft)) {
      if (String(value) !== String(original[key])) {
        diff[key] = value;
      }
    }
    return diff;
  }

  function filterKeyValues(values, filter) {
    const query = (filter || "").toLowerCase();
    const entries = Object.entries(values);
    if (!query) return entries;
    return entries.filter(([key, value]) => {
      return key.toLowerCase().includes(query) || String(value).toLowerCase().includes(query);
    });
  }

  function formatAltStartMap(edge) {
    const entries = edge?.altStartMap || [];
    if (!entries.length) return "";
    return entries
      .map((entry) => {
        const startId = entry?.startId ?? "";
        const altStartId = entry?.altStartId ?? "";
        return startId && altStartId ? `${startId}/${altStartId}` : "";
      })
      .filter(Boolean)
      .join("\n");
  }

  function normalizeAltStartText(value) {
    return (value || "")
      .split(/\r?\n/)
      .map((line) => line.trim())
      .filter(Boolean)
      .join("\n");
  }

  function parseAltStartText(value) {
    const entries = [];
    const seen = new Set();
    const lines = (value || "").split(/\r?\n/);
    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed) continue;
      const parts = trimmed
        .split(/->|=|\/|,/)
        .map((part) => part.trim())
        .filter(Boolean);
      if (parts.length < 2) {
        return { error: `Invalid alt-start entry: "${trimmed}"`, entries: [] };
      }
      const startId = parts[0];
      const altStartId = parts[1];
      if (seen.has(startId)) {
        return { error: `Duplicate start node: "${startId}"`, entries: [] };
      }
      seen.add(startId);
      entries.push({ startId, altStartId });
    }
    return { error: null, entries };
  }

  function displayNodeName(node) {
    if (!node) return "";
    const name = (node.name || "").trim();
    return name || node.id || "";
  }

  function normalizeRuntimeValue(value) {
    if (value === null || value === undefined) return "";
    return String(value).replace(/#[a-zA-Z]#/g, "");
  }

  function runtimeVarLine(def) {
    if (!def) return "";
    const type = (def.type || "").trim();
    const name = (def.name || "").trim();
    const expr = (def.expr || "").trim();
    const hasLiveValue = Object.prototype.hasOwnProperty.call(runtimeValues, name);
    const value = normalizeRuntimeValue(hasLiveValue ? runtimeValues[name] : def.value);
    const initial = normalizeRuntimeValue(runtimeInitialValues[name]);
    const showInitial = hasLiveValue && initial !== "" && value !== initial;
    const head = [type, name].filter(Boolean).join(" ");
    if (value) {
      const displayValue = showInitial ? `${value} (${initial})` : value;
      return head ? `${head} = ${displayValue}` : displayValue;
    }
    if (!expr) return head;
    if (!head) return expr;
    return `${head} = ${expr}`;
  }

  function nodeTypeSummary(nodes) {
    if (!Array.isArray(nodes) || !nodes.length) return "";
    let basicCount = 0;
    let superCount = 0;
    nodes.forEach((node) => {
      if (node?.type === "Super") {
        superCount += 1;
      } else {
        basicCount += 1;
      }
    });
    const parts = [];
    if (basicCount) parts.push(`${basicCount} basic`);
    if (superCount) parts.push(`${superCount} super`);
    return parts.join(", ");
  }

  function edgeTypeSummary(edges) {
    if (!Array.isArray(edges) || !edges.length) return "";
    const order = ["EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE"];
    const labels = {
      EEDGE: "epsilon",
      CEDGE: "conditional",
      PEDGE: "probabilistic",
      TEDGE: "timeout",
      FEDGE: "fork",
      IEDGE: "interrupt"
    };
    const counts = new Map();
    edges.forEach((edge) => {
      const type = edge?.type || "EEDGE";
      counts.set(type, (counts.get(type) || 0) + 1);
    });
    const parts = [];
    order.forEach((type) => {
      const count = counts.get(type);
      if (count) {
        const label = labels[type] || type.toLowerCase();
        parts.push(`${count} ${label}`);
      }
    });
    return parts.join(", ");
  }

  function commentLabel(comment, index) {
    const text = (comment?.text || "").trim().replace(/\s+/g, " ");
    if (!text) return `Comment ${index + 1}`;
    if (text.length <= 32) return text;
    return `${text.slice(0, 32)}...`;
  }

  function pEdgeTargetLabel(edge) {
    if (!edge) return "";
    const target = sceneFlow?.nodes?.find((node) => node.id === edge.targetId) || null;
    return displayNodeName(target) || edge.targetId || edge.id || "";
  }

  function validatePEdgeDrafts(drafts) {
    let sum = 0;
    let valid = true;
    for (const draft of drafts || []) {
      const raw = String(draft?.value ?? "").trim();
      if (!raw) {
        valid = false;
        continue;
      }
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isFinite(parsed) || parsed < 0 || parsed > 100) {
        valid = false;
        continue;
      }
      sum += parsed;
    }
    return { sum, valid };
  }

  function isPEdgeDirty(drafts, group) {
    if (!drafts?.length || !group?.length) return false;
    const byId = new Map(group.map((edge) => [edge.id, edge]));
    for (const draft of drafts) {
      const edge = byId.get(draft.edgeId);
      if (!edge) return true;
      const current = String(edge.probability ?? "").trim();
      const next = String(draft.value ?? "").trim();
      if (current !== next) return true;
    }
    return false;
  }

  function syncPEdgeDrafts() {
    if (!selectedEdge || selectedEdge.type !== "PEDGE") {
      resetPEdgeDrafts();
      return;
    }
    pEdgeSourceId = selectedEdge.sourceId || "";
    pEdgeDraftKey = pEdgeGroupKey;
    pEdgeDrafts =
      (pEdgeGroup || []).map((edge) => ({
        edgeId: edge.id,
        targetId: edge.targetId || "",
        label: pEdgeTargetLabel(edge),
        value: String(edge.probability ?? 0)
      })) || [];
    pEdgeError = "";
  }

  function resetPEdgeDrafts() {
    pEdgeDrafts = [];
    pEdgeDraftKey = "";
    pEdgeSourceId = "";
    pEdgeError = "";
  }

  function updatePEdgeDraft(edgeId, value) {
    pEdgeError = "";
    pEdgeDrafts = pEdgeDrafts.map((draft) =>
      draft.edgeId === edgeId ? { ...draft, value } : draft
    );
  }

  function normalizePEdgeDrafts() {
    pEdgeError = "";
    if (!pEdgeDrafts.length) return;
    const entries = pEdgeDrafts.map((draft) => {
      const parsed = Number.parseInt(String(draft.value ?? "").trim(), 10);
      return Number.isFinite(parsed) && parsed > 0 ? parsed : 0;
    });
    const sum = entries.reduce((total, value) => total + value, 0);
    if (sum <= 0) {
      pEdgeError = "Enter positive probabilities to normalize.";
      return;
    }
    const raw = entries.map((value) => (value / sum) * 100);
    const floored = raw.map((value) => Math.floor(value));
    let remainder = 100 - floored.reduce((total, value) => total + value, 0);
    const order = raw
      .map((value, index) => ({ index, frac: value - Math.floor(value) }))
      .sort((a, b) => b.frac - a.frac);
    const next = [...floored];
    let cursor = 0;
    while (remainder > 0 && order.length) {
      next[order[cursor % order.length].index] += 1;
      remainder -= 1;
      cursor += 1;
    }
    pEdgeDrafts = pEdgeDrafts.map((draft, index) => ({
      ...draft,
      value: String(next[index] ?? 0)
    }));
  }

  function uniformPEdgeDrafts() {
    pEdgeError = "";
    const count = pEdgeDrafts.length;
    if (!count) return;
    const base = Math.floor(100 / count);
    let remainder = 100 % count;
    pEdgeDrafts = pEdgeDrafts.map((draft, index) => {
      const value = base + (remainder > 0 ? 1 : 0);
      if (remainder > 0) remainder -= 1;
      return { ...draft, value: String(value) };
    });
  }

  async function applyPEdgeGroup() {
    pEdgeError = "";
    if (!selectedProjectId || !selectedEdge || selectedEdge.type !== "PEDGE") return;
    const parsed = pEdgeDrafts.map((draft) => ({
      edgeId: draft.edgeId,
      targetId: draft.targetId,
      probability: Number.parseInt(String(draft.value ?? "").trim(), 10)
    }));
    if (parsed.some((entry) => !Number.isFinite(entry.probability))) {
      pEdgeError = "Probability must be a number.";
      return;
    }
    if (parsed.some((entry) => entry.probability < 0 || entry.probability > 100)) {
      pEdgeError = "Probabilities must be between 0 and 100.";
      return;
    }
    const sum = parsed.reduce((total, entry) => total + entry.probability, 0);
    if (sum !== 100) {
      pEdgeError = `Total probability must be 100%. Current sum: ${sum}%.`;
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Edge.PEdge.UpdateGroup", {
      projectId: selectedProjectId,
      sourceId: selectedEdge.sourceId,
      updates: parsed
    });
    if (!response) {
      pEdgeError = sceneFlowError || "Failed to update probabilities.";
      return;
    }
    syncPEdgeDrafts();
  }

  function edgeTypeLabel(type) {
    if (!type) return "";
    return edgeTypeLabels[type] || type;
  }

  function sceneLanguageLabel(language) {
    const trimmed = (language || "").trim();
    return trimmed ? trimmed : "default";
  }

  function playSceneCommand(name) {
    const raw = String(name || "");
    const escaped = raw.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
    return `PlayScene("${escaped}")`;
  }

  function startBlockDrag(event, payload) {
    if (!event?.dataTransfer || !payload) return;
    event.dataTransfer.setData(BLOCK_DRAG_TYPE, JSON.stringify(payload));
    event.dataTransfer.effectAllowed = "copy";
  }

  function startSceneDrag(event, group, language) {
    if (!event?.dataTransfer || !group?.name) return;
    const payload = {
      name: group.name,
      language: language || ""
    };
    event.dataTransfer.setData(SCENE_DRAG_TYPE, JSON.stringify(payload));
    event.dataTransfer.setData("text/plain", JSON.stringify(payload));
    event.dataTransfer.effectAllowed = "copy";
  }

  function parseSceneDrop(event) {
    const data = event?.dataTransfer;
    if (!data) return null;
    const raw = data.getData(SCENE_DRAG_TYPE);
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

  function isSceneDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(SCENE_DRAG_TYPE) || types.includes("text/plain");
  }

  function handleSceneDropOver(event) {
    if (!isSceneDrag(event)) return;
    event.preventDefault();
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = "copy";
    }
  }

  async function addSceneCommandToNode(nodeId, sceneName, { selectNode = false } = {}) {
    if (!selectedProjectId || !nodeId || !sceneName) return;
    const response = await runSceneFlowCommand("SceneFlow.Node.Cmd.Add", {
      projectId: selectedProjectId,
      nodeId,
      command: { text: playSceneCommand(sceneName) }
    });
    if (response && selectNode) {
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
    }
  }

  async function handleSceneFlowSceneDrop(payload) {
    if (!payload?.name || !selectedProjectId) return;
    if (payload.targetNodeId) {
      await addSceneCommandToNode(payload.targetNodeId, payload.name, { selectNode: true });
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      nodeType: "Basic",
      name: payload.name,
      x: payload.x,
      y: payload.y
    });
    if (!response?.nodeId) {
      return;
    }
    await addSceneCommandToNode(response.nodeId, payload.name, { selectNode: true });
  }

  async function handleBlockDrop(payload) {
    if (!payload || !selectedProjectId) return;
    if (payload.kind === "node") {
      await createSceneFlowNode(payload.nodeType || "Basic", { x: payload.x, y: payload.y });
      return;
    }
    if (payload.kind === "comment") {
      await createSceneFlowComment({ x: payload.x, y: payload.y });
      return;
    }
    if (payload.kind === "edge") {
      edgeCreateType = payload.edgeType || "EEDGE";
      edgeCreateMode = true;
      edgeCreateSourceId = payload.targetNodeId || "";
      sceneFlowSelection = edgeCreateSourceId ? { type: "node", id: edgeCreateSourceId } : null;
      sceneFlowMultiSelection = edgeCreateSourceId ? [{ type: "node", id: edgeCreateSourceId }] : [];
    }
  }

  async function handleCommandSceneDrop(event) {
    const payload = parseSceneDrop(event);
    if (!payload?.name || !nodeEditorTarget?.id) return;
    event.preventDefault();
    await addSceneCommandToNode(nodeEditorTarget.id, payload.name);
  }

  function sceneFlowSelectionList() {
    if (Array.isArray(sceneFlowMultiSelection) && sceneFlowMultiSelection.length) {
      return sceneFlowMultiSelection;
    }
    return sceneFlowSelection ? [sceneFlowSelection] : [];
  }

  function selectionKey(list) {
    if (!Array.isArray(list) || !list.length) return "";
    return list
      .map((entry) => `${entry.type}:${entry.id}`)
      .sort()
      .join("|");
  }

  async function copySceneFlowSelection() {
    if (!sceneFlow || !selectedProjectId) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    const nodeIds = selectionList.filter((item) => item.type === "node").map((item) => item.id);
    const commentIds = selectionList.filter((item) => item.type === "comment").map((item) => item.id);
    if (!nodeIds.length && !commentIds.length) return;
    const nodeMap = new Map((sceneFlow.nodes || []).map((node) => [node.id, node]));
    const commentMap = new Map((sceneFlow.comments || []).map((comment) => [comment.id, comment]));
    const nodes = nodeIds.map((id) => nodeMap.get(id)).filter((node) => node && !node.isHistory);
    const comments = commentIds
      .map((id) => commentMap.get(id))
      .filter(Boolean)
      .map((comment) => ({
        id: comment.id,
        text: comment.text || "",
        x: comment.rect?.x ?? 0,
        y: comment.rect?.y ?? 0,
        w: comment.rect?.w ?? 120,
        h: comment.rect?.h ?? 90
      }));
    let minX = Infinity;
    let minY = Infinity;
    nodes.forEach((node) => {
      minX = Math.min(minX, node.graphics?.x ?? 0);
      minY = Math.min(minY, node.graphics?.y ?? 0);
    });
    comments.forEach((comment) => {
      minX = Math.min(minX, comment.x);
      minY = Math.min(minY, comment.y);
    });
    if (!Number.isFinite(minX) || !Number.isFinite(minY)) {
      return;
    }
    const copiedNodeIds = nodes.map((node) => node.id);
    sceneFlowClipboard = {
      nodeIds: copiedNodeIds,
      comments,
      origin: { x: minX, y: minY }
    };
    sceneFlowPasteIndex = 0;
    if (copiedNodeIds.length) {
      const response = await runSceneFlowCommand("SceneFlow.Selection.Copy", {
        projectId: selectedProjectId,
        nodeIds: copiedNodeIds
      });
      if (!response) {
        sceneFlowClipboard.nodeIds = [];
      }
    }
  }

  async function pasteSceneFlowSelectionWithOffset(dx, dy) {
    if (!sceneFlowClipboard || !selectedProjectId) return;
    const newSelections = [];

    if (sceneFlowClipboard.nodeIds?.length) {
      const response = await runSceneFlowCommand("SceneFlow.Selection.Paste", {
        projectId: selectedProjectId,
        dx,
        dy
      });
      if (!response) {
        return;
      }
      const nodeIds = Array.isArray(response.nodeIds) ? response.nodeIds : [];
      nodeIds.forEach((id) => {
        if (id) {
          newSelections.push({ type: "node", id });
        }
      });
    }

    for (const comment of sceneFlowClipboard.comments || []) {
      const response = await runSceneFlowCommand("SceneFlow.Comment.Create", {
        projectId: selectedProjectId,
        x: comment.x + dx,
        y: comment.y + dy,
        width: comment.w,
        height: comment.h,
        text: comment.text || ""
      });
      const newId = response?.commentId;
      if (newId) {
        newSelections.push({ type: "comment", id: newId });
      }
    }

    if (newSelections.length) {
      sceneFlowSelection = newSelections[0];
      sceneFlowMultiSelection = newSelections;
    }
  }

  async function pasteSceneFlowSelection() {
    if (!sceneFlowClipboard || !selectedProjectId) return;
    const view = sceneFlowViewBox;
    const center = view
      ? { x: view.x + view.width / 2, y: view.y + view.height / 2 }
      : sceneFlowCenter();
    const offset = 24 * sceneFlowPasteIndex;
    const dx = center.x - sceneFlowClipboard.origin.x + offset;
    const dy = center.y - sceneFlowClipboard.origin.y + offset;
    sceneFlowPasteIndex += 1;
    await pasteSceneFlowSelectionWithOffset(dx, dy);
  }

  async function cutSceneFlowSelection() {
    if (!selectedProjectId || sceneFlowBusy) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    await copySceneFlowSelection();
    await deleteSceneFlowSelection();
  }

  async function duplicateSceneFlowSelection() {
    if (!selectedProjectId || sceneFlowBusy) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    const hasCopyable = selectionList.some((entry) => entry.type === "node" || entry.type === "comment");
    if (!hasCopyable) return;
    const key = selectionKey(selectionList);
    if (key !== sceneFlowDuplicateKey) {
      sceneFlowDuplicateKey = key;
      sceneFlowDuplicateIndex = 0;
    }
    await copySceneFlowSelection();
    const offset = 24 * (sceneFlowDuplicateIndex + 1);
    sceneFlowDuplicateIndex += 1;
    await pasteSceneFlowSelectionWithOffset(offset, offset);
  }

  function sceneGroupTotal(groups) {
    if (!Array.isArray(groups)) return 0;
    return groups.reduce((total, group) => total + (group?.count ?? 0), 0);
  }

  function sceneLanguageOptionList(languages) {
    if (!Array.isArray(languages)) {
      return [{ value: SCENE_LANGUAGE_ALL, label: "All" }];
    }
    const options = languages.map((lang) => {
      const value = lang?.language ?? "";
      return { value, label: sceneLanguageLabel(value) };
    });
    const unique = new Map();
    options.forEach((opt) => {
      if (!unique.has(opt.value)) {
        unique.set(opt.value, opt);
      }
    });
    return [{ value: SCENE_LANGUAGE_ALL, label: "All" }, ...Array.from(unique.values())];
  }

  function filterSceneLanguages(languages, query, languageFilter) {
    if (!Array.isArray(languages)) return [];
    const langFilter = languageFilter ?? SCENE_LANGUAGE_ALL;
    const filteredLanguages =
      langFilter && langFilter !== SCENE_LANGUAGE_ALL
        ? languages.filter((lang) => (lang?.language ?? "") === langFilter)
        : languages;
    const needle = (query || "").trim().toLowerCase();
    if (!needle) return filteredLanguages;
    return filteredLanguages
      .map((lang) => {
        const groups = Array.isArray(lang.groups) ? lang.groups : [];
        const filtered = groups.filter((group) => (group?.name || "").toLowerCase().includes(needle));
        return { ...lang, groups: filtered };
      })
      .filter((lang) => Array.isArray(lang.groups) && lang.groups.length > 0);
  }

  function resetTypeDefEditor() {
    typeDefDraft = null;
    typeDefEditIndex = null;
    typeDefError = "";
    typeDefSelectedIndex = null;
  }

  function resetVarDefEditor() {
    varDefDraft = null;
    varDefEditIndex = null;
    varDefError = "";
    varDefSelectedIndex = null;
  }

  function resetCmdEditor() {
    cmdDraft = "";
    cmdEditIndex = null;
    cmdError = "";
    cmdSelectedIndex = null;
    cmdInlineDrafts = [];
    cmdDialogNodeId = "";
  }

  function syncCmdInlineDrafts() {
    cmdInlineDrafts = nodeEditorCommands.map((cmd) => cmd.text ?? "");
    cmdDialogNodeId = nodeEditorTarget?.id || "";
  }

  function closeCmdDialog() {
    cmdDialogOpen = false;
    resetCmdEditor();
  }

  function defaultTypeDefDraft() {
    return {
      name: "",
      flavour: "Struct",
      elementType: "Int",
      members: [{ name: "member", type: "Bool" }]
    };
  }

  function defaultVarExpression(typeName) {
    const name = (typeName || "").trim();
    if (!name) return "";
    if (name === "Int") return "0";
    if (name === "Bool") return "true";
    if (name === "Float") return "0";
    if (name === "String") return "\"\"";
    const match = nodeEditorTypeCatalog.find((entry) => entry?.name === name);
    if (match?.flavour === "List") return "[ ]";
    if (match?.flavour === "Struct") return "{ }";
    return "";
  }

  function defaultVarDefDraft() {
    const preferred = nodeEditorTypeOptions.includes("Bool") ? "Bool" : nodeEditorTypeOptions[0] || "Bool";
    return {
      name: "",
      type: preferred,
      expression: defaultVarExpression(preferred)
    };
  }

  function typeDefSummary(def) {
    if (!def) return "";
    if (def.flavour === "List") {
      const elementType = (def.elementType || "").trim();
      return elementType ? elementType : "?";
    }
    if (def.flavour === "Struct") {
      const members = Array.isArray(def.members) ? def.members : [];
      const summary = members
        .map((member) => {
          const name = (member?.name ?? "").trim();
          const type = (member?.type ?? "").trim();
          if (!name && !type) return "";
          if (!name) return type;
          if (!type) return name;
          return `${name}:${type}`;
        })
        .filter(Boolean)
        .join(", ");
      if (summary) {
        return summary;
      }
      return members.length ? `${members.length} members` : "";
    }
    return def.flavour || "Type";
  }

  function typeDefLine(def) {
    if (!def) return "";
    const flavour = (def.flavour ?? "").trim();
    const name = (def.name ?? "").trim();
    const summary = typeDefSummary(def);
    let line = "";
    if (flavour) {
      line += flavour;
    }
    if (name) {
      line += line ? ` ${name}` : name;
    }
    if (summary) {
      line += ` = ${summary}`;
    }
    return line || name || summary || "Unnamed type";
  }

  function typeDefSignature(def) {
    if (!def) return "";
    const name = (def.name ?? "").trim();
    const flavour = (def.flavour ?? "").trim();
    const elementType = (def.elementType ?? "").trim();
    const members = Array.isArray(def.members)
      ? def.members
          .map((member) => `${(member?.name ?? "").trim()}:${(member?.type ?? "").trim()}`)
          .join("|")
      : "";
    return `${name}::${flavour}::${elementType}::${members}`;
  }

  function varDefLine(def) {
    if (!def) return "";
    const type = (def.type ?? "").trim();
    const name = (def.name ?? "").trim();
    const expr = (def.expression ?? "").trim();
    let line = "";
    if (type) {
      line += type;
    }
    if (name) {
      line += line ? ` ${name}` : name;
    }
    if (expr) {
      line += ` = ${expr}`;
    }
    return line || def.syntax || "";
  }

  function varDefSignature(def) {
    if (!def) return "";
    const type = (def.type ?? "").trim();
    const name = (def.name ?? "").trim();
    const expr = (def.expression ?? "").trim();
    return `${type}::${name}::${expr}`;
  }

  function cmdText(cmd) {
    if (!cmd) return "";
    return String(cmd.text || "")
      .trim()
      .replace(/\s+/g, " ");
  }

  function cmdLine(cmd) {
    return cmdText(cmd);
  }

  function cmdSignature(cmd) {
    return cmdText(cmd);
  }

  function startTypeDefAdd() {
    typeDefError = "";
    typeDefEditIndex = -1;
    typeDefSelectedIndex = null;
    typeDefDraft = defaultTypeDefDraft();
  }

  function startTypeDefEdit(index) {
    const def = nodeEditorTypeDefs[index];
    if (!def) return;
    typeDefError = "";
    typeDefEditIndex = index;
    typeDefSelectedIndex = index;
    typeDefDraft = {
      name: def.name ?? "",
      flavour: def.flavour ?? "Struct",
      elementType: def.elementType ?? "Int",
      members: Array.isArray(def.members) ? def.members.map((member) => ({ ...member })) : []
    };
  }

  function selectTypeDef(index) {
    if (index === typeDefSelectedIndex) {
      typeDefSelectedIndex = null;
    } else {
      typeDefSelectedIndex = index;
    }
  }

  async function moveSelectedTypeDef(direction) {
    if (typeDefSelectedIndex === null) return;
    const current = typeDefSelectedIndex;
    const targetIndex = current + direction;
    if (targetIndex < 0 || targetIndex >= nodeEditorTypeDefs.length) return;
    const currentDef = nodeEditorTypeDefs[current];
    if (!currentDef) return;
    typeDefError = "";
    const signature = typeDefSignature(currentDef);
    const response = await moveTypeDef(current, direction);
    if (!response) {
      typeDefError = sceneFlowError || "Failed to move type definition.";
      return;
    }
    const list = nodeEditorTypeDefs;
    if (targetIndex >= 0 && targetIndex < list.length && typeDefSignature(list[targetIndex]) === signature) {
      typeDefSelectedIndex = targetIndex;
      return;
    }
    const foundIndex = list.findIndex((def) => typeDefSignature(def) === signature);
    typeDefSelectedIndex = foundIndex >= 0 ? foundIndex : null;
  }

  async function deleteSelectedTypeDef() {
    if (typeDefSelectedIndex === null) return;
    const index = typeDefSelectedIndex;
    await deleteTypeDef(index);
    typeDefSelectedIndex = null;
  }

  function editSelectedTypeDef() {
    if (typeDefSelectedIndex === null) return;
    startTypeDefEdit(typeDefSelectedIndex);
  }

  function addTypeDefMember() {
    if (!typeDefDraft) return;
    const members = Array.isArray(typeDefDraft.members) ? [...typeDefDraft.members] : [];
    members.push({ name: "", type: "Bool" });
    typeDefDraft = { ...typeDefDraft, members };
  }

  function removeTypeDefMember(index) {
    if (!typeDefDraft || !Array.isArray(typeDefDraft.members)) return;
    const members = [...typeDefDraft.members];
    members.splice(index, 1);
    typeDefDraft = { ...typeDefDraft, members };
  }

  async function applyTypeDefEdit() {
    typeDefError = "";
    if (!selectedProjectId || !nodeEditorTarget || !typeDefDraft) return;
    const name = (typeDefDraft.name ?? "").trim();
    if (!name) {
      typeDefError = "Type name is required.";
      return;
    }
    const flavour = typeDefDraft.flavour === "List" ? "List" : "Struct";
    const payload = {
      name,
      flavour
    };
    if (flavour === "List") {
      payload.elementType = (typeDefDraft.elementType || "Int").trim() || "Int";
    } else {
      const members = Array.isArray(typeDefDraft.members) ? typeDefDraft.members : [];
      const cleaned = members.map((member) => ({
        name: (member?.name || "").trim(),
        type: (member?.type || "").trim()
      }));
      const invalid = cleaned.some((member) => !member.name || !member.type);
      if (invalid) {
        typeDefError = "All struct members need a name and type.";
        return;
      }
      payload.members = cleaned.filter((member) => member.name && member.type);
    }
    const commandName = typeDefEditIndex >= 0 ? "SceneFlow.Node.TypeDef.Update" : "SceneFlow.Node.TypeDef.Add";
    const commandPayload = {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      typeDef: payload
    };
    if (typeDefEditIndex >= 0) {
      commandPayload.index = typeDefEditIndex;
    }
    const response = await runSceneFlowCommand(commandName, commandPayload);
    if (!response) {
      typeDefError = sceneFlowError || "Failed to update type definitions.";
      return;
    }
    resetTypeDefEditor();
  }

  async function moveTypeDef(index, direction) {
    if (!selectedProjectId || !nodeEditorTarget) return null;
    if (!nodeEditorTypeDefs[index]) return null;
    const target = index + direction;
    if (target < 0 || target >= nodeEditorTypeDefs.length) return null;
    return await runSceneFlowCommand("SceneFlow.Node.TypeDef.Move", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
  }

  async function deleteTypeDef(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    await runSceneFlowCommand("SceneFlow.Node.TypeDef.Delete", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      index
    });
    if (typeDefEditIndex === index) {
      resetTypeDefEditor();
    }
  }

  function startVarDefAdd() {
    varDefError = "";
    varDefEditIndex = -1;
    varDefSelectedIndex = null;
    varDefDraft = defaultVarDefDraft();
  }

  function startVarDefEdit(index) {
    const def = nodeEditorVarDefs[index];
    if (!def) return;
    varDefError = "";
    varDefEditIndex = index;
    varDefSelectedIndex = index;
    varDefDraft = {
      name: def.name ?? "",
      type: def.type ?? (nodeEditorTypeOptions[0] || "Bool"),
      expression: def.expression ?? ""
    };
  }

  function selectVarDef(index) {
    if (index === varDefSelectedIndex) {
      varDefSelectedIndex = null;
    } else {
      varDefSelectedIndex = index;
    }
  }

  function updateVarDefType() {
    if (!varDefDraft) return;
    const expr = (varDefDraft.expression || "").trim();
    if (!expr) {
      varDefDraft = {
        ...varDefDraft,
        expression: defaultVarExpression(varDefDraft.type)
      };
    }
  }

  async function applyVarDefEdit() {
    varDefError = "";
    if (!selectedProjectId || !nodeEditorTarget || !varDefDraft) return;
    const name = (varDefDraft.name ?? "").trim();
    if (!name) {
      varDefError = "Variable name is required.";
      return;
    }
    const type = (varDefDraft.type ?? "").trim();
    if (!type) {
      varDefError = "Variable type is required.";
      return;
    }
    const payload = {
      name,
      type,
      expression: varDefDraft.expression ?? ""
    };
    const commandName = varDefEditIndex >= 0 ? "SceneFlow.Node.VarDef.Update" : "SceneFlow.Node.VarDef.Add";
    const commandPayload = {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      varDef: payload
    };
    if (varDefEditIndex >= 0) {
      commandPayload.index = varDefEditIndex;
    }
    const response = await runSceneFlowCommand(commandName, commandPayload);
    if (!response) {
      varDefError = sceneFlowError || "Failed to update variable definitions.";
      return;
    }
    resetVarDefEditor();
    refreshRuntimeVars(nodeEditorTarget);
  }

  async function moveVarDef(index, direction) {
    if (!selectedProjectId || !nodeEditorTarget) return null;
    if (!nodeEditorVarDefs[index]) return null;
    const target = index + direction;
    if (target < 0 || target >= nodeEditorVarDefs.length) return null;
    const response = await runSceneFlowCommand("SceneFlow.Node.VarDef.Move", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
    if (response) {
      refreshRuntimeVars(nodeEditorTarget);
    }
    return response;
  }

  async function moveSelectedVarDef(direction) {
    if (varDefSelectedIndex === null) return;
    const current = varDefSelectedIndex;
    const targetIndex = current + direction;
    if (targetIndex < 0 || targetIndex >= nodeEditorVarDefs.length) return;
    const currentDef = nodeEditorVarDefs[current];
    if (!currentDef) return;
    varDefError = "";
    const signature = varDefSignature(currentDef);
    const response = await moveVarDef(current, direction);
    if (!response) {
      varDefError = sceneFlowError || "Failed to move variable definition.";
      return;
    }
    const list = nodeEditorVarDefs;
    if (targetIndex >= 0 && targetIndex < list.length && varDefSignature(list[targetIndex]) === signature) {
      varDefSelectedIndex = targetIndex;
      return;
    }
    const foundIndex = list.findIndex((def) => varDefSignature(def) === signature);
    varDefSelectedIndex = foundIndex >= 0 ? foundIndex : null;
  }

  async function deleteVarDef(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    await runSceneFlowCommand("SceneFlow.Node.VarDef.Delete", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      index
    });
    refreshRuntimeVars(nodeEditorTarget);
    if (varDefEditIndex === index) {
      resetVarDefEditor();
    }
  }

  async function deleteSelectedVarDef() {
    if (varDefSelectedIndex === null) return;
    const index = varDefSelectedIndex;
    await deleteVarDef(index);
    varDefSelectedIndex = null;
  }

  function editSelectedVarDef() {
    if (varDefSelectedIndex === null) return;
    startVarDefEdit(varDefSelectedIndex);
  }

  async function openCmdDialog(nodeId = null) {
    const targetId = nodeId || nodeEditorTarget?.id || "";
    if (!targetId && !nodeEditorTarget) return;
    if (nodeId && (sceneFlowSelection?.type !== "node" || sceneFlowSelection.id !== nodeId)) {
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
      await tick();
    }
    cmdDialogOpen = true;
    syncCmdInlineDrafts();
  }

  async function startCmdAdd() {
    await openCmdDialog();
    cmdError = "";
    cmdInlineDrafts = [...cmdInlineDrafts, ""];
    cmdSelectedIndex = cmdInlineDrafts.length - 1;
  }

  async function startCmdEdit(index) {
    if (index < 0 || index >= nodeEditorCommands.length) return;
    await openCmdDialog();
    cmdError = "";
    cmdSelectedIndex = index;
  }

  function updateCmdInlineDraft(index, value) {
    cmdError = "";
    cmdInlineDrafts = cmdInlineDrafts.map((entry, idx) => (idx === index ? value : entry));
  }

  function handleCmdInlineKeydown(event, index) {
    if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      commitCmdInlineDraft(index);
      event.currentTarget?.blur?.();
      return;
    }
    if (event.key === "Escape") {
      event.preventDefault();
      if (index < nodeEditorCommands.length) {
        updateCmdInlineDraft(index, nodeEditorCommands[index]?.text ?? "");
      } else {
        cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
        if (cmdSelectedIndex === index) {
          cmdSelectedIndex = null;
        }
      }
      event.currentTarget?.blur?.();
    }
  }

  async function commitCmdInlineDraft(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    const raw = cmdInlineDrafts[index] ?? "";
    const text = String(raw).trim();
    const isExisting = index < nodeEditorCommands.length;
    if (!text) {
      cmdError = "Command text is required.";
      if (isExisting) {
        cmdInlineDrafts = cmdInlineDrafts.map((entry, idx) =>
          idx === index ? nodeEditorCommands[index]?.text ?? "" : entry
        );
      } else {
        cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
        if (cmdSelectedIndex === index) {
          cmdSelectedIndex = null;
        }
      }
      return;
    }
    if (isExisting) {
      const existing = (nodeEditorCommands[index]?.text ?? "").trim();
      if (existing === text) {
        return;
      }
    }
    const commandName = isExisting ? "SceneFlow.Node.Cmd.Update" : "SceneFlow.Node.Cmd.Add";
    const commandPayload = {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      command: { text }
    };
    if (isExisting) {
      commandPayload.index = index;
    }
    const response = await runSceneFlowCommand(commandName, commandPayload);
    if (!response) {
      cmdError = sceneFlowError || "Failed to update commands.";
      return;
    }
    cmdError = "";
    if (!isExisting) {
      syncCmdInlineDrafts();
    } else {
      cmdInlineDrafts = cmdInlineDrafts.map((entry, idx) => (idx === index ? text : entry));
    }
  }

  async function moveCmd(index, direction) {
    if (!selectedProjectId || !nodeEditorTarget) return null;
    if (!nodeEditorCommands[index]) return null;
    const target = index + direction;
    if (target < 0 || target >= nodeEditorCommands.length) return null;
    return await runSceneFlowCommand("SceneFlow.Node.Cmd.Move", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
  }

  async function deleteCmd(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    return await runSceneFlowCommand("SceneFlow.Node.Cmd.Delete", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      index
    });
  }

  function selectCmd(index) {
    if (index === cmdSelectedIndex) {
      cmdSelectedIndex = null;
    } else {
      cmdSelectedIndex = index;
    }
  }

  async function moveSelectedCmd(direction) {
    if (cmdSelectedIndex === null) return;
    if (cmdSelectedIndex >= nodeEditorCommands.length) return;
    const current = cmdSelectedIndex;
    const targetIndex = current + direction;
    if (targetIndex < 0 || targetIndex >= nodeEditorCommands.length) return;
    const currentCmd = nodeEditorCommands[current];
    if (!currentCmd) return;
    cmdError = "";
    const signature = cmdSignature(currentCmd);
    const response = await moveCmd(current, direction);
    if (!response) {
      cmdError = sceneFlowError || "Failed to move command.";
      return;
    }
    if (cmdDialogOpen) {
      const next = [...cmdInlineDrafts];
      const [entry] = next.splice(current, 1);
      next.splice(targetIndex, 0, entry);
      cmdInlineDrafts = next;
    }
    const list = nodeEditorCommands;
    if (targetIndex >= 0 && targetIndex < list.length && cmdSignature(list[targetIndex]) === signature) {
      cmdSelectedIndex = targetIndex;
      return;
    }
    const foundIndex = list.findIndex((cmd) => cmdSignature(cmd) === signature);
    cmdSelectedIndex = foundIndex >= 0 ? foundIndex : null;
  }

  async function deleteSelectedCmd() {
    if (cmdSelectedIndex === null) return;
    const index = cmdSelectedIndex;
    if (index >= nodeEditorCommands.length) {
      cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
      cmdSelectedIndex = null;
      return;
    }
    const response = await deleteCmd(index);
    if (response && cmdDialogOpen) {
      cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
    }
    cmdSelectedIndex = null;
  }

  function editSelectedCmd() {
    if (cmdSelectedIndex === null) return;
    startCmdEdit(cmdSelectedIndex);
  }

  function filterScriptElements(elements, query) {
    const source = elements || {};
    const acticon = Array.isArray(source.acticon) ? source.acticon : [];
    const gesticon = Array.isArray(source.gesticon) ? source.gesticon : [];
    const visicon = Array.isArray(source.visicon) ? source.visicon : [];
    const needle = (query || "").trim().toLowerCase();
    if (!needle) {
      return { acticon, gesticon, visicon };
    }
    const matches = (value) => String(value || "").toLowerCase().includes(needle);
    const filteredActicon = acticon.filter((action) => matches(action?.name) || matches(action?.script));
    const filteredGesticon = gesticon
      .map((agent) => {
        const gestures = Array.isArray(agent?.gestures) ? agent.gestures : [];
        const agentMatch = matches(agent?.agent);
        const filteredGestures = agentMatch
          ? gestures
          : gestures.filter(
              (gesture) =>
                matches(gesture?.character) ||
                matches(gesture?.animName) ||
                matches(gesture?.animPath) ||
                matches(gesture?.category) ||
                matches(gesture?.script)
            );
        if (!filteredGestures.length) return null;
        return { ...agent, gestures: filteredGestures };
      })
      .filter(Boolean);
    const filteredVisicon = visicon
      .map((agent) => {
        const visemes = Array.isArray(agent?.visemes) ? agent.visemes : [];
        const agentMatch = matches(agent?.agent);
        const filteredVisemes = agentMatch
          ? visemes
          : visemes.filter((viseme) => matches(viseme?.key) || matches(viseme?.value));
        if (!filteredVisemes.length) return null;
        return { ...agent, visemes: filteredVisemes };
      })
      .filter(Boolean);
    return {
      acticon: filteredActicon,
      gesticon: filteredGesticon,
      visicon: filteredVisicon
    };
  }

  function countActicon(actions) {
    return Array.isArray(actions) ? actions.length : 0;
  }

  function countGesticon(agents) {
    if (!Array.isArray(agents)) return 0;
    return agents.reduce((total, agent) => total + (agent?.gestures?.length || 0), 0);
  }

  function countVisicon(agents) {
    if (!Array.isArray(agents)) return 0;
    return agents.reduce((total, agent) => total + (agent?.visemes?.length || 0), 0);
  }

  function gestureLabel(gesture) {
    if (!gesture) return "Gesture";
    const parts = [];
    if (gesture.character) parts.push(gesture.character);
    if (gesture.animName) parts.push(gesture.animName);
    if (parts.length) return parts.join(" / ");
    return gesture.category || "Gesture";
  }

  function gestureMeta(gesture) {
    if (!gesture) return "";
    const parts = [];
    if (gesture.category) parts.push(gesture.category);
    if (gesture.duration) parts.push(`${gesture.duration}ms`);
    if (gesture.blendable) parts.push("blendable");
    return parts.join(" / ");
  }

  function superNodeFrameColor(snapshot) {
    const flavour = (snapshot?.superNode?.flavour || "").toLowerCase();
    if (flavour === "enode") return SCENEFLOW_FRAME_COLORS.edges.eedge;
    if (flavour === "fnode") return SCENEFLOW_FRAME_COLORS.edges.fedge;
    if (flavour === "tnode") return SCENEFLOW_FRAME_COLORS.edges.tedge;
    if (flavour === "cnode") return SCENEFLOW_FRAME_COLORS.edges.cedge;
    if (flavour === "pnode") return SCENEFLOW_FRAME_COLORS.edges.pedge;
    if (flavour === "inode") return SCENEFLOW_FRAME_COLORS.edges.iedge;
    return SCENEFLOW_FRAME_COLORS.node;
  }

  function sceneFlowCenter() {
    const box = sceneFlowViewBox || sceneFlowWorldBox;
    if (!box) {
      return { x: 120, y: 120 };
    }
    return {
      x: box.x + box.width / 2,
      y: box.y + box.height / 2
    };
  }

  async function runSceneFlowCommand(name, payload) {
    if (!selectedProjectId) return null;
    sceneFlowError = "";
    sceneFlowBusy = true;
    try {
      const response = await sendCommand(name, payload);
      if (response?.snapshot) {
        sceneFlow = response.snapshot;
      }
      return response;
    } catch (err) {
      sceneFlowError = err.message || "SceneFlow command failed.";
      return null;
    } finally {
      sceneFlowBusy = false;
    }
  }

  async function createSceneFlowNode(nodeType, position = null) {
    if (!selectedProjectId) return;
    const center = position || sceneFlowCenter();
    await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      nodeType,
      x: center.x,
      y: center.y
    });
  }

  async function createSceneFlowComment(position = null) {
    if (!selectedProjectId) return;
    const center = position || sceneFlowCenter();
    await runSceneFlowCommand("SceneFlow.Comment.Create", {
      projectId: selectedProjectId,
      x: center.x,
      y: center.y
    });
  }

  async function createSceneFlowEdge(sourceId, targetId) {
    if (!selectedProjectId || !sourceId || !targetId) return;
    await runSceneFlowCommand("SceneFlow.Edge.Create", {
      projectId: selectedProjectId,
      sourceId,
      targetId,
      edgeType: edgeCreateType || "EEDGE"
    });
  }

  function toggleEdgeCreateMode() {
    edgeCreateMode = !edgeCreateMode;
    edgeCreateSourceId = "";
    if (!edgeCreateMode) {
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
    }
  }

  function startEdgeCreate(type) {
    if (edgeCreateMode && edgeCreateType === type) {
      edgeCreateMode = false;
      edgeCreateSourceId = "";
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
      return;
    }
    edgeCreateType = type;
    edgeCreateMode = true;
    edgeCreateSourceId = "";
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
  }

  async function handleEdgePick(nodeId) {
    if (!edgeCreateMode || !nodeId) return;
    if (!edgeCreateSourceId) {
      edgeCreateSourceId = nodeId;
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
      return;
    }
    if (edgeCreateSourceId === nodeId) {
      edgeCreateSourceId = "";
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
      return;
    }
    await createSceneFlowEdge(edgeCreateSourceId, nodeId);
    edgeCreateSourceId = "";
    edgeCreateMode = false;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
  }

  async function moveSceneFlowNode(nodeId, x, y, snap) {
    if (!selectedProjectId || !nodeId) return;
    const previous = sceneFlow;
    if (sceneFlow?.nodes?.length) {
      const nextNodes = sceneFlow.nodes.map((node) => {
        if (node.id !== nodeId) return node;
        return {
          ...node,
          graphics: { ...(node.graphics || {}), x, y }
        };
      });
      sceneFlow = { ...sceneFlow, nodes: nextNodes };
    }
    const payload = {
      projectId: selectedProjectId,
      nodeId,
      x,
      y,
      snap: snap ?? sceneFlowSnap
    };
    const response = await runSceneFlowCommand("SceneFlow.Node.Move", payload);
    if (!response?.snapshot && previous) {
      sceneFlow = previous;
    }
  }

  async function updateSceneFlowComment(commentId, x, y, width, height, text) {
    if (!selectedProjectId || !commentId) return;
    const previous = sceneFlow;
    if (sceneFlow?.comments?.length) {
      const nextComments = sceneFlow.comments.map((comment) => {
        if (comment.id !== commentId) return comment;
        const rect = { ...(comment.rect || {}) };
        if (Number.isFinite(x)) {
          rect.x = x;
        }
        if (Number.isFinite(y)) {
          rect.y = y;
        }
        if (Number.isFinite(width)) {
          rect.w = width;
        }
        if (Number.isFinite(height)) {
          rect.h = height;
        }
        const next = { ...comment, rect };
        if (text !== undefined) {
          next.text = text;
        }
        return next;
      });
      sceneFlow = { ...sceneFlow, comments: nextComments };
    }
    const payload = {
      projectId: selectedProjectId,
      commentId,
      x,
      y
    };
    if (Number.isFinite(width)) {
      payload.width = width;
    }
    if (Number.isFinite(height)) {
      payload.height = height;
    }
    if (text !== undefined) {
      payload.text = text;
    }
    const response = await runSceneFlowCommand("SceneFlow.Comment.Update", payload);
    if (!response?.snapshot && previous) {
      sceneFlow = previous;
    }
  }

  async function updateSceneFlowEdgeControl(edgeId, handle, cx, cy) {
    if (!selectedProjectId || !edgeId) return;
    if (!Number.isFinite(cx) || !Number.isFinite(cy)) return;
    const previous = sceneFlow;
    let nextPoints = null;
    if (sceneFlow?.edges?.length) {
      const nextEdges = sceneFlow.edges.map((edge) => {
        if (edge.id !== edgeId) return edge;
        const points = Array.isArray(edge.graphics?.points)
          ? edge.graphics.points.map((pt) => ({ ...pt }))
          : [];
        if (points.length < 2) {
          return edge;
        }
        const idx = handle === "ctrl1" ? 0 : points.length - 1;
        const target = points[idx] || {};
        points[idx] = {
          ...target,
          cx,
          cy
        };
        nextPoints = points;
        return {
          ...edge,
          graphics: {
            ...(edge.graphics || {}),
            points
          }
        };
      });
      sceneFlow = { ...sceneFlow, edges: nextEdges };
    }
    if (!nextPoints) {
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Edge.Update", {
      projectId: selectedProjectId,
      edgeId,
      fields: { points: nextPoints }
    });
    if (!response?.snapshot && previous) {
      sceneFlow = previous;
    }
  }

  async function toggleNodeStart() {
    if (!selectedProjectId || !selectedNode || !nodeDraft || selectedNode.isHistory) return;
    const previous = nodeDraft;
    const next = !previous.isStart;
    nodeDraft = { ...previous, isStart: next };
    nodeEditError = "";
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      nodeId: selectedNode.id,
      fields: { isStart: next }
    });
    if (!response) {
      nodeDraft = previous;
      nodeEditError = sceneFlowError || "Failed to update start node.";
    }
  }

  async function toggleSuperNodeStart() {
    if (!selectedProjectId || !nodeEditorTarget || !superNodeDraft || superNodeStartLocked) return;
    const previous = superNodeDraft;
    const next = !previous.isStart;
    superNodeDraft = { ...previous, isStart: next };
    superNodeEditError = "";
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      fields: { isStart: next }
    });
    if (!response) {
      superNodeDraft = previous;
      superNodeEditError = sceneFlowError || "Failed to update start node.";
    }
  }

  function resetNodeDraft() {
    if (!selectedNode) return;
    nodeDraftId = selectedNode.id;
    nodeDraft = {
      name: selectedNode.name ?? "",
      comment: selectedNode.comment ?? "",
      isStart: !!selectedNode.isStart
    };
    nodeEditError = "";
  }

  function resetSuperNodeDraft() {
    if (!nodeEditorTarget) return;
    const draftKey = nodeEditorTarget.id || "__root__";
    const isRoot = !!nodeEditorTarget.isRoot;
    superNodeDraftId = draftKey;
    superNodeDraft = {
      name: nodeEditorTarget.name ?? "",
      isStart: isRoot ? true : !!nodeEditorTarget.isStart
    };
    superNodeEditError = "";
  }

  function resetEdgeDraft() {
    if (!selectedEdge) return;
    edgeDraftId = selectedEdge.id;
    edgeDraft = {
      condition: selectedEdge.condition ?? "",
      probability: selectedEdge.probability !== undefined ? String(selectedEdge.probability) : "",
      timeoutMs: selectedEdge.timeoutMs !== undefined ? String(selectedEdge.timeoutMs) : "",
      altStartText: formatAltStartMap(selectedEdge)
    };
    edgeEditError = "";
  }

  async function applyNodeEdits() {
    nodeEditError = "";
    if (!selectedProjectId || !selectedNode || !nodeDraft) return;
    const fields = {};
    const name = (nodeDraft.name ?? "").trim();
    if (!selectedNode.isHistory) {
      if (!name) {
        nodeEditError = "Name is required.";
        return;
      }
      if (name !== (selectedNode.name ?? "")) {
        fields.name = name;
      }
    } else if (name !== (selectedNode.name ?? "")) {
      nodeEditError = "History node name cannot be changed.";
      return;
    }
    const comment = nodeDraft.comment ?? "";
    if (comment !== (selectedNode.comment ?? "")) {
      fields.comment = comment;
    }
    if (!!nodeDraft.isStart !== !!selectedNode.isStart) {
      fields.isStart = !!nodeDraft.isStart;
    }
    if (!Object.keys(fields).length) {
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      nodeId: selectedNode.id,
      fields
    });
    if (response) {
      nodeDraftId = "";
    }
  }

  async function applySuperNodeEdits() {
    superNodeEditError = "";
    if (!selectedProjectId || !nodeEditorTarget || !superNodeDraft) return;
    const name = (superNodeDraft.name ?? "").trim();
    if (!name) {
      superNodeEditError = "Name is required.";
      return;
    }
    const fields = {};
    if (name !== (nodeEditorTarget.name ?? "")) {
      fields.name = name;
    }
    if (!superNodeStartLocked && !!superNodeDraft.isStart !== !!nodeEditorTarget.isStart) {
      fields.isStart = !!superNodeDraft.isStart;
    }
    if (!Object.keys(fields).length) {
      superNodeEditError = "No changes to apply.";
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      fields
    });
    if (response) {
      superNodeDraftId = "";
    }
  }

  async function applyEdgeEdits() {
    edgeEditError = "";
    if (!selectedProjectId || !selectedEdge || !edgeDraft) return;
    const fields = {};
    const type = selectedEdge.type;
    if (type === "CEDGE" || type === "IEDGE") {
      const condition = (edgeDraft.condition ?? "").trim();
      if (!condition) {
        edgeEditError = "Condition is required.";
        return;
      }
      if (condition !== (selectedEdge.condition ?? "")) {
        fields.condition = condition;
      }
    } else if (type === "PEDGE") {
      // Probability edits are managed via the probability manager.
    } else if (type === "TEDGE") {
      const raw = String(edgeDraft.timeoutMs ?? "").trim();
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isFinite(parsed)) {
        edgeEditError = "Timeout must be a number.";
        return;
      }
      if (parsed < 0) {
        edgeEditError = "Timeout must be >= 0.";
        return;
      }
      if (parsed !== (selectedEdge.timeoutMs ?? 0)) {
        fields.timeoutMs = parsed;
      }
    } else {
      edgeEditError = "Selected edge has no editable fields yet.";
      return;
    }
    if (edgeAltStartEnabled) {
      const nextText = normalizeAltStartText(edgeDraft.altStartText);
      const currentText = normalizeAltStartText(formatAltStartMap(selectedEdge));
      if (nextText !== currentText) {
        const parsed = parseAltStartText(edgeDraft.altStartText);
        if (parsed.error) {
          edgeEditError = parsed.error;
          return;
        }
        fields.altStartMap = parsed.entries;
      }
    }
    if (!Object.keys(fields).length) {
      edgeEditError = "No changes to apply.";
      return;
    }
    const response = await runSceneFlowCommand("SceneFlow.Edge.Update", {
      projectId: selectedProjectId,
      edgeId: selectedEdge.id,
      fields
    });
    if (response) {
      edgeDraftId = "";
    }
  }

  async function deleteSceneFlowSelection() {
    if (!selectedProjectId || sceneFlowBusy) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    const selection = sceneFlowSelection;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    const nodeIds = selectionList.filter((item) => item.type === "node").map((item) => item.id);
    const commentIds = selectionList.filter((item) => item.type === "comment").map((item) => item.id);
    const edgeIds = selectionList.filter((item) => item.type === "edge").map((item) => item.id);

    for (const nodeId of nodeIds) {
      await runSceneFlowCommand("SceneFlow.Node.Delete", { projectId: selectedProjectId, nodeId });
    }
    for (const commentId of commentIds) {
      await runSceneFlowCommand("SceneFlow.Comment.Delete", { projectId: selectedProjectId, commentId });
    }
    if (!nodeIds.length) {
      for (const edgeId of edgeIds) {
        const edge = sceneFlow?.edges?.find((entry) => entry.id === edgeId);
        const payload = { projectId: selectedProjectId, edgeId };
        if (edge?.sourceId) {
          payload.sourceId = edge.sourceId;
        }
        if (edge?.targetId) {
          payload.targetId = edge.targetId;
        }
        await runSceneFlowCommand("SceneFlow.Edge.Delete", payload);
      }
    }
  }

  async function undoSceneFlow() {
    if (!selectedProjectId) return;
    await runSceneFlowCommand("SceneFlow.Undo", { projectId: selectedProjectId });
  }

  async function redoSceneFlow() {
    if (!selectedProjectId) return;
    await runSceneFlowCommand("SceneFlow.Redo", { projectId: selectedProjectId });
  }

  function isEditableTarget(target) {
    if (!target) return false;
    const tag = target.tagName ? target.tagName.toLowerCase() : "";
    if (tag === "input" || tag === "textarea" || tag === "select") return true;
    return !!target.isContentEditable;
  }

  function handleGlobalKeydown(event) {
    if (!event) return;
    if (isEditableTarget(event.target)) return;
    const key = event.key;
    const isMod = event.metaKey || event.ctrlKey;
    if (isMod && key.toLowerCase() === "z") {
      event.preventDefault();
      if (event.shiftKey) {
        redoSceneFlow();
      } else {
        undoSceneFlow();
      }
      return;
    }
    if (isMod && key.toLowerCase() === "y") {
      event.preventDefault();
      redoSceneFlow();
      return;
    }
    if ((key === "Delete" || key === "Backspace") && sceneFlowSelectionList().length) {
      event.preventDefault();
      deleteSceneFlowSelection();
      return;
    }
    if (key === "Escape" && edgeCreateMode) {
      edgeCreateMode = false;
      edgeCreateSourceId = "";
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
    }
  }
</script>

<svelte:window on:keydown={handleGlobalKeydown} />

<main>
  <header class="hero">
    <div>
      <h1>Visual SceneMaker Web UI</h1>
      <p>Revision {infoRevision} Build date {infoBuildDate}</p>
    </div>
    <div class="badge">
      <span class:ok={wsConnected}>WS {wsConnected ? "connected" : "offline"}</span>
    </div>
  </header>

  <section class="panel connect">
    <div class="field">
      <label for="token">Token</label>
      <input id="token" placeholder="Paste token from server log" bind:value={token} />
    </div>
    <div class="row">
      <button type="button" class="primary" on:click={connectAll}>Connect</button>
      <button type="button" class="ghost" on:click={refreshInfo}>Refresh Info</button>
    </div>
    {#if info}
      <div class="info">
        <div>Server: {info.name}</div>
        <div>Port: {info.port}</div>
        <div>Token required: {info.tokenRequired ? "yes" : "no"}</div>
      </div>
    {/if}
    {#if error}
      <p class="error">{error}</p>
    {/if}
    {#if wsError}
      <p class="error">{wsError}</p>
    {/if}
    {#if statusMessage}
      <p class="status">{statusMessage}</p>
    {/if}
  </section>

  <div class="grid">
    <section class="panel">
      <header class="panel-title">
        <h2>Projects</h2>
        <button type="button" class="ghost" on:click={loadProjects}>Refresh</button>
      </header>

      <div class="project-list">
        {#if projects.length === 0}
          <p class="muted">No open projects.</p>
        {/if}
        {#each projects as project}
          <button
            type="button"
            class:selected={project.projectId === selectedProjectId}
            on:click={() => {
              selectedProjectId = project.projectId;
              loadConfig(project.projectId);
            }}
          >
            <span>{project.name}</span>
            <span class="meta">
              {project.dirty ? "*" : ""} {project.runtimeState}
            </span>
          </button>
        {/each}
      </div>

      <div class="actions">
        <button type="button" on:click={() => activateProject(selectedProjectId)} disabled={!selectedProjectId || !wsConnected}>
          Activate
        </button>
        <button type="button" on:click={() => saveProject(selectedProjectId)} disabled={!selectedProjectId}>
          Save
        </button>
        <button type="button" on:click={() => closeProject(selectedProjectId)} disabled={!selectedProjectId}>
          Close
        </button>
      </div>

      <details class="stack" on:toggle={(e) => e.currentTarget.open && loadRecent()}>
        <summary>Open Recent</summary>
        {#each recent as project}
          <button type="button" on:click={() => openProject(project.path)}>
            {project.name}
          </button>
        {/each}
      </details>

      <details class="stack" on:toggle={(e) => e.currentTarget.open && loadSamples()}>
        <summary>Open Sample</summary>
        {#each samples as project}
          <button type="button" on:click={() => openProject(project.path)}>
            {project.name}
          </button>
        {/each}
      </details>

      <details class="stack" on:toggle={(e) => e.currentTarget.open && loadTutorials()}>
        <summary>Open Tutorial</summary>
        {#each tutorials as project}
          <button type="button" on:click={() => openProject(project.path)}>
            {project.name}
          </button>
        {/each}
      </details>

      <div class="stack">
        <label for="open-path">Open by path</label>
        <div class="row">
          <input id="open-path" placeholder="/abs/path/to/project" bind:value={openPath} />
          <button type="button" on:click={() => openProject(openPath)}>Open</button>
        </div>
      </div>

      <div class="stack">
        <label for="new-project-name">New project name</label>
        <input id="new-project-name" placeholder="Project name" bind:value={newName} />
        <label for="new-project-base">Base dir (optional)</label>
        <input id="new-project-base" placeholder="Base dir (optional)" bind:value={newBaseDir} />
        <button type="button" on:click={createProject}>Create</button>
      </div>

      <div class="stack">
        <label for="save-as-path">Save as path</label>
        <div class="row">
          <input id="save-as-path" placeholder="/abs/path/to/project" bind:value={saveAsPath} />
          <button type="button" on:click={() => saveAsProject(selectedProjectId)} disabled={!selectedProjectId}>
            Save As
          </button>
        </div>
      </div>
    </section>

    <section class="panel">
      <header class="panel-title">
        <h2>Preferences</h2>
        <button type="button" class="ghost" on:click={loadPreferences}>Reload</button>
      </header>
      <input class="search" placeholder="Filter preferences" bind:value={prefFilter} />
      <div class="kv-list">
        {#each filteredPrefs as [key, value]}
          <div class="kv-row">
            <span>{key}</span>
            <input bind:value={prefDraft[key]} />
          </div>
        {/each}
      </div>
      <div class="actions">
        <button type="button" class="primary" on:click={applyPreferences} disabled={!wsConnected}>Apply</button>
      </div>
    </section>

    <section class="panel">
      <header class="panel-title">
        <h2>Project Config</h2>
        <div class="badge subtle">
          {selectedProject ? selectedProject.name : "No project selected"}
        </div>
      </header>
      <input class="search" placeholder="Filter config" bind:value={configFilter} disabled={!selectedProject} />
      <div class="kv-list">
        {#if !selectedProject}
          <p class="muted">Select a project to edit config.</p>
        {:else}
          {#each filteredConfig as [key, value]}
            <div class="kv-row">
              <span>{key}</span>
              <input bind:value={configDraft[key]} />
            </div>
          {/each}
        {/if}
      </div>
      <div class="actions">
        <button type="button" class="primary" on:click={applyConfig} disabled={!selectedProject || !wsConnected}>
          Apply
        </button>
        {#if configSaved !== null}
          <span class="muted">{configSaved ? "Saved" : "Pending save"}</span>
        {/if}
      </div>
    </section>

    <section class="panel sceneflow-panel">
      <header class="panel-title">
        <h2>SceneFlow</h2>
        <div class="badge subtle">
          {selectedProject ? selectedProject.name : "No project selected"}
        </div>
      </header>
      <div class="sceneflow-toolbar">
        <button
          type="button"
          class="ghost"
          on:click={() => loadSceneFlow(selectedProjectId)}
          disabled={!selectedProject || sceneFlowLoading}
        >
          Reload
        </button>
        <button
          type="button"
          class="ghost"
          on:click={deleteSceneFlowSelection}
          disabled={!sceneFlowSelection || sceneFlowBusy}
        >
          Delete
        </button>
        <button type="button" class="ghost" on:click={undoSceneFlow} disabled={!wsConnected || sceneFlowBusy}>
          Undo
        </button>
        <button type="button" class="ghost" on:click={redoSceneFlow} disabled={!wsConnected || sceneFlowBusy}>
          Redo
        </button>
        <div class="runtime-controls">
          <span class={`runtime-state ${runtimeState}`}>{runtimeStateLabel}</span>
          <button
            type="button"
            class="ghost icon-button"
            on:click={() => runRuntimeCommand("Runtime.Play")}
            disabled={!runtimeCanPlay}
            aria-label={runtimePlayLabel}
            title={runtimePlayLabel}
          >
            <IconStart className="icon" />
          </button>
          <button
            type="button"
            class="ghost icon-button"
            on:click={() => runRuntimeCommand("Runtime.Pause")}
            disabled={!runtimeCanPause}
            aria-label="Pause"
            title="Pause"
          >
            <IconPause className="icon" />
          </button>
          <button
            type="button"
            class="ghost icon-button danger"
            on:click={() => runRuntimeCommand("Runtime.Stop")}
            disabled={!runtimeCanStop}
            aria-label="Stop"
            title="Stop"
          >
            <IconStop className="icon" />
          </button>
        </div>
        {#if sceneFlowPathNodes.length || sceneFlow?.path?.length}
          <div class="sceneflow-breadcrumbs-row">
            {#if sceneFlowPathNodes.length}
              <nav class="sceneflow-breadcrumbs" aria-label="SceneFlow path">
                {#each sceneFlowPathNodes as node, idx}
                  {#if idx > 0}
                    <span class="crumb-sep">/</span>
                  {/if}
                  {#if idx < sceneFlowPathNodes.length - 1}
                    <button
                      type="button"
                      class="crumb"
                      on:click={() => navigateSceneFlow(node.id || SCENEFLOW_ROOT_ID)}
                      disabled={!wsConnected || sceneFlowBusy}
                    >
                      {node.name || "SceneFlow"}
                    </button>
                  {:else}
                    <span class="crumb-current">{node.name || "SceneFlow"}</span>
                  {/if}
                {/each}
              </nav>
            {:else}
              <span class="muted">Path: {sceneFlow.path.join(" / ")}</span>
            {/if}
          </div>
        {/if}
      </div>
      {#if !selectedProject}
        <p class="muted">Select a project to view the SceneFlow graph.</p>
      {:else if sceneFlow}
        <div class="sceneflow-layout">
          <aside class="sceneflow-blocks">
            <div class="blocks-section blocks-section--icons">
              <div class="blocks-grid blocks-grid--icons">
                <button
                  type="button"
                  class="block-icon"
                  title="Supernode"
                  aria-label="Supernode"
                  draggable="true"
                  on:click={() => createSceneFlowNode("Super")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "node", nodeType: "Super" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <rect x="4" y="4" width="16" height="16" rx="2" />
                    <rect x="8" y="8" width="8" height="8" rx="1" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  title="Node"
                  aria-label="Node"
                  draggable="true"
                  on:click={() => createSceneFlowNode("Basic")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "node", nodeType: "Basic" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <circle cx="12" cy="12" r="7" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  title="Comment"
                  aria-label="Comment"
                  draggable="true"
                  on:click={createSceneFlowComment}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "comment" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M6 6h12a2 2 0 0 1 2 2v7a2 2 0 0 1-2 2H10l-4 3v-3H6a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2z" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "EEDGE"}
                  title="Epsilon edge"
                  aria-label="Epsilon edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("EEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "EEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">E</text>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "PEDGE"}
                  title="Probabilistic edge"
                  aria-label="Probabilistic edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("PEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "PEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">P</text>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "FEDGE"}
                  title="Fork edge"
                  aria-label="Fork edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("FEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "FEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">F</text>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "CEDGE"}
                  title="Conditional edge"
                  aria-label="Conditional edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("CEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "CEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">C</text>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "TEDGE"}
                  title="Timeout edge"
                  aria-label="Timeout edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("TEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "TEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">T</text>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "IEDGE"}
                  title="Interruptive edge"
                  aria-label="Interruptive edge"
                  draggable="true"
                  on:click={() => startEdgeCreate("IEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "IEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 12h11" />
                    <path d="M11 7l5 5-5 5" />
                    <text class="block-icon-text" x="5" y="9">I</text>
                  </svg>
                </button>
              </div>
              {#if edgeCreateMode}
                <p class="muted edge-hint">
                  {edgeCreateSourceId
                    ? `Edge ${edgeTypeLabel(edgeCreateType)}: pick target node`
                    : `Edge ${edgeTypeLabel(edgeCreateType)}: pick source node`}
                </p>
              {/if}
            </div>
            <div class="blocks-section blocks-section--scenes">
              <div class="block-section-title">Scenes</div>
              <div class="scene-selector">
                <select
                  bind:value={scriptScenesLanguage}
                  disabled={!selectedProject}
                  aria-label="Scene language"
                >
                  {#each sceneLanguageOptions as option}
                    <option value={option.value}>{option.label}</option>
                  {/each}
                </select>
                <button
                  type="button"
                  class="ghost"
                  on:click={() => loadScriptScenes(selectedProjectId)}
                  disabled={!selectedProject}
                >
                  Reload
                </button>
              </div>
              <input
                class="search"
                placeholder="Filter scenes"
                bind:value={scriptScenesFilter}
                disabled={!selectedProject}
              />
              {#if !selectedProject}
                <p class="muted">Select a project to view scenes.</p>
              {:else if scriptScenesLoading}
                <p class="muted">Loading scenes...</p>
              {:else if scriptScenesError}
                <p class="error">{scriptScenesError}</p>
              {:else if filteredScriptScenes.length === 0}
                <p class="muted">No scenes found.</p>
              {:else}
                <div class="scene-list">
                  {#each filteredScriptScenes as lang}
                    <div class="scene-group">
                      <div class="scene-group-title">
                        <span>{sceneLanguageLabel(lang.language)}</span>
                        <span class="scene-count">{sceneGroupTotal(lang.groups)}</span>
                      </div>
                      <div class="scene-items" role="list">
                        {#each lang.groups as group}
                          <div class="scene-item" role="listitem">
                            <div class="scene-item-main">
                              <span
                                class="scene-drag-handle"
                                draggable="true"
                                on:dragstart={(event) => startSceneDrag(event, group, lang.language)}
                                role="button"
                                tabindex="0"
                                aria-label={`Drag scene ${group.name}`}
                                title="Drag scene"
                              >
                                <svg
                                  class="scene-drag-icon"
                                  xmlns="http://www.w3.org/2000/svg"
                                  fill="none"
                                  viewBox="0 0 24 24"
                                  stroke-width="1.5"
                                  stroke="currentColor"
                                  aria-hidden="true"
                                >
                                  <path
                                    stroke-linecap="round"
                                    stroke-linejoin="round"
                                    d="M19.5 14.25v-2.625a3.375 3.375 0 0 0-3.375-3.375h-1.5A1.125 1.125 0 0 1 13.5 7.125v-1.5a3.375 3.375 0 0 0-3.375-3.375H8.25m0 12.75h7.5m-7.5 3H12M10.5 2.25H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V11.25a9 9 0 0 0-9-9Z"
                                  />
                                </svg>
                              </span>
                              <span class="scene-name">{group.name}</span>
                            </div>
                            <span class="scene-count">{group.count}</span>
                          </div>
                        {/each}
                      </div>
                    </div>
                  {/each}
                </div>
              {/if}
            </div>
          </aside>
          <div class="sceneflow-container" style={sceneFlowFrameStyle} bind:this={sceneFlowContainerEl}>
            <div class="sceneflow-scroll">
              <SceneFlowView
                bind:this={sceneFlowRef}
                bind:zoomLevel={sceneFlowZoom}
                bind:worldBox={sceneFlowWorldBox}
                bind:viewBoxState={sceneFlowViewBox}
                bind:selection={sceneFlowSelection}
                bind:multiSelection={sceneFlowMultiSelection}
                config={configDraft}
                snapshot={sceneFlow}
                activityNodes={activityNodeIds}
                activityEdges={activityEdgeList}
                onNavigate={navigateSceneFlow}
                onNodeMove={moveSceneFlowNode}
                onCommentUpdate={updateSceneFlowComment}
                onEdgeControlUpdate={updateSceneFlowEdgeControl}
                onDeleteSelection={deleteSceneFlowSelection}
                onUndo={undoSceneFlow}
                onRedo={redoSceneFlow}
                snapToGrid={sceneFlowSnap}
                edgeCreateMode={edgeCreateMode}
                edgeCreateSourceId={edgeCreateSourceId}
                edgeCreateType={edgeCreateType}
                onEdgePick={handleEdgePick}
                onSceneDrop={handleSceneFlowSceneDrop}
                sceneDragType={SCENE_DRAG_TYPE}
                onBlockDrop={handleBlockDrop}
                blockDragType={BLOCK_DRAG_TYPE}
                showCommandText={sceneFlowShowCmdText}
                onCommandOpen={openCmdDialog}
                onCopySelection={copySceneFlowSelection}
                onPasteSelection={pasteSceneFlowSelection}
                onCutSelection={cutSceneFlowSelection}
                onDuplicateSelection={duplicateSceneFlowSelection}
              />
            </div>
            <div class="sceneflow-toggles">
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowSnap}
                on:click={() => (sceneFlowSnap = !sceneFlowSnap)}
                aria-pressed={sceneFlowSnap}
                disabled={!sceneFlow}
              >
                grid snap
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={varBadgeState.visible}
                on:click={toggleVarBadges}
                aria-pressed={varBadgeState.visible}
                disabled={!sceneFlow}
              >
                show vars
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowShowCmdText}
                on:click={() => (sceneFlowShowCmdText = !sceneFlowShowCmdText)}
                aria-pressed={sceneFlowShowCmdText}
              >
                show cmds
              </button>
            </div>
            {#if varBadgeState.visible}
              <div
                class="sceneflow-var-badge"
                style:left={`${varBadgeState.global?.x ?? 0}px`}
                style:top={`${varBadgeState.global?.y ?? 0}px`}
                style:width={`${varBadgeState.global?.w ?? VAR_BADGE_MIN_WIDTH}px`}
                style:height={`${varBadgeState.global?.h ?? VAR_BADGE_MIN_HEIGHT}px`}
                data-badge="global"
                on:pointerdown|stopPropagation={(event) => handleVarBadgePointerDown(event, "global")}
                on:mousedown|stopPropagation={(event) => handleVarBadgePointerDown(event, "global")}
                role="presentation"
              >
                <div class="sceneflow-var-title">
                  <span>Variables</span>
                </div>
                <div class="sceneflow-var-content">
                  <div class="sceneflow-var-list">
                    {#if runtimeError}
                      <span class="error">{runtimeError}</span>
                    {:else if runtimeLoading}
                      <span class="muted">Loading...</span>
                    {:else if runtimeDisplayGlobals.length === 0}
                      <span class="muted">No variables.</span>
                    {:else}
                      {#each runtimeDisplayGlobals as variable}
                        <div class="sceneflow-var-row" title={runtimeVarLine(variable)}>
                          {runtimeVarLine(variable) || variable?.name || "Variable"}
                        </div>
                      {/each}
                    {/if}
                  </div>
                </div>
                <svg
                  class="var-resize-handle"
                  viewBox={`0 0 ${VAR_BADGE_HANDLE_SIZE} ${VAR_BADGE_HANDLE_SIZE}`}
                  aria-hidden="true"
                  on:pointerdown|stopPropagation={(event) => startVarBadgeResize(event, "global")}
                  on:mousedown|stopPropagation={(event) => startVarBadgeResize(event, "global")}
                >
                  <path class="var-resize-fill" d={VAR_BADGE_HANDLE_PATH} />
                </svg>
              </div>
              {#if showLocalVarBadge}
                <div
                  class="sceneflow-var-badge"
                  style:left={`${varBadgeState.local?.x ?? 0}px`}
                  style:top={`${varBadgeState.local?.y ?? 0}px`}
                  style:width={`${varBadgeState.local?.w ?? VAR_BADGE_MIN_WIDTH}px`}
                  style:height={`${varBadgeState.local?.h ?? VAR_BADGE_MIN_HEIGHT}px`}
                  data-badge="local"
                  on:pointerdown|stopPropagation={(event) => handleVarBadgePointerDown(event, "local")}
                  on:mousedown|stopPropagation={(event) => handleVarBadgePointerDown(event, "local")}
                  role="presentation"
                >
                  <div class="sceneflow-var-title">
                    <span>Local variables</span>
                    <span class="muted">{currentSuperName}</span>
                  </div>
                  <div class="sceneflow-var-content">
                    <div class="sceneflow-var-list">
                      {#if runtimeError}
                        <span class="error">{runtimeError}</span>
                      {:else if runtimeLoading}
                        <span class="muted">Loading...</span>
                      {:else if runtimeLocals.length === 0}
                        <span class="muted">No local variables.</span>
                      {:else}
                        {#each runtimeLocals as variable}
                          <div class="sceneflow-var-row" title={runtimeVarLine(variable)}>
                            {runtimeVarLine(variable) || variable?.name || "Variable"}
                          </div>
                        {/each}
                      {/if}
                    </div>
                  </div>
                  <svg
                    class="var-resize-handle"
                    viewBox={`0 0 ${VAR_BADGE_HANDLE_SIZE} ${VAR_BADGE_HANDLE_SIZE}`}
                    aria-hidden="true"
                    on:pointerdown|stopPropagation={(event) => startVarBadgeResize(event, "local")}
                    on:mousedown|stopPropagation={(event) => startVarBadgeResize(event, "local")}
                  >
                    <path class="var-resize-fill" d={VAR_BADGE_HANDLE_PATH} />
                  </svg>
                </div>
              {/if}
            {/if}
            <div class="sceneflow-navigator">
              <div class="sceneflow-zoom-controls">
                {#if sceneFlow}
                  <span class="sceneflow-zoom-label">{Math.round(sceneFlowZoom * 100)}%</span>
                {/if}
                <button
                  type="button"
                  class="sceneflow-zoom-button"
                  on:click={() => sceneFlowRef?.zoomIn()}
                  disabled={!sceneFlow}
                  aria-label="Zoom in"
                  title="Zoom in"
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <line x1="12" y1="5" x2="12" y2="19" />
                    <line x1="5" y1="12" x2="19" y2="12" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="sceneflow-zoom-button"
                  on:click={() => sceneFlowRef?.zoomOut()}
                  disabled={!sceneFlow}
                  aria-label="Zoom out"
                  title="Zoom out"
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <line x1="5" y1="12" x2="19" y2="12" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="sceneflow-zoom-button"
                  on:click={() => sceneFlowRef?.fitToView()}
                  disabled={!sceneFlow}
                  aria-label="Fit to view"
                  title="Fit to view"
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5" />
                  </svg>
                </button>
              </div>
              <SceneFlowMiniMap
                snapshot={sceneFlow}
                worldBox={sceneFlowWorldBox}
                viewBox={sceneFlowViewBox}
                onCenter={(x, y) => sceneFlowRef?.centerOn(x, y)}
              />
            </div>
          </div>
          <aside class="sceneflow-inspector">
            {#if multiSelectionActive}
              <h3 class="inspector-title">Selection ({selectionList.length})</h3>
              <div class="inspector-meta">
                <div class="inspector-row">
                  <span>Nodes</span>
                  <span>
                    {selectionNodes.length
                      ? `${selectionNodes.length}${selectionNodeSummary ? ` (${selectionNodeSummary})` : ""}`
                      : "0"}
                  </span>
                </div>
                <div class="inspector-row">
                  <span>Edges</span>
                  <span>
                    {selectionEdges.length
                      ? `${selectionEdges.length}${selectionEdgeSummary ? ` (${selectionEdgeSummary})` : ""}`
                      : "0"}
                  </span>
                </div>
                <div class="inspector-row">
                  <span>Comments</span>
                  <span>{selectionComments.length || 0}</span>
                </div>
                {#if selectionNodes.length}
                  <div class="inspector-row">
                    <span>Start nodes</span>
                    <span>{selectionStartCount ? selectionStartCount : "None"}</span>
                  </div>
                {/if}
              </div>
              {#if selectionNodePreview.length}
                <div class="definition-section">
                  <header class="definition-header">
                    <h4>Nodes</h4>
                    <span class="muted">{selectionNodes.length}</span>
                  </header>
                  <div class="definition-list">
                    {#each selectionNodePreview as node}
                      <div class="definition-row">
                        <span>{displayNodeName(node)}</span>
                        <span class="muted">{node.type === "Super" ? "Super" : "Basic"}</span>
                      </div>
                    {/each}
                    {#if selectionNodeRemaining > 0}
                      <div class="definition-row muted">+ {selectionNodeRemaining} more</div>
                    {/if}
                  </div>
                </div>
              {/if}
              {#if selectionCommentPreview.length}
                <div class="definition-section">
                  <header class="definition-header">
                    <h4>Comments</h4>
                    <span class="muted">{selectionComments.length}</span>
                  </header>
                  <div class="definition-list">
                    {#each selectionCommentPreview as comment, index}
                      <div class="definition-row">
                        <span>{commentLabel(comment, index)}</span>
                        <span class="muted">
                          {comment.rect?.w ?? 0} x {comment.rect?.h ?? 0}
                        </span>
                      </div>
                    {/each}
                    {#if selectionCommentRemaining > 0}
                      <div class="definition-row muted">+ {selectionCommentRemaining} more</div>
                    {/if}
                  </div>
                </div>
              {/if}
              <div class="actions">
                <button type="button" class="ghost" on:click={copySceneFlowSelection} disabled={!wsConnected || sceneFlowBusy}>
                  Copy
                </button>
                <button type="button" class="ghost" on:click={cutSceneFlowSelection} disabled={!wsConnected || sceneFlowBusy}>
                  Cut
                </button>
                <button
                  type="button"
                  class="ghost"
                  on:click={duplicateSceneFlowSelection}
                  disabled={!wsConnected || sceneFlowBusy}
                >
                  Duplicate
                </button>
                <button
                  type="button"
                  class="ghost"
                  on:click={deleteSceneFlowSelection}
                  disabled={!wsConnected || sceneFlowBusy}
                >
                  Delete
                </button>
              </div>
            {:else if sceneFlowSelection?.type === "node" && selectedNode && nodeDraft}
              <div class="node-header">
                <input
                  class="node-title-input"
                  aria-label="Node name"
                  bind:value={nodeDraft.name}
                  disabled={selectedNode.isHistory}
                  on:change={applyNodeEdits}
                  on:keydown={(event) => {
                    if (event.key === "Enter") {
                      event.preventDefault();
                      applyNodeEdits();
                    }
                  }}
                />
                <button
                  type="button"
                  class="ghost icon-button start-toggle"
                  class:active={nodeDraft.isStart}
                  on:click={toggleNodeStart}
                  disabled={selectedNode.isHistory}
                  aria-pressed={nodeDraft.isStart}
                  aria-label="Toggle start node"
                  title="Start node"
                >
                  <IconStart className="icon" />
                </button>
              </div>
              {#if nodeEditError}
                <p class="error">{nodeEditError}</p>
              {/if}
            {:else if sceneFlowSelection?.type === "edge" && selectedEdge && edgeDraft}
              <h3 class="inspector-title">Edge {selectedEdge.sourceId} → {selectedEdge.targetId}</h3>
              <div class="stack">
                {#if selectedEdge.type === "CEDGE" || selectedEdge.type === "IEDGE"}
                  <label for="edge-condition">Condition</label>
                  <input id="edge-condition" bind:value={edgeDraft.condition} />
                {:else if selectedEdge.type === "PEDGE"}
                  <div class="prob-manager">
                    <div class="prob-header">
                      <span>Probabilities</span>
                      <span class="prob-sum" class:ok={pEdgeValid && pEdgeSum === 100}>
                        {pEdgeValid ? `Sum ${pEdgeSum}%` : "Sum --"}
                      </span>
                    </div>
                    <div class="def-table prob-table">
                      <div class="def-list prob-list">
                        {#if pEdgeDrafts.length === 0}
                          <div class="def-empty">No probability edges yet.</div>
                        {:else}
                          {#each pEdgeDrafts as draft}
                            <div class="def-row prob-row" class:selected={draft.edgeId === selectedEdge.id}>
                              <span class="prob-label">{draft.label}</span>
                              <input
                                class="prob-input"
                                type="number"
                                min="0"
                                max="100"
                                value={draft.value}
                                on:input={(event) => updatePEdgeDraft(draft.edgeId, event.currentTarget.value)}
                              />
                            </div>
                          {/each}
                        {/if}
                      </div>
                      <div class="def-actions prob-actions">
                        <button type="button" class="ghost" on:click={normalizePEdgeDrafts} disabled={!pEdgeDrafts.length}>
                          Normalize
                        </button>
                        <button type="button" class="ghost" on:click={uniformPEdgeDrafts} disabled={!pEdgeDrafts.length}>
                          Uniform
                        </button>
                        <button
                          type="button"
                          class="primary"
                          on:click={applyPEdgeGroup}
                          disabled={!wsConnected || sceneFlowBusy || !pEdgeDirty || !pEdgeValid || pEdgeSum !== 100}
                        >
                          Apply
                        </button>
                        <button type="button" class="ghost" on:click={syncPEdgeDrafts} disabled={!pEdgeDirty}>
                          Reset
                        </button>
                      </div>
                    </div>
                    {#if pEdgeError}
                      <p class="error">{pEdgeError}</p>
                    {/if}
                  </div>
                {:else if selectedEdge.type === "TEDGE"}
                  <label for="edge-timeout">Timeout (ms)</label>
                  <input id="edge-timeout" type="number" min="0" bind:value={edgeDraft.timeoutMs} />
                {:else}
                  <p class="muted">No editable fields for this edge type yet.</p>
                {/if}
                {#if edgeAltStartEnabled}
                  <label for="edge-alt-start">Alt start nodes (start/alt per line)</label>
                  <textarea
                    id="edge-alt-start"
                    rows="4"
                    placeholder="N1/N2"
                    bind:value={edgeDraft.altStartText}
                  ></textarea>
                {:else}
                  <p class="muted">Alt start nodes require a super node target.</p>
                {/if}
              </div>
              <div class="actions">
                <button type="button" class="primary" on:click={applyEdgeEdits} disabled={!edgeDirty || !wsConnected}>
                  Apply
                </button>
                <button type="button" class="ghost" on:click={resetEdgeDraft} disabled={!edgeDirty}>
                  Reset
                </button>
              </div>
              {#if edgeEditError}
                <p class="error">{edgeEditError}</p>
              {/if}
            {:else if sceneFlowSelection?.type === "comment" && selectedComment}
              <h3 class="inspector-title">Comment</h3>
              <div class="inspector-meta">
                <div class="inspector-row">
                  <span>Position</span>
                  <span>{selectedComment.rect?.x ?? 0}, {selectedComment.rect?.y ?? 0}</span>
                </div>
                <div class="inspector-row">
                  <span>Size</span>
                  <span>{selectedComment.rect?.w ?? 0} x {selectedComment.rect?.h ?? 0}</span>
                </div>
              </div>
            {:else}
              {#if superNodeDraft}
                <div class="node-header">
                  <input
                    class="node-title-input"
                    aria-label="Node name"
                    bind:value={superNodeDraft.name}
                  />
                  <button
                    type="button"
                    class="ghost icon-button start-toggle"
                    class:active={superNodeDraft.isStart}
                    on:click={toggleSuperNodeStart}
                    disabled={superNodeStartLocked}
                    aria-pressed={superNodeDraft.isStart}
                    aria-label="Toggle start node"
                    title={superNodeStartLocked ? "Start node (locked)" : "Start node"}
                  >
                    <IconStart className="icon" />
                  </button>
                </div>
              {:else}
                <h3 class="inspector-title">{currentSuperName}</h3>
              {/if}
              <div class="inspector-meta">
                <div class="inspector-row">
                  <span>Start nodes</span>
                  <span>{startNodes.length ? startNodes.map(displayNodeName).join(", ") : "None"}</span>
                </div>
              </div>
              {#if superNodeDirty}
                <div class="actions">
                  <button type="button" class="primary" on:click={applySuperNodeEdits} disabled={!wsConnected || sceneFlowBusy}>
                    Apply
                  </button>
                  <button type="button" class="ghost" on:click={resetSuperNodeDraft} disabled={!superNodeDirty}>
                    Reset
                  </button>
                </div>
              {/if}
              {#if superNodeEditError}
                <p class="error">{superNodeEditError}</p>
              {/if}
            {/if}

            {#if nodeEditorTarget && !multiSelectionActive}
              <div class="definition-section">
                <header class="definition-header">
                  <h4>Type definitions</h4>
                  <span class="muted">{nodeEditorTypeDefs.length} total</span>
                </header>
                <div class="def-table">
                  <div class="def-list">
                    {#if nodeEditorTypeDefs.length === 0}
                      <div class="def-empty">No type definitions yet.</div>
                    {:else}
                      {#each nodeEditorTypeDefs as def, index}
                        <button
                          type="button"
                          class="def-row"
                          class:selected={typeDefSelectedIndex === index}
                          on:click={() => selectTypeDef(index)}
                          aria-pressed={typeDefSelectedIndex === index}
                        >
                          <span class="def-line">
                            {typeDefLine(def)}
                          </span>
                        </button>
                      {/each}
                    {/if}
                  </div>
                  <div class="def-actions">
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={startTypeDefAdd}
                      disabled={!wsConnected || sceneFlowBusy}
                      aria-label="Add type definition"
                      title="Add type definition"
                    >
                      <IconPlus className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={deleteSelectedTypeDef}
                      disabled={!wsConnected || sceneFlowBusy || typeDefSelectedIndex === null}
                      aria-label="Remove type definition"
                      title="Remove type definition"
                    >
                      <IconTrash className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={editSelectedTypeDef}
                      disabled={!wsConnected || sceneFlowBusy || typeDefSelectedIndex === null}
                      aria-label="Edit type definition"
                      title="Edit type definition"
                    >
                      <IconPencil className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedTypeDef(-1)}
                      disabled={!wsConnected || sceneFlowBusy || typeDefSelectedIndex === null || typeDefSelectedIndex === 0}
                      aria-label="Move type definition up"
                      title="Move up"
                    >
                      <IconChevronUp className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedTypeDef(1)}
                      disabled={
                        !wsConnected ||
                        sceneFlowBusy ||
                        typeDefSelectedIndex === null ||
                        typeDefSelectedIndex === nodeEditorTypeDefs.length - 1
                      }
                      aria-label="Move type definition down"
                      title="Move down"
                    >
                      <IconChevronDown className="icon" />
                    </button>
                  </div>
                </div>
              </div>

              <div class="definition-section">
                <header class="definition-header">
                  <h4>Variable definitions</h4>
                  <span class="muted">{nodeEditorVarDefs.length} total</span>
                </header>
                <div class="var-table">
                  <div class="var-list">
                    {#if nodeEditorVarDefs.length === 0}
                      <div class="var-empty">No variable definitions yet.</div>
                    {:else}
                      {#each nodeEditorVarDefs as def, index}
                        <button
                          type="button"
                          class="var-row"
                          class:selected={varDefSelectedIndex === index}
                          on:click={() => selectVarDef(index)}
                          aria-pressed={varDefSelectedIndex === index}
                        >
                          <span class="var-line">
                            {varDefLine(def)}
                          </span>
                        </button>
                      {/each}
                    {/if}
                  </div>
                  <div class="var-actions">
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={startVarDefAdd}
                      disabled={!wsConnected || sceneFlowBusy}
                      aria-label="Add variable definition"
                      title="Add variable definition"
                    >
                      <IconPlus className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={deleteSelectedVarDef}
                      disabled={!wsConnected || sceneFlowBusy || varDefSelectedIndex === null}
                      aria-label="Remove variable definition"
                      title="Remove variable definition"
                    >
                      <IconTrash className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={editSelectedVarDef}
                      disabled={!wsConnected || sceneFlowBusy || varDefSelectedIndex === null}
                      aria-label="Edit variable definition"
                      title="Edit variable definition"
                    >
                      <IconPencil className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedVarDef(-1)}
                      disabled={!wsConnected || sceneFlowBusy || varDefSelectedIndex === null || varDefSelectedIndex === 0}
                      aria-label="Move variable definition up"
                      title="Move up"
                    >
                      <IconChevronUp className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedVarDef(1)}
                      disabled={
                        !wsConnected ||
                        sceneFlowBusy ||
                        varDefSelectedIndex === null ||
                        varDefSelectedIndex === nodeEditorVarDefs.length - 1
                      }
                      aria-label="Move variable definition down"
                      title="Move down"
                    >
                      <IconChevronDown className="icon" />
                    </button>
                  </div>
                </div>

              </div>

              <div class="definition-section">
                <header class="definition-header">
                  <h4>Command executions</h4>
                  <span class="muted">{nodeEditorCommands.length} total</span>
                </header>
                <div class="def-table">
                  <div
                    class="def-list"
                    role="list"
                    aria-label="Command executions"
                    on:dragover={handleSceneDropOver}
                    on:drop={handleCommandSceneDrop}
                  >
                    {#if nodeEditorCommands.length === 0}
                      <div class="def-empty">No commands yet.</div>
                    {:else}
                      {#each nodeEditorCommands as cmd, index}
                        <button
                          type="button"
                          class="def-row"
                          class:selected={cmdSelectedIndex === index}
                          on:click={() => selectCmd(index)}
                          on:dblclick={() => startCmdEdit(index)}
                          aria-pressed={cmdSelectedIndex === index}
                        >
                          <span class="def-line">
                            {cmdLine(cmd) || "Command"}
                          </span>
                        </button>
                      {/each}
                    {/if}
                  </div>
                  <div class="def-actions">
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={startCmdAdd}
                      disabled={!wsConnected || sceneFlowBusy}
                      aria-label="Add command"
                      title="Add command"
                    >
                      <IconPlus className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={deleteSelectedCmd}
                      disabled={!wsConnected || sceneFlowBusy || cmdSelectedIndex === null}
                      aria-label="Remove command"
                      title="Remove command"
                    >
                      <IconTrash className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={editSelectedCmd}
                      disabled={!wsConnected || sceneFlowBusy || cmdSelectedIndex === null}
                      aria-label="Edit command"
                      title="Edit command"
                    >
                      <IconPencil className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedCmd(-1)}
                      disabled={!wsConnected || sceneFlowBusy || cmdSelectedIndex === null || cmdSelectedIndex === 0}
                      aria-label="Move command up"
                      title="Move up"
                    >
                      <IconChevronUp className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedCmd(1)}
                      disabled={
                        !wsConnected ||
                        sceneFlowBusy ||
                        cmdSelectedIndex === null ||
                        cmdSelectedIndex === nodeEditorCommands.length - 1
                      }
                      aria-label="Move command down"
                      title="Move down"
                    >
                      <IconChevronDown className="icon" />
                    </button>
                  </div>
                </div>
              </div>
            {/if}
          </aside>
        </div>
      {:else}
        <p class="muted">No SceneFlow data loaded yet.</p>
      {/if}
      {#if sceneFlowError || runtimeError || edgeCreateMode || sceneFlowLoading || sceneFlow?.revision}
        <div class="sceneflow-status">
          <div class="sceneflow-status-left">
            {#if sceneFlowError}
              <span class="error">{sceneFlowError}</span>
            {/if}
            {#if runtimeError}
              <span class="error">{runtimeError}</span>
            {/if}
            {#if edgeCreateMode}
              <span class="muted">
                Edge {edgeTypeLabel(edgeCreateType)}: {edgeCreateSourceId ? `source ${edgeCreateSourceId} → pick target` : "pick source node"}
              </span>
            {/if}
            {#if sceneFlowLoading}
              <span class="muted">Loading...</span>
            {/if}
          </div>
          <div class="sceneflow-status-right">
            {#if sceneFlow?.revision}
              <span class="muted">rev {sceneFlow.revision}</span>
            {/if}
          </div>
        </div>
      {/if}
    </section>

    <section class="panel script-panel panel-wide">
      <header class="panel-title">
        <h2>Scene Script</h2>
        <div class="badge subtle">
          {selectedProject ? selectedProject.name : "No project selected"}
        </div>
      </header>
      <div class="script-toolbar">
        <button type="button" class="ghost" on:click={() => loadScript(selectedProjectId)} disabled={!selectedProject}>
          Reload
        </button>
        <button
          type="button"
          class="primary"
          on:click={applyScript}
          disabled={!selectedProject || !wsConnected || !scriptDirty}
        >
          Apply
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => scriptEditorRef?.openSearch()}
          disabled={!selectedProject}
        >
          Search
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => scriptEditorRef?.jumpToPreviousDiagnostic()}
          disabled={!selectedProject || scriptDiagnostics.length === 0}
        >
          Prev issue
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => scriptEditorRef?.jumpToNextDiagnostic()}
          disabled={!selectedProject || scriptDiagnostics.length === 0}
        >
          Next issue
        </button>
        {#if scriptVersion !== null}
          <span class="muted">v{scriptVersion}</span>
        {/if}
        {#if scriptDirty}
          <span class="muted">Unsaved edits</span>
        {/if}
        <div class="runtime-controls">
          <span class={`runtime-state ${runtimeState}`}>{runtimeStateLabel}</span>
          <button
            type="button"
            class="ghost icon-button"
            on:click={() => runRuntimeCommand("Runtime.Play")}
            disabled={!runtimeCanPlay}
            aria-label={runtimePlayLabel}
            title={runtimePlayLabel}
          >
            <IconStart className="icon" />
          </button>
          <button
            type="button"
            class="ghost icon-button"
            on:click={() => runRuntimeCommand("Runtime.Pause")}
            disabled={!runtimeCanPause}
            aria-label="Pause"
            title="Pause"
          >
            <IconPause className="icon" />
          </button>
          <button
            type="button"
            class="ghost icon-button danger"
            on:click={() => runRuntimeCommand("Runtime.Stop")}
            disabled={!runtimeCanStop}
            aria-label="Stop"
            title="Stop"
          >
            <IconStop className="icon" />
          </button>
        </div>
      </div>
      {#if !selectedProject}
        <p class="muted">Select a project to edit the scene script.</p>
      {:else}
        <ScriptEditor
          bind:this={scriptEditorRef}
          value={scriptDraft}
          readOnly={!selectedProject}
          hasServerError={!scriptParseOk}
          diagnostics={scriptDiagnostics}
          onChange={(value) => {
            scriptDraft = value;
            scheduleScriptDiagnostics();
          }}
        />
      {/if}
      {#if scriptStatus}
        <p class="status">{scriptStatus}</p>
      {/if}
      {#if scriptError}
        <p class="error">{scriptError}</p>
      {/if}
    </section>
  </div>

  {#if typeDefDraft}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={resetTypeDefEditor}
      aria-label="Close dialog"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="type-def-dialog-title">
        <h3 id="type-def-dialog-title">{typeDefEditIndex >= 0 ? "Edit type definition" : "Add type definition"}</h3>
        <div class="modal-body">
          <label for="type-def-name">Name</label>
          <input id="type-def-name" bind:value={typeDefDraft.name} />
          <label for="type-def-flavour">Flavour</label>
          <select
            id="type-def-flavour"
            bind:value={typeDefDraft.flavour}
            on:change={() => {
              if (typeDefDraft.flavour === "List" && !typeDefDraft.elementType) {
                typeDefDraft = { ...typeDefDraft, elementType: "Int" };
              }
              if (typeDefDraft.flavour === "Struct" && !Array.isArray(typeDefDraft.members)) {
                typeDefDraft = { ...typeDefDraft, members: [{ name: "", type: "Bool" }] };
              }
            }}
          >
            <option value="Struct">Struct</option>
            <option value="List">List</option>
          </select>
          {#if typeDefDraft.flavour === "List"}
            <label for="type-def-element">Element type</label>
            <select id="type-def-element" bind:value={typeDefDraft.elementType}>
              <option value="Bool">Bool</option>
              <option value="Int">Int</option>
              <option value="Float">Float</option>
              <option value="String">String</option>
              <option value="Object">Object</option>
            </select>
          {:else}
            <div class="member-header">
              <span>Members</span>
              <button
                type="button"
                class="ghost icon-button"
                on:click={addTypeDefMember}
                aria-label="Add member"
                title="Add member"
              >
                <IconPlus className="icon" />
              </button>
            </div>
            {#if Array.isArray(typeDefDraft.members) && typeDefDraft.members.length > 0}
              <div class="member-list">
                {#each typeDefDraft.members as member, memberIndex}
                  <div class="member-row">
                    <input
                      placeholder="name"
                      bind:value={member.name}
                      on:input={() => {
                        typeDefDraft = { ...typeDefDraft, members: [...typeDefDraft.members] };
                      }}
                    />
                    <select
                      bind:value={member.type}
                      on:change={() => {
                        typeDefDraft = { ...typeDefDraft, members: [...typeDefDraft.members] };
                      }}
                    >
                      <option value="Bool">Bool</option>
                      <option value="Int">Int</option>
                      <option value="Float">Float</option>
                      <option value="String">String</option>
                    </select>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={() => removeTypeDefMember(memberIndex)}
                      aria-label="Remove member"
                      title="Remove member"
                    >
                      <IconTrash className="icon" />
                    </button>
                  </div>
                {/each}
              </div>
            {:else}
              <p class="muted">No members defined.</p>
            {/if}
          {/if}
        </div>
        <div class="actions">
          <button type="button" class="primary" on:click={applyTypeDefEdit} disabled={!wsConnected || sceneFlowBusy}>
            {typeDefEditIndex >= 0 ? "Save" : "Add"}
          </button>
          <button type="button" class="ghost" on:click={resetTypeDefEditor}>Cancel</button>
        </div>
        {#if typeDefError}
          <p class="error">{typeDefError}</p>
        {/if}
      </div>
    </button>
  {/if}

  {#if cmdDialogOpen}
    <div class="modal-backdrop cmd-modal-backdrop">
      <div class="modal cmd-modal" role="dialog" aria-modal="true" aria-labelledby="cmd-dialog-title">
        <h3 id="cmd-dialog-title">Command executions of {nodeEditorTarget?.name || "(unnamed)"}</h3>
        <div class="cmd-dialog">
          <div class="def-table cmd-inline-table">
            <div
              class="def-list"
              role="list"
              aria-label="Command executions"
              on:dragover={handleSceneDropOver}
              on:drop={handleCommandSceneDrop}
            >
              {#if cmdInlineDrafts.length === 0}
                <div class="def-empty">No commands yet.</div>
              {:else}
                {#each cmdInlineDrafts as cmdText, index}
                  <div class="cmd-row" class:selected={cmdSelectedIndex === index}>
                    <input
                      class="cmd-inline-input"
                      value={cmdText}
                      on:input={(event) => updateCmdInlineDraft(index, event.target.value)}
                      on:focus={() => (cmdSelectedIndex = index)}
                      on:blur={() => commitCmdInlineDraft(index)}
                      on:keydown={(event) => handleCmdInlineKeydown(event, index)}
                      disabled={!wsConnected || sceneFlowBusy}
                    />
                  </div>
                {/each}
              {/if}
            </div>
            <div class="def-actions">
              <button
                type="button"
                class="ghost icon-button"
                on:click={startCmdAdd}
                disabled={!wsConnected || sceneFlowBusy}
                aria-label="Add command"
                title="Add command"
              >
                <IconPlus className="icon" />
              </button>
              <button
                type="button"
                class="ghost icon-button danger"
                on:click={deleteSelectedCmd}
                disabled={!wsConnected || sceneFlowBusy || cmdSelectedIndex === null}
                aria-label="Remove command"
                title="Remove command"
              >
                <IconTrash className="icon" />
              </button>
              <button
                type="button"
                class="ghost icon-button"
                on:click={() => moveSelectedCmd(-1)}
                disabled={
                  !wsConnected ||
                  sceneFlowBusy ||
                  cmdSelectedIndex === null ||
                  cmdSelectedIndex === 0 ||
                  cmdSelectedIndex >= nodeEditorCommands.length
                }
                aria-label="Move command up"
                title="Move up"
              >
                <IconChevronUp className="icon" />
              </button>
              <button
                type="button"
                class="ghost icon-button"
                on:click={() => moveSelectedCmd(1)}
                disabled={
                  !wsConnected ||
                  sceneFlowBusy ||
                  cmdSelectedIndex === null ||
                  cmdSelectedIndex >= nodeEditorCommands.length - 1
                }
                aria-label="Move command down"
                title="Move down"
              >
                <IconChevronDown className="icon" />
              </button>
            </div>
          </div>
          <div class="actions cmd-dialog-actions">
            <button type="button" class="primary cmd-close" on:click={closeCmdDialog}>Close</button>
          </div>
          {#if cmdError}
            <p class="error">{cmdError}</p>
          {/if}
        </div>
      </div>
    </div>
  {/if}

  {#if varDefDraft}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={resetVarDefEditor}
      aria-label="Close dialog"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="var-def-dialog-title">
        <h3 id="var-def-dialog-title">{varDefEditIndex >= 0 ? "Edit variable definition" : "Add variable definition"}</h3>
        <div class="modal-body">
          <label for="var-def-name">Name</label>
          <input id="var-def-name" bind:value={varDefDraft.name} />
          <label for="var-def-type">Type</label>
          <select id="var-def-type" bind:value={varDefDraft.type} on:change={updateVarDefType}>
            {#each nodeEditorTypeOptions as option}
              <option value={option}>{option}</option>
            {/each}
          </select>
          <label for="var-def-exp">Expression</label>
          <input id="var-def-exp" bind:value={varDefDraft.expression} />
        </div>
        <div class="actions">
          <button type="button" class="primary" on:click={applyVarDefEdit} disabled={!wsConnected || sceneFlowBusy}>
            {varDefEditIndex >= 0 ? "Save" : "Add"}
          </button>
          <button type="button" class="ghost" on:click={resetVarDefEditor}>Cancel</button>
        </div>
        {#if varDefError}
          <p class="error">{varDefError}</p>
        {/if}
      </div>
    </button>
  {/if}
</main>
