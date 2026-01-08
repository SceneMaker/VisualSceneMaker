<script>
  import { tick, onMount } from "svelte";
  import SceneFlowMiniMap from "./SceneFlowMiniMap.svelte";
  import SceneFlowView from "./SceneFlowView.svelte";
  import ScriptEditor from "./ScriptEditor.svelte";
  import IconChevronDown from "./icons/IconChevronDown.svelte";
  import IconChevronUp from "./icons/IconChevronUp.svelte";
  import IconGear from "./icons/IconGear.svelte";
  import IconPencil from "./icons/IconPencil.svelte";
  import IconPlus from "./icons/IconPlus.svelte";
  import IconPuzzle from "./icons/IconPuzzle.svelte";
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
  let autoConnectAttempted = false;
  let info = null;
  let error = "";
  let statusMessage = "";
  let sessionReady = false;
  let showEditor = false;
  let projectLoadAttempted = false;
  let projectLoadProjectId = "";
  let showTokenSection = false;

  const SCENE_DRAG_TYPE = "application/x-vsm-scene";
  const AGENT_DRAG_TYPE = "application/x-vsm-agent";
  const BLOCK_DRAG_TYPE = "application/x-vsm-block";
  const SCENE_LANGUAGE_ALL = "__all__";
  const SCENEFLOW_ROOT_ID = "__root__";
  const SCENEFLOW_ZOOM_KEY = "vsm_scene_flow_zoom";
  const SCENEFLOW_ZOOM_MIN = 0.3;
  const SCENEFLOW_ZOOM_MAX = 3.5;
  const AGENT_ICON_PATHS = {
    input:
      "M8.25 9V5.25A2.25 2.25 0 0 1 10.5 3h6a2.25 2.25 0 0 1 2.25 2.25v13.5A2.25 2.25 0 0 1 16.5 21h-6a2.25 2.25 0 0 1-2.25-2.25V15M12 9l3 3m0 0-3 3m3-3H2.25",
    processing:
      "M19.5 12c0-1.232-.046-2.453-.138-3.662a4.006 4.006 0 0 0-3.7-3.7 48.678 48.678 0 0 0-7.324 0 4.006 4.006 0 0 0-3.7 3.7c-.017.22-.032.441-.046.662M19.5 12l3-3m-3 3-3-3m-12 3c0 1.232.046 2.453.138 3.662a4.006 4.006 0 0 0 3.7 3.7 48.656 48.656 0 0 0 7.324 0 4.006 4.006 0 0 0 3.7-3.7c.017-.22.032-.441.046-.662M4.5 12l3 3m-3-3-3 3",
    output:
      "M8.25 9V5.25A2.25 2.25 0 0 1 10.5 3h6a2.25 2.25 0 0 1 2.25 2.25v13.5A2.25 2.25 0 0 1 16.5 21h-6a2.25 2.25 0 0 1-2.25-2.25V15M12 9l3 3m0 0-3 3m3-3H2.25"
  };
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
  const PREF_NODE_DEFAULT = 90;
  const PREF_GRID_DEFAULT = 1;
  const PREF_WORKSPACE_FONT_DEFAULT = 11;
  const PREF_SCRIPT_FONT_DEFAULT = "Monospaced";
  const PREF_SCRIPT_FONT_SIZE_DEFAULT = 16;
  const PREF_NODE_MIN = 20;
  const PREF_NODE_MAX = 200;
  const PREF_GRID_MIN = 1;
  const PREF_GRID_MAX = 8;
  const PREF_FONT_MIN = 8;
  const PREF_FONT_MAX = 16;
  const SCRIPT_FONT_OPTIONS = [
    "Monospaced",
    "IBM Plex Mono",
    "Fira Code",
    "Fira Mono",
    "JetBrains Mono",
    "Source Code Pro",
    "Menlo",
    "Monaco",
    "Consolas",
    "Courier New",
    "SF Mono"
  ];
  const REV_WORDS_A = [
    "amber",
    "ancient",
    "brisk",
    "calm",
    "candid",
    "clever",
    "cool",
    "copper",
    "coral",
    "crisp",
    "daring",
    "dawn",
    "dusk",
    "eager",
    "ember",
    "faint",
    "feral",
    "fine",
    "fresh",
    "gentle",
    "glad",
    "gold",
    "grand",
    "green",
    "hazy",
    "icy",
    "jade",
    "keen",
    "kind",
    "lively",
    "lunar",
    "mellow",
    "mild",
    "moss",
    "neat",
    "nimble",
    "noble",
    "ocean",
    "olive",
    "opal",
    "peach",
    "quick",
    "quiet",
    "rapid",
    "raven",
    "royal",
    "sage",
    "sharp",
    "shy",
    "silent",
    "silver",
    "small",
    "solid",
    "soft",
    "solar",
    "still",
    "stone",
    "storm",
    "swift",
    "tender",
    "vivid",
    "warm",
    "wild",
    "young"
  ];
  const REV_WORDS_B = [
    "ash",
    "azure",
    "blue",
    "bold",
    "brass",
    "breeze",
    "bronze",
    "cedar",
    "charm",
    "clear",
    "cloud",
    "coast",
    "comet",
    "creek",
    "dawn",
    "delta",
    "drift",
    "dune",
    "echo",
    "ember",
    "field",
    "flame",
    "flash",
    "frost",
    "gale",
    "glade",
    "grove",
    "harbor",
    "hill",
    "horizon",
    "isle",
    "lake",
    "leaf",
    "light",
    "maple",
    "mist",
    "moon",
    "night",
    "nova",
    "oak",
    "orbit",
    "pearl",
    "pine",
    "plain",
    "plume",
    "rain",
    "reed",
    "ridge",
    "river",
    "shore",
    "sky",
    "snow",
    "spark",
    "spring",
    "star",
    "stone",
    "surf",
    "tide",
    "trail",
    "vale",
    "wave",
    "wind",
    "wood",
    "zenith"
  ];
  const REV_WORDS_C = [
    "anchor",
    "arc",
    "atlas",
    "beacon",
    "blade",
    "bloom",
    "breeze",
    "bridge",
    "brook",
    "canyon",
    "circle",
    "cloud",
    "crest",
    "crown",
    "dawn",
    "delta",
    "ember",
    "fern",
    "field",
    "flame",
    "flare",
    "forest",
    "garden",
    "glen",
    "grove",
    "harbor",
    "haven",
    "hill",
    "horizon",
    "isle",
    "key",
    "lagoon",
    "light",
    "meadow",
    "mirror",
    "mountain",
    "nest",
    "oak",
    "ocean",
    "orchard",
    "path",
    "peak",
    "pine",
    "river",
    "root",
    "rose",
    "salt",
    "shadow",
    "shore",
    "sky",
    "spark",
    "spring",
    "star",
    "stone",
    "storm",
    "summit",
    "sun",
    "tide",
    "vale",
    "valley",
    "wave",
    "wild",
    "wind",
    "wood"
  ];

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

  function superNodeIconPath(w, h, power = 5, steps = 24) {
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

  function revisionSlug(revision) {
    if (!revision || revision === "unknown") return "unknown";
    const hex = String(revision).replace(/[^0-9a-f]/gi, "").toLowerCase();
    if (!hex) return "unknown";
    const padded = hex.padEnd(6, "0");
    const a = parseInt(padded.slice(0, 2), 16);
    const b = parseInt(padded.slice(2, 4), 16);
    const c = parseInt(padded.slice(4, 6), 16);
    const w1 = REV_WORDS_A[a % REV_WORDS_A.length];
    const w2 = REV_WORDS_B[b % REV_WORDS_B.length];
    const w3 = REV_WORDS_C[c % REV_WORDS_C.length];
    return `${w1}-${w2}-${w3}`;
  }

  function middleEllipsis(text, maxChars = 32) {
    const value = (text ?? "").toString();
    if (value.length <= maxChars) return value;
    if (maxChars <= 5) return value.slice(0, maxChars);
    const keep = maxChars - 3;
    const head = Math.ceil(keep / 2);
    const tail = Math.floor(keep / 2);
    return `${value.slice(0, head)}...${value.slice(value.length - tail)}`;
  }

  function fitMiddleEllipsis(node, params) {
    let text = params?.text ?? "";
    let rafId = null;
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    let observer = null;

    const schedule = () => {
      if (rafId !== null) return;
      rafId = requestAnimationFrame(update);
    };

    const update = () => {
      rafId = null;
      if (!node) return;
      const full = (text ?? "").toString();
      const maxWidth = node.clientWidth;
      if (!maxWidth) {
        node.textContent = full;
        return;
      }
      const style = getComputedStyle(node);
      ctx.font = style.font || `${style.fontSize} ${style.fontFamily}`;
      if (ctx.measureText(full).width <= maxWidth) {
        node.textContent = full;
        return;
      }
      const ellipsis = "...";
      let low = 1;
      let high = full.length;
      let best = full;
      while (low <= high) {
        const keep = Math.floor((low + high) / 2);
        const leftCount = Math.ceil(keep / 2);
        const rightCount = Math.max(1, keep - leftCount);
        const candidate =
          full.slice(0, leftCount) + ellipsis + full.slice(full.length - rightCount);
        if (ctx.measureText(candidate).width <= maxWidth) {
          best = candidate;
          low = keep + 1;
        } else {
          high = keep - 1;
        }
      }
      node.textContent = best;
    };

    observer = new ResizeObserver(schedule);
    observer.observe(node);
    schedule();

    return {
      update(newParams) {
        text = newParams?.text ?? "";
        schedule();
      },
      destroy() {
        if (observer) observer.disconnect();
        if (rafId !== null) cancelAnimationFrame(rafId);
      }
    };
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
  let recentLoaded = false;
  let recentLoading = false;
  let recentError = "";
  let recentFailureOpen = false;
  let recentFailureProject = null;
  let recentFailureMessage = "";
  let tutorials = [];

  let openPath = "";
  let newName = "";
  let newBaseDir = "";
  let saveAsPath = "";
  let saveAsDialogOpen = false;

  let preferences = {};
  let prefDraft = {};
  let prefFilter = "";

  let config = {};
  let configDraft = {};
  let configSaved = null;
  let configLoading = false;
  let configLoaded = false;
  let configError = "";
  let lastConfigProjectId = "";
  let lastProjectConfigProjectId = "";
  let projectConfigDialogOpen = false;
  let projectConfig = null;
  let projectConfigDraft = null;
  let projectConfigLoading = false;
  let projectConfigError = "";
  let projectConfigSaved = null;
  let projectConfigPending = false;
  let projectConfigSelection = { type: "project" };
  let projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
  let projectConfigNewAgent = { name: "", device: "" };
  let projectConfigNewFeature = { key: "", value: "" };
  let availableDevices = [];
  let availableDevicesLoading = false;
  let availableDevicesError = "";
  let exportableKeyCache = {};
  let exportableKeyLoading = {};
  let exportableKeyError = {};
  let prefsDialogOpen = false;
  let prefsDialogDraft = null;
  let prefsDialogError = "";
  let prefsDialogBusy = false;
  let prefsPreviewStyle = "";

  let scriptText = "";
  let scriptDraft = "";
  let scriptVersion = null;
  let scriptStatus = "";
  let scriptError = "";
  let scriptLoading = false;
  let scriptLoaded = false;
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
  let scriptScenesLoaded = false;
  let scriptElements = { acticon: [], gesticon: [], visicon: [] };
  let scriptElementsFilter = "";
  let scriptElementsError = "";
  let scriptElementsLoading = false;
  let scriptElementsLoaded = false;
  let sceneAgentNames = [];
  let deviceAgentNames = [];
  let agentGroups = { input: [], processing: [], output: [] };
  const SELECTION_PREVIEW_LIMIT = 6;

  let sceneFlow = null;
  let sceneFlowError = "";
  let sceneFlowLoading = false;
  let sceneFlowLoaded = false;
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
  let runtimeLoaded = false;
  let lastRuntimeProjectId = "";
  let runtimeValues = {};
  let runtimeInitialValues = {};
  let runtimeInitialProjectId = "";
  let runtimeInitialState = "stopped";
  let activityNodeCounts = new Map();
  let activityEdgeHits = new Map();
  let activityNodeIds = [];
  let activityEdgeList = [];
  let timeoutEdgeRuns = new Map();
  let timeoutEdgeList = [];
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
  let loadConfirmOpen = false;
  let loadConfirmReasons = [];

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
  $: projectConfigView = normalizeProjectConfig(projectConfigDraft || projectConfig || {});
  $: projectConfigPlugins = projectConfigView.plugins;
  $: projectConfigAgents = projectConfigView.agents;
  $: projectConfigPlayer = projectConfigView.player;
  $: projectConfigAgentsByPlugin = projectConfigPlugins.map((plugin) =>
    projectConfigAgents
      .map((agent, agentIndex) => (agent.device === plugin.name ? { agent, agentIndex } : null))
      .filter(Boolean)
  );
  $: selectedProjectPlugin =
    projectConfigSelection?.type === "plugin"
      ? projectConfigPlugins[projectConfigSelection.pluginIndex]
      : null;
  $: selectedProjectAgent =
    projectConfigSelection?.type === "agent"
      ? projectConfigAgents[projectConfigSelection.agentIndex]
      : null;
  $: selectedProjectAgentPluginIndex =
    projectConfigSelection?.type === "agent" && selectedProjectAgent
      ? projectConfigPlugins.findIndex((plugin) => plugin.name === selectedProjectAgent.device)
      : -1;
  $: selectedProjectAgentPlugin =
    selectedProjectAgentPluginIndex >= 0 ? projectConfigPlugins[selectedProjectAgentPluginIndex] : null;
  $: activeProjectPluginIndex =
    projectConfigSelection?.type === "plugin"
      ? projectConfigSelection.pluginIndex
      : projectConfigSelection?.type === "agent"
        ? selectedProjectAgentPluginIndex
        : -1;
  $: activeProjectPlugin = activeProjectPluginIndex >= 0 ? projectConfigPlugins[activeProjectPluginIndex] : null;
  $: if (!projectConfigNewAgent.device && projectConfigPlugins.length) {
    projectConfigNewAgent = { ...projectConfigNewAgent, device: projectConfigPlugins[0].name || "" };
  }
  $: selectedProjectPluginKeys =
    selectedProjectPlugin?.className || selectedProjectPlugin?.name
      ? exportableKeyCache[
          keyHintId(selectedProjectPlugin.name, "plugin", selectedProjectPlugin.className)
        ]
      : null;
  $: selectedProjectAgentKeys =
    selectedProjectAgent?.device || activeProjectPlugin?.className
      ? exportableKeyCache[
          keyHintId(selectedProjectAgent?.device || "", "agent", activeProjectPlugin?.className || "")
        ]
      : null;
  $: pluginKeyOptions = keyHintOptions(selectedProjectPluginKeys);
  $: agentKeyOptions = keyHintOptions(selectedProjectAgentKeys);
  $: selectedProjectPluginKeysLoading =
    selectedProjectPlugin?.className || selectedProjectPlugin?.name
      ? exportableKeyLoading[
          keyHintId(selectedProjectPlugin.name, "plugin", selectedProjectPlugin.className)
        ]
      : false;
  $: selectedProjectAgentKeysLoading =
    selectedProjectAgent?.device || activeProjectPlugin?.className
      ? exportableKeyLoading[
          keyHintId(selectedProjectAgent?.device || "", "agent", activeProjectPlugin?.className || "")
        ]
      : false;
  $: selectedProjectPluginKeysError =
    selectedProjectPlugin?.className || selectedProjectPlugin?.name
      ? exportableKeyError[
          keyHintId(selectedProjectPlugin.name, "plugin", selectedProjectPlugin.className)
        ]
      : "";
  $: selectedProjectAgentKeysError =
    selectedProjectAgent?.device || activeProjectPlugin?.className
      ? exportableKeyError[
          keyHintId(selectedProjectAgent?.device || "", "agent", activeProjectPlugin?.className || "")
        ]
      : "";
  $: if (
    projectConfigSelection?.type === "plugin" &&
    (projectConfigSelection.pluginIndex == null ||
      projectConfigSelection.pluginIndex < 0 ||
      projectConfigSelection.pluginIndex >= projectConfigPlugins.length)
  ) {
    projectConfigSelection = { type: "project" };
  }
  $: if (
    projectConfigSelection?.type === "agent" &&
    (projectConfigSelection.agentIndex == null ||
      projectConfigSelection.agentIndex < 0 ||
      projectConfigSelection.agentIndex >= projectConfigAgents.length)
  ) {
    projectConfigSelection = { type: "project" };
  }
  $: scriptDirty = scriptDraft !== scriptText;
  $: configDirty = Object.keys(diffValues(config, configDraft)).length > 0;
  $: projectConfigDirty =
    projectConfigDraft && projectConfig ? JSON.stringify(projectConfigDraft) !== JSON.stringify(projectConfig) : false;
  $: projectLoadErrors = buildProjectLoadErrors();
  $: projectLoadComplete =
    !!selectedProjectId &&
    configLoaded &&
    scriptLoaded &&
    scriptScenesLoaded &&
    scriptElementsLoaded &&
    sceneFlowLoaded &&
    runtimeLoaded &&
    !configError &&
    !scriptError &&
    !scriptScenesError &&
    !scriptElementsError &&
    !sceneFlowError &&
    !runtimeError;
  $: projectLoadPending =
    projectLoadAttempted &&
    !projectLoadComplete &&
    (configLoading ||
      scriptLoading ||
      scriptScenesLoading ||
      scriptElementsLoading ||
      sceneFlowLoading ||
      runtimeLoading);
  $: if (projectLoadComplete && !showEditor) {
    showEditor = true;
  }
  $: if (!sessionReady || !selectedProjectId) {
    showEditor = false;
  }
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
  $: sceneFlowVarDefs = Array.isArray(sceneFlow?.superNodeData?.varDefs) ? sceneFlow.superNodeData.varDefs : [];
  $: sceneFlowIntVarNames = sceneFlowVarDefs
    .filter((def) => (def?.type || "").trim().toLowerCase() === "int" && (def?.name || "").trim())
    .map((def) => (def.name || "").trim());
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
  $: timeoutEdgeList = Array.from(timeoutEdgeRuns.values());
  $: runtimeCanPlay = wsConnected && !!selectedProjectId && (runtimeState === "stopped" || runtimeState === "paused");
  $: runtimeCanPause = wsConnected && !!selectedProjectId && runtimeState === "running";
  $: runtimeCanStop = wsConnected && !!selectedProjectId && runtimeState !== "stopped";
  $: runtimePlayLabel = runtimeState === "paused" ? "Resume" : "Start";
  $: infoRevision = info?.revision || info?.buildRevision || info?.build || info?.version || "unknown";
  $: infoRevisionSlug = revisionSlug(infoRevision);
  $: infoBuildDate = info?.buildDate || info?.buildTime || "unknown";
  $: projectRequiresSaveAs = (() => {
    if (!selectedProject) return false;
    if (selectedProject.saveAsOnly !== undefined) {
      return selectedProject.saveAsOnly === true;
    }
    return !selectedProject.path || selectedProject.pending === true;
  })();
  $: {
    if (typeof document !== "undefined") {
      const status = wsConnected ? "connected" : "offline";
      const projectLabel = showEditor && selectedProject?.name ? ` — ${selectedProject.name}` : "";
      document.title = `Visual SceneMaker Web ${projectLabel} (${status})`;
    }
  }
  $: filteredScriptScenes = filterSceneLanguages(scriptScenes, scriptScenesFilter, scriptScenesLanguage);
  $: sceneLanguageOptions = sceneLanguageOptionList(scriptScenes);
  $: filteredScriptElements = filterScriptElements(scriptElements, scriptElementsFilter);
  $: sceneAgentNames = extractSceneAgents(scriptDraft);
  $: deviceAgentNames = extractDeviceAgents(projectConfigAgents);
  $: agentGroups = buildAgentGroups(sceneAgentNames, deviceAgentNames);
  $: prefsPreviewStyle = buildPrefsPreviewStyle(prefsDialogDraft);

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
      timeoutSpec: edgeTimeoutSpec(selectedEdge),
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
      return edgeTimeoutSpec(selectedEdge) !== String(edgeDraft.timeoutSpec ?? "") || altDirty;
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
    lastProjectConfigProjectId = "";
    lastScriptProjectId = "";
    lastSceneFlowProjectId = "";
    lastRuntimeProjectId = "";
    resetProjectLoadState();
    projectLoadAttempted = false;
    projectLoadProjectId = "";
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== projectLoadProjectId) {
    projectLoadProjectId = selectedProjectId;
    projectLoadAttempted = true;
    showEditor = false;
    resetProjectLoadState();
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastConfigProjectId) {
    lastConfigProjectId = selectedProjectId;
    loadConfig(selectedProjectId);
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastProjectConfigProjectId) {
    lastProjectConfigProjectId = selectedProjectId;
    loadProjectConfig(selectedProjectId);
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
    lastProjectConfigProjectId = "";
    projectConfig = null;
    projectConfigDraft = null;
    projectConfigError = "";
    projectConfigLoading = false;
    projectConfigSaved = null;
    projectConfigPending = false;
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
    resetProjectLoadState();
    projectLoadAttempted = false;
    projectLoadProjectId = "";
    showEditor = false;
  }

  $: if (!showEditor && wsConnected && !recentLoaded && !recentLoading) {
    loadRecent();
  }

  async function connectAll({ allowTokenRetry = true } = {}) {
    error = "";
    statusMessage = "";
    sessionReady = false;
    try {
      await fetchLocalToken();
      await loadInfo();
      await Promise.all([loadProjects(), loadPreferences(), loadRecent(), loadTutorials()]);
      const wsOk = await connectWs();
      if (!wsOk) {
        error = wsError || "WebSocket connection failed.";
        return;
      }
      sessionReady = true;
      showTokenSection = false;
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
      const message = err?.message || "Failed to connect.";
      const tokenIssue = /token/i.test(message);
      if (allowTokenRetry && tokenIssue && isLocalHost()) {
        token = "";
        localStorage.removeItem("vsm_token");
        const refreshed = await fetchLocalToken();
        if (refreshed) {
          return connectAll({ allowTokenRetry: false });
        }
      }
      error = message;
    }
  }

  async function loadInfo() {
    info = await apiGet("/api/v1/info");
    localStorage.setItem("vsm_token", token);
  }

  async function fetchLocalToken() {
    if (token) return false;
    try {
      const response = await fetch("/api/v1/token");
      if (!response.ok) {
        return false;
      }
      const data = await response.json();
      if (data?.token) {
        token = data.token;
        localStorage.setItem("vsm_token", token);
        return true;
      }
    } catch (err) {
      return false;
    }
    return false;
  }

  function isLocalHost() {
    if (typeof window === "undefined") return false;
    const host = window.location.hostname;
    return host === "localhost" || host === "127.0.0.1" || host === "::1";
  }

  async function autoConnectIfLocal() {
    if (autoConnectAttempted) return;
    autoConnectAttempted = true;
    if (!token) {
      const fetched = await fetchLocalToken();
      if (!fetched) {
        return;
      }
    }
    await connectAll();
  }

  onMount(() => {
    autoConnectIfLocal();
  });

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
    recentLoading = true;
    recentError = "";
    try {
      const data = await apiGet("/api/v1/projects/recent");
      recent = data.projects || [];
      recentLoaded = true;
    } catch (err) {
      recentError = err.message || "Failed to load recent projects.";
    } finally {
      recentLoading = false;
    }
  }

  async function loadTutorials() {
    const data = await apiGet("/api/v1/projects/tutorials");
    tutorials = data.projects || [];
  }

  async function openProject(path) {
    if (!path) return;
    const response = await apiPost("/api/v1/projects/open", { path });
    openPath = "";
    if (response?.projectId) {
      selectedProjectId = response.projectId;
    }
    await loadProjects();
    await loadRecent();
  }

  async function openRecentProject(project) {
    if (!project?.path) return;
    try {
      await openProject(project.path);
    } catch (err) {
      recentFailureProject = project;
      recentFailureMessage = err?.message || "Failed to open recent project.";
      recentFailureOpen = true;
    }
  }

  async function createProject() {
    if (!newName) return;
    const payload = { name: newName };
    if (newBaseDir) {
      payload.baseDir = newBaseDir;
    }
    const response = await apiPost("/api/v1/projects", payload);
    newName = "";
    newBaseDir = "";
    if (response?.projectId) {
      selectedProjectId = response.projectId;
    }
    await loadProjects();
    await loadRecent();
  }

  async function saveProject(projectId) {
    if (!projectId) return;
    try {
      await apiPost(`/api/v1/projects/${projectId}/save`, {});
      await loadProjects();
      await loadRecent();
    } catch (err) {
      const message = err?.message || "Failed to save project.";
      const needsSaveAs = /save-as|save as|pending|no path/i.test(message);
      statusMessage = message;
      if (needsSaveAs) {
        openSaveAsDialog();
      }
    }
  }

  async function saveAsProject(projectId, overridePath) {
    const targetPath = overridePath || saveAsPath;
    if (!projectId || !targetPath) return;
    await apiPost(`/api/v1/projects/${projectId}/save-as`, { path: targetPath });
    saveAsPath = "";
    await loadProjects();
    await loadRecent();
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
    if (
      selectedProjectId &&
      Object.prototype.hasOwnProperty.call(values, "workspace_fontsize")
    ) {
      const workspaceSize = values.workspace_fontsize;
      if (workspaceSize !== undefined && workspaceSize !== null) {
        const normalized = String(workspaceSize);
        const current = config?.workspace_fontsize ?? "";
        configDraft = { ...configDraft, workspace_fontsize: normalized };
        if (String(current) !== normalized) {
          const configResponse = await sendCommand("Config.Update", {
            projectId: selectedProjectId,
            values: { workspace_fontsize: normalized }
          });
          config = configResponse.config || {};
          configDraft = { ...config };
          configSaved = configResponse.saved === true;
        }
      }
    }
  }

  async function loadConfig(projectId) {
    if (!projectId) return;
    configLoading = true;
    configError = "";
    configLoaded = false;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/config`);
      if (projectId !== selectedProjectId) {
        return;
      }
      config = data.config || {};
      configDraft = { ...config };
      configSaved = null;
      configLoaded = true;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      configError = err.message || "Failed to load preferences.";
      configLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        configLoading = false;
      }
    }
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

  async function loadProjectConfig(projectId) {
    if (!projectId) return;
    projectConfigLoading = true;
    projectConfigError = "";
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/project-config`);
      projectConfig = normalizeProjectConfig(data.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      projectConfigSaved = data.saved ?? null;
      projectConfigPending = data.pending === true;
    } catch (err) {
      projectConfigError = err.message || "Failed to load project config.";
    } finally {
      projectConfigLoading = false;
    }
  }

  async function loadAvailableDevices() {
    availableDevicesLoading = true;
    availableDevicesError = "";
    try {
      const data = await apiGet("/api/v1/devices");
      const list = Array.isArray(data.devices) ? data.devices : [];
      availableDevices = list
        .map((device) => ({
          name: device?.name ?? "",
          className: device?.className ?? ""
        }))
        .filter((device) => device.className);
    } catch (err) {
      availableDevicesError = err.message || "Failed to load device list.";
    } finally {
      availableDevicesLoading = false;
    }
  }

  function keyHintId(deviceName, scope, className = "") {
    const key = (className || "").trim() || (deviceName || "").trim();
    return `${scope}:${key}`;
  }

  async function loadExportableKeys(deviceName, scope, className = "") {
    const trimmedDevice = (deviceName || "").trim();
    const trimmedClass = (className || "").trim();
    if (!selectedProjectId || (!trimmedDevice && !trimmedClass)) return;
    const id = keyHintId(trimmedDevice, scope, trimmedClass);
    if (exportableKeyCache[id] || exportableKeyLoading[id]) {
      return;
    }
    exportableKeyLoading = { ...exportableKeyLoading, [id]: true };
    exportableKeyError = { ...exportableKeyError, [id]: "" };
    try {
      const params = new URLSearchParams();
      if (trimmedDevice) {
        params.set("device", trimmedDevice);
      }
      if (trimmedClass) {
        params.set("className", trimmedClass);
      }
      params.set("scope", scope);
      const data = await apiGet(`/api/v1/projects/${selectedProjectId}/project-config/keys?${params.toString()}`);
      exportableKeyCache = { ...exportableKeyCache, [id]: data };
    } catch (err) {
      exportableKeyError = { ...exportableKeyError, [id]: err.message || "Failed to load keys." };
    } finally {
      exportableKeyLoading = { ...exportableKeyLoading, [id]: false };
    }
  }

  async function applyProjectConfig() {
    if (!selectedProjectId || !projectConfigDraft) return;
    projectConfigError = "";
    try {
      const response = await sendCommand("ProjectConfig.Update", {
        projectId: selectedProjectId,
        config: projectConfigDraft
      });
      projectConfig = normalizeProjectConfig(response.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      projectConfigSaved = response.saved ?? null;
      projectConfigPending = response.pending === true;
    } catch (err) {
      projectConfigError = err.message || "Failed to update project config.";
    }
  }

  function openProjectConfigDialog() {
    if (!selectedProjectId) return;
    projectConfigDialogOpen = true;
    projectConfigSelection = { type: "project" };
    projectConfigError = "";
    projectConfigSaved = null;
    projectConfigPending = false;
    loadProjectConfig(selectedProjectId);
    loadAvailableDevices();
  }

  $: if (projectConfigDialogOpen && (selectedProjectPlugin?.className || selectedProjectPlugin?.name)) {
    loadExportableKeys(selectedProjectPlugin.name, "plugin", selectedProjectPlugin.className);
  }

  $: if (projectConfigDialogOpen && (selectedProjectAgent?.device || activeProjectPlugin?.className)) {
    loadExportableKeys(selectedProjectAgent?.device || "", "agent", activeProjectPlugin?.className || "");
  }

  function closeProjectConfigDialog() {
    projectConfigDialogOpen = false;
    projectConfigError = "";
    projectConfigSelection = { type: "project" };
    projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
    projectConfigNewAgent = { name: "", device: "" };
    projectConfigNewFeature = { key: "", value: "" };
  }

  function selectProjectConfig(selection) {
    projectConfigSelection = selection;
    projectConfigError = "";
    projectConfigNewFeature = { key: "", value: "" };
    if (selection?.type === "plugin") {
      const plugin = projectConfigPlugins[selection.pluginIndex];
      if (plugin?.name) {
        projectConfigNewAgent = { ...projectConfigNewAgent, device: plugin.name };
      }
    }
  }

  function updateProjectName(value) {
    projectConfigDraft = {
      ...projectConfigDraft,
      name: value
    };
  }

  function updatePluginField(index, field, value) {
    const plugins = projectConfigPlugins.map((plugin, idx) =>
      idx === index ? { ...plugin, [field]: value } : plugin
    );
    projectConfigDraft = {
      ...projectConfigDraft,
      plugins
    };
  }

  function updatePluginName(index, value) {
    const plugins = [...projectConfigPlugins];
    const current = plugins[index];
    if (!current) return;
    const oldName = current.name;
    plugins[index] = { ...current, name: value };
    const agents = projectConfigAgents.map((agent) =>
      agent.device === oldName ? { ...agent, device: value } : agent
    );
    projectConfigDraft = {
      ...projectConfigDraft,
      plugins,
      agents
    };
  }

  function updateAgentField(index, field, value) {
    const agents = projectConfigAgents.map((agent, idx) =>
      idx === index ? { ...agent, [field]: value } : agent
    );
    projectConfigDraft = {
      ...projectConfigDraft,
      agents
    };
  }

  function updateFeatureList(list, index, field, value) {
    return list.map((feature, idx) =>
      idx === index ? { ...feature, [field]: value } : feature
    );
  }

  function updatePluginFeature(pluginIndex, featureIndex, field, value) {
    const plugins = [...projectConfigPlugins];
    const plugin = plugins[pluginIndex];
    if (!plugin) return;
    const features = updateFeatureList(plugin.features, featureIndex, field, value);
    plugins[pluginIndex] = { ...plugin, features };
    projectConfigDraft = { ...projectConfigDraft, plugins };
  }

  function updateAgentFeature(agentIndex, featureIndex, field, value) {
    const agents = [...projectConfigAgents];
    const agent = agents[agentIndex];
    if (!agent) return;
    const features = updateFeatureList(agent.features, featureIndex, field, value);
    agents[agentIndex] = { ...agent, features };
    projectConfigDraft = { ...projectConfigDraft, agents };
  }

  function updatePlayerFeature(featureIndex, field, value) {
    const features = updateFeatureList(projectConfigPlayer.features, featureIndex, field, value);
    projectConfigDraft = {
      ...projectConfigDraft,
      player: { features }
    };
  }

  function addFeatureToSelection() {
    const key = (projectConfigNewFeature.key || "").trim();
    if (!key) {
      projectConfigError = "Feature key is required.";
      return;
    }
    const value = projectConfigNewFeature.value ?? "";
    if (projectConfigSelection.type === "plugin") {
      const pluginIndex = projectConfigSelection.pluginIndex;
      const plugin = projectConfigPlugins[pluginIndex];
      if (!plugin) return;
      const features = [...plugin.features, { key, value }];
      const plugins = [...projectConfigPlugins];
      plugins[pluginIndex] = { ...plugin, features };
      projectConfigDraft = { ...projectConfigDraft, plugins };
    } else if (projectConfigSelection.type === "agent") {
      const agentIndex = projectConfigSelection.agentIndex;
      const agent = projectConfigAgents[agentIndex];
      if (!agent) return;
      const features = [...agent.features, { key, value }];
      const agents = [...projectConfigAgents];
      agents[agentIndex] = { ...agent, features };
      projectConfigDraft = { ...projectConfigDraft, agents };
    } else if (projectConfigSelection.type === "player") {
      const features = [...projectConfigPlayer.features, { key, value }];
      projectConfigDraft = {
        ...projectConfigDraft,
        player: { features }
      };
    }
    projectConfigNewFeature = { key: "", value: "" };
    projectConfigError = "";
  }

  function removePluginFeature(pluginIndex, featureIndex) {
    const plugins = [...projectConfigPlugins];
    const plugin = plugins[pluginIndex];
    if (!plugin) return;
    const features = plugin.features.filter((_, idx) => idx !== featureIndex);
    plugins[pluginIndex] = { ...plugin, features };
    projectConfigDraft = { ...projectConfigDraft, plugins };
  }

  function removeAgentFeature(agentIndex, featureIndex) {
    const agents = [...projectConfigAgents];
    const agent = agents[agentIndex];
    if (!agent) return;
    const features = agent.features.filter((_, idx) => idx !== featureIndex);
    agents[agentIndex] = { ...agent, features };
    projectConfigDraft = { ...projectConfigDraft, agents };
  }

  function removePlayerFeature(featureIndex) {
    const features = projectConfigPlayer.features.filter((_, idx) => idx !== featureIndex);
    projectConfigDraft = {
      ...projectConfigDraft,
      player: { features }
    };
  }

  function addPlugin() {
    const name = (projectConfigNewPlugin.name || "").trim();
    const className = (projectConfigNewPlugin.className || "").trim();
    const type = (projectConfigNewPlugin.type || "device").trim() || "device";
    if (!name || !className) {
      projectConfigError = "Device name and class are required.";
      return;
    }
    if (projectConfigPlugins.some((plugin) => plugin.name === name)) {
      projectConfigError = "Device name already exists.";
      return;
    }
    const next = {
      name,
      className,
      type,
      load: projectConfigNewPlugin.load !== false,
      features: []
    };
    const plugins = [...projectConfigPlugins, next];
    projectConfigDraft = { ...projectConfigDraft, plugins };
    projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
    selectProjectConfig({ type: "plugin", pluginIndex: plugins.length - 1 });
  }

  function addAgent(deviceOverride) {
    const name = (projectConfigNewAgent.name || "").trim();
    const device = (deviceOverride || projectConfigNewAgent.device || "").trim();
    if (!name || !device) {
      projectConfigError = "Agent name and device are required.";
      return;
    }
    if (projectConfigAgents.some((agent) => agent.name === name)) {
      projectConfigError = "Agent name already exists.";
      return;
    }
    const agent = {
      name,
      device,
      features: []
    };
    const agents = [...projectConfigAgents, agent];
    projectConfigDraft = { ...projectConfigDraft, agents };
    projectConfigNewAgent = { name: "", device };
    selectProjectConfig({ type: "agent", agentIndex: agents.length - 1 });
  }

  function removePlugin(index) {
    const plugin = projectConfigPlugins[index];
    if (!plugin) return;
    const plugins = projectConfigPlugins.filter((_, idx) => idx !== index);
    const agents = projectConfigAgents.filter((agent) => agent.device !== plugin.name);
    projectConfigDraft = { ...projectConfigDraft, plugins, agents };
    if (projectConfigSelection.type === "plugin" && projectConfigSelection.pluginIndex === index) {
      projectConfigSelection = { type: "project" };
    }
  }

  function removeAgent(index) {
    const agents = projectConfigAgents.filter((_, idx) => idx !== index);
    projectConfigDraft = { ...projectConfigDraft, agents };
    if (projectConfigSelection.type === "agent" && projectConfigSelection.agentIndex === index) {
      projectConfigSelection = { type: "project" };
    }
  }

  function readConfigValue(key, fallback) {
    if (Object.prototype.hasOwnProperty.call(configDraft, key)) {
      const value = configDraft[key];
      if (value !== undefined && value !== null && value !== "") {
        return value;
      }
    }
    if (Object.prototype.hasOwnProperty.call(config, key)) {
      const value = config[key];
      if (value !== undefined && value !== null && value !== "") {
        return value;
      }
    }
    return fallback;
  }

  function readConfigInt(key, fallback) {
    const raw = readConfigValue(key, fallback);
    const parsed = Number.parseInt(raw, 10);
    return Number.isFinite(parsed) ? parsed : fallback;
  }

  function readConfigBool(key, fallback) {
    const raw = readConfigValue(key, fallback);
    if (raw === undefined || raw === null || raw === "") return fallback;
    if (typeof raw === "boolean") return raw;
    return String(raw).toLowerCase() === "true";
  }

  function readConfigString(key, fallback) {
    const raw = readConfigValue(key, fallback);
    if (raw === undefined || raw === null) return fallback;
    const text = String(raw).trim();
    return text ? text : fallback;
  }

  function normalizeConfigValue(value) {
    if (value === undefined || value === null) return "";
    return String(value);
  }

  function parsePrefsInt(value, min, max, label) {
    const parsed = Number.parseInt(String(value).trim(), 10);
    if (!Number.isFinite(parsed)) {
      prefsDialogError = `${label} must be a number.`;
      return null;
    }
    if (parsed < min || parsed > max) {
      prefsDialogError = `${label} must be between ${min} and ${max}.`;
      return null;
    }
    return parsed;
  }

  function quoteFontFamily(font) {
    const trimmed = String(font || "").trim();
    const cleaned = trimmed.replace(/["]/g, "");
    if (!cleaned) return `"${PREF_SCRIPT_FONT_DEFAULT}"`;
    return `"${cleaned}"`;
  }

  function buildPrefsPreviewStyle(draft) {
    if (!draft) return "";
    const size = Number.parseInt(String(draft.scriptFontSize || ""), 10);
    const fontSize = Number.isFinite(size) ? size : PREF_SCRIPT_FONT_SIZE_DEFAULT;
    const family = quoteFontFamily(draft.scriptFontType);
    return `font-family:${family}, monospace; font-size:${fontSize}px;`;
  }

  function openPrefsDialog() {
    if (!selectedProjectId) return;
    const width = readConfigInt("node_width", PREF_NODE_DEFAULT);
    const height = readConfigInt("node_height", width);
    const nodeSize = width || height || PREF_NODE_DEFAULT;
    prefsDialogDraft = {
      nodeSize: String(nodeSize),
      gridScale: String(readConfigInt("grid_x", PREF_GRID_DEFAULT)),
      workspaceFontSize: String(readConfigInt("workspace_fontsize", PREF_WORKSPACE_FONT_DEFAULT)),
      drawGrid: readConfigBool("grid", true),
      activityVisualization: readConfigBool("visualization", true),
      activityTrace: readConfigBool("visualizationtrace", true),
      showNodeId: readConfigBool("shownodeid", true),
      scriptFontType: readConfigString("scriptfonttype", PREF_SCRIPT_FONT_DEFAULT),
      scriptFontSize: String(readConfigInt("scriptfonsize", PREF_SCRIPT_FONT_SIZE_DEFAULT))
    };
    prefsDialogError = "";
    prefsDialogOpen = true;
  }

  function closePrefsDialog() {
    prefsDialogOpen = false;
    prefsDialogDraft = null;
    prefsDialogError = "";
  }

  async function applyPrefsDialog() {
    if (!selectedProjectId || !prefsDialogDraft) return;
    prefsDialogError = "";
    const nodeSize = parsePrefsInt(prefsDialogDraft.nodeSize, PREF_NODE_MIN, PREF_NODE_MAX, "Node size");
    if (nodeSize === null) return;
    const gridScale = parsePrefsInt(prefsDialogDraft.gridScale, PREF_GRID_MIN, PREF_GRID_MAX, "Grid scale");
    if (gridScale === null) return;
    const workspaceFontSize = parsePrefsInt(
      prefsDialogDraft.workspaceFontSize,
      PREF_FONT_MIN,
      PREF_FONT_MAX,
      "Workspace font size"
    );
    if (workspaceFontSize === null) return;
    const scriptFontSize = parsePrefsInt(
      prefsDialogDraft.scriptFontSize,
      PREF_FONT_MIN,
      PREF_FONT_MAX,
      "Script font size"
    );
    if (scriptFontSize === null) return;
    const scriptFontType = String(prefsDialogDraft.scriptFontType || "").trim();
    if (!scriptFontType) {
      prefsDialogError = "Script font type is required.";
      return;
    }
    const changes = {};
    const addChange = (key, value) => {
      const next = String(value);
      const current = normalizeConfigValue(config?.[key]);
      if (current !== next) {
        changes[key] = next;
      }
    };
    addChange("node_width", nodeSize);
    addChange("node_height", nodeSize);
    addChange("grid_x", gridScale);
    addChange("grid_y", gridScale);
    addChange("workspace_fontsize", workspaceFontSize);
    addChange("grid", prefsDialogDraft.drawGrid);
    addChange("visualization", prefsDialogDraft.activityVisualization);
    addChange("visualizationtrace", prefsDialogDraft.activityTrace);
    addChange("shownodeid", prefsDialogDraft.showNodeId);
    addChange("scriptfonsize", scriptFontSize);
    addChange("scriptfonttype", scriptFontType);
    if (!Object.keys(changes).length) {
      prefsDialogError = "No changes to apply.";
      return;
    }
    prefsDialogBusy = true;
    try {
      const response = await sendCommand("Config.Update", {
        projectId: selectedProjectId,
        values: changes
      });
      if (response?.config) {
        config = response.config;
      }
      configDraft = { ...configDraft, ...changes };
      configSaved = response?.saved === true;
      statusMessage = response?.pending
        ? "Config stored; save the project to persist."
        : "Config updated.";
      closePrefsDialog();
    } catch (err) {
      prefsDialogError = err.message || "Failed to update preferences.";
    } finally {
      prefsDialogBusy = false;
    }
  }

  async function loadScript(projectId) {
    if (!projectId) return;
    scriptLoading = true;
    scriptError = "";
    scriptStatus = "";
    scriptParseOk = true;
    scriptLoaded = false;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script`);
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptText = data.text || "";
      scriptDraft = scriptText;
      scriptVersion = data.version ?? null;
      scriptDiagnostics = data.parseErrors || [];
      scriptParseOk = data.parseOk !== false;
      scriptLoaded = true;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptError = err.message || "Failed to load script.";
      scriptLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        scriptLoading = false;
      }
    }
  }

  async function loadScriptScenes(projectId) {
    if (!projectId) return;
    scriptScenesError = "";
    scriptScenesLoading = true;
    scriptScenesLoaded = false;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script/scenes`);
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptScenes = Array.isArray(data.languages) ? data.languages : [];
      scriptScenesLoaded = true;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptScenesError = err.message || "Failed to load scenes.";
      scriptScenes = [];
      scriptScenesLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        scriptScenesLoading = false;
      }
    }
  }

  async function loadScriptElements(projectId) {
    if (!projectId) return;
    scriptElementsError = "";
    scriptElementsLoading = true;
    scriptElementsLoaded = false;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/script/elements`);
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptElements = {
        acticon: Array.isArray(data.acticon) ? data.acticon : [],
        gesticon: Array.isArray(data.gesticon) ? data.gesticon : [],
        visicon: Array.isArray(data.visicon) ? data.visicon : []
      };
      scriptElementsLoaded = true;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      scriptElementsError = err.message || "Failed to load script elements.";
      scriptElements = { acticon: [], gesticon: [], visicon: [] };
      scriptElementsLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        scriptElementsLoading = false;
      }
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
    sceneFlowLoaded = false;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    edgeCreateSourceId = "";
    clearSceneFlowActivity();
    try {
      const query = superNodeId ? `?superNodeId=${encodeURIComponent(superNodeId)}` : "";
      const data = await apiGet(`/api/v1/projects/${projectId}/sceneflow${query}`);
      if (projectId !== selectedProjectId) {
        return;
      }
      sceneFlow = data;
      sceneFlowLoaded = true;
      loadRuntime(projectId);
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      sceneFlowError = err.message || "Failed to load SceneFlow.";
      sceneFlow = null;
      sceneFlowLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        sceneFlowLoading = false;
      }
    }
  }

  async function loadRuntime(projectId) {
    if (!projectId) return;
    runtimeError = "";
    runtimeLoading = true;
    runtimeLoaded = false;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/runtime`);
      if (projectId !== selectedProjectId) {
        return;
      }
      runtimeInfo = data;
      applyRuntimeValuesFromData(data);
      captureRuntimeInitialValues(data, projectId);
      runtimeLoaded = true;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      runtimeError = err.message || "Failed to load runtime.";
      runtimeInfo = null;
      runtimeLoaded = false;
    } finally {
      if (projectId === selectedProjectId) {
        runtimeLoading = false;
      }
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
    timeoutEdgeRuns = new Map();
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

  function registerTimeoutEdge(edgeId, startedAt, timeoutMs) {
    if (!edgeId) return;
    const resolvedTimeout = Number(timeoutMs);
    if (!Number.isFinite(resolvedTimeout) || resolvedTimeout <= 0) {
      return;
    }
    const resolvedStart = Number.isFinite(Number(startedAt)) ? Number(startedAt) : Date.now();
    const next = new Map(timeoutEdgeRuns);
    next.set(edgeId, { id: edgeId, startedAt: resolvedStart, timeoutMs: resolvedTimeout });
    timeoutEdgeRuns = next;
    const removalStart = resolvedStart;
    setTimeout(() => {
      const current = timeoutEdgeRuns.get(edgeId);
      if (!current || current.startedAt !== removalStart) return;
      const updated = new Map(timeoutEdgeRuns);
      updated.delete(edgeId);
      timeoutEdgeRuns = updated;
    }, resolvedTimeout + 50);
  }

  function clearTimeoutEdge(edgeId) {
    if (!edgeId) return;
    if (!timeoutEdgeRuns.has(edgeId)) return;
    const next = new Map(timeoutEdgeRuns);
    next.delete(edgeId);
    timeoutEdgeRuns = next;
  }

  function clearTimeoutEdgesForNode(nodeId) {
    if (!nodeId || !sceneFlow?.edges?.length) return;
    const tedgeIds = sceneFlow.edges
      .filter((edge) => edge.type === "TEDGE" && edge.sourceId === nodeId)
      .map((edge) => edge.id);
    if (tedgeIds.length === 0) return;
    const next = new Map(timeoutEdgeRuns);
    let changed = false;
    for (const edgeId of tedgeIds) {
      if (next.delete(edgeId)) {
        changed = true;
      }
    }
    if (changed) {
      timeoutEdgeRuns = next;
    }
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
        recentLoaded = false;
        recentError = "";
        if (!showEditor) {
          loadRecent();
        }
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
        if (message.payload?.projectId && message.payload.projectId !== selectedProjectId) {
          return;
        }
        config = message.payload.config;
        configDraft = { ...config };
      }
      if (message.name === "ProjectConfig.Changed" && message.payload?.config) {
        if (message.payload?.projectId && message.payload.projectId !== selectedProjectId) {
          return;
        }
        projectConfig = normalizeProjectConfig(message.payload.config);
        if (!projectConfigDialogOpen) {
          projectConfigDraft = cloneProjectConfig(projectConfig);
        }
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
          clearTimeoutEdgesForNode(nodeId);
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
          if ((message.payload?.edgeType || "") === "TEDGE") {
            clearTimeoutEdge(edgeId);
          } else {
            registerEdgeActivity(edgeId);
          }
        }
      }
      if (message.name === "SceneFlow.Timeout.Started") {
        if (!activityProjectMatches(message.payload)) return;
        const superNodeId = (message.payload?.superNodeId || "").trim();
        if (superNodeId && superNodeId !== (sceneFlow?.superNodeId || "")) {
          return;
        }
        const edgeId = resolveActivityEdgeId(message.payload);
        if (edgeId) {
          registerTimeoutEdge(edgeId, message.payload?.startedAt, message.payload?.timeoutMs);
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

  function resetProjectLoadState() {
    configLoading = false;
    configLoaded = false;
    configError = "";
    scriptLoading = false;
    scriptLoaded = false;
    scriptScenesLoaded = false;
    scriptElementsLoaded = false;
    sceneFlowLoaded = false;
    runtimeLoaded = false;
    scriptError = "";
    scriptScenesError = "";
    scriptElementsError = "";
    sceneFlowError = "";
    runtimeError = "";
  }

  function buildProjectLoadErrors() {
    const errors = [];
    if (configError) {
      errors.push({ section: "Preferences", message: configError });
    }
    if (scriptError) {
      errors.push({ section: "Scene Script", message: scriptError });
    }
    if (scriptScenesError) {
      errors.push({ section: "Scenes", message: scriptScenesError });
    }
    if (scriptElementsError) {
      errors.push({ section: "Script Elements", message: scriptElementsError });
    }
    if (sceneFlowError) {
      errors.push({ section: "SceneFlow", message: sceneFlowError });
    }
    if (runtimeError) {
      errors.push({ section: "Runtime", message: runtimeError });
    }
    return errors;
  }

  function collectUnsavedReasons() {
    const reasons = [];
    if (selectedProject?.dirty) {
      reasons.push("SceneFlow: graph edits are not saved.");
    }
    if (scriptDirty) {
      reasons.push("Scene Script: unapplied edits.");
    }
    if (configDirty) {
      reasons.push("Preferences: edits are not applied.");
    } else if (configSaved === false) {
      reasons.push("Preferences: applied but not saved to the project.");
    }
    if (projectConfigDirty) {
      reasons.push("Project settings: edits are not applied.");
    } else if (projectConfigPending) {
      reasons.push("Project settings: applied but not saved to project.xml.");
    }
    return Array.from(new Set(reasons));
  }

  async function returnToLanding(closeProjectOnServer = false) {
    loadConfirmOpen = false;
    loadConfirmReasons = [];
    const projectId = selectedProjectId;
    if (closeProjectOnServer && projectId) {
      await closeProject(projectId);
      return;
    }
    selectedProjectId = "";
    showEditor = false;
    projectLoadAttempted = false;
    projectLoadProjectId = "";
    resetProjectLoadState();
    recentLoaded = false;
  }

  function cancelLoadConfirm() {
    loadConfirmOpen = false;
    loadConfirmReasons = [];
  }

  async function confirmReturnToLanding() {
    await returnToLanding(true);
  }

  function requestReturnToLanding() {
    const reasons = collectUnsavedReasons();
    if (reasons.length) {
      loadConfirmReasons = reasons;
      loadConfirmOpen = true;
      return;
    }
    returnToLanding(true);
  }

  function openSaveAsDialog() {
    saveAsPath = "";
    saveAsDialogOpen = true;
  }

  function closeSaveAsDialog() {
    saveAsDialogOpen = false;
  }

  async function confirmSaveAs() {
    const target = saveAsPath;
    if (!target) return;
    await saveAsProject(selectedProjectId, target);
    saveAsDialogOpen = false;
  }

  async function removeRecentProject(path) {
    if (!path) return;
    await apiPost("/api/v1/projects/recent/remove", { path });
    await loadRecent();
  }

  function closeRecentFailureDialog() {
    recentFailureOpen = false;
    recentFailureProject = null;
    recentFailureMessage = "";
  }

  function filterKeyValues(values, filter) {
    const query = (filter || "").toLowerCase();
    const entries = Object.entries(values);
    if (!query) return entries;
    return entries.filter(([key, value]) => {
      return key.toLowerCase().includes(query) || String(value).toLowerCase().includes(query);
    });
  }

  function normalizeConfigFeatures(features) {
    if (!Array.isArray(features)) return [];
    return features.map((feature) => ({
      key: feature?.key ?? "",
      value: feature?.value ?? ""
    }));
  }

  function normalizeProjectConfig(config) {
    const safe = config || {};
    return {
      name: safe.name ?? "",
      plugins: Array.isArray(safe.plugins)
        ? safe.plugins.map((plugin) => ({
            type: plugin?.type ?? "device",
            name: plugin?.name ?? "",
            className: plugin?.className ?? "",
            load: plugin?.load !== false,
            features: normalizeConfigFeatures(plugin?.features)
          }))
        : [],
      agents: Array.isArray(safe.agents)
        ? safe.agents.map((agent) => ({
            name: agent?.name ?? "",
            device: agent?.device ?? "",
            features: normalizeConfigFeatures(agent?.features)
          }))
        : [],
      player: {
        features: normalizeConfigFeatures(safe.player?.features)
      }
    };
  }

  function keyHintOptions(keys) {
    if (!keys || keys.supported === false) return [];
    const required = Array.isArray(keys.required)
      ? keys.required.map((entry) => ({ ...entry, kind: "required" }))
      : [];
    const optional = Array.isArray(keys.optional)
      ? keys.optional.map((entry) => ({ ...entry, kind: "optional" }))
      : [];
    return [...required, ...optional];
  }

  function keyHintLabel(entry) {
    if (!entry) return "";
    const desc = (entry.description || "").trim();
    const prefix = entry.kind === "required" ? "required" : "optional";
    return desc ? `${prefix}: ${desc}` : prefix;
  }

  function cloneProjectConfig(config) {
    return JSON.parse(JSON.stringify(config));
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

  function edgeTimeoutSpec(edge) {
    if (!edge) return "";
    const expr = (edge.timeoutExpr ?? "").trim();
    if (expr) return expr;
    if (edge.timeoutMs !== undefined && edge.timeoutMs !== null) {
      return String(edge.timeoutMs);
    }
    return "";
  }

  function isTimeoutNumber(value) {
    return /^\d+$/.test(String(value || "").trim());
  }

  function isTimeoutVarName(value) {
    const name = String(value || "").trim();
    if (!name) return false;
    return sceneFlowIntVarNames.includes(name);
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

  function playAgentCommand(name) {
    const raw = String(name || "");
    const escaped = raw.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
    return `PlayAction("[${escaped} command]")`;
  }

  function extractSceneAgents(text) {
    if (!text) return [];
    const map = new Map();
    const lines = String(text).split(/\r?\n/);
    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith("//") || trimmed.startsWith("#")) {
        continue;
      }
      const match = trimmed.match(/^([A-Za-z0-9_][^:]*)\s*:/);
      if (!match) continue;
      const name = match[1].trim();
      if (!name) continue;
      const key = name.toLowerCase();
      if (key === "scene") continue;
      if (!map.has(key)) {
        map.set(key, name);
      }
    }
    return Array.from(map.values()).sort((a, b) => a.localeCompare(b));
  }

  function extractDeviceAgents(agents) {
    if (!Array.isArray(agents)) return [];
    const map = new Map();
    for (const agent of agents) {
      const name = String(agent?.name || "").trim();
      if (!name) continue;
      const key = name.toLowerCase();
      if (!map.has(key)) {
        map.set(key, name);
      }
    }
    return Array.from(map.values()).sort((a, b) => a.localeCompare(b));
  }

  function buildAgentGroups(sceneAgents, deviceAgents) {
    const sceneMap = new Map();
    const deviceMap = new Map();
    for (const name of sceneAgents || []) {
      sceneMap.set(String(name).toLowerCase(), name);
    }
    for (const name of deviceAgents || []) {
      deviceMap.set(String(name).toLowerCase(), name);
    }
    const output = [];
    const processing = [];
    for (const [key, name] of sceneMap.entries()) {
      output.push({ name, type: "output", shared: deviceMap.has(key) });
    }
    output.sort((a, b) => a.name.localeCompare(b.name));
    for (const [key, name] of deviceMap.entries()) {
      if (sceneMap.has(key)) continue;
      processing.push({ name, type: "processing", shared: false });
    }
    processing.sort((a, b) => a.name.localeCompare(b.name));
    return {
      input: [],
      processing,
      output
    };
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

  function startAgentDrag(event, agent, agentType) {
    if (!event?.dataTransfer || !agent?.name) return;
    const payload = {
      kind: "agent",
      name: agent.name,
      type: agentType || agent.type || "processing"
    };
    event.dataTransfer.setData(AGENT_DRAG_TYPE, JSON.stringify(payload));
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

  function parseAgentDrop(event) {
    const data = event?.dataTransfer;
    if (!data) return null;
    const raw = data.getData(AGENT_DRAG_TYPE);
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

  function isSceneDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(SCENE_DRAG_TYPE) || (types.includes("text/plain") && !types.includes(AGENT_DRAG_TYPE));
  }

  function isAgentDrag(event) {
    const types = Array.from(event?.dataTransfer?.types || []);
    return types.includes(AGENT_DRAG_TYPE);
  }

  function handleSceneDropOver(event) {
    if (!isSceneDrag(event) && !isAgentDrag(event)) return;
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

  async function addAgentCommandToNode(nodeId, agentName, { selectNode = false } = {}) {
    if (!selectedProjectId || !nodeId || !agentName) return;
    const response = await runSceneFlowCommand("SceneFlow.Node.Cmd.Add", {
      projectId: selectedProjectId,
      nodeId,
      command: { text: playAgentCommand(agentName) }
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

  async function handleSceneFlowAgentDrop(payload) {
    if (!payload?.name || !selectedProjectId) return;
    if (payload.targetNodeId) {
      await addAgentCommandToNode(payload.targetNodeId, payload.name, { selectNode: true });
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
    await addAgentCommandToNode(response.nodeId, payload.name, { selectNode: true });
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
    const agentPayload = parseAgentDrop(event);
    if (agentPayload?.name && nodeEditorTarget?.id) {
      event.preventDefault();
      await addAgentCommandToNode(nodeEditorTarget.id, agentPayload.name);
      return;
    }
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
      timeoutSpec: edgeTimeoutSpec(selectedEdge),
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
      const raw = String(edgeDraft.timeoutSpec ?? "").trim();
      if (!raw) {
        edgeEditError = "Timeout is required.";
        return;
      }
      if (isTimeoutNumber(raw)) {
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
        if (selectedEdge.timeoutExpr) {
          fields.timeoutExpr = "";
        }
      } else {
        if (!isTimeoutVarName(raw)) {
          edgeEditError = sceneFlowIntVarNames.length
            ? "Timeout must be an integer sceneflow variable."
            : "No integer sceneflow variables defined.";
          return;
        }
        if (raw !== (selectedEdge.timeoutExpr ?? "")) {
          fields.timeoutExpr = raw;
        }
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
  {#if !showEditor}
    <header class="hero">
      <div class="hero-brand">
        <img class="hero-logo" src="/images/vsm_logo.svg" alt="Visual SceneMaker" />
        <div>
          <h1>Visual SceneMaker Web</h1>
        <p>
          Revision <span title={infoRevision}>{infoRevisionSlug}</span>&nbsp;•&nbsp;Build date {infoBuildDate}
        </p>
        </div>
      </div>
      <button
        type="button"
        class="badge badge-toggle"
        on:click={() => (showTokenSection = !showTokenSection)}
        aria-pressed={showTokenSection}
        aria-label="Toggle token entry"
        title="Toggle token entry"
      >
        <span class:ok={wsConnected}>{wsConnected ? "connected" : "offline"}</span>
      </button>
    </header>

    {#if showTokenSection}
      <section class="panel connect">
        <div class="field">
          <label for="token">Token</label>
          <input id="token" placeholder="Paste token from server log" bind:value={token} />
        </div>
        <p class="muted">
          Localhost auto-fetches tokens from `/api/v1/token`. For LAN access, use the token printed on server start.
          Flags: `--allow-lan` to bind 0.0.0.0, `--no-browser` to disable auto-open.
        </p>
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
    {/if}
  {/if}

  <div class="grid">
    {#if !showEditor}
    <section class="panel">
      <header class="panel-title">
        <h2>Projects</h2>
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

      {#if projectLoadAttempted}
        {#if projectLoadErrors.length}
          <div class="project-load-errors">
            <div class="project-load-title">Project load failed</div>
            <ul>
              {#each projectLoadErrors as entry}
                <li>
                  <strong>{entry.section}:</strong> {entry.message}
                </li>
              {/each}
            </ul>
          </div>
        {:else if projectLoadPending}
          <p class="muted">Loading project data...</p>
        {/if}
      {/if}

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

    </section>
    <section class="panel">
        <header class="panel-title">
          <h2>Recent projects</h2>
        </header>
        <div class="project-list">
          {#if recentLoading}
            <p class="muted">Loading recent projects...</p>
          {:else if recentError}
            <p class="error">{recentError}</p>
          {:else if recent.length === 0}
            <p class="muted">No recent projects.</p>
          {:else}
            {#each recent as project}
              <button type="button" on:click={() => openRecentProject(project)}>
                <span>{project.name}</span>
                <span class="meta">{project.date || ""}</span>
              </button>
            {/each}
          {/if}
        </div>
      </section>
      <section class="panel">
        <header class="panel-title">
          <h2>Tutorials</h2>
        </header>
        <div class="project-list">
          {#if tutorials.length === 0}
            <p class="muted">No tutorials available.</p>
          {:else}
            {#each tutorials as project}
              <button type="button" on:click={() => openProject(project.path)}>
                <span>{project.name}</span>
              </button>
            {/each}
          {/if}
        </div>
      </section>
    {/if}

    {#if showEditor}
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
          class="ghost danger"
          on:click={requestReturnToLanding}
          disabled={!selectedProject}
        >
          Close
        </button>
        {#if projectRequiresSaveAs}
          <button
            type="button"
            class="ghost"
            on:click={openSaveAsDialog}
            disabled={!selectedProject}
          >
            Save As
          </button>
        {:else}
          <button
            type="button"
            class="ghost"
            on:click={() => saveProject(selectedProjectId)}
            disabled={!selectedProject}
          >
            Save
          </button>
        {/if}
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
            <button
              type="button"
              class="sceneflow-gear"
              on:click={openProjectConfigDialog}
              disabled={!selectedProject || !wsConnected}
              aria-label="Open project modules"
              title="Project modules"
            >
              <IconPuzzle className="icon" />
            </button>
            <button
              type="button"
              class="sceneflow-gear"
              on:click={openPrefsDialog}
              disabled={!selectedProject || !wsConnected}
              aria-label="Open preferences"
              title="Preferences"
            >
              <IconGear className="icon" />
            </button>
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
              <div class="sceneflow-breadcrumbs">
                <span class="muted">Path: {sceneFlow.path.join(" / ")}</span>
              </div>
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
                  style="color:#7A7D81"
                  draggable="true"
                  on:click={() => createSceneFlowNode("Super")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "node", nodeType: "Super" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d={superNodeIconPath(16, 16)} transform="translate(4 4)" fill="currentColor" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  title="Node"
                  aria-label="Node"
                  style="color:#7A7D81"
                  draggable="true"
                  on:click={() => createSceneFlowNode("Basic")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "node", nodeType: "Basic" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <ellipse cx="12" cy="12" rx="7" ry="7" fill="currentColor" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  title="Comment"
                  aria-label="Comment"
                  style="color:#7A7D81"
                  draggable="true"
                  on:click={createSceneFlowComment}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "comment" })}
                  disabled={!selectedProject || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <rect x="4.5" y="6" width="15" height="12" rx="3.5" ry="3.5" />
                    <line x1="7" y1="10" x2="17" y2="10" />
                    <line x1="7" y1="14" x2="15" y2="14" />
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "EEDGE"}
                  title="Epsilon edge"
                  aria-label="Epsilon edge"
                  style="color:#7A7D81"
                  draggable="true"
                  on:click={() => startEdgeCreate("EEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "EEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 0)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9"></text>
                    </g>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "PEDGE"}
                  title="Probabilistic edge"
                  aria-label="Probabilistic edge"
                  style="color:#5BAE7A"
                  draggable="true"
                  on:click={() => startEdgeCreate("PEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "PEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 4)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9">P</text>
                    </g>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "FEDGE"}
                  title="Fork edge"
                  aria-label="Fork edge"
                  style="color:#5B8EDC"
                  draggable="true"
                  on:click={() => startEdgeCreate("FEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "FEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 0)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9"></text>
                    </g>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "CEDGE"}
                  title="Conditional edge"
                  aria-label="Conditional edge"
                  style="color:#FFC857"
                  draggable="true"
                  on:click={() => startEdgeCreate("CEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "CEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 4)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9">C</text>
                    </g>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "TEDGE"}
                  title="Timeout edge"
                  aria-label="Timeout edge"
                  style="color:#A06A4B"
                  draggable="true"
                  on:click={() => startEdgeCreate("TEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "TEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 4)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9">T</text>
                    </g>
                  </svg>
                </button>
                <button
                  type="button"
                  class="block-icon"
                  class:active={edgeCreateMode && edgeCreateType === "IEDGE"}
                  title="Interruptive edge"
                  aria-label="Interruptive edge"
                  style="color:#E26D5A"
                  draggable="true"
                  on:click={() => startEdgeCreate("IEDGE")}
                  on:dragstart={(event) => startBlockDrag(event, { kind: "edge", edgeType: "IEDGE" })}
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <g transform="translate(0 4)">
                      <path d="M4 12h12" stroke="currentColor" />
                      <path d="M14 9.5l5 2.5-5 2.5z" fill="currentColor" />
                      <text class="block-icon-text edge-symbol" x="5" y="9">I</text>
                    </g>
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
            <div class="blocks-section blocks-section--agents">
              <div class="block-section-title">Agents</div>
              {#if !selectedProject}
                <p class="muted">Select a project to view agents.</p>
              {:else if scriptLoading || projectConfigLoading}
                <p class="muted">Loading agents...</p>
              {:else if agentGroups.input.length === 0 &&
                agentGroups.processing.length === 0 &&
                agentGroups.output.length === 0}
                <p class="muted">No agents found.</p>
              {:else}
                <div class="agent-list">
                  <div class="scene-group agent-group">
                    <div class="scene-group-title">Input</div>
                    <div class="scene-items" role="list">
                      {#if agentGroups.input.length === 0}
                        <div class="agent-empty">No agents.</div>
                      {:else}
                        {#each agentGroups.input as agent}
                          <div class="scene-item agent-item" role="listitem">
                            <div class="scene-item-main">
                              <span
                                class="scene-drag-handle agent-drag-handle"
                                draggable="true"
                                on:dragstart={(event) => startAgentDrag(event, agent, "input")}
                                role="button"
                                tabindex="0"
                                aria-label={`Drag agent ${agent.name}`}
                                title="Drag agent"
                              >
                                <svg
                                  class="scene-drag-icon agent-icon"
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
                                    d={AGENT_ICON_PATHS.input}
                                  />
                                </svg>
                              </span>
                              <span class="scene-name agent-name">{agent.name}</span>
                            </div>
                          </div>
                        {/each}
                      {/if}
                    </div>
                  </div>
                  <div class="scene-group agent-group">
                    <div class="scene-group-title">Processing</div>
                    <div class="scene-items" role="list">
                      {#if agentGroups.processing.length === 0}
                        <div class="agent-empty">No agents.</div>
                      {:else}
                        {#each agentGroups.processing as agent}
                          <div class="scene-item agent-item" role="listitem">
                            <div class="scene-item-main">
                              <span
                                class="scene-drag-handle agent-drag-handle"
                                draggable="true"
                                on:dragstart={(event) => startAgentDrag(event, agent, "processing")}
                                role="button"
                                tabindex="0"
                                aria-label={`Drag agent ${agent.name}`}
                                title="Drag agent"
                              >
                                <svg
                                  class="scene-drag-icon agent-icon"
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
                                    d={AGENT_ICON_PATHS.processing}
                                  />
                                </svg>
                              </span>
                              <span class="scene-name agent-name">{agent.name}</span>
                            </div>
                          </div>
                        {/each}
                      {/if}
                    </div>
                  </div>
                  <div class="scene-group agent-group">
                    <div class="scene-group-title">Output</div>
                    <div class="scene-items" role="list">
                      {#if agentGroups.output.length === 0}
                        <div class="agent-empty">No agents.</div>
                      {:else}
                        {#each agentGroups.output as agent}
                          <div class="scene-item agent-item" role="listitem">
                            <div class="scene-item-main">
                              <span
                                class="scene-drag-handle agent-drag-handle"
                                draggable="true"
                                on:dragstart={(event) => startAgentDrag(event, agent, "output")}
                                role="button"
                                tabindex="0"
                                aria-label={`Drag agent ${agent.name}`}
                                title="Drag agent"
                              >
                                <svg
                                  class="scene-drag-icon agent-icon"
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
                                    d={AGENT_ICON_PATHS.output}
                                  />
                                </svg>
                              </span>
                              <span class="scene-name agent-name" class:shared={agent.shared}>{agent.name}</span>
                            </div>
                          </div>
                        {/each}
                      {/if}
                    </div>
                  </div>
                </div>
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
                              <span
                                class="scene-name"
                                title={group.name}
                                use:fitMiddleEllipsis={{ text: group.name }}
                              ></span>
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
                timeoutEdges={timeoutEdgeList}
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
                onAgentDrop={handleSceneFlowAgentDrop}
                agentDragType={AGENT_DRAG_TYPE}
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
                  <label for="edge-timeout">Timeout (ms or int variable)</label>
                  <input
                    id="edge-timeout"
                    type="text"
                    list="edge-timeout-vars"
                    placeholder="1000 or timeout_ms"
                    bind:value={edgeDraft.timeoutSpec}
                  />
                  <datalist id="edge-timeout-vars">
                    {#each sceneFlowIntVarNames as varName}
                      <option value={varName}></option>
                    {/each}
                  </datalist>
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
    {/if}
  </div>

  {#if loadConfirmOpen}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={cancelLoadConfirm}
      aria-label="Close dialog"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="load-confirm-title">
        <h3 id="load-confirm-title">Close project?</h3>
        <div class="modal-body">
          <p>Closing will discard unsaved changes in these areas:</p>
          <ul class="load-confirm-list">
            {#each loadConfirmReasons as reason}
              <li>{reason}</li>
            {/each}
          </ul>
        </div>
        <div class="row">
          <button type="button" class="ghost" on:click={cancelLoadConfirm}>Cancel</button>
          <button type="button" class="danger" on:click={confirmReturnToLanding}>Close</button>
        </div>
      </div>
    </button>
  {/if}

  {#if saveAsDialogOpen}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={closeSaveAsDialog}
      aria-label="Close dialog"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="save-as-title">
        <h3 id="save-as-title">Save project as</h3>
        <div class="modal-body">
          <label for="save-as-path">Save to path</label>
          <input
            id="save-as-path"
            placeholder="/abs/path/to/project"
            bind:value={saveAsPath}
          />
          <p class="muted">Choose a new folder for this project.</p>
        </div>
        <div class="row">
          <button type="button" class="ghost" on:click={closeSaveAsDialog}>Cancel</button>
          <button type="button" class="primary" on:click={confirmSaveAs} disabled={!saveAsPath}>Save As</button>
        </div>
      </div>
    </button>
  {/if}

  {#if recentFailureOpen}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={closeRecentFailureDialog}
      aria-label="Close dialog"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="recent-failure-title">
        <h3 id="recent-failure-title">Cannot open recent project</h3>
        <div class="modal-body">
          <p>{recentFailureMessage || "The project could not be opened."}</p>
          {#if recentFailureProject}
            <div class="stack">
              <div><strong>{recentFailureProject.name}</strong></div>
              <div class="muted">{recentFailureProject.path}</div>
            </div>
          {/if}
        </div>
        <div class="row">
          <button type="button" class="ghost" on:click={closeRecentFailureDialog}>Cancel</button>
          <button
            type="button"
            class="danger"
            on:click={async () => {
              await removeRecentProject(recentFailureProject?.path);
              closeRecentFailureDialog();
            }}
          >
            Remove from recent
          </button>
        </div>
      </div>
    </button>
  {/if}

  {#if projectConfigDialogOpen}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={closeProjectConfigDialog}
      aria-label="Close dialog"
    >
      <div class="modal project-config-modal" role="dialog" aria-modal="true" aria-labelledby="project-config-title">
        <div class="project-config-header">
          <div class="project-config-title">
            <span class="project-config-icon">
              <IconPuzzle className="icon" />
            </span>
            <div>
              <h3 id="project-config-title">Project settings</h3>
              <span class="muted">Stored in project.xml</span>
            </div>
          </div>
          <div class="project-config-header-actions">
            <button
              type="button"
              class="ghost"
              on:click={() => loadProjectConfig(selectedProjectId)}
              disabled={!selectedProject || projectConfigLoading}
            >
              Reload
            </button>
          </div>
        </div>
        <div class="project-config-body">
          <aside class="project-config-tree">
            <button
              type="button"
              class="project-config-tree-item root"
              class:active={projectConfigSelection.type === "project"}
              on:click={() => selectProjectConfig({ type: "project" })}
            >
              <span>{projectConfigView.name || "Project"}</span>
            </button>
            <div class="project-config-tree-section">
              <button
                type="button"
                class="project-config-tree-item"
                class:active={projectConfigSelection.type === "devices"}
                on:click={() => selectProjectConfig({ type: "devices" })}
              >
                <span>Devices</span>
              </button>
              {#if projectConfigPlugins.length === 0}
                <p class="muted">No devices configured.</p>
              {:else}
                {#each projectConfigPlugins as plugin, pluginIndex}
                  <button
                    type="button"
                    class="project-config-tree-item plugin"
                    class:active={
                      projectConfigSelection.type === "plugin" && projectConfigSelection.pluginIndex === pluginIndex
                    }
                    on:click={() => selectProjectConfig({ type: "plugin", pluginIndex })}
                  >
                    <span>{plugin.name || "Unnamed device"}</span>
                  </button>
                  <div class="project-config-tree-children">
                    {#each projectConfigAgentsByPlugin[pluginIndex] as entry}
                      <button
                        type="button"
                        class="project-config-tree-item agent"
                        class:active={
                          projectConfigSelection.type === "agent" &&
                          projectConfigSelection.agentIndex === entry.agentIndex
                        }
                        on:click={() => selectProjectConfig({ type: "agent", agentIndex: entry.agentIndex })}
                      >
                        <span>{entry.agent.name || "Unnamed agent"}</span>
                      </button>
                    {/each}
                  </div>
                {/each}
              {/if}
              <button
                type="button"
                class="project-config-tree-add"
                on:click={() => selectProjectConfig({ type: "devices" })}
              >
                + Add device
              </button>
            </div>
          </aside>
          <section class="project-config-main">
            {#if !selectedProject}
              <p class="muted">Select a project to edit settings.</p>
            {:else if projectConfigLoading}
              <p class="muted">Loading project settings...</p>
            {:else if projectConfigError}
              <p class="error">{projectConfigError}</p>
            {:else}
              {#if projectConfigSelection.type === "project"}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <h4>Project</h4>
                    <span class="muted">project.xml</span>
                  </div>
                  <div class="project-config-grid">
                    <label for="project-name-input">Project name</label>
                    <input
                      id="project-name-input"
                      value={projectConfigView.name}
                      on:input={(event) => updateProjectName(event.target.value)}
                    />
                  </div>
                  <div class="project-config-meta">
                    <span>{projectConfigPlugins.length} devices</span>
                    <span>{projectConfigAgents.length} agents</span>
                  </div>
                </div>
              {:else if projectConfigSelection.type === "devices"}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <h4>Add device</h4>
                    <span class="muted">Plugins loaded by this project</span>
                  </div>
                  <div class="project-config-grid">
                    <label for="device-name">Device name</label>
                    <input id="device-name" bind:value={projectConfigNewPlugin.name} />
                    <label for="device-class-select">Available devices</label>
                    <select
                      id="device-class-select"
                      value={projectConfigNewPlugin.className}
                      disabled={availableDevicesLoading || availableDevices.length === 0}
                      on:change={(event) => {
                        projectConfigNewPlugin = { ...projectConfigNewPlugin, className: event.target.value };
                      }}
                    >
                      <option value="">Select a device class</option>
                      {#each availableDevices as device}
                        <option value={device.className}>{device.name}</option>
                      {/each}
                    </select>
                    <label for="device-class">Class</label>
                    <input
                      id="device-class"
                      list="device-class-list"
                      bind:value={projectConfigNewPlugin.className}
                      placeholder={availableDevicesLoading ? "Loading devices..." : "Select or enter class"}
                    />
                  </div>
                  {#if availableDevicesError}
                    <p class="error">{availableDevicesError}</p>
                  {/if}
                  <div class="actions">
                    <button type="button" class="ghost" on:click={addPlugin}>Add device</button>
                  </div>
                </div>
              {:else if projectConfigSelection.type === "plugin" && selectedProjectPlugin}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <div>
                      <h4>Device</h4>
                      <span class="muted">{selectedProjectPlugin.name || "Unnamed device"}</span>
                    </div>
                    <button type="button" class="ghost danger" on:click={() => removePlugin(projectConfigSelection.pluginIndex)}>
                      Delete
                    </button>
                  </div>
                  <div class="project-config-info-grid">
                    <div class="project-config-info-row">
                      <label for="plugin-name" class="project-config-info-label">Device name</label>
                      <input
                        id="plugin-name"
                        value={selectedProjectPlugin.name}
                        on:input={(event) => updatePluginName(projectConfigSelection.pluginIndex, event.target.value)}
                      />
                    </div>
                    <div class="project-config-info-row">
                      <label for="plugin-class-select" class="project-config-info-label">Available devices</label>
                      <select
                        id="plugin-class-select"
                        value={selectedProjectPlugin.className}
                        disabled={availableDevicesLoading || availableDevices.length === 0}
                        on:change={(event) =>
                          updatePluginField(projectConfigSelection.pluginIndex, "className", event.target.value)
                        }
                      >
                        <option value="">Select a device class</option>
                        {#each availableDevices as device}
                          <option value={device.className}>{device.name}</option>
                        {/each}
                      </select>
                    </div>
                    <div class="project-config-info-row">
                      <label for="plugin-class" class="project-config-info-label">Class</label>
                      <input
                        id="plugin-class"
                        list="device-class-list"
                        value={selectedProjectPlugin.className}
                        on:input={(event) => updatePluginField(projectConfigSelection.pluginIndex, "className", event.target.value)}
                      />
                    </div>
                    <label class="project-config-toggle">
                      <input
                        type="checkbox"
                        checked={selectedProjectPlugin.load}
                        on:change={(event) =>
                          updatePluginField(projectConfigSelection.pluginIndex, "load", event.target.checked)
                        }
                      />
                      <span>Load plugin</span>
                    </label>
                  </div>
                  <div class="project-config-table">
                    <div class="project-config-table-header">
                      <span>Key</span>
                      <span>Value</span>
                      <span></span>
                    </div>
                    {#if selectedProjectPlugin.features.length === 0}
                      <div class="project-config-table-empty">No entries yet.</div>
                    {:else}
                      {#each selectedProjectPlugin.features as feature, featureIndex}
                        <div class="project-config-table-row">
                          <input
                            list="plugin-key-hints"
                            value={feature.key}
                            placeholder="key"
                            on:input={(event) =>
                              updatePluginFeature(
                                projectConfigSelection.pluginIndex,
                                featureIndex,
                                "key",
                                event.target.value
                              )
                            }
                          />
                          <input
                            value={feature.value}
                            placeholder="value"
                            on:input={(event) =>
                              updatePluginFeature(
                                projectConfigSelection.pluginIndex,
                                featureIndex,
                                "value",
                                event.target.value
                              )
                            }
                          />
                          <button
                            type="button"
                            class="ghost icon-button danger"
                            on:click={() => removePluginFeature(projectConfigSelection.pluginIndex, featureIndex)}
                          >
                            <IconTrash className="icon" />
                          </button>
                        </div>
                      {/each}
                    {/if}
                    <div class="project-config-table-add">
                      <input list="plugin-key-hints" placeholder="key" bind:value={projectConfigNewFeature.key} />
                      <input placeholder="value" bind:value={projectConfigNewFeature.value} />
                      <button type="button" class="ghost" on:click={addFeatureToSelection}>Add</button>
                    </div>
                    <datalist id="plugin-key-hints">
                      {#each pluginKeyOptions as option}
                        <option value={option.name} label={keyHintLabel(option)}>{option.name}</option>
                      {/each}
                    </datalist>
                  </div>
                  {#if selectedProjectPluginKeysLoading}
                    <p class="muted">Loading key hints...</p>
                  {:else if selectedProjectPluginKeysError}
                    <p class="error">{selectedProjectPluginKeysError}</p>
                  {:else if selectedProjectPluginKeys}
                    {#if selectedProjectPluginKeys.supported === false}
                      <p class="muted">No key hints provided by this extension.</p>
                    {:else}
                      <div class="project-config-keylist">
                        <div class="project-config-keylist-title">Key hints</div>
                        <div class="project-config-keygrid">
                          <div>
                            <div class="project-config-key-title">Required</div>
                            {#if selectedProjectPluginKeys.required?.length}
                              <div class="project-config-key-list">
                                {#each selectedProjectPluginKeys.required as entry}
                                  <div class="project-config-key-item">
                                    <span>{entry.name}</span>
                                    {#if entry.description}
                                      <span class="project-config-key-desc">{entry.description}</span>
                                    {/if}
                                  </div>
                                {/each}
                              </div>
                            {:else}
                              <div class="muted">None</div>
                            {/if}
                          </div>
                          <div>
                            <div class="project-config-key-title">Optional</div>
                            {#if selectedProjectPluginKeys.optional?.length}
                              <div class="project-config-key-list">
                                {#each selectedProjectPluginKeys.optional as entry}
                                  <div class="project-config-key-item">
                                    <span>{entry.name}</span>
                                    {#if entry.description}
                                      <span class="project-config-key-desc">{entry.description}</span>
                                    {/if}
                                  </div>
                                {/each}
                              </div>
                            {:else}
                              <div class="muted">None</div>
                            {/if}
                          </div>
                        </div>
                      </div>
                    {/if}
                  {/if}
                  <div class="project-config-agent-add">
                    <div class="project-config-agent-add-title">Add agent</div>
                    <div class="project-config-agent-add-row">
                      <input placeholder="Agent name" bind:value={projectConfigNewAgent.name} />
                      <button type="button" class="ghost" on:click={() => addAgent(selectedProjectPlugin.name)}>
                        Add
                      </button>
                    </div>
                  </div>
                </div>
              {:else if projectConfigSelection.type === "agent" && selectedProjectAgent}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <div>
                      <h4>Agent</h4>
                      <span class="muted">{selectedProjectAgent.name || "Unnamed agent"}</span>
                    </div>
                    <button type="button" class="ghost danger" on:click={() => removeAgent(projectConfigSelection.agentIndex)}>
                      Delete
                    </button>
                  </div>
                  <div class="project-config-info-grid">
                    <div class="project-config-info-row">
                      <label for="agent-name-edit" class="project-config-info-label">Agent name</label>
                      <input
                        id="agent-name-edit"
                        value={selectedProjectAgent.name}
                        on:input={(event) => updateAgentField(projectConfigSelection.agentIndex, "name", event.target.value)}
                      />
                    </div>
                    <div class="project-config-info-row">
                      <label for="agent-device-edit" class="project-config-info-label">Device</label>
                      <select
                        id="agent-device-edit"
                        value={selectedProjectAgent.device}
                        on:change={(event) => updateAgentField(projectConfigSelection.agentIndex, "device", event.target.value)}
                      >
                        {#each projectConfigPlugins as plugin}
                          <option value={plugin.name}>{plugin.name}</option>
                        {/each}
                      </select>
                    </div>
                    <div class="project-config-info-row">
                      <span class="project-config-info-label">Class</span>
                      <span class="project-config-info-value">{activeProjectPlugin?.className || "Unknown"}</span>
                    </div>
                    <label class="project-config-toggle">
                      <input
                        type="checkbox"
                        checked={activeProjectPlugin?.load ?? true}
                        disabled={activeProjectPluginIndex < 0}
                        on:change={(event) => {
                          if (activeProjectPluginIndex >= 0) {
                            updatePluginField(activeProjectPluginIndex, "load", event.target.checked);
                          }
                        }}
                      />
                      <span>Load plugin</span>
                    </label>
                  </div>
                  <div class="project-config-table">
                    <div class="project-config-table-header">
                      <span>Key</span>
                      <span>Value</span>
                      <span></span>
                    </div>
                    {#if selectedProjectAgent.features.length === 0}
                      <div class="project-config-table-empty">No entries yet.</div>
                    {:else}
                      {#each selectedProjectAgent.features as feature, featureIndex}
                        <div class="project-config-table-row">
                          <input
                            list="agent-key-hints"
                            value={feature.key}
                            placeholder="key"
                            on:input={(event) =>
                              updateAgentFeature(projectConfigSelection.agentIndex, featureIndex, "key", event.target.value)
                            }
                          />
                          <input
                            value={feature.value}
                            placeholder="value"
                            on:input={(event) =>
                              updateAgentFeature(projectConfigSelection.agentIndex, featureIndex, "value", event.target.value)
                            }
                          />
                          <button
                            type="button"
                            class="ghost icon-button danger"
                            on:click={() => removeAgentFeature(projectConfigSelection.agentIndex, featureIndex)}
                          >
                            <IconTrash className="icon" />
                          </button>
                        </div>
                      {/each}
                    {/if}
                    <div class="project-config-table-add">
                      <input list="agent-key-hints" placeholder="key" bind:value={projectConfigNewFeature.key} />
                      <input placeholder="value" bind:value={projectConfigNewFeature.value} />
                      <button type="button" class="ghost" on:click={addFeatureToSelection}>Add</button>
                    </div>
                    <datalist id="agent-key-hints">
                      {#each agentKeyOptions as option}
                        <option value={option.name} label={keyHintLabel(option)}>{option.name}</option>
                      {/each}
                    </datalist>
                  </div>
                  {#if selectedProjectAgentKeysLoading}
                    <p class="muted">Loading key hints...</p>
                  {:else if selectedProjectAgentKeysError}
                    <p class="error">{selectedProjectAgentKeysError}</p>
                  {:else if selectedProjectAgentKeys}
                    {#if selectedProjectAgentKeys.supported === false}
                      <p class="muted">No key hints provided by this extension.</p>
                    {:else}
                      <div class="project-config-keylist">
                        <div class="project-config-keylist-title">Key hints</div>
                        <div class="project-config-keygrid">
                          <div>
                            <div class="project-config-key-title">Required</div>
                            {#if selectedProjectAgentKeys.required?.length}
                              <div class="project-config-key-list">
                                {#each selectedProjectAgentKeys.required as entry}
                                  <div class="project-config-key-item">
                                    <span>{entry.name}</span>
                                    {#if entry.description}
                                      <span class="project-config-key-desc">{entry.description}</span>
                                    {/if}
                                  </div>
                                {/each}
                              </div>
                            {:else}
                              <div class="muted">None</div>
                            {/if}
                          </div>
                          <div>
                            <div class="project-config-key-title">Optional</div>
                            {#if selectedProjectAgentKeys.optional?.length}
                              <div class="project-config-key-list">
                                {#each selectedProjectAgentKeys.optional as entry}
                                  <div class="project-config-key-item">
                                    <span>{entry.name}</span>
                                    {#if entry.description}
                                      <span class="project-config-key-desc">{entry.description}</span>
                                    {/if}
                                  </div>
                                {/each}
                              </div>
                            {:else}
                              <div class="muted">None</div>
                            {/if}
                          </div>
                        </div>
                      </div>
                    {/if}
                  {/if}
                </div>
              {:else if projectConfigSelection.type === "player"}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <h4>Player</h4>
                    <span class="muted">Runtime player properties</span>
                  </div>
                  <div class="project-config-table">
                    <div class="project-config-table-header">
                      <span>Key</span>
                      <span>Value</span>
                      <span></span>
                    </div>
                    {#if projectConfigPlayer.features.length === 0}
                      <div class="project-config-table-empty">No entries yet.</div>
                    {:else}
                      {#each projectConfigPlayer.features as feature, featureIndex}
                        <div class="project-config-table-row">
                          <input
                            value={feature.key}
                            placeholder="key"
                            on:input={(event) => updatePlayerFeature(featureIndex, "key", event.target.value)}
                          />
                          <input
                            value={feature.value}
                            placeholder="value"
                            on:input={(event) => updatePlayerFeature(featureIndex, "value", event.target.value)}
                          />
                          <button type="button" class="ghost icon-button danger" on:click={() => removePlayerFeature(featureIndex)}>
                            <IconTrash className="icon" />
                          </button>
                        </div>
                      {/each}
                    {/if}
                    <div class="project-config-table-add">
                      <input placeholder="key" bind:value={projectConfigNewFeature.key} />
                      <input placeholder="value" bind:value={projectConfigNewFeature.value} />
                      <button type="button" class="ghost" on:click={addFeatureToSelection}>Add</button>
                    </div>
                  </div>
                </div>
              {/if}
            {/if}
          </section>
        </div>
        <datalist id="device-class-list">
          {#each availableDevices as device}
            <option value={device.className} label={device.name}>{device.name}</option>
          {/each}
        </datalist>
        <div class="project-config-footer">
          <div class="project-config-status">
            {#if projectConfigError}
              <span class="error">{projectConfigError}</span>
            {/if}
            {#if projectConfigSaved !== null}
              <span class="muted">
                {projectConfigSaved ? "Saved" : projectConfigPending ? "Pending save" : "Not saved"}
              </span>
            {/if}
          </div>
          <div class="actions">
            <button
              type="button"
              class="primary"
              on:click={applyProjectConfig}
              disabled={!selectedProject || !wsConnected || !projectConfigDraft}
            >
              Apply
            </button>
            <button type="button" class="ghost" on:click={closeProjectConfigDialog}>Close</button>
          </div>
        </div>
      </div>
    </button>
  {/if}

  {#if prefsDialogOpen && prefsDialogDraft}
    <button
      type="button"
      class="modal-backdrop"
      on:click|self={closePrefsDialog}
      aria-label="Close dialog"
    >
      <div class="modal prefs-modal" role="dialog" aria-modal="true" aria-labelledby="prefs-dialog-title">
        <div class="prefs-header">
          <div class="prefs-title">
            <span class="prefs-title-icon">
              <IconGear className="icon" />
            </span>
            <div>
              <h3 id="prefs-dialog-title">Preferences</h3>
              <span class="prefs-subtitle">
                Applies to {selectedProject ? selectedProject.name : "the active project"}
              </span>
            </div>
          </div>
        </div>
        <div class="prefs-body">
          <section class="prefs-card">
            <header class="prefs-card-header">
              <h4>Visual appearance</h4>
              <span class="muted">Workspace sizing and visibility.</span>
            </header>
            <div class="prefs-group">
              <div class="prefs-group-title">Sizing</div>
              <div class="prefs-rows">
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-node-size">Node size</label>
                    <span class="prefs-help">Base size for nodes and supernodes.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-node-size"
                        type="number"
                        min={PREF_NODE_MIN}
                        max={PREF_NODE_MAX}
                        step="2"
                        bind:value={prefsDialogDraft.nodeSize}
                      />
                      <span>px</span>
                    </div>
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-grid-scale">Grid scale</label>
                    <span class="prefs-help">Spacing multiplier for snap points.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-grid-scale"
                        type="number"
                        min={PREF_GRID_MIN}
                        max={PREF_GRID_MAX}
                        step="1"
                        bind:value={prefsDialogDraft.gridScale}
                      />
                      <span>x</span>
                    </div>
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-workspace-font">Workspace font size</label>
                    <span class="prefs-help">Canvas labels and edge text size.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-workspace-font"
                        type="number"
                        min={PREF_FONT_MIN}
                        max={PREF_FONT_MAX}
                        step="1"
                        bind:value={prefsDialogDraft.workspaceFontSize}
                      />
                      <span>pt</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="prefs-group">
              <div class="prefs-group-title">Display</div>
              <div class="prefs-toggles">
                <label class="pref-toggle">
                  <input type="checkbox" bind:checked={prefsDialogDraft.drawGrid} />
                  <span class="pref-toggle-indicator" aria-hidden="true"></span>
                  <span class="pref-toggle-label">Draw grid</span>
                </label>
                <label class="pref-toggle">
                  <input type="checkbox" bind:checked={prefsDialogDraft.activityVisualization} />
                  <span class="pref-toggle-indicator" aria-hidden="true"></span>
                  <span class="pref-toggle-label">Activity visualization</span>
                </label>
                <label class="pref-toggle">
                  <input type="checkbox" bind:checked={prefsDialogDraft.activityTrace} />
                  <span class="pref-toggle-indicator" aria-hidden="true"></span>
                  <span class="pref-toggle-label">Activity trace</span>
                </label>
                <label class="pref-toggle">
                  <input type="checkbox" bind:checked={prefsDialogDraft.showNodeId} />
                  <span class="pref-toggle-indicator" aria-hidden="true"></span>
                  <span class="pref-toggle-label">Draw node ID</span>
                </label>
              </div>
            </div>
          </section>
          <section class="prefs-card">
            <header class="prefs-card-header">
              <h4>Script options</h4>
              <span class="muted">Editor font and preview.</span>
            </header>
            <div class="prefs-group">
              <div class="prefs-rows">
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-script-font">Script font</label>
                    <span class="prefs-help">Mono font family for the script editor.</span>
                  </div>
                  <div class="prefs-control">
                    <input
                      id="pref-script-font"
                      class="prefs-input"
                      list="script-font-options"
                      bind:value={prefsDialogDraft.scriptFontType}
                    />
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-script-font-size">Script font size</label>
                    <span class="prefs-help">Size used by the script editor.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-script-font-size"
                        type="number"
                        min={PREF_FONT_MIN}
                        max={PREF_FONT_MAX}
                        step="1"
                        bind:value={prefsDialogDraft.scriptFontSize}
                      />
                      <span>pt</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="prefs-preview" style={prefsPreviewStyle}>
              // sample script line
            </div>
            <datalist id="script-font-options">
              {#each SCRIPT_FONT_OPTIONS as option}
                <option value={option}>{option}</option>
              {/each}
            </datalist>
          </section>
        </div>
        <div class="actions">
          <button
            type="button"
            class="primary"
            on:click={applyPrefsDialog}
            disabled={!wsConnected || prefsDialogBusy}
          >
            Apply
          </button>
          <button type="button" class="ghost" on:click={closePrefsDialog} disabled={prefsDialogBusy}>
            Cancel
          </button>
        </div>
        {#if prefsDialogError}
          <p class="error">{prefsDialogError}</p>
        {/if}
      </div>
    </button>
  {/if}

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
