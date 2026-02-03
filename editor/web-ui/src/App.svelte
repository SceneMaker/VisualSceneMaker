<script>
  import { tick, onMount, onDestroy } from "svelte";
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
  import IconMonitor from "./icons/IconMonitor.svelte";

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
  let autoConnectTimer = null;
  let autoConnectAttempts = 0;
  let autoConnectInFlight = false;
  let info = null;
  let error = "";
  let statusMessage = "";
  let sessionReady = false;
  let showEditor = false;
  let editorManuallyClosed = false;
  let projectLoadAttempted = false;
  let projectLoadProjectId = "";
  let showTokenSection = false;
  const protocolBadgeText = "Protocol UI";
  const protocolBadgeTitle = "UI protocol (v1)";

  // Remote connection state (Phase 8.4)
  let showConnectDialog = false;
  let remoteServerUrl = localStorage.getItem("vsm_remote_server") || "";
  let remoteServerInput = "";
  let remoteConnecting = false;
  let remoteConnectionError = "";
  let isRemoteConnection = false; // true if connected to a server other than location.host
  let connectedServerName = ""; // Display name of connected server

  const SCENE_DRAG_TYPE = "application/x-vsm-scene";
  const AGENT_DRAG_TYPE = "application/x-vsm-agent";
  const BLOCK_DRAG_TYPE = "application/x-vsm-block";
  const SCENE_LANGUAGE_ALL = "__all__";
  const SCENEFLOW_ROOT_ID = "__root__";
  const SCENEFLOW_ZOOM_KEY = "vsm_scene_flow_zoom";
  const SCENEFLOW_ZOOM_MIN = 0.3;
  const SCENEFLOW_ZOOM_MAX = 3.5;
  const SCENEFLOW_TOGGLE_COOKIE = "vsm_sceneflow_toggles";
  const WS_REQUEST_TIMEOUT_MS = 20000;
  const AGENT_ICON_PATHS = {
    input:
      "M8.25 9V5.25A2.25 2.25 0 0 1 10.5 3h6a2.25 2.25 0 0 1 2.25 2.25v13.5A2.25 2.25 0 0 1 16.5 21h-6a2.25 2.25 0 0 1-2.25-2.25V15M12 9l3 3m0 0-3 3m3-3H2.25",
    processing:
      "M19.5 12c0-1.232-.046-2.453-.138-3.662a4.006 4.006 0 0 0-3.7-3.7 48.678 48.678 0 0 0-7.324 0 4.006 4.006 0 0 0-3.7 3.7c-.017.22-.032.441-.046.662M19.5 12l3-3m-3 3-3-3m-12 3c0 1.232.046 2.453.138 3.662a4.006 4.006 0 0 0 3.7 3.7 48.656 48.656 0 0 0 7.324 0 4.006 4.006 0 0 0 3.7-3.7c.017-.22.032-.441.046-.662M4.5 12l3 3m-3-3-3 3",
    output:
      "M8.25 9V5.25A2.25 2.25 0 0 1 10.5 3h6a2.25 2.25 0 0 1 2.25 2.25v13.5A2.25 2.25 0 0 1 16.5 21h-6a2.25 2.25 0 0 1-2.25-2.25V15M12 9l3 3m0 0-3 3m3-3H2.25"
  };
  const DEFAULT_AGENT_HOST = "localhost";
  const DEFAULT_AGENT_PORT = "7777";
  const DEFAULT_VAR_BADGE_STATE = {
    visible: true,
    global: { x: 16, y: 12, w: 240, h: 150 },
    local: { x: 16, y: 190, w: 240, h: 150 }
  };
  const DEFAULT_SCENEFLOW_TOGGLES = {
    nodeSnap: true,
    showCmds: true,
    showVars: true,
    showBlocks: true,
    showInspector: true
  };
  const AUTOSAVE_DELAY_MS = 5000;
  const VAR_BADGE_COOKIE = "vsm_var_badges";
  const VAR_BADGE_MIN_WIDTH = 180;
  const VAR_BADGE_MIN_HEIGHT = 90;
  const ACTIVITY_SUPERNODE_DECAY_MS = 900;
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
  const PREF_UNDO_DEFAULT = 500;
  const PREF_UNDO_MIN = 10;
  const PREF_UNDO_MAX = 5000;
  const PREF_COMMAND_LOG_DEFAULT = 5000;
  const PREF_COMMAND_LOG_MIN = 100;
  const PREF_COMMAND_LOG_MAX = 50000;
  const AUTO_CONNECT_MAX_ATTEMPTS = 12;
  const AUTO_CONNECT_RETRY_MS = 1000;
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

  function normalizeSceneFlowToggles(state) {
    return {
      nodeSnap: state?.nodeSnap !== undefined ? !!state.nodeSnap : DEFAULT_SCENEFLOW_TOGGLES.nodeSnap,
      showCmds: state?.showCmds !== undefined ? !!state.showCmds : DEFAULT_SCENEFLOW_TOGGLES.showCmds,
      showVars: state?.showVars !== undefined ? !!state.showVars : DEFAULT_SCENEFLOW_TOGGLES.showVars,
      showBlocks:
        state?.showBlocks !== undefined ? !!state.showBlocks : DEFAULT_SCENEFLOW_TOGGLES.showBlocks,
      showInspector:
        state?.showInspector !== undefined
          ? !!state.showInspector
          : DEFAULT_SCENEFLOW_TOGGLES.showInspector
    };
  }

  function loadSceneFlowToggles() {
    const raw = readCookie(SCENEFLOW_TOGGLE_COOKIE);
    if (!raw) {
      return { ...DEFAULT_SCENEFLOW_TOGGLES };
    }
    try {
      const parsed = JSON.parse(decodeURIComponent(raw));
      return normalizeSceneFlowToggles(parsed);
    } catch (err) {
      return { ...DEFAULT_SCENEFLOW_TOGGLES };
    }
  }

  function persistSceneFlowToggles(state) {
    const payload = encodeURIComponent(JSON.stringify(state));
    writeCookie(SCENEFLOW_TOGGLE_COOKIE, payload);
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
    sceneFlowShowVars = !sceneFlowShowVars;
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
    if (!sceneFlowShowVars) return;
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
  let openPathError = "";
  let createProjectError = "";
  let saveAsPath = "";
  let saveAsDialogOpen = false;
  let saveAsError = "";

  let openPathInput;
  let openPathPickerInput;
  let newProjectNameInput;
  let saveAsInputEl;
  let saveAsDialogEl;
  let loadConfirmDialogEl;
  let recentFailureDialogEl;
  let missingAgentDialogEl;
  let projectConfigDialogEl;
  let prefsDialogEl;
  let monitorDialogEl;
  let cmdDialogEl;
  let typeDefDialogEl;
  let typeDefNameInputEl;
  let varDefDialogEl;
  let varDefNameInputEl;
  let lastFocusedElement = null;

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
  let projectSaving = false;
  let autoSaveTimer = null;
  let autoSaving = false;
  let autoSaveStatus = "";
  let autoSaveReady = false;
  let autoSaveEnabled = true;

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
  let scriptAutoApplyTimer = null;
  let scriptAutoApplyInFlight = false;
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
  let missingAgentNames = [];
  let missingAgentDialogOpen = false;
  let missingAgentDrafts = [];
  let missingAgentDeviceOptions = [];
  let missingAgentError = "";
  let missingAgentBusy = false;
  const SELECTION_PREVIEW_LIMIT = 6;

  let sceneFlow = null;
  let sceneFlowError = "";
  let sceneFlowLoading = false;
  let sceneFlowLoaded = false;
  let sceneFlowDirty = false;
  let sceneFlowCanUndo = false;
  let sceneFlowCanRedo = false;
  let lastSceneFlowProjectId = "";
  let sceneFlowRef;
  let sceneFlowZoom = readSceneFlowZoom();
  let sceneFlowWorldBox = null;
  let sceneFlowViewBox = null;
  let sceneFlowSelection = null;
  let sceneFlowMultiSelection = [];
  let nodeEditorTypeOptions = ["Int", "Bool", "Float", "String"];
  let nodeEditorTypeCatalog = [];
  let pinnedNodeSelectionId = "";
  let pinnedNodeSelectionRevision = null;
  let pinnedEdgeSelectionId = "";
  let pendingNodePositions = new Set();
  let sceneFlowClipboard = null;
  let sceneFlowPasteIndex = 0;
  let sceneFlowDuplicateIndex = 0;
  let sceneFlowDuplicateKey = "";
  let sceneFlowFrameColor = "#7d7d7d";
  let sceneFlowFrameStyle = "";
  let sceneFlowLayoutStyle = "";
  let sceneFlowIntVarNames = [];
  const sceneFlowToggleState = loadSceneFlowToggles();
  let sceneFlowNodeSnap = sceneFlowToggleState.nodeSnap;
  let sceneFlowShowCmdText = sceneFlowToggleState.showCmds;
  let sceneFlowShowVars = sceneFlowToggleState.showVars;
  let sceneFlowShowBlocks = sceneFlowToggleState.showBlocks;
  let sceneFlowShowInspector = sceneFlowToggleState.showInspector;
  let agentsCollapsed = false;
  let scenesCollapsed = false;
  let sceneFlowBusy = false;
  let runtimeInfo = null;
  let runtimeError = "";
  let runtimeLoading = false;
  let runtimeLoaded = false;
  let monitorDialogOpen = false;
  let monitorSelectedKey = "";
  let monitorSelectedVar = null;
  let monitorValueDraft = "";
  let monitorQueryDraft = "";
  let monitorStatus = "";
  let monitorError = "";
  let monitorCanEdit = false;
  let monitorGlobals = [];
  let monitorLocals = [];
  let lastRuntimeProjectId = "";
  let lastRuntimeSuperNodeId = "";
  let runtimeValues = {};
  let runtimeInitialValues = {};
  let runtimeInitialProjectId = "";
  let runtimeInitialState = "stopped";
  let activityNodeCounts = new Map();
  let activityNodeDecayTokens = new Map();
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
  let edgeRestrictionNodeId = "";
  let edgeRestrictionAllowed = new Set();
  let edgeTypeDisabledMap = {
    EEDGE: false,
    CEDGE: false,
    PEDGE: false,
    TEDGE: false,
    FEDGE: false,
    IEDGE: false
  };
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
  let startListSelectedId = "";
  let cmdDialogOpen = false;
  let cmdInlineDrafts = [];
  let cmdDialogNodeId = "";
  let cmdInlineInputEls = [];
  let cmdHelperOpen = false;
  let cmdHelperType = "PlayScene";
  let cmdHelperScene = "";
  let cmdHelperAgent = "";
  let cmdHelperAction = "";
  let cmdHelperArgs = [];
  let cmdHelperVarName = "";
  let cmdHelperVarType = "Int";
  let cmdHelperVarExpr = "";
  let cmdHelperVarStep = "1";
  let cmdHelperSceneBindings = {};
  let cmdHelperVarScope = "global";
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
  $: headerDirty = !!(selectedProject?.dirty || sceneFlowDirty || scriptDirty || projectConfigDirty);
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
  $: {
    const canAutoApply =
      showEditor &&
      wsConnected &&
      !!selectedProjectId &&
      scriptDirty &&
      scriptParseOk &&
      !scriptError &&
      scriptDiagnostics.length === 0 &&
      !scriptLoading &&
      !projectSaving &&
      scriptDiagTimer === null;
    if (canAutoApply) {
      clearScriptAutoApplyTimer();
      scriptAutoApplyTimer = setTimeout(runScriptAutoApply, 650);
    } else {
      clearScriptAutoApplyTimer();
    }
  }
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
  $: if (projectLoadComplete && !showEditor && !editorManuallyClosed) {
    showEditor = true;
  }
  $: if (!sessionReady || !selectedProjectId) {
    showEditor = false;
  }
  $: {
    // Debug logging for project load state
    console.log("[PROJECT LOAD STATE]", {
      selectedProjectId: !!selectedProjectId,
      configLoaded,
      scriptLoaded,
      scriptScenesLoaded,
      scriptElementsLoaded,
      sceneFlowLoaded,
      runtimeLoaded,
      configError,
      scriptError,
      scriptScenesError,
      scriptElementsError,
      sceneFlowError,
      runtimeError,
      projectLoadComplete,
      showEditor,
      sessionReady
    });
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

  $: edgeRestrictionNodeId = sceneFlowSelection?.type === "node" ? sceneFlowSelection.id : "";
  $: edgeRestrictionAllowed = allowedEdgeTypesForSource(edgeRestrictionNodeId || "", sceneFlow);
  $: {
    const allowed = edgeRestrictionAllowed;
    if (!edgeRestrictionNodeId) {
      edgeTypeDisabledMap = {
        EEDGE: false,
        CEDGE: false,
        PEDGE: false,
        TEDGE: false,
        FEDGE: false,
        IEDGE: false
      };
    } else {
      edgeTypeDisabledMap = {
        EEDGE: !allowed.has("EEDGE"),
        CEDGE: !allowed.has("CEDGE"),
        PEDGE: !allowed.has("PEDGE"),
        TEDGE: !allowed.has("TEDGE"),
        FEDGE: !allowed.has("FEDGE"),
        IEDGE: !allowed.has("IEDGE")
      };
    }
  }

  function registerDebugGlobals() {
    if (typeof window === "undefined") return;
    try {
      if (!window.__vsmGetState) {
        window.__vsmGetState = () => ({
          sceneFlow,
          edgeRestriction: {
            nodeId: edgeRestrictionNodeId,
            allowed: Array.from(edgeRestrictionAllowed)
          },
          selection: sceneFlowSelection,
          multiSelection: sceneFlowMultiSelection
        });
      }
      if (!Object.getOwnPropertyDescriptor(window, "__vsmSceneFlow")) {
        Object.defineProperty(window, "__vsmSceneFlow", {
          configurable: true,
          get: () => sceneFlow
        });
      }
      if (!Object.getOwnPropertyDescriptor(window, "__vsmEdgeRestriction")) {
        Object.defineProperty(window, "__vsmEdgeRestriction", {
          configurable: true,
          get: () => ({
            nodeId: edgeRestrictionNodeId,
            allowed: Array.from(edgeRestrictionAllowed)
          })
        });
      }
    } catch (err) {
      // Ignore debug global failures in restricted environments.
    }
  }

  onMount(() => {
    registerDebugGlobals();
  });

  $: registerDebugGlobals();
  $: selectionStartCount = selectionNodes.filter((node) => node.isStart).length;
  $: selectionHasMovableNodes = selectionNodes.length > 1;
  $: selectionCanDistribute = selectionNodes.length > 2;
  $: selectionCanToggleStart = selectionNodes.some((node) => !node.isHistory);
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
  $: console.log("[SELDBG] selection-state", {
    sceneFlowSelection,
    sceneFlowMultiSelection,
    nodeEditorTargetId,
    superNodeId: sceneFlow?.superNodeId,
    revision: sceneFlow?.revision
  });

  $: {
    const rawNames = Array.isArray(sceneFlow?.intVarNames)
      ? sceneFlow.intVarNames
      : sceneFlowVarDefs
          .filter((def) => (def?.type || "").trim().toLowerCase() === "int" && (def?.name || "").trim())
          .map((def) => (def.name || "").trim());
    const cleaned = rawNames.map((name) => String(name || "").trim()).filter(Boolean);
    sceneFlowIntVarNames = Array.from(new Set(cleaned));
  }
  function buildTypeCatalog(typeDefs) {
    if (!Array.isArray(typeDefs)) return [];
    return typeDefs
      .map((def) => {
        if (!def) return null;
        return {
          name: (def.name ?? "").trim(),
          flavour: (def.flavour ?? "").trim(),
          elementType: (def.elementType ?? "").trim(),
          members: Array.isArray(def.members) ? def.members : []
        };
      })
      .filter((entry) => entry && entry.name);
  }

  $: nodeEditorTypeCatalog = Array.isArray(nodeEditorTarget?.typeCatalog)
    ? nodeEditorTarget.typeCatalog
    : buildTypeCatalog(nodeEditorTypeDefs);
  $: {
    const base = ["Int", "Bool", "Float", "String"];
    const serverOptions = Array.isArray(nodeEditorTarget?.typeOptions) ? nodeEditorTarget.typeOptions : base;
    const extras = nodeEditorTypeCatalog.map((entry) => entry.name).filter(Boolean);
    nodeEditorTypeOptions = Array.from(new Set([...serverOptions, ...extras]));
  }
  $: currentSuperName =
    sceneFlow?.path?.length ? sceneFlow.path[sceneFlow.path.length - 1] : sceneFlow?.superNodeId || "SceneFlow";
  $: sceneFlowPathNodes = Array.isArray(sceneFlow?.pathNodes) ? sceneFlow.pathNodes : [];
  $: sceneFlowBreadcrumbNodes = (() => {
    if (!sceneFlowPathNodes.length) return [];
    const draftName = sceneFlowSelection
      ? ""
      : String(superNodeDraft?.name ?? "").trim();
    const lastId = sceneFlowPathNodes[sceneFlowPathNodes.length - 1]?.id || "";
    return sceneFlowPathNodes.map((node, idx) => {
      if (idx === sceneFlowPathNodes.length - 1 && draftName && nodeEditorTarget?.id === lastId) {
        return { ...node, name: draftName };
      }
      return node;
    });
  })();
  $: startNodes = sceneFlow?.nodes ? sceneFlow.nodes.filter((node) => node.isStart && !node.isHistory) : [];
  $: superNodeChildren = sceneFlow?.nodes ? sceneFlow.nodes.filter((node) => !node.isHistory) : [];
  $: superNodeStartList = superNodeChildren
    .slice()
    .sort((a, b) => Number(!!b.isStart) - Number(!!a.isStart) || displayNodeName(a).localeCompare(displayNodeName(b)));
  $: {
    if (superNodeStartList.length) {
      const exists = superNodeStartList.some((node) => node.id === startListSelectedId);
      if (!exists) {
        startListSelectedId = superNodeStartList[0].id;
      }
    } else {
      startListSelectedId = "";
    }
  }
  $: startListSelectedNode = superNodeStartList.find((node) => node.id === startListSelectedId) || null;
  $: helperVarCandidates = (() => {
    const list = [];
    const seen = new Set();
    const addVar = (def, scope) => {
      if (!def) return;
      const name = (def.name || "").trim();
      if (!name || seen.has(name)) return;
      seen.add(name);
      list.push({ name, type: (def.type || "").trim(), scope });
    };
    (nodeEditorVarDefs || []).forEach((def) => addVar(def, "local"));
    (sceneFlowVarDefs || []).forEach((def) => addVar(def, "global"));
    return list;
  })();
  $: cmdHelperVarExists = (() => {
    const name = (cmdHelperVarName || "").trim();
    if (!name) return false;
    return helperVarCandidates.some((entry) => entry.name === name);
  })();
  $: sceneFlowFrameColor = superNodeFrameColor(sceneFlow);
  $: sceneFlowFrameStyle = `--sf-frame-color:${sceneFlowFrameColor};`;
  $: {
    let columns = "minmax(0, 1fr)";
    let gap = "0";
    if (sceneFlowShowBlocks && sceneFlowShowInspector) {
      columns = "var(--sf-side-width) minmax(0, 1fr) var(--sf-side-width)";
      gap = "var(--sf-gap)";
    } else if (sceneFlowShowBlocks && !sceneFlowShowInspector) {
      columns = "var(--sf-side-width) minmax(0, 1fr)";
      gap = "var(--sf-gap)";
    } else if (!sceneFlowShowBlocks && sceneFlowShowInspector) {
      columns = "minmax(0, 1fr) var(--sf-side-width)";
      gap = "var(--sf-gap)";
    }
    sceneFlowLayoutStyle = `grid-template-columns:${columns};gap:${gap};`;
  }
  $: if (varBadgeState.visible !== sceneFlowShowVars) {
    varBadgeState = { ...varBadgeState, visible: sceneFlowShowVars };
  }
  $: persistSceneFlowToggles({
    nodeSnap: sceneFlowNodeSnap,
    showCmds: sceneFlowShowCmdText,
    showVars: sceneFlowShowVars,
    showBlocks: sceneFlowShowBlocks,
    showInspector: sceneFlowShowInspector
  });
  $: activePathNode = sceneFlowPathNodes.length ? sceneFlowPathNodes[sceneFlowPathNodes.length - 1] : null;
  $: isSceneFlowRoot =
    activePathNode?.isRoot === true ||
    sceneFlow?.superNodeData?.isRoot === true ||
    sceneFlowPathNodes.length === 1;
  $: showLocalVarBadge = !!sceneFlow && !isSceneFlowRoot;
  $: runtimeState = runtimeInfo?.state || selectedProject?.runtimeState || "stopped";
  $: runtimeStateLabel = RUNTIME_STATE_LABELS[runtimeState] || runtimeState;
  $: runtimeGlobals = Array.isArray(runtimeInfo?.globalVariables) ? runtimeInfo.globalVariables : [];
  $: runtimeLocals = Array.isArray(runtimeInfo?.localVariables) ? runtimeInfo.localVariables : [];
  $: runtimeRootVars = runtimeGlobals.length ? runtimeGlobals : runtimeLocals;
  $: runtimeDisplayGlobals = isSceneFlowRoot ? runtimeRootVars : runtimeGlobals;
  $: displayGlobalVarList = (() => {
    const merged = [];
    const seen = new Set();
    runtimeDisplayGlobals.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name) return;
      seen.add(name);
      merged.push(def);
    });
    sceneFlowVarDefs.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name || seen.has(name)) return;
      merged.push(def);
    });
    return merged;
  })();
  $: displayLocalVarList = (() => {
    const merged = [];
    const seen = new Set();
    runtimeLocals.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name) return;
      seen.add(name);
      merged.push(def);
    });
    nodeEditorVarDefs.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name || seen.has(name)) return;
      merged.push(def);
    });
    return merged;
  })();
  $: monitorGlobals = runtimeDisplayGlobals;
  $: monitorLocals = isSceneFlowRoot ? [] : runtimeLocals;
  $: activityNodeIds = Array.from(activityNodeCounts.keys());
  $: activityEdgeList = Array.from(activityEdgeHits.values());
  $: timeoutEdgeList = Array.from(timeoutEdgeRuns.values());
  $: runtimeCanPlay = wsConnected && !!selectedProjectId && (runtimeState === "stopped" || runtimeState === "paused");
  $: runtimeCanPause = wsConnected && !!selectedProjectId && runtimeState === "running";
  $: runtimeCanStop = wsConnected && !!selectedProjectId && runtimeState !== "stopped";
  $: runtimePlayLabel = runtimeState === "paused" ? "Resume" : "Start";
  $: monitorCanEdit = runtimeState !== "stopped";
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
  $: autoSaveReady =
    autoSaveEnabled &&
    showEditor &&
    !!selectedProjectId &&
    !projectRequiresSaveAs &&
    !scriptDirty &&
    !configDirty &&
    !projectConfigDirty &&
    (selectedProject?.dirty || sceneFlowDirty || configSaved === false || projectConfigPending);
  $: {
    const shouldScheduleAutoSave = autoSaveReady && !projectSaving && !autoSaving;
    if (shouldScheduleAutoSave) {
      clearAutoSaveTimer();
      autoSaveTimer = setTimeout(runAutoSave, AUTOSAVE_DELAY_MS);
    } else {
      clearAutoSaveTimer();
      if ((!autoSaveReady || !headerDirty) && !autoSaving) {
        autoSaveStatus = "";
      }
    }
  }
  $: {
    if (typeof document !== "undefined") {
      const status = wsConnected ? "connected" : "offline";
      const projectLabel = showEditor && selectedProject?.name ? ` — ${selectedProject.name}` : "";
      document.title = `Visual SceneMaker Web ${projectLabel} (${status})`;
    }
  }
  $: scriptScenesLive = buildSceneGroupsFromScript(scriptDraft || "");
  $: filteredScriptScenes = filterSceneLanguages(
    scriptScenesLive.length ? scriptScenesLive : scriptScenes,
    scriptScenesFilter,
    scriptScenesLanguage
  );
  $: helperSceneIndex = (() => {
    const source = scriptScenesLive.length ? scriptScenesLive : scriptScenes;
    const index = new Map();
    if (!Array.isArray(source)) return index;
    source.forEach((lang) => {
      const groups = Array.isArray(lang?.groups) ? lang.groups : [];
      groups.forEach((group) => {
        const name = (group?.name || "").trim();
        if (!name) return;
        const params = Array.isArray(group?.params) ? group.params : [];
        if (!index.has(name)) {
          index.set(name, params);
          return;
        }
        const merged = new Set([...(index.get(name) || []), ...params]);
        index.set(name, Array.from(merged));
      });
    });
    return index;
  })();
  $: helperScenes = (() => {
    return Array.from(helperSceneIndex.keys()).sort((a, b) => a.localeCompare(b));
  })();
  $: sceneLanguageOptions = sceneLanguageOptionList(scriptScenesLive.length ? scriptScenesLive : scriptScenes);
  $: filteredScriptElements = filterScriptElements(scriptElements, scriptElementsFilter);
  $: sceneAgentNames = extractSceneAgents(scriptDraft);
  $: deviceAgentNames = extractDeviceAgents(projectConfigAgents);
  $: agentGroups = buildAgentGroups(sceneAgentNames, deviceAgentNames);
  $: missingAgentNames = extractMissingAgents(sceneAgentNames, projectConfigAgents);
  $: missingAgentDeviceOptions = buildMissingAgentDeviceOptions(projectConfigPlugins);
  $: monitorSelectedVar = findMonitorVar(monitorSelectedKey);
  $: prefsPreviewStyle = buildPrefsPreviewStyle(prefsDialogDraft);
  $: projectConfigDirty =
    projectConfigFingerprint(projectConfigDraft) !== projectConfigFingerprint(projectConfig);

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
      const draftName = (nodeEditorTarget.name ?? "").trim();
      const displayName = isRoot && !draftName ? "SceneFlow" : nodeEditorTarget.name ?? "";
      superNodeDraftId = draftKey;
      superNodeDraft = {
        name: displayName,
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
    editorManuallyClosed = false;
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
    clearScriptAutoApplyTimer();
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
  $: if (sessionReady && selectedProjectId && sceneFlowLoaded) {
    const currentSuperId = sceneFlow?.superNodeId || "";
    if (currentSuperId !== lastRuntimeSuperNodeId) {
      lastRuntimeSuperNodeId = currentSuperId;
      loadRuntime(selectedProjectId);
    }
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
    lastRuntimeSuperNodeId = "";
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
        return false;
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
      return true;
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
      return false;
    }
  }

  // Phase 8.4: Remote server connection functions
  async function connectToRemoteServer(serverUrl) {
    if (!serverUrl || !serverUrl.trim()) {
      remoteConnectionError = "Please enter a server URL";
      return false;
    }

    remoteConnecting = true;
    remoteConnectionError = "";

    try {
      // Normalize URL (add http:// if missing)
      let normalizedUrl = serverUrl.trim();
      if (!normalizedUrl.startsWith("http://") && !normalizedUrl.startsWith("https://")) {
        normalizedUrl = "http://" + normalizedUrl;
      }

      // Test connection by fetching server info
      const infoResponse = await fetch(`${normalizedUrl}/api/v1/info`);
      if (!infoResponse.ok) {
        throw new Error(`Server returned ${infoResponse.status}`);
      }
      const serverInfo = await infoResponse.json();

      // If token required, try to get one (only works for localhost)
      if (serverInfo.tokenRequired) {
        // For remote servers, user must provide token manually
        // For now, try without token first
      }

      // Save the remote server URL
      remoteServerUrl = normalizedUrl;
      localStorage.setItem("vsm_remote_server", normalizedUrl);

      // Update info with remote server info BEFORE connecting WebSocket
      // (connectWs checks info.tokenRequired)
      info = serverInfo;

      // Mark as remote connection BEFORE loading projects
      isRemoteConnection = true;
      connectedServerName = serverInfo.name || new URL(normalizedUrl).host;

      // Close existing connection and reconnect to remote
      const wsOk = await connectWs(normalizedUrl);
      if (!wsOk) {
        throw new Error("WebSocket connection failed");
      }

      // Clear projects since we're connecting to different server
      projects = [];
      selectedProjectId = null;
      showEditor = false;
      // Keep sessionReady false to prevent reactive statements from interfering
      sessionReady = false;

      // Load projects and preferences from remote server
      console.log("[REMOTE] Loading projects from remote server...");
      await Promise.all([loadProjects(), loadPreferences()]);
      console.log("[REMOTE] Projects loaded:", projects.length, projects);

      // If there are projects, load all data BEFORE setting sessionReady
      if (projects.length > 0) {
        const projectId = projects[0].projectId;
        console.log("[REMOTE] First project ID:", projectId);

        // Set all tracking variables FIRST to prevent reactive resets
        projectLoadProjectId = projectId;
        projectLoadAttempted = true;
        lastConfigProjectId = projectId;
        lastProjectConfigProjectId = projectId;
        lastScriptProjectId = projectId;
        lastSceneFlowProjectId = projectId;
        lastRuntimeProjectId = projectId;

        // Set selectedProjectId AFTER tracking variables
        selectedProjectId = projectId;
        console.log("[REMOTE] Set selectedProjectId to:", selectedProjectId);

        // Wait for Svelte to process state changes
        await tick();

        // Now load all project data
        console.log("[REMOTE] Loading project data...");
        await Promise.all([
          loadConfig(projectId),
          loadProjectConfig(projectId),
          loadScript(projectId),
          loadScriptScenes(projectId),
          loadScriptElements(projectId),
          loadSceneFlow(projectId),
          loadRuntime(projectId)
        ]);
        console.log("[REMOTE] Project data loaded");

        // Wait for loads to complete and Svelte to update
        await tick();
      } else {
        console.log("[REMOTE] No projects found on remote server");
      }

      // NOW set sessionReady to true - this triggers the UI to show
      sessionReady = true;
      console.log("[REMOTE] Set sessionReady to true");

      // Wait for Svelte to process sessionReady change
      await tick();

      showConnectDialog = false;
      remoteConnecting = false;
      statusMessage = `Connected to ${connectedServerName}`;
      console.log("[REMOTE] Connection complete, showEditor:", showEditor);
      return true;

    } catch (err) {
      remoteConnectionError = err.message || "Connection failed";
      remoteConnecting = false;
      return false;
    }
  }

  async function disconnectFromRemote() {
    if (ws) {
      ws.close();
    }
    isRemoteConnection = false;
    remoteServerUrl = "";
    localStorage.removeItem("vsm_remote_server");
    connectedServerName = "";

    // Reconnect to local server
    await connectAll();
  }

  function openConnectDialog() {
    remoteServerInput = remoteServerUrl || "localhost:8091";
    remoteConnectionError = "";
    showConnectDialog = true;
  }

  function closeConnectDialog() {
    showConnectDialog = false;
    remoteConnectionError = "";
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
    if (!isLocalHost() || isRemoteConnection || sessionReady || autoConnectInFlight) return;
    autoConnectAttempted = true;
    if (autoConnectTimer) {
      clearTimeout(autoConnectTimer);
      autoConnectTimer = null;
    }
    autoConnectAttempts += 1;
    console.log("[AUTO-CONNECT] Attempt", autoConnectAttempts, "token exists:", !!token);
    if (!token) {
      const fetched = await fetchLocalToken();
      console.log("[AUTO-CONNECT] Token fetch result:", fetched, "token now:", !!token);
      // Continue even if token fetch fails - server might not require token
    }
    console.log("[AUTO-CONNECT] Calling connectAll()");
    autoConnectInFlight = true;
    const ok = await connectAll();
    autoConnectInFlight = false;
    console.log("[AUTO-CONNECT] connectAll() completed, sessionReady:", sessionReady);
    if (!ok) {
      scheduleAutoConnectRetry("connectAll-failed");
    }
  }

  onMount(() => {
    autoConnectAttempts = 0;
    autoConnectIfLocal();
  });

  onDestroy(() => {
    clearAutoSaveTimer();
    if (autoConnectTimer) {
      clearTimeout(autoConnectTimer);
      autoConnectTimer = null;
    }
  });

  function scheduleAutoConnectRetry(reason) {
    if (!isLocalHost() || isRemoteConnection || sessionReady) return;
    if (autoConnectTimer || autoConnectInFlight) return;
    if (autoConnectAttempts >= AUTO_CONNECT_MAX_ATTEMPTS) return;
    const delay = Math.min(AUTO_CONNECT_RETRY_MS * Math.max(autoConnectAttempts, 1), 5000);
    console.log("[AUTO-CONNECT] Scheduling retry", { reason, delay, attempt: autoConnectAttempts });
    autoConnectTimer = setTimeout(() => {
      autoConnectTimer = null;
      autoConnectIfLocal();
    }, delay);
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

  function rememberFocus() {
    if (typeof document === "undefined") return;
    if (lastFocusedElement) return;
    const active = document.activeElement;
    lastFocusedElement = active instanceof HTMLElement ? active : null;
  }

  function restoreFocus() {
    if (lastFocusedElement && lastFocusedElement.isConnected) {
      lastFocusedElement.focus({ preventScroll: true });
    }
    lastFocusedElement = null;
  }

  async function focusDialog(dialogEl, preferredEl = null) {
    await tick();
    const target =
      preferredEl ||
      dialogEl?.querySelector?.("input, select, textarea, button, [tabindex]:not([tabindex='-1'])") ||
      dialogEl;
    if (target && typeof target.focus === "function") {
      target.focus({ preventScroll: true });
    }
  }

  async function openProject(path, options = {}) {
    const targetPath = (path || "").trim();
    const surfaceError = options.surfaceError !== false;
    if (!targetPath) {
      if (surfaceError) {
        openPathError = "Path is required.";
        await tick();
        openPathInput?.focus();
      }
      return { ok: false, error: "Path is required." };
    }
    if (surfaceError) {
      openPathError = "";
    }
    try {
      const response = await apiPost("/api/v1/projects/open", { path: targetPath });
      openPath = "";
      if (response?.projectId) {
        editorManuallyClosed = false;
        selectedProjectId = response.projectId;
      }
      await loadProjects();
      await loadRecent();
      return { ok: true };
    } catch (err) {
      const message = err?.message || "Failed to open project.";
      if (surfaceError) {
        openPathError = message;
        await tick();
        openPathInput?.focus();
      }
      return { ok: false, error: message };
    }
  }

  function handleDirectoryInputChange(event) {
    const files = event?.target?.files;
    const first = files && files.length ? files[0] : null;
    const relative = first?.webkitRelativePath || "";
    const root = relative.split("/")[0] || "";
    const basePath = first?.path || "";
    if (basePath) {
      openPath = basePath.replace(new RegExp(`${root}$`), "");
      openPathError = "";
      return;
    }
    if (root) {
      openPath = root;
      openPathError = `Selected \"${root}\". Please augment it to the full absolute path.`;
    } else {
      openPathError = "This browser cannot provide a full path. Please paste it manually.";
    }
  }

  function openProjectFromLanding(project) {
    const projectId = project?.projectId;
    if (!projectId) return;
    editorManuallyClosed = false;
    if (projectId === selectedProjectId) {
      if (projectLoadComplete) {
        showEditor = true;
        return;
      }
      // Force the reactive loaders to run again for the same project id.
      projectLoadProjectId = "";
      projectLoadAttempted = false;
      lastConfigProjectId = "";
      lastProjectConfigProjectId = "";
      lastScriptProjectId = "";
      lastSceneFlowProjectId = "";
      lastRuntimeProjectId = "";
      resetProjectLoadState();
      loadConfig(projectId);
      return;
    }
    selectedProjectId = projectId;
    loadConfig(projectId);
  }

  async function browseForProjectDir() {
    openPathError = "";
    try {
      if (typeof window !== "undefined" && typeof window.showDirectoryPicker === "function") {
        const handle = await window.showDirectoryPicker();
        // Most browsers do not expose absolute paths; show a helpful message instead.
        if (handle?.name) {
          openPath = handle.name;
          openPathError = `Selected \"${handle.name}\". Please augment it to the full absolute path.`;
        } else {
          openPathError = "Please paste the full path manually.";
        }
        return;
      }
    } catch (err) {
      if (err?.name === "AbortError") {
        return;
      }
      // Fall through to the input picker below.
    }
    if (openPathPickerInput) {
      openPathPickerInput.value = "";
      openPathPickerInput.click();
    } else {
      openPathError = "Directory picker is not available in this browser.";
    }
  }

  function handleProjectDrop(event) {
    event.preventDefault();
    openPathError = "";
    const items = Array.from(event?.dataTransfer?.items || []);
    const entry = items
      .map((item) => (typeof item.webkitGetAsEntry === "function" ? item.webkitGetAsEntry() : null))
      .find((candidate) => candidate && candidate.isDirectory);
    const dirName = entry?.name || "";
    const file = event?.dataTransfer?.files?.[0];
    const basePath = file?.path || "";
    if (basePath) {
      openPath = basePath.replace(new RegExp(`${dirName}$`), "");
      return;
    }
    if (dirName) {
      openPath = dirName;
      openPathError = `Dropped "${dirName}". Please augment it to the full absolute path.`;
      return;
    }
    openPathError = "Could not read the dropped folder path. Please paste it manually.";
  }

  async function openRecentProject(project) {
    if (!project?.path) return;
    const result = await openProject(project.path, { surfaceError: false });
    if (!result?.ok) {
      rememberFocus();
      recentFailureProject = project;
      recentFailureMessage = result?.error || "Failed to open recent project.";
      recentFailureOpen = true;
      focusDialog(recentFailureDialogEl);
    }
  }

  async function createProject() {
    const name = (newName || "").trim();
    const baseDir = (newBaseDir || "").trim();
    if (!name) {
      createProjectError = "Project name is required.";
      await tick();
      newProjectNameInput?.focus();
      return;
    }
    createProjectError = "";
    const payload = { name };
    if (baseDir) {
      payload.baseDir = baseDir;
    }
    try {
      const response = await apiPost("/api/v1/projects", payload);
      newName = "";
      newBaseDir = "";
      if (response?.projectId) {
        selectedProjectId = response.projectId;
      }
      await loadProjects();
      await loadRecent();
    } catch (err) {
      createProjectError = err?.message || "Failed to create project.";
      await tick();
      newProjectNameInput?.focus();
    }
  }

  async function saveProject(projectId) {
    if (!projectId || projectSaving) return false;
    let ok = false;
    projectSaving = true;
    try {
      await apiPost(`/api/v1/projects/${projectId}/save`, {});
      await loadProjects();
      await loadRecent();
      ok = true;
      // Locally clear dirty flags to avoid redundant autosaves until the server refresh arrives.
      sceneFlowDirty = false;
      projectConfigPending = false;
      configSaved = null;
      projects = projects.map((p) =>
        p.projectId === projectId ? { ...p, dirty: false, pending: false } : p
      );
    } catch (err) {
      const message = err?.message || "Failed to save project.";
      const needsSaveAs = /save-as|save as|pending|no path/i.test(message);
      statusMessage = message;
      if (needsSaveAs) {
        openSaveAsDialog();
      }
    } finally {
      projectSaving = false;
    }
    return ok;
  }

  function clearAutoSaveTimer() {
    if (autoSaveTimer) {
      clearTimeout(autoSaveTimer);
      autoSaveTimer = null;
    }
  }

  async function runAutoSave() {
    clearAutoSaveTimer();
    if (!autoSaveReady || projectSaving || autoSaving || !selectedProjectId) return;
    autoSaving = true;
    autoSaveStatus = "Autosaving…";
    if (scriptDirty && scriptParseOk && scriptDiagnostics.length === 0) {
      await applyScript();
    }
    const ok = await saveProject(selectedProjectId);
    autoSaving = false;
    if (ok) {
      autoSaveStatus = "Saved";
      setTimeout(() => {
        if (!headerDirty) {
          autoSaveStatus = "";
        }
      }, 1200);
    } else {
      autoSaveStatus = "Autosave failed — use Save";
    }
  }

  async function toggleAutoSave() {
    if (!selectedProjectId || projectSaving || autoSaving) return;
    const next = !autoSaveEnabled;
    const nextValue = String(next);
    configDraft = { ...configDraft, autosave: nextValue };
    config = { ...config, autosave: nextValue };
    autoSaveEnabled = next;
    const response = await sendCommand("Config.Update", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId || "",
      values: { autosave: nextValue }
    });
    if (response?.config) {
      const merged = { ...config, ...response.config, autosave: nextValue };
      config = merged;
      configDraft = { ...merged };
      autoSaveEnabled = resolveConfigBool(merged.autosave, true);
    }
    configSaved = response?.saved === true;
  }

  async function saveAsProject(projectId, overridePath) {
    const targetPath = (overridePath || saveAsPath || "").trim();
    if (!projectId || !targetPath || projectSaving) return false;
    projectSaving = true;
    try {
      await apiPost(`/api/v1/projects/${projectId}/save-as`, { path: targetPath });
      saveAsPath = "";
      saveAsError = "";
      await loadProjects();
      await loadRecent();
      return true;
    } catch (err) {
      saveAsError = err?.message || "Failed to save project.";
      return false;
    } finally {
      projectSaving = false;
    }
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
            superNodeId: sceneFlow?.superNodeId || "",
            values: { workspace_fontsize: normalized }
          });
          config = configResponse.config || {};
          configDraft = { ...config };
          autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
          autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
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
      autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
      autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
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
      superNodeId: sceneFlow?.superNodeId || "",
      values
    });
    config = response.config || {};
    configDraft = { ...config };
    autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
    autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
    configSaved = response.saved === true;
    if (response?.snapshot) {
      applySceneFlowSnapshot(response.snapshot);
    }
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
    if (!projectConfigDirty) {
      projectConfigError = "No project configuration changes to apply.";
      return;
    }
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
    rememberFocus();
    projectConfigDialogOpen = true;
    projectConfigSelection = { type: "project" };
    projectConfigError = "";
    projectConfigSaved = null;
    projectConfigPending = false;
    loadProjectConfig(selectedProjectId);
    loadAvailableDevices();
    focusDialog(projectConfigDialogEl);
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
    restoreFocus();
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

  function resolveConfigBool(value, fallback) {
    if (value === undefined || value === null || value === "") return fallback;
    if (typeof value === "boolean") return value;
    return String(value).toLowerCase() === "true";
  }

  function readConfigString(key, fallback) {
    const raw = readConfigValue(key, fallback);
    if (raw === undefined || raw === null) return fallback;
    const text = String(raw).trim();
    return text ? text : fallback;
  }

  function readPreferenceString(key, fallback) {
    if (Object.prototype.hasOwnProperty.call(preferences, key)) {
      const value = preferences[key];
      if (value !== undefined && value !== null) {
        const text = String(value).trim();
        if (text) return text;
      }
    }
    return fallback;
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
    rememberFocus();
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
        autoSaveEnabled: readConfigBool("autosave", true),
        undoMaxDepth: String(readConfigInt("undo_max_depth", PREF_UNDO_DEFAULT)),
        commandLogMax: String(readConfigInt("command_log_max", PREF_COMMAND_LOG_DEFAULT)),
        scriptFontType: readConfigString("scriptfonttype", PREF_SCRIPT_FONT_DEFAULT),
        scriptFontSize: String(readConfigInt("scriptfonsize", PREF_SCRIPT_FONT_SIZE_DEFAULT)),
      sceneflowNamespace: readPreferenceString("xmlns", "xml.sceneflow.dfki.de"),
      sceneflowInstance: readPreferenceString("xmlns_xsi", "http://www.w3.org/2001/XMLSchema-instance"),
      sceneflowSchema: readPreferenceString("xsi_schemeLocation", "res/xsd/sceneflow.xsd")
    };
    prefsDialogError = "";
    prefsDialogOpen = true;
    focusDialog(prefsDialogEl);
  }

  function closePrefsDialog() {
    prefsDialogOpen = false;
    prefsDialogDraft = null;
    prefsDialogError = "";
    restoreFocus();
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
    const undoMaxDepth = parsePrefsInt(
      prefsDialogDraft.undoMaxDepth,
      PREF_UNDO_MIN,
      PREF_UNDO_MAX,
      "Undo history depth"
    );
    if (undoMaxDepth === null) return;
    const commandLogMax = parsePrefsInt(
      prefsDialogDraft.commandLogMax,
      PREF_COMMAND_LOG_MIN,
      PREF_COMMAND_LOG_MAX,
      "Command log max"
    );
    if (commandLogMax === null) return;
    const scriptFontType = String(prefsDialogDraft.scriptFontType || "").trim();
    if (!scriptFontType) {
      prefsDialogError = "Script font type is required.";
      return;
    }
    const configChanges = {};
    const prefChanges = {};
    const addConfigChange = (key, value) => {
      const next = String(value);
      const current = normalizeConfigValue(config?.[key]);
      if (current !== next) {
        configChanges[key] = next;
      }
    };
    const addPrefChange = (key, value) => {
      const next = String(value);
      const current = normalizeConfigValue(preferences?.[key]);
      if (current !== next) {
        prefChanges[key] = next;
      }
    };
    addConfigChange("node_width", nodeSize);
    addConfigChange("node_height", nodeSize);
    addConfigChange("grid_x", gridScale);
    addConfigChange("grid_y", gridScale);
    addConfigChange("workspace_fontsize", workspaceFontSize);
    addConfigChange("grid", prefsDialogDraft.drawGrid);
    addConfigChange("visualization", prefsDialogDraft.activityVisualization);
    addConfigChange("visualizationtrace", prefsDialogDraft.activityTrace);
    addConfigChange("shownodeid", prefsDialogDraft.showNodeId);
    addConfigChange("autosave", prefsDialogDraft.autoSaveEnabled);
    addConfigChange("undo_max_depth", undoMaxDepth);
    addConfigChange("command_log_max", commandLogMax);
    addConfigChange("scriptfonsize", scriptFontSize);
    addConfigChange("scriptfonttype", scriptFontType);
    addPrefChange("xmlns", prefsDialogDraft.sceneflowNamespace);
    addPrefChange("xmlns_xsi", prefsDialogDraft.sceneflowInstance);
    addPrefChange("xsi_schemeLocation", prefsDialogDraft.sceneflowSchema);
    if (!Object.keys(configChanges).length && !Object.keys(prefChanges).length) {
      prefsDialogError = "No changes to apply.";
      return;
    }
    prefsDialogBusy = true;
    try {
      let configResponse = null;
      if (Object.keys(configChanges).length) {
        configResponse = await sendCommand("Config.Update", {
          projectId: selectedProjectId,
          superNodeId: sceneFlow?.superNodeId || "",
          values: configChanges
        });
        if (configResponse?.config) {
          config = configResponse.config;
        }
        configDraft = { ...configDraft, ...configChanges };
        autoSaveEnabled = resolveConfigBool(configDraft.autosave, true);
        configSaved = configResponse?.saved === true;
        if (configResponse?.snapshot) {
          applySceneFlowSnapshot(configResponse.snapshot);
        }
      }
      let prefResponse = null;
      if (Object.keys(prefChanges).length) {
        prefResponse = await sendCommand("Preferences.Update", {
          values: prefChanges
        });
        if (prefResponse?.preferences) {
          preferences = prefResponse.preferences;
          prefDraft = { ...prefResponse.preferences };
        }
      }
      const messages = [];
      if (Object.keys(prefChanges).length) {
        messages.push("Preferences updated.");
      }
      if (Object.keys(configChanges).length) {
        messages.push(
          configResponse?.pending ? "Config stored; save the project to persist." : "Config updated."
        );
      }
      statusMessage = messages.filter(Boolean).join(" ");
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
    const targetSuperId = (superNodeId || sceneFlow?.superNodeId || "").trim();
    const currentSuperId = (sceneFlow?.superNodeId || "").trim();
    const canPreserveNode =
      sceneFlowSelection?.type === "node" && targetSuperId === currentSuperId && sceneFlowSelection.id;
    const canPreserveEdge =
      sceneFlowSelection?.type === "edge" && targetSuperId === currentSuperId && sceneFlowSelection.id;
    if (canPreserveNode) {
      pinnedNodeSelectionId = sceneFlowSelection.id;
      pinnedNodeSelectionRevision = sceneFlow?.revision ?? null;
    } else if (canPreserveEdge) {
      pinnedEdgeSelectionId = sceneFlowSelection.id;
    }
    console.log("[SELDBG] loadSceneFlow start", {
      projectId,
      superNodeId,
      targetSuperId,
      currentSuperId,
      pinnedNodeSelectionId,
      pinnedEdgeSelectionId,
      beforeSelection: sceneFlowSelection,
      beforeMulti: sceneFlowMultiSelection
    });
    sceneFlowError = "";
    sceneFlowLoading = true;
    sceneFlowLoaded = false;
    sceneFlowDirty = false;
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
      const pinnedExists =
        pinnedNodeSelectionId && Array.isArray(data?.nodes)
          ? data.nodes.some((node) => node.id === pinnedNodeSelectionId)
          : false;
      const pinnedEdgeExists =
        !pinnedExists && pinnedEdgeSelectionId && Array.isArray(data?.edges)
          ? data.edges.some((edge) => edge.id === pinnedEdgeSelectionId)
          : false;
      sceneFlow = data;
      sceneFlowLoaded = true;
      if (pinnedExists) {
        sceneFlowSelection = { type: "node", id: pinnedNodeSelectionId };
        sceneFlowMultiSelection = [{ type: "node", id: pinnedNodeSelectionId }];
      } else if (pinnedEdgeExists) {
        sceneFlowSelection = { type: "edge", id: pinnedEdgeSelectionId };
        sceneFlowMultiSelection = [{ type: "edge", id: pinnedEdgeSelectionId }];
      }
      pinnedNodeSelectionId = "";
      pinnedNodeSelectionRevision = null;
      pinnedEdgeSelectionId = "";
      console.log("[SELDBG] loadSceneFlow success", {
        projectId,
        superNodeId,
        revision: sceneFlow?.revision,
        pinnedExists,
        pinnedEdgeExists,
        afterSelection: sceneFlowSelection,
        afterMulti: sceneFlowMultiSelection
      });
      loadRuntime(projectId);
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      sceneFlowError = err.message || "Failed to load SceneFlow.";
      sceneFlow = null;
      sceneFlowLoaded = false;
      pinnedNodeSelectionId = "";
      pinnedNodeSelectionRevision = null;
      pinnedEdgeSelectionId = "";
      console.log("[SELDBG] loadSceneFlow error", {
        projectId,
        superNodeId,
        error: sceneFlowError
      });
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
      const superNodeId = sceneFlow?.superNodeId || "";
      const query = superNodeId ? `?superNodeId=${encodeURIComponent(superNodeId)}` : "";
      const data = await apiGet(`/api/v1/projects/${projectId}/runtime${query}`);
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

  async function executeRuntimeCommand(command) {
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

  async function runRuntimeCommand(command, options = {}) {
    if (!selectedProjectId) return;
    if (command === "Runtime.Play" && !options.skipMissingAgentCheck && missingAgentNames.length) {
      openMissingAgentDialog();
      return;
    }
    await executeRuntimeCommand(command);
  }

  function clearSceneFlowActivity() {
    activityNodeCounts = new Map();
    if (activityNodeDecayTokens.size) {
      for (const token of activityNodeDecayTokens.values()) {
        clearTimeout(token);
      }
      activityNodeDecayTokens = new Map();
    }
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

  function isSuperNodeId(nodeId) {
    if (!nodeId || !sceneFlow?.nodes) return false;
    const match = sceneFlow.nodes.find((node) => node.id === nodeId);
    return match?.type === "Super";
  }

  function scheduleSuperNodeDecay(nodeId) {
    if (!nodeId) return;
    const existing = activityNodeDecayTokens.get(nodeId);
    if (existing) {
      clearTimeout(existing);
    }
    const token = setTimeout(() => {
      const current = activityNodeDecayTokens.get(nodeId);
      if (current === token) {
        activityNodeDecayTokens.delete(nodeId);
        clearActivityNode(nodeId);
      }
    }, ACTIVITY_SUPERNODE_DECAY_MS);
    activityNodeDecayTokens.set(nodeId, token);
  }

  function clearActivityNode(nodeId) {
    if (!nodeId) return;
    const next = new Map(activityNodeCounts);
    next.delete(nodeId);
    activityNodeCounts = next;
  }

  function incrementActivityNode(nodeId) {
    if (!nodeId) return;
    if (isSuperNodeId(nodeId)) {
      const next = new Map(activityNodeCounts);
      next.set(nodeId, 1);
      activityNodeCounts = next;
      scheduleSuperNodeDecay(nodeId);
      return;
    }
    const next = new Map(activityNodeCounts);
    const count = next.get(nodeId) || 0;
    next.set(nodeId, count + 1);
    activityNodeCounts = next;
  }

  function decrementActivityNode(nodeId) {
    if (!nodeId) return;
    if (isSuperNodeId(nodeId)) {
      clearActivityNode(nodeId);
      const existing = activityNodeDecayTokens.get(nodeId);
      if (existing) {
        clearTimeout(existing);
        activityNodeDecayTokens.delete(nodeId);
      }
      return;
    }
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
    } finally {
      if (requestId === scriptDiagRequestId) {
        scriptDiagTimer = null;
      }
    }
  }

  function clearScriptAutoApplyTimer() {
    if (scriptAutoApplyTimer) {
      clearTimeout(scriptAutoApplyTimer);
      scriptAutoApplyTimer = null;
    }
  }

  async function runScriptAutoApply() {
    if (!selectedProjectId || scriptAutoApplyInFlight) return;
    scriptAutoApplyInFlight = true;
    try {
      await applyScript();
    } finally {
      scriptAutoApplyInFlight = false;
    }
  }

  function connectWs(serverUrl = null) {
    wsError = "";
    if (ws) {
      rejectPendingRequests("WebSocket reconnecting.");
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

      let baseUrl;
      if (serverUrl) {
        // Remote connection: serverUrl is like "localhost:8091" or "192.168.1.10:8091"
        const protocol = serverUrl.startsWith("https") ? "wss" : "ws";
        const cleanUrl = serverUrl.replace(/^https?:\/\//, "").replace(/\/$/, "");
        baseUrl = `${protocol}://${cleanUrl}/ws`;
        isRemoteConnection = true;
        connectedServerName = cleanUrl;
      } else {
        // Local connection: use current page host
        const protocol = location.protocol === "https:" ? "wss" : "ws";
        baseUrl = `${protocol}://${location.host}/ws`;
        isRemoteConnection = false;
        connectedServerName = location.host;
      }

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
        rejectPendingRequests("WebSocket closed.");
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
        scheduleAutoConnectRetry("ws-closed");
        finish(false);
      };
      ws.onerror = () => {
        wsError = "WebSocket connection failed.";
        rejectPendingRequests("WebSocket connection failed.");
        scheduleAutoConnectRetry("ws-error");
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
      if (entry.timer) {
        clearTimeout(entry.timer);
      }
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
      if (message.event) {
        handleUiProtocolEvent(message);
      }
      return;
    }
  }

  function rejectPendingRequests(reason) {
    if (!pending.size) return;
    const error = new Error(reason || "WebSocket request cancelled.");
    for (const entry of pending.values()) {
      if (entry?.reject) {
        if (entry.timer) {
          clearTimeout(entry.timer);
        }
        entry.reject(error);
      }
    }
    pending.clear();
  }

 

  function handleUiProtocolEvent(message) {
    const payload = message.payload || {};
    const eventName = message.event;
    if (!eventName) return;
    if (eventName === "system.preferences" && payload?.preferences) {
      preferences = payload.preferences;
      prefDraft = { ...preferences };
      return;
    }
    if (eventName === "project.dirty") {
      applyProtocolDirty(payload);
      return;
    }
    if (eventName === "project.loaded" || eventName === "project.saved" || eventName === "project.closed") {
      loadProjects();
      return;
    }
    if (eventName === "project.config") {
      if (payload.projectId && payload.projectId !== selectedProjectId) return;
      const configPayload =
        payload.config || (payload.plugins || payload.agents || payload.player ? payload : null);
      if (configPayload) {
        projectConfig = normalizeProjectConfig(configPayload);
        if (!projectConfigDialogOpen) {
          projectConfigDraft = cloneProjectConfig(projectConfig);
        }
        projectConfigLoading = false;
        projectConfigError = "";
      }
      return;
    }
    if (eventName === "script.snapshot") {
      applyScriptSnapshot(payload);
      return;
    }
    if (eventName === "script.elements") {
      if (payload.projectId && payload.projectId !== selectedProjectId) return;
      const elementsPayload =
        payload.elements || (payload.acticon || payload.gesticon || payload.visicon ? payload : null);
      if (elementsPayload) {
        scriptElements = {
          acticon: Array.isArray(elementsPayload.acticon) ? elementsPayload.acticon : [],
          gesticon: Array.isArray(elementsPayload.gesticon) ? elementsPayload.gesticon : [],
          visicon: Array.isArray(elementsPayload.visicon) ? elementsPayload.visicon : []
        };
        scriptElementsLoaded = true;
        scriptElementsLoading = false;
        scriptElementsError = "";
      }
      return;
    }
    if (eventName === "sceneflow.snapshot") {
      applySceneFlowSnapshot(payload);
      return;
    }
    if (eventName === "sceneflow.edgeUpdated") {
      if (selectedProjectId) {
        loadSceneFlow(selectedProjectId, sceneFlow?.superNodeId || "");
      }
      return;
    }
    if (eventName === "sceneflow.selection") {
      applyProtocolSelection(payload.selection);
      return;
    }
    if (eventName === "runtime.state") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) {
        return;
      }
      const status = (payload.status || payload.state || "").toLowerCase();
      if (status === "stopped") {
        clearSceneFlowActivity();
      }
      if (selectedProjectId) {
        loadRuntime(selectedProjectId);
      }
      return;
    }
    if (eventName === "runtime.nodeActive") {
      if (!activityProjectMatches(payload)) return;
      const nodeId = resolveActivityNodeId(payload);
      if (nodeId) {
        incrementActivityNode(nodeId);
      }
      return;
    }
    if (eventName === "runtime.nodeStopped") {
      if (!activityProjectMatches(payload)) return;
      const nodeId = resolveActivityNodeId(payload);
      if (nodeId) {
        decrementActivityNode(nodeId);
        clearTimeoutEdgesForNode(nodeId);
      }
      return;
    }
    if (eventName === "runtime.edgeActive") {
      if (!activityProjectMatches(payload)) return;
      const edgeType = normalizeProtocolEdgeType(payload.edgeType);
      const edgeId = resolveActivityEdgeId({ ...payload, edgeType });
      if (edgeId) {
        if (edgeType === "TEDGE") {
          registerTimeoutEdge(edgeId, payload.startedAt, payload.timeoutMs);
        } else {
          registerEdgeActivity(edgeId);
        }
      }
      return;
    }
    if (eventName === "runtime.timeoutProgress") {
      if (!activityProjectMatches(payload)) return;
      const edgeType = normalizeProtocolEdgeType(payload.edgeType);
      const edgeId = resolveActivityEdgeId({ ...payload, edgeType });
      if (edgeId) {
        const startedAt =
          payload.startedAt ??
          (Number.isFinite(payload.elapsedMs) ? Date.now() - Number(payload.elapsedMs) : undefined);
        registerTimeoutEdge(edgeId, startedAt, payload.timeoutMs);
      }
      return;
    }
    if (eventName === "vars.updated") {
      const items = Array.isArray(payload.variables) ? payload.variables : [];
      if (items.length) {
        for (const item of items) {
          const name = (item?.name || "").trim();
          if (name) {
            applyRuntimeVarUpdate(name, item.value);
          }
        }
        return;
      }
      const name = (payload.name || "").trim();
      if (name) {
        applyRuntimeVarUpdate(name, payload.value);
      }
    }
  }

  function applySceneFlowSnapshot(payload) {
    const snapshot = payload?.snapshot || payload;
    if (!snapshot || !snapshot.nodes || !snapshot.edges) return;
    if (snapshot.projectId && snapshot.projectId !== selectedProjectId) return;
    const pinnedExists =
      pinnedNodeSelectionId && Array.isArray(snapshot.nodes)
        ? snapshot.nodes.some((node) => node.id === pinnedNodeSelectionId)
        : false;
    const pinnedEdgeExists =
      !pinnedExists && pinnedEdgeSelectionId && Array.isArray(snapshot.edges)
        ? snapshot.edges.some((edge) => edge.id === pinnedEdgeSelectionId)
        : false;
    const currentSelection = sceneFlowSelection;
    const currentExists =
      currentSelection?.type === "node"
        ? snapshot.nodes.some((node) => node.id === currentSelection.id)
        : currentSelection?.type === "edge"
          ? snapshot.edges.some((edge) => edge.id === currentSelection.id)
          : currentSelection?.type === "comment"
            ? snapshot.comments?.some((comment) => comment.id === currentSelection.id)
            : false;
    console.log("[SELDBG] sceneflow.snapshot", {
      revision: snapshot?.revision,
      superNodeId: snapshot?.superNodeId,
      pinnedNodeSelectionId,
      pinnedNodeSelectionRevision,
      pinnedExists,
      pinnedEdgeSelectionId,
      pinnedEdgeExists,
      beforeSelection: sceneFlowSelection,
      beforeMulti: sceneFlowMultiSelection
    });
    sceneFlow = snapshot;
    if (snapshot?.undoState) {
      sceneFlowCanUndo = !!snapshot.undoState.canUndo;
      sceneFlowCanRedo = !!snapshot.undoState.canRedo;
    }
    pendingNodePositions.clear();
    sceneFlowError = "";
    sceneFlowLoaded = true;
    sceneFlowLoading = false;
    if (currentExists) {
      sceneFlowSelection = currentSelection;
      sceneFlowMultiSelection = currentSelection ? [currentSelection] : [];
    } else if (pinnedExists) {
      sceneFlowSelection = { type: "node", id: pinnedNodeSelectionId };
      sceneFlowMultiSelection = [{ type: "node", id: pinnedNodeSelectionId }];
    } else if (pinnedEdgeExists) {
      sceneFlowSelection = { type: "edge", id: pinnedEdgeSelectionId };
      sceneFlowMultiSelection = [{ type: "edge", id: pinnedEdgeSelectionId }];
    } else {
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
    }
    pinnedNodeSelectionId = "";
    pinnedNodeSelectionRevision = null;
    pinnedEdgeSelectionId = "";
    console.log("[SELDBG] sceneflow.snapshot applied", {
      revision: sceneFlow?.revision,
      superNodeId: sceneFlow?.superNodeId,
      afterSelection: sceneFlowSelection,
      afterMulti: sceneFlowMultiSelection
    });
    edgeCreateSourceId = "";
    clearSceneFlowActivity();
    if (selectedProjectId) {
      loadRuntime(selectedProjectId);
    }
  }

  function applyScriptSnapshot(payload) {
    const snapshot = payload?.snapshot || payload;
    if (!snapshot) return;
    if (snapshot.projectId && snapshot.projectId !== selectedProjectId) return;
    if (snapshot.text !== undefined) {
      scriptText = snapshot.text || "";
      scriptDraft = scriptText;
    }
    if (snapshot.version !== undefined) {
      scriptVersion = snapshot.version;
    }
    if (snapshot?.undoState) {
      sceneFlowCanUndo = !!snapshot.undoState.canUndo;
      sceneFlowCanRedo = !!snapshot.undoState.canRedo;
    }
    scriptDiagnostics = Array.isArray(snapshot.parseErrors) ? snapshot.parseErrors : [];
    scriptParseOk = snapshot.parseOk !== false;
    scriptError = "";
    scriptStatus = "";
    scriptLoaded = true;
    scriptLoading = false;
    if (selectedProjectId) {
      loadScriptScenes(selectedProjectId);
    }
  }

  function applyProtocolDirty(payload) {
    loadProjects();
    if (!selectedProjectId) return;
    const areas = Array.isArray(payload?.areas) ? payload.areas : [];
    const hasArea = (value) => !areas.length || areas.includes(value);
    if (hasArea("sceneflow")) {
      loadSceneFlow(selectedProjectId, sceneFlow?.superNodeId || "");
    }
    if (hasArea("script")) {
      loadScript(selectedProjectId);
      loadScriptScenes(selectedProjectId);
      loadScriptElements(selectedProjectId);
    }
    if (hasArea("config")) {
      loadConfig(selectedProjectId);
      loadProjectConfig(selectedProjectId);
    }
  }

  function applyProtocolSelection(selection) {
    if (sceneFlowSelection?.type && sceneFlowSelection.id) {
      return;
    }
    const nodes = Array.isArray(selection?.nodes) ? selection.nodes : [];
    const edges = Array.isArray(selection?.edges) ? selection.edges : [];
    const comments = Array.isArray(selection?.comments) ? selection.comments : [];
    const list = [
      ...nodes.map((id) => ({ type: "node", id })),
      ...edges.map((id) => ({ type: "edge", id })),
      ...comments.map((id) => ({ type: "comment", id }))
    ];
    console.log("[SELDBG] sceneflow.selection", {
      selection,
      pinnedNodeSelectionId,
      pinnedNodeSelectionRevision,
      beforeSelection: sceneFlowSelection,
      beforeMulti: sceneFlowMultiSelection,
      nextSelection: list[0] || null,
      nextMulti: list
    });
    sceneFlowMultiSelection = list;
    sceneFlowSelection = list.length ? list[0] : null;
  }

  function normalizeProtocolEdgeType(edgeType) {
    const normalized = (edgeType || "").trim().toLowerCase();
    if (!normalized) return "";
    if (normalized === "eedge" || normalized === "cedge" || normalized === "pedge" || normalized === "iedge" || normalized === "tedge" || normalized === "fedge") {
      return normalized.toUpperCase();
    }
    switch (normalized) {
      case "epsilon":
        return "EEDGE";
      case "conditional":
        return "CEDGE";
      case "probabilistic":
        return "PEDGE";
      case "interruptive":
        return "IEDGE";
      case "timeout":
        return "TEDGE";
      case "fork":
        return "FEDGE";
      default:
        return "";
    }
  }

  function applyRuntimeVarUpdate(name, rawValue) {
    if (!name) return;
    const value = normalizeRuntimeValue(rawValue);
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

  function monitorVarKey(scope, name) {
    return `${scope}:${name}`;
  }

  function findMonitorVar(key) {
    if (!key) return null;
    const [scope, ...rest] = key.split(":");
    const name = rest.join(":");
    if (!name) return null;
    const list = scope === "local" ? monitorLocals : monitorGlobals;
    return list.find((entry) => entry?.name === name) || null;
  }

  function monitorVarValue(def) {
    const name = (def?.name || "").trim();
    const hasLiveValue = Object.prototype.hasOwnProperty.call(runtimeValues, name);
    const value = normalizeRuntimeValue(hasLiveValue ? runtimeValues[name] : def?.value);
    const expr = normalizeRuntimeValue(def?.expr);
    const initial = normalizeRuntimeValue(runtimeInitialValues[name]);
    const showInitial = hasLiveValue && initial !== "" && value !== initial;
    const displayValue = value || expr || "—";
    return { value, initial, showInitial, displayValue };
  }

  function selectMonitorVar(scope, def) {
    if (!def?.name) return;
    monitorSelectedKey = monitorVarKey(scope, def.name);
    const details = monitorVarValue(def);
    monitorValueDraft = details.value || (details.displayValue === "—" ? "" : details.displayValue);
    monitorError = "";
    monitorStatus = "";
  }

  function openMonitorDialog() {
    if (!selectedProjectId) return;
    rememberFocus();
    monitorDialogOpen = true;
    monitorError = "";
    monitorStatus = "";
    monitorValueDraft = "";
    if (!runtimeInfo) {
      loadRuntime(selectedProjectId);
    }
    if (!monitorSelectedVar) {
      if (monitorGlobals.length) {
        selectMonitorVar("global", monitorGlobals[0]);
      } else if (monitorLocals.length) {
        selectMonitorVar("local", monitorLocals[0]);
      }
    }
    focusDialog(monitorDialogEl);
  }

  function closeMonitorDialog() {
    monitorDialogOpen = false;
    monitorSelectedKey = "";
    monitorValueDraft = "";
    monitorStatus = "";
    monitorError = "";
    restoreFocus();
  }

  async function applyMonitorValue() {
    if (!selectedProjectId || !monitorSelectedVar?.name) return;
    monitorError = "";
    monitorStatus = "";
    const value = monitorValueDraft.trim();
    if (!value) {
      monitorError = "Enter a value to apply.";
      return;
    }
    try {
      const response = await sendCommand("Runtime.Variable.Set", {
        projectId: selectedProjectId,
        name: monitorSelectedVar.name,
        value
      });
      monitorStatus = "Value updated.";
      if (response?.value !== undefined) {
        applyRuntimeVarUpdate(monitorSelectedVar.name, response.value);
      }
    } catch (err) {
      monitorError = err.message || "Failed to update variable.";
    }
  }

  async function runMonitorQuery() {
    if (!selectedProjectId) return;
    monitorError = "";
    monitorStatus = "";
    const query = monitorQueryDraft.trim();
    if (!query) {
      monitorError = "Enter a query to run.";
      return;
    }
    try {
      const response = await sendCommand("Runtime.Query", {
        projectId: selectedProjectId,
        query
      });
      const count = Number.isFinite(response?.count) ? response.count : 0;
      monitorStatus = `Query ok: ${count} solution${count === 1 ? "" : "s"}.`;
    } catch (err) {
      monitorError = err.message || "Failed to run query.";
    }
  }

  function sendCommand(name, payload) {
    console.log("[sendCommand] START", name, "ws state:", ws?.readyState);
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      console.error("[sendCommand] WebSocket not connected!");
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
    console.log("[sendCommand] Sending message:", JSON.stringify(message));
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        console.error("[sendCommand] Request timed out:", name);
        pending.delete(id);
        reject(new Error(`Request timed out: ${name}`));
      }, WS_REQUEST_TIMEOUT_MS);
      pending.set(id, { resolve, reject, timer });
      try {
        ws.send(JSON.stringify(message));
        console.log("[sendCommand] Message sent successfully");
      } catch (err) {
        console.error("[sendCommand] Send error:", err);
        pending.delete(id);
        clearTimeout(timer);
        reject(err);
      }
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
    // When connected to a remote server, prefix API paths with the remote URL
    const url = isRemoteConnection && remoteServerUrl ? `${remoteServerUrl}${path}` : path;
    const response = await fetch(url, {
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
    if (selectedProject?.dirty || sceneFlowDirty) {
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
      if (runtimeState !== "stopped") {
        await runRuntimeCommand("Runtime.Stop");
      }
      await closeProject(projectId);
      return;
    }
    selectedProjectId = "";
    showEditor = false;
    projectLoadAttempted = false;
    projectLoadProjectId = "";
    resetProjectLoadState();
    recentLoaded = false;
    await tick();
    openPathInput?.focus();
  }

  async function exitEditorView() {
    loadConfirmOpen = false;
    loadConfirmReasons = [];
    editorManuallyClosed = true;
    showEditor = false;
    recentLoaded = false;
    await tick();
    openPathInput?.focus();
  }

  function cancelLoadConfirm() {
    loadConfirmOpen = false;
    loadConfirmReasons = [];
    restoreFocus();
  }

  async function confirmSaveAndClose() {
    if (!selectedProjectId) {
      loadConfirmOpen = false;
      restoreFocus();
      return;
    }
    if (projectRequiresSaveAs) {
      // Can't save without a path; fall back to the Save As flow.
      loadConfirmOpen = false;
      loadConfirmReasons = [];
      openSaveAsDialog();
      return;
    }
    await saveProject(selectedProjectId);
    loadConfirmOpen = false;
    loadConfirmReasons = [];
    await returnToLanding(true);
    restoreFocus();
  }

  async function confirmReturnToLanding() {
    loadConfirmOpen = false;
    loadConfirmReasons = [];
    await returnToLanding(true);
    restoreFocus();
  }

  function requestReturnToLanding() {
    const reasons = collectUnsavedReasons();
    if (reasons.length) {
      rememberFocus();
      loadConfirmReasons = reasons;
      loadConfirmOpen = true;
      focusDialog(loadConfirmDialogEl);
      return;
    }
    returnToLanding(true);
  }

  function openSaveAsDialog() {
    saveAsPath = "";
    saveAsError = "";
    rememberFocus();
    saveAsDialogOpen = true;
    focusDialog(saveAsDialogEl, saveAsInputEl);
  }

  function closeSaveAsDialog() {
    saveAsDialogOpen = false;
    saveAsError = "";
    restoreFocus();
  }

  async function confirmSaveAs() {
    const target = (saveAsPath || "").trim();
    if (!target) {
      saveAsError = "Path is required.";
      await tick();
      saveAsInputEl?.focus();
      return;
    }
    const ok = await saveAsProject(selectedProjectId, target);
    if (ok) {
      saveAsDialogOpen = false;
      restoreFocus();
    } else {
      await tick();
      saveAsInputEl?.focus();
    }
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
    restoreFocus();
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

  function projectConfigFingerprint(config) {
    try {
      return JSON.stringify(config || {});
    } catch (err) {
      return "";
    }
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
    const expr = (def.expr ?? def.expression ?? "").trim();
    const hasLiveValue = Object.prototype.hasOwnProperty.call(runtimeValues, name);
    const value = normalizeRuntimeValue(hasLiveValue ? runtimeValues[name] : def.value);
    // Use captured initial value, or fall back to expr (definition expression) for hot-connect scenarios
    const capturedInitial = normalizeRuntimeValue(runtimeInitialValues[name]);
    const initial = capturedInitial || expr;
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

  const ALL_EDGE_TYPES = ["EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE"];

  function normalizeEdgeType(type) {
    return String(type || "EEDGE").trim().toUpperCase() || "EEDGE";
  }

  function allowedEdgeTypesForSource(nodeId, snapshot = sceneFlow) {
    if (!nodeId) return new Set(ALL_EDGE_TYPES);
    const sourceKey = String(nodeId).trim();
    const edges = (snapshot?.edges || []).filter((edge) => {
      if (!edge) return false;
      const edgeSource = String(edge.sourceId ?? "").trim();
      if (edgeSource !== sourceKey) return false;
      const targetId = String(edge.targetId ?? "").trim();
      return targetId.length > 0;
    });
    if (!edges.length) return new Set(ALL_EDGE_TYPES);
    const types = edges.map((edge) => normalizeEdgeType(edge?.type));
    const hasC = types.includes("CEDGE");
    const hasP = types.includes("PEDGE");
    const hasI = types.includes("IEDGE");
    const hasF = types.includes("FEDGE");
    const hasE = types.includes("EEDGE");
    const hasT = types.includes("TEDGE");
    const hasD = hasE || hasT;
    if (hasP) return new Set(["PEDGE"]);
    if (hasI) return new Set(["IEDGE"]);
    if (hasF) return new Set(["FEDGE"]);
    if (hasC) {
      const allowed = new Set(["CEDGE"]);
      if (!hasD) {
        allowed.add("EEDGE");
        allowed.add("TEDGE");
      }
      return allowed;
    }
    if (hasD) return new Set(["CEDGE"]);
    return new Set(ALL_EDGE_TYPES);
  }

  function edgeTypeAllowedForSource(type, nodeId) {
    if (!nodeId) return true;
    const allowed = edgeRestrictionAllowed;
    return allowed.has(normalizeEdgeType(type));
  }

  function edgeTypeDisabled(type) {
    if (!edgeRestrictionNodeId) return false;
    return !edgeTypeAllowedForSource(type, edgeRestrictionNodeId);
  }

  function edgeTypeLabel(type) {
    if (!type) return "";
    return edgeTypeLabels[type] || type;
  }

  function sceneLanguageLabel(language) {
    const trimmed = (language || "").trim();
    return trimmed ? trimmed : "default";
  }

  function sceneParamsForName(name) {
    if (!name) return [];
    const params = helperSceneIndex?.get(name);
    return Array.isArray(params) ? params.filter((param) => String(param || "").trim()) : [];
  }

  function playSceneCommand(name, params = []) {
    const raw = String(name || "");
    const escaped = raw.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
    const cleaned = Array.isArray(params) ? params.map((param) => String(param || "").trim()).filter(Boolean) : [];
    if (!cleaned.length) {
      return `PlayScene("${escaped}")`;
    }
    const bindings = cleaned.map((param) => `${param}=""`).join(", ");
    return `PlayScene("${escaped}", { ${bindings} })`;
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

  function extractMissingAgents(sceneAgents, agents) {
    if (!Array.isArray(sceneAgents) || !sceneAgents.length) return [];
    const configured = new Set();
    if (Array.isArray(agents)) {
      for (const agent of agents) {
        const name = String(agent?.name || "").trim().toLowerCase();
        if (name) configured.add(name);
      }
    }
    return sceneAgents.filter((name) => {
      const key = String(name || "").trim().toLowerCase();
      return key && !configured.has(key);
    });
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

  function buildMissingAgentDeviceOptions(plugins) {
    const options = [];
    const seen = new Set();
    const addOption = (value, label) => {
      const trimmed = (value || "").trim();
      if (!trimmed || seen.has(trimmed)) return;
      seen.add(trimmed);
      options.push({ value: trimmed, label: label || trimmed });
    };
    if (Array.isArray(plugins)) {
      for (const plugin of plugins) {
        addOption(plugin?.name, plugin?.name);
      }
    }
    return options;
  }

  function defaultMissingAgentDevice(plugins) {
    const options = buildMissingAgentDeviceOptions(plugins);
    return options.length ? options[0].value : "";
  }

  function buildMissingAgentDrafts(names, plugins) {
    const device = defaultMissingAgentDevice(plugins);
    return (names || []).map((name) => ({
      name,
      device,
      host: DEFAULT_AGENT_HOST,
      port: DEFAULT_AGENT_PORT
    }));
  }

  function openMissingAgentDialog() {
    if (!selectedProjectId) return;
    rememberFocus();
    missingAgentError = "";
    missingAgentBusy = false;
    missingAgentDrafts = buildMissingAgentDrafts(missingAgentNames, projectConfigPlugins);
    missingAgentDialogOpen = true;
    loadAvailableDevices();
    if (!projectConfigDraft) {
      loadProjectConfig(selectedProjectId);
    }
    focusDialog(missingAgentDialogEl);
  }

  function closeMissingAgentDialog() {
    missingAgentDialogOpen = false;
    missingAgentDrafts = [];
    missingAgentError = "";
    missingAgentBusy = false;
    restoreFocus();
  }

  function updateMissingAgentDraft(index, field, value) {
    missingAgentDrafts = missingAgentDrafts.map((draft, idx) =>
      idx === index ? { ...draft, [field]: value } : draft
    );
  }

  async function applyMissingAgentsAndRun() {
    if (!selectedProjectId) return;
    missingAgentError = "";
    missingAgentBusy = true;
    try {
      const base = normalizeProjectConfig(projectConfigDraft || projectConfig || {});
      const existing = new Set(
        (base.agents || []).map((agent) => (agent?.name || "").trim().toLowerCase()).filter(Boolean)
      );
      const nextAgents = [...base.agents];
      for (const draft of missingAgentDrafts) {
        const name = (draft?.name || "").trim();
        if (!name || existing.has(name.toLowerCase())) continue;
        const device = (draft?.device || "").trim();
        if (!device) {
          missingAgentError = `Select a device for ${name}.`;
          return;
        }
        const features = [];
        nextAgents.push({ name, device, features });
        existing.add(name.toLowerCase());
      }
      const response = await sendCommand("ProjectConfig.Update", {
        projectId: selectedProjectId,
        config: { ...base, agents: nextAgents }
      });
      projectConfig = normalizeProjectConfig(response.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      projectConfigSaved = response.saved ?? null;
      projectConfigPending = response.pending === true;
      missingAgentDialogOpen = false;
      missingAgentDrafts = [];
      await executeRuntimeCommand("Runtime.Play");
    } catch (err) {
      missingAgentError = err.message || "Failed to update project config.";
    } finally {
      missingAgentBusy = false;
    }
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
    const params = sceneParamsForName(sceneName);
    const response = await runSceneFlowCommand("SceneFlow.Node.Cmd.Add", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId,
      command: { text: playSceneCommand(sceneName, params) }
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
      superNodeId: sceneFlow?.superNodeId,
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
    const target = snapSceneFlowPosition({ x: payload.x, y: payload.y });
    const response = await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeType: "Basic",
      name: payload.name,
      x: target.x,
      y: target.y
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
    const target = snapSceneFlowPosition({ x: payload.x, y: payload.y });
    const response = await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeType: "Basic",
      name: payload.name,
      x: target.x,
      y: target.y
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
      const nextType = payload.edgeType || "EEDGE";
      const targetId = payload.targetNodeId || "";
      if (targetId && !edgeTypeAllowedForSource(nextType, targetId)) {
        sceneFlowError = `Edge ${edgeTypeLabel(nextType)} not allowed for this node.`;
        return;
      }
      edgeCreateType = nextType;
      edgeCreateMode = true;
      edgeCreateSourceId = targetId;
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

  function buildSceneGroupsFromScript(text) {
    const output = [];
    if (!text) return output;
    const lines = String(text).split(/\r?\n/);
    const grouped = new Map();
    let current = null;
    let hasContent = false;
    const flush = () => {
      if (current && hasContent) {
        const lang = current.language || "";
        const name = current.name || "";
        if (name) {
          if (!grouped.has(lang)) grouped.set(lang, new Map());
          const map = grouped.get(lang);
          const entry = map.get(name) || { count: 0, params: new Set() };
          entry.count += 1;
          current.params.forEach((param) => entry.params.add(param));
          map.set(name, entry);
        }
      }
      current = null;
      hasContent = false;
    };
    for (let i = 0; i < lines.length; i += 1) {
      const raw = lines[i];
      const line = raw.trim();
      if (!line || line.startsWith("//") || line.startsWith("#")) {
        continue;
      }
      const match = line.match(/^scene\s+(\S+)\s+(.+)$/i);
      if (match) {
        flush();
        current = { language: match[1], name: match[2].trim(), params: new Set() };
        continue;
      }
      if (current) {
        hasContent = true;
        const paramMatches = line.matchAll(/\$([A-Za-z_][A-Za-z0-9_]*)/g);
        for (const param of paramMatches) {
          if (param?.[1]) {
            current.params.add(param[1]);
          }
        }
      }
    }
    flush();
    for (const [language, groups] of grouped.entries()) {
      const groupList = Array.from(groups.entries())
        .map(([name, entry]) => ({
          name,
          count: entry.count,
          params: Array.from(entry.params)
        }))
        .sort((a, b) => a.name.localeCompare(b.name));
      output.push({ language, groups: groupList });
    }
    output.sort((a, b) => String(a.language || "").localeCompare(String(b.language || "")));
    return output;
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

  function closeTypeDefDialog() {
    resetTypeDefEditor();
    restoreFocus();
  }

  function closeVarDefDialog() {
    resetVarDefEditor();
    restoreFocus();
  }

  function resetCmdEditor() {
    cmdDraft = "";
    cmdEditIndex = null;
    cmdError = "";
    cmdSelectedIndex = null;
    cmdInlineDrafts = [];
    cmdDialogNodeId = "";
    cmdInlineInputEls = [];
    cmdHelperOpen = false;
  }

  function syncCmdInlineDrafts() {
    cmdInlineDrafts = nodeEditorCommands.map((cmd) => cmd.text ?? "");
    cmdDialogNodeId = nodeEditorTarget?.id || "";
  }

  async function closeCmdDialog() {
    if (cmdDialogOpen && wsConnected && !sceneFlowBusy && cmdInlineDrafts.length) {
      for (let i = cmdInlineDrafts.length - 1; i >= 0; i -= 1) {
        await commitCmdInlineDraft(i);
      }
    }
    cmdDialogOpen = false;
    resetCmdEditor();
    restoreFocus();
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
    return buildTypeExample(name, 0, false);
  }

  function varExpressionHint(typeName) {
    const name = (typeName || "").trim();
    if (!name) return "";
    const example = buildTypeExample(name, 0, true);
    return example ? `e.g. ${example}` : "";
  }

  function buildTypeExample(typeName, depth, preferHint) {
    const name = (typeName || "").trim();
    if (!name) return "";
    if (name === "Int") return preferHint ? "0" : "0";
    if (name === "Bool") return preferHint ? "true / false" : "true";
    if (name === "Float") return preferHint ? "0.0" : "0.0";
    if (name === "String") return preferHint ? "\"text\"" : "\"\"";
    const match = nodeEditorTypeCatalog.find((entry) => entry?.name === name);
    if (!match || depth > 2) {
      return "";
    }
    if (match.flavour === "List") {
      const elementType = (match.elementType || "").trim();
      const elementExample = elementType ? buildTypeExample(elementType, depth + 1, false) : "";
      return elementExample ? `[${elementExample}]` : "[ ]";
    }
    if (match.flavour === "Struct") {
      const members = Array.isArray(match.members) ? match.members : [];
      const parts = members.slice(0, 3).map((member) => {
        const memberName = (member?.name ?? "").trim();
        const memberType = (member?.type ?? "").trim();
        if (!memberName && !memberType) return "";
        const value = memberType ? buildTypeExample(memberType, depth + 1, false) : "";
        if (!memberName) return value;
        if (!value) return `${memberName} =`;
        return `${memberName} = ${value}`;
      }).filter(Boolean);
      if (parts.length) {
        return `{ ${parts.join(", ")} }`;
      }
      return "{ }";
    }
    return "";
  }

  function defaultVarDefDraft() {
    const preferred = nodeEditorTypeOptions.includes("Bool") ? "Bool" : nodeEditorTypeOptions[0] || "Bool";
    return {
      name: "",
      type: preferred,
      expression: ""
    };
  }

  function typeDefSummary(def) {
    if (!def) return "";
    if (def.flavour === "List") {
      const elementType = (def.elementType || "").trim();
      return elementType ? elementType : "";
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
    if (flavour === "List") {
      const base = name ? `List ${name}` : "List";
      return summary ? `${base}(${summary})` : base;
    }
    if (flavour === "Struct") {
      const base = name ? `Struct ${name}` : "Struct";
      return summary ? `${base}(${summary})` : base;
    }
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
    rememberFocus();
    typeDefError = "";
    typeDefEditIndex = -1;
    typeDefSelectedIndex = null;
    typeDefDraft = defaultTypeDefDraft();
    focusDialog(typeDefDialogEl, typeDefNameInputEl);
  }

  function startTypeDefEdit(index) {
    const def = nodeEditorTypeDefs[index];
    if (!def) return;
    rememberFocus();
    typeDefError = "";
    typeDefEditIndex = index;
    typeDefSelectedIndex = index;
    typeDefDraft = {
      name: def.name ?? "",
      flavour: def.flavour ?? "Struct",
      elementType: def.elementType ?? "Int",
      members: Array.isArray(def.members) ? def.members.map((member) => ({ ...member })) : []
    };
    focusDialog(typeDefDialogEl, typeDefNameInputEl);
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
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      typeDef: payload
    };
    if (typeDefEditIndex >= 0) {
      commandPayload.index = typeDefEditIndex;
    }
    pinSelectedNodeSelection();
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
    pinSelectedNodeSelection();
    return await runSceneFlowCommand("SceneFlow.Node.TypeDef.Move", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
  }

  async function deleteTypeDef(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    pinSelectedNodeSelection();
    await runSceneFlowCommand("SceneFlow.Node.TypeDef.Delete", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      index
    });
    if (typeDefEditIndex === index) {
      resetTypeDefEditor();
    }
  }

  function startVarDefAdd() {
    rememberFocus();
    varDefError = "";
    varDefEditIndex = -1;
    varDefSelectedIndex = null;
    varDefDraft = defaultVarDefDraft();
    focusDialog(varDefDialogEl, varDefNameInputEl);
  }

  function startVarDefEdit(index) {
    const def = nodeEditorVarDefs[index];
    if (!def) return;
    rememberFocus();
    varDefError = "";
    varDefEditIndex = index;
    varDefSelectedIndex = index;
    varDefDraft = {
      name: def.name ?? "",
      type: def.type ?? (nodeEditorTypeOptions[0] || "Bool"),
      expression: def.expression ?? ""
    };
    focusDialog(varDefDialogEl, varDefNameInputEl);
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
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      varDef: payload
    };
    if (varDefEditIndex >= 0) {
      commandPayload.index = varDefEditIndex;
    }
    pinSelectedNodeSelection();
    const response = await runSceneFlowCommand(commandName, commandPayload);
    if (!response) {
      varDefError = sceneFlowError || "Failed to update variable definitions.";
      return;
    }
    resetVarDefEditor();
    if (response.scriptChanged) {
      loadScript(selectedProjectId);
      loadScriptScenes(selectedProjectId);
      loadScriptElements(selectedProjectId);
    }
    refreshRuntimeVars(nodeEditorTarget);
  }

  function handleVarDefKeydown(event) {
    if (event.key !== "Enter") return;
    event.preventDefault();
    if (!wsConnected || sceneFlowBusy) return;
    applyVarDefEdit();
  }

  async function moveVarDef(index, direction) {
    if (!selectedProjectId || !nodeEditorTarget) return null;
    if (!nodeEditorVarDefs[index]) return null;
    const target = index + direction;
    if (target < 0 || target >= nodeEditorVarDefs.length) return null;
    pinSelectedNodeSelection();
    const response = await runSceneFlowCommand("SceneFlow.Node.VarDef.Move", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
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
    pinSelectedNodeSelection();
    await runSceneFlowCommand("SceneFlow.Node.VarDef.Delete", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
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
    rememberFocus();
    if (nodeId && (sceneFlowSelection?.type !== "node" || sceneFlowSelection.id !== nodeId)) {
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
      await tick();
    }
    cmdDialogOpen = true;
    cmdInlineInputEls = [];
    cmdHelperOpen = false;
    syncCmdInlineDrafts();
    focusDialog(cmdDialogEl);
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
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      command: { text }
    };
    if (isExisting) {
      commandPayload.index = index;
    }
    pinSelectedNodeSelection();
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
    pinSelectedNodeSelection();
    return await runSceneFlowCommand("SceneFlow.Node.Cmd.Move", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
  }

  async function deleteCmd(index) {
    if (!selectedProjectId || !nodeEditorTarget) return;
    pinSelectedNodeSelection();
    return await runSceneFlowCommand("SceneFlow.Node.Cmd.Delete", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
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

  function openCmdHelper() {
    cmdHelperType = "PlayScene";
    cmdHelperScene = helperScenes?.[0] || "";
    cmdHelperAgent = "";
    const actionOption = Array.isArray(scriptElements?.acticon) ? scriptElements.acticon[0] : null;
    cmdHelperAction = actionOption?.name || actionOption?.script || "";
    cmdHelperArgs = [];
    cmdHelperVarName = helperVarCandidates?.[0]?.name || "";
    cmdHelperVarType = helperVarCandidates?.[0]?.type || "Int";
    cmdHelperVarExpr = "";
    cmdHelperVarStep = "1";
    cmdHelperSceneBindings = {};
    cmdHelperVarScope = "global";
    cmdHelperOpen = true;
  }

  function updateCmdHelperType() {
    if (cmdHelperType === "Inc" || cmdHelperType === "Dec") {
      cmdHelperVarType = "Int";
      cmdHelperVarStep = cmdHelperVarStep || "1";
      cmdHelperVarExpr = "";
    }
    if (cmdHelperType === "Assign") {
      cmdHelperVarExpr = cmdHelperVarExpr || "";
    }
  }

  $: if (cmdHelperOpen && cmdHelperType === "PlayScene") {
    const params = helperSceneIndex.get(cmdHelperScene) || [];
    const next = {};
    params.forEach((param) => {
      next[param] = cmdHelperSceneBindings?.[param] || "";
    });
    cmdHelperSceneBindings = next;
  }

  function closeCmdHelper() {
    cmdHelperOpen = false;
  }

  function addCmdHelperArg() {
    cmdHelperArgs = [...cmdHelperArgs, { key: "", value: "" }];
  }

  function removeCmdHelperArg(index) {
    cmdHelperArgs = cmdHelperArgs.filter((_, idx) => idx !== index);
  }

  function updateCmdHelperArg(index, field, value) {
    cmdHelperArgs = cmdHelperArgs.map((entry, idx) => (idx === index ? { ...entry, [field]: value } : entry));
  }

  function commandFromHelper() {
    if (cmdHelperType === "PlayScene") {
      const scene = (cmdHelperScene || "").trim();
      if (!scene) return "";
      const bindings = Object.entries(cmdHelperSceneBindings || {})
        .map(([key, value]) => {
          const name = (key || "").trim();
          const mapped = (value || "").trim();
          if (!name || !mapped) return "";
          return `${name} = ${mapped}`;
        })
        .filter(Boolean);
      if (bindings.length) {
        return `PlayScene("${scene}", { ${bindings.join(", ")} })`;
      }
      return `PlayScene("${scene}")`;
    }
    if (cmdHelperType === "PlayAction") {
      const agent = (cmdHelperAgent || "").trim();
      const action = (cmdHelperAction || "").trim();
      if (!agent || !action) return "";
      const args = cmdHelperArgs
        .map((entry) => {
          const key = (entry?.key || "").trim();
          const value = (entry?.value || "").trim();
          if (!key || !value) return "";
          return `${key}=${value}`;
        })
        .filter(Boolean)
        .join(" ");
      const payload = [agent, action, args].filter(Boolean).join(" ");
      return `PlayAction("[${payload}]")`;
    }
    if (cmdHelperType === "Assign") {
      const name = (cmdHelperVarName || "").trim();
      const expr = (cmdHelperVarExpr || "").trim();
      if (!name || !expr) return "";
      return `${name} = ${expr}`;
    }
    if (cmdHelperType === "Inc") {
      const name = (cmdHelperVarName || "").trim();
      const step = (cmdHelperVarStep || "").trim() || "1";
      if (!name) return "";
      return `${name} = ${name} + ${step}`;
    }
    if (cmdHelperType === "Dec") {
      const name = (cmdHelperVarName || "").trim();
      const step = (cmdHelperVarStep || "").trim() || "1";
      if (!name) return "";
      return `${name} = ${name} - ${step}`;
    }
    return "";
  }

  async function ensureHelperVarExists() {
    const name = (cmdHelperVarName || "").trim();
    if (!name || !nodeEditorTarget || !selectedProjectId) return true;
    const existing = helperVarCandidates.find((entry) => entry.name === name);
    if (existing) return true;
    const type = (cmdHelperVarType || "Int").trim() || "Int";
    const confirmCreate = window.confirm(`Create variable "${name}" (${type})?`);
    if (!confirmCreate) return false;
    let targetNodeId = "";
    if (cmdHelperVarScope === "local") {
      targetNodeId = nodeEditorTarget?.id || "";
    } else if (cmdHelperVarScope === "parent") {
      const path = Array.isArray(sceneFlowPathNodes) ? sceneFlowPathNodes : [];
      if (path.length > 1) {
        targetNodeId = path[path.length - 2]?.id || "";
      }
    }
    const payload = {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: targetNodeId,
      varDef: {
        name,
        type,
        expression: defaultVarExpression(type)
      }
    };
    const response = await runSceneFlowCommand("SceneFlow.Node.VarDef.Add", payload);
    return !!response;
  }

  function insertHelperCommand(text) {
    if (!text) return;
    if (cmdSelectedIndex === null) {
      cmdInlineDrafts = [...cmdInlineDrafts, text];
      cmdSelectedIndex = cmdInlineDrafts.length - 1;
      return;
    }
    const el = cmdInlineInputEls[cmdSelectedIndex];
    if (el && typeof el.setRangeText === "function") {
      const start = el.selectionStart ?? el.value.length;
      const end = el.selectionEnd ?? el.value.length;
      el.setRangeText(text, start, end, "end");
      updateCmdInlineDraft(cmdSelectedIndex, el.value);
      el.focus();
      return;
    }
    const current = cmdInlineDrafts[cmdSelectedIndex] ?? "";
    const next = current ? `${current} ${text}` : text;
    updateCmdInlineDraft(cmdSelectedIndex, next);
  }

  async function applyCmdHelperInsert() {
    cmdError = "";
    if (cmdHelperType === "PlayAction") {
      if (!(cmdHelperAgent || "").trim()) {
        cmdError = "Agent name is required.";
        return;
      }
    }
    if (cmdHelperType === "Assign" || cmdHelperType === "Inc" || cmdHelperType === "Dec") {
      const ok = await ensureHelperVarExists();
      if (!ok) return;
    }
    const text = commandFromHelper();
    if (!text) {
      cmdError = "Helper command is incomplete.";
      return;
    }
    insertHelperCommand(text);
    cmdHelperOpen = false;
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
    if (!box || !Number.isFinite(box.width) || !Number.isFinite(box.height) || box.width <= 0 || box.height <= 0) {
      return { x: 120, y: 120 };
    }
    const cx = box.x + box.width / 2;
    const cy = box.y + box.height / 2;
    if (!Number.isFinite(cx) || !Number.isFinite(cy)) {
      return { x: 120, y: 120 };
    }
    return { x: cx, y: cy };
  }

  function snapSceneFlowPosition(position) {
    if (!position) return position;
    const nodeWidth = readConfigInt("node_width", PREF_NODE_DEFAULT);
    const nodeHeight = readConfigInt("node_height", nodeWidth);
    const bounds = sceneFlowViewBox || sceneFlowWorldBox;
    const clampToBounds = (pos) => {
      if (!bounds || !Number.isFinite(bounds.width) || !Number.isFinite(bounds.height)) {
        return pos;
      }
      const maxX = bounds.x + bounds.width - nodeWidth;
      const maxY = bounds.y + bounds.height - nodeHeight;
      return {
        x: Math.min(Math.max(pos.x, bounds.x), maxX),
        y: Math.min(Math.max(pos.y, bounds.y), maxY)
      };
    };
    const gridScaleX = readConfigInt("grid_x", PREF_GRID_DEFAULT);
    const gridScaleY = readConfigInt("grid_y", gridScaleX);
    const gridX = Math.max(8, nodeWidth * gridScaleX);
    const gridY = Math.max(8, nodeHeight * gridScaleY);
    const originX = nodeWidth / 2 + nodeWidth / 3;
    const originY = nodeHeight / 2 + nodeHeight / 3;
    let base = {
      x: Math.max(1, Math.round(position.x)),
      y: Math.max(1, Math.round(position.y))
    };
    if (sceneFlowNodeSnap) {
      const centerX = base.x + nodeWidth / 2;
      const centerY = base.y + nodeHeight / 2;
      if (!Number.isFinite(centerX) || !Number.isFinite(centerY)) return base;
      const snappedCenterX = originX + Math.round((centerX - originX) / gridX) * gridX;
      const snappedCenterY = originY + Math.round((centerY - originY) / gridY) * gridY;
      base = {
        x: Math.max(1, Math.round(snappedCenterX - nodeWidth / 2)),
        y: Math.max(1, Math.round(snappedCenterY - nodeHeight / 2))
      };
    }
    const occupied = [];
    for (const node of sceneFlow?.nodes || []) {
      if (!node) continue;
      const nxRaw = node?.graphics?.x ?? node?.x;
      const nyRaw = node?.graphics?.y ?? node?.y;
      const nx = Number(nxRaw);
      const ny = Number(nyRaw);
      if (!Number.isFinite(nx) || !Number.isFinite(ny)) continue;
      occupied.push({ x: nx, y: ny, w: nodeWidth, h: nodeHeight });
    }
    for (const key of pendingNodePositions) {
      const parts = String(key).split("|");
      if (parts.length !== 2) continue;
      const px = Number(parts[0]);
      const py = Number(parts[1]);
      if (!Number.isFinite(px) || !Number.isFinite(py)) continue;
      occupied.push({ x: px, y: py, w: nodeWidth, h: nodeHeight });
    }
    const overlaps = (pos) => {
      const rect = { x: pos.x, y: pos.y, w: nodeWidth, h: nodeHeight };
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
    base = clampToBounds(base);
    if (!overlaps(base)) {
      pendingNodePositions.add(`${Math.round(base.x)}|${Math.round(base.y)}`);
      return base;
    }
    const baseCenterX = base.x + nodeWidth / 2;
    const baseCenterY = base.y + nodeHeight / 2;
    const baseIx = Math.round((baseCenterX - originX) / gridX);
    const baseIy = Math.round((baseCenterY - originY) / gridY);
    const maxGridX = bounds && Number.isFinite(bounds.width) ? Math.ceil(bounds.width / gridX) + 2 : 40;
    const maxGridY = bounds && Number.isFinite(bounds.height) ? Math.ceil(bounds.height / gridY) + 2 : 40;
    const maxRadius = Math.max(10, Math.ceil(Math.sqrt(occupied.length || 0)) + 10, Math.max(maxGridX, maxGridY));
    const maxSteps = (maxRadius * 2 + 1) ** 2;
    let stepX = 0;
    let stepY = 0;
    let dirX = 1;
    let dirY = 0;
    let segmentLength = 1;
    let segmentPassed = 0;
    let segmentCount = 0;
    for (let step = 0; step < maxSteps; step += 1) {
      if (!(stepX === 0 && stepY === 0) && Math.max(Math.abs(stepX), Math.abs(stepY)) <= maxRadius) {
        const centerX = originX + (baseIx + stepX) * gridX;
        const centerY = originY + (baseIy + stepY) * gridY;
        const candidate = {
          x: Math.round(centerX - nodeWidth / 2),
          y: Math.round(centerY - nodeHeight / 2)
        };
        if (bounds) {
          if (candidate.x < bounds.x || candidate.y < bounds.y) {
            // skip
          } else if (candidate.x + nodeWidth > bounds.x + bounds.width) {
            // skip
          } else if (candidate.y + nodeHeight > bounds.y + bounds.height) {
            // skip
          } else if (!overlaps(candidate)) {
            pendingNodePositions.add(`${Math.round(candidate.x)}|${Math.round(candidate.y)}`);
            return candidate;
          }
        } else if (!overlaps(candidate)) {
          pendingNodePositions.add(`${Math.round(candidate.x)}|${Math.round(candidate.y)}`);
          return candidate;
        }
      }
      stepX += dirX;
      stepY += dirY;
      segmentPassed += 1;
      if (segmentPassed === segmentLength) {
        segmentPassed = 0;
        segmentCount += 1;
        const nextDirX = -dirY;
        const nextDirY = dirX;
        dirX = nextDirX;
        dirY = nextDirY;
        if (segmentCount % 2 === 0) {
          segmentLength += 1;
        }
      }
    }
    return base;
  }

  function nodeScaledSize(node) {
    const baseWidth = Number.isFinite(node?.size?.w) ? node.size.w : PREF_NODE_DEFAULT;
    const baseHeight = Number.isFinite(node?.size?.h) ? node.size.h : baseWidth;
    if (node?.type !== "Super") {
      return { w: baseWidth, h: baseHeight };
    }
    const count = Number.isFinite(node?.childCount) ? node.childCount : 0;
    const steps = Math.max(0, Math.floor(count / 5));
    const scale = 1 + steps * 0.05;
    return { w: baseWidth * scale, h: baseHeight * scale };
  }

  function nodeBounds(node) {
    const x = Number.isFinite(node?.graphics?.x) ? node.graphics.x : 0;
    const y = Number.isFinite(node?.graphics?.y) ? node.graphics.y : 0;
    const size = nodeScaledSize(node);
    const cx = x + size.w / 2;
    const cy = y + size.h / 2;
    return { x, y, w: size.w, h: size.h, cx, cy };
  }

  async function runSceneFlowCommand(name, payload) {
    console.log("[runSceneFlowCommand] START", name, payload);
    if (!selectedProjectId) {
      console.log("[runSceneFlowCommand] No selectedProjectId, returning null");
      return null;
    }
    sceneFlowError = "";
    sceneFlowBusy = true;
    try {
      console.log("[runSceneFlowCommand] Calling sendCommand...");
      const response = await sendCommand(name, payload);
      console.log("[runSceneFlowCommand] Response:", response);
      if (response?.snapshot) {
        sceneFlow = response.snapshot;
      } else if (response?.nodes && response?.edges) {
        sceneFlow = response;
      }
      if (response) {
        sceneFlowDirty = true;
      }
      return response;
    } catch (err) {
      console.error("[runSceneFlowCommand] ERROR:", err);
      sceneFlowError = err.message || "SceneFlow command failed.";
      return null;
    } finally {
      sceneFlowBusy = false;
    }
  }

  function snapshotFileSlug(value) {
    return String(value || "")
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, "-")
      .replace(/^-+|-+$/g, "") || "sceneflow";
  }

  function sceneFlowSnapshotFilename() {
    const projectName = snapshotFileSlug(selectedProject?.name || "project");
    const flowName = snapshotFileSlug(currentSuperName || "sceneflow");
    const stamp = new Date().toISOString().replace(/[:.]/g, "-");
    return `${projectName}-${flowName}-${stamp}.png`;
  }

  function triggerDownload(dataUrl, filename) {
    const link = document.createElement("a");
    link.href = dataUrl;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    link.remove();
  }

  async function normalizeAllEdges() {
    if (!selectedProjectId || sceneFlowBusy) return;
    await runSceneFlowCommand("SceneFlow.Edge.NormalizeAll", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId
    });
  }

  async function straightenAllEdges() {
    if (!selectedProjectId || sceneFlowBusy) return;
    await runSceneFlowCommand("SceneFlow.Edge.StraightenAll", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId
    });
  }

  async function downloadSceneFlowSnapshot() {
    if (!sceneFlowRef?.exportPng) return;
    sceneFlowError = "";
    const dataUrl = await sceneFlowRef.exportPng();
    if (!dataUrl) {
      sceneFlowError = "Failed to export SceneFlow snapshot.";
      return;
    }
    triggerDownload(dataUrl, sceneFlowSnapshotFilename());
  }

  async function alignSelectedNodes(mode) {
    if (!selectedProjectId || !selectionHasMovableNodes || !wsConnected || sceneFlowBusy) return;
    const items = selectionNodes.map((node) => ({ node, bounds: nodeBounds(node) }));
    if (items.length < 2) return;
    const xs = items.map((entry) => entry.bounds.x);
    const ys = items.map((entry) => entry.bounds.y);
    const rights = items.map((entry) => entry.bounds.x + entry.bounds.w);
    const bottoms = items.map((entry) => entry.bounds.y + entry.bounds.h);
    const centersX = items.map((entry) => entry.bounds.cx);
    const centersY = items.map((entry) => entry.bounds.cy);
    let targetX = null;
    let targetY = null;
    if (mode === "left") targetX = Math.min(...xs);
    if (mode === "right") targetX = Math.max(...rights);
    if (mode === "center") targetX = (Math.min(...centersX) + Math.max(...centersX)) / 2;
    if (mode === "top") targetY = Math.min(...ys);
    if (mode === "bottom") targetY = Math.max(...bottoms);
    if (mode === "middle") targetY = (Math.min(...centersY) + Math.max(...centersY)) / 2;
    const moves = [];
    for (const entry of items) {
      const bounds = entry.bounds;
      let nextX = bounds.x;
      let nextY = bounds.y;
      if (targetX !== null) {
        nextX = mode === "right" ? targetX - bounds.w : targetX - bounds.w / 2;
        if (mode === "left") {
          nextX = targetX;
        }
      }
      if (targetY !== null) {
        nextY = mode === "bottom" ? targetY - bounds.h : targetY - bounds.h / 2;
        if (mode === "top") {
          nextY = targetY;
        }
      }
      if (nextX !== bounds.x || nextY !== bounds.y) {
        moves.push({ id: entry.node.id, x: nextX, y: nextY });
      }
    }
    if (!moves.length) return;
    if (moves.length === 1) {
      const move = moves[0];
      await moveSceneFlowNode(move.id, move.x, move.y, sceneFlowNodeSnap);
      return;
    }
    await moveSceneFlowNodeGroup(moves, sceneFlowNodeSnap);
  }

  async function distributeSelectedNodes(axis) {
    if (!selectedProjectId || !selectionCanDistribute || !wsConnected || sceneFlowBusy) return;
    const items = selectionNodes.map((node) => ({ node, bounds: nodeBounds(node) }));
    if (items.length < 3) return;
    const key = axis === "y" ? "cy" : "cx";
    items.sort((a, b) => a.bounds[key] - b.bounds[key]);
    const first = items[0].bounds[key];
    const last = items[items.length - 1].bounds[key];
    const gap = (last - first) / (items.length - 1);
    if (!Number.isFinite(gap)) return;
    const moves = [];
    for (let i = 1; i < items.length - 1; i += 1) {
      const entry = items[i];
      const targetCenter = first + gap * i;
      const bounds = entry.bounds;
      const nextX = axis === "x" ? targetCenter - bounds.w / 2 : bounds.x;
      const nextY = axis === "y" ? targetCenter - bounds.h / 2 : bounds.y;
      if (nextX !== bounds.x || nextY !== bounds.y) {
        moves.push({ id: entry.node.id, x: nextX, y: nextY });
      }
    }
    if (!moves.length) return;
    if (moves.length === 1) {
      const move = moves[0];
      await moveSceneFlowNode(move.id, move.x, move.y, sceneFlowNodeSnap);
      return;
    }
    await moveSceneFlowNodeGroup(moves, sceneFlowNodeSnap);
  }

  async function setSelectedNodesStart(value) {
    if (!selectedProjectId || !selectionNodes.length || !wsConnected || sceneFlowBusy) return;
    const updates = selectionNodes.filter((node) => !node.isHistory && node.isStart !== value);
    for (const node of updates) {
      await runSceneFlowCommand("SceneFlow.Node.Update", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId,
        nodeId: node.id,
        fields: { isStart: value }
      });
    }
  }

  async function createSceneFlowNode(nodeType, position = null) {
    if (!selectedProjectId) return;
    const fallback = sceneFlowCenter();
    const center = position || fallback;
    const safeCenter = {
      x: Number.isFinite(center?.x) ? center.x : fallback.x,
      y: Number.isFinite(center?.y) ? center.y : fallback.y
    };
    const target = snapSceneFlowPosition(safeCenter);
    const response = await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeType,
      x: target.x,
      y: target.y
    });
    // Select the newly created node
    if (response?.nodeId) {
      sceneFlowSelection = { type: "node", id: response.nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: response.nodeId }];
    }
  }

  async function createSceneFlowComment(position = null) {
    if (!selectedProjectId) return;
    const center = position || sceneFlowCenter();
    const response = await runSceneFlowCommand("SceneFlow.Comment.Create", {
      projectId: selectedProjectId,
      x: center.x,
      y: center.y
    });
    // Select the newly created comment
    if (response?.commentId) {
      sceneFlowSelection = { type: "comment", id: response.commentId };
      sceneFlowMultiSelection = [{ type: "comment", id: response.commentId }];
    }
  }

  async function createSceneFlowEdge(sourceId, targetId) {
    if (!selectedProjectId || !sourceId || !targetId) return null;
    const response = await runSceneFlowCommand("SceneFlow.Edge.Create", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      sourceId,
      targetId,
      edgeType: edgeCreateType || "EEDGE"
    });
    return response || null;
  }

  function toggleEdgeCreateMode() {
    edgeCreateMode = !edgeCreateMode;
    edgeCreateSourceId = "";
  }

  function startEdgeCreate(type) {
    if (edgeCreateSourceId && !edgeTypeAllowedForSource(type, edgeCreateSourceId)) {
      sceneFlowError = `Edge ${edgeTypeLabel(type)} not allowed for this node.`;
      return;
    }
    if (edgeCreateMode && edgeCreateType === type) {
      edgeCreateMode = false;
      edgeCreateSourceId = "";
      return;
    }
    edgeCreateType = type;
    edgeCreateMode = true;
    edgeCreateSourceId = "";
  }

  async function handleEdgePick(nodeId) {
    if (!edgeCreateMode || !nodeId) return;
    if (!edgeCreateSourceId) {
      if (!edgeTypeAllowedForSource(edgeCreateType, nodeId)) {
        sceneFlowError = `Edge ${edgeTypeLabel(edgeCreateType)} not allowed for this node.`;
        return;
      }
      edgeCreateSourceId = nodeId;
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
      return;
    }
    if (!edgeTypeAllowedForSource(edgeCreateType, edgeCreateSourceId)) {
      sceneFlowError = `Edge ${edgeTypeLabel(edgeCreateType)} not allowed for this node.`;
      edgeCreateSourceId = "";
      edgeCreateMode = false;
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
      return;
    }
    const sourceId = edgeCreateSourceId;
    pinnedNodeSelectionId = sourceId;
    pinnedNodeSelectionRevision = sceneFlow?.revision ?? null;
    const response = await createSceneFlowEdge(sourceId, nodeId);
    edgeCreateSourceId = "";
    edgeCreateMode = false;
    // Keep source node selected to preserve edge restriction context
    if (response) {
      sceneFlowSelection = { type: "node", id: sourceId };
      sceneFlowMultiSelection = [{ type: "node", id: sourceId }];
      pinnedNodeSelectionId = "";
      pinnedNodeSelectionRevision = null;
    } else {
      pinnedNodeSelectionId = "";
      pinnedNodeSelectionRevision = null;
      sceneFlowSelection = null;
      sceneFlowMultiSelection = [];
    }
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
      superNodeId: sceneFlow?.superNodeId,
      nodeId,
      x,
      y,
      snap: snap ?? sceneFlowNodeSnap
    };
    const response = await runSceneFlowCommand("SceneFlow.Node.Move", payload);
    if (!response?.snapshot && previous) {
      sceneFlow = previous;
    }
  }

  async function moveSceneFlowNodeGroup(nodes, snap) {
    if (!selectedProjectId || !Array.isArray(nodes) || nodes.length === 0) return;
    const previous = sceneFlow;
    if (sceneFlow?.nodes?.length) {
      const moveMap = new Map(nodes.map((entry) => [entry.id, entry]));
      const nextNodes = sceneFlow.nodes.map((node) => {
        const move = moveMap.get(node.id);
        if (!move) return node;
        return {
          ...node,
          graphics: { ...(node.graphics || {}), x: move.x, y: move.y }
        };
      });
      sceneFlow = { ...sceneFlow, nodes: nextNodes };
    }
    const payload = {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodes: nodes.map((entry) => ({ id: entry.id, x: entry.x, y: entry.y })),
      snap: snap ?? sceneFlowNodeSnap
    };
    const response = await runSceneFlowCommand("SceneFlow.Node.MoveGroup", payload);
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
    if (sceneFlowSelection?.type === "edge" && sceneFlowSelection.id === edgeId) {
      pinnedEdgeSelectionId = edgeId;
    }
    const isBend = handle === "bend";
    const isReset = handle === "reset";
    if (!isBend && !isReset && (!Number.isFinite(cx) || !Number.isFinite(cy))) return;
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
        if (isReset) {
          for (let i = 0; i < points.length; i += 1) {
            points[i] = { ...points[i], cx: points[i].x, cy: points[i].y };
          }
        } else if (isBend) {
          const dx = Number.isFinite(cx) ? cx : 0;
          const dy = Number.isFinite(cy) ? cy : 0;
          const lastIdx = points.length - 1;
          points[0] = {
            ...points[0],
            cx: (points[0].cx ?? points[0].x) + dx,
            cy: (points[0].cy ?? points[0].y) + dy
          };
          points[lastIdx] = {
            ...points[lastIdx],
            cx: (points[lastIdx].cx ?? points[lastIdx].x) + dx,
            cy: (points[lastIdx].cy ?? points[lastIdx].y) + dy
          };
        } else {
          const idx = handle === "ctrl1" ? 0 : points.length - 1;
          const target = points[idx] || {};
          points[idx] = {
            ...target,
            cx,
            cy
          };
        }
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
      superNodeId: sceneFlow?.superNodeId,
      edgeId,
      fields: { points: nextPoints }
    });
    if (!response?.snapshot && previous) {
      sceneFlow = previous;
    }
  }

  async function retargetSceneFlowEdge(edgeId, targetId, x, y) {
    if (!selectedProjectId || !edgeId || !targetId) return;
    await runSceneFlowCommand("SceneFlow.Edge.Retarget", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeId,
      targetId,
      dropX: x,
      dropY: y
    });
  }

  async function normalizeSelectedEdge() {
    if (!selectedProjectId || !selectedEdge) return;
    await runSceneFlowCommand("SceneFlow.Edge.Normalize", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeId: selectedEdge.id
    });
  }

  async function straightenSelectedEdge() {
    if (!selectedProjectId || !selectedEdge) return;
    await runSceneFlowCommand("SceneFlow.Edge.Straighten", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeId: selectedEdge.id
    });
  }

  async function straightenSelectedEdges() {
    if (!selectedProjectId || !selectionEdges.length) return;
    const edgeIds = Array.from(new Set(selectionEdges.map((edge) => edge.id).filter(Boolean)));
    if (edgeIds.length < 2) return;
    await runSceneFlowCommand("SceneFlow.Edge.StraightenGroup", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeIds
    });
  }

  async function normalizeSelectedEdges() {
    if (!selectedProjectId || !selectionEdges.length) return;
    const edgeIds = Array.from(new Set(selectionEdges.map((edge) => edge.id).filter(Boolean)));
    if (edgeIds.length < 2) return;
    await runSceneFlowCommand("SceneFlow.Edge.NormalizeGroup", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeIds
    });
  }

  async function toggleNodeStart() {
    if (!selectedProjectId || !selectedNode || !nodeDraft || selectedNode.isHistory) return;
    pinnedNodeSelectionId = selectedNode.id;
    pinnedNodeSelectionRevision = sceneFlow?.revision ?? null;
    console.log("[SELDBG] toggleNodeStart begin", {
      nodeId: selectedNode.id,
      revision: sceneFlow?.revision,
      superNodeId: sceneFlow?.superNodeId,
      selection: sceneFlowSelection,
      multi: sceneFlowMultiSelection
    });
    const previous = nodeDraft;
    const next = !previous.isStart;
    nodeDraft = { ...previous, isStart: next };
    nodeEditError = "";
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: selectedNode.id,
      fields: { isStart: next }
    });
    if (!response) {
      nodeDraft = previous;
      nodeEditError = sceneFlowError || "Failed to update start node.";
    }
    console.log("[SELDBG] toggleNodeStart end", {
      nodeId: selectedNode?.id,
      revision: sceneFlow?.revision,
      superNodeId: sceneFlow?.superNodeId,
      pinnedNodeSelectionId,
      pinnedNodeSelectionRevision,
      selection: sceneFlowSelection,
      multi: sceneFlowMultiSelection,
      response
    });
  }

  async function toggleSuperNodeStart() {
    if (!selectedProjectId || !nodeEditorTarget || !superNodeDraft || superNodeStartLocked) return;
    const previous = superNodeDraft;
    const next = !previous.isStart;
    superNodeDraft = { ...previous, isStart: next };
    superNodeEditError = "";
    const response = await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: nodeEditorTarget.id,
      fields: { isStart: next }
    });
    if (!response) {
      superNodeDraft = previous;
      superNodeEditError = sceneFlowError || "Failed to update start node.";
    }
  }

  async function toggleChildStart(node) {
    if (!selectedProjectId || !node || node.isHistory) return;
    await runSceneFlowCommand("SceneFlow.Node.Update", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId: node.id,
      fields: { isStart: !node.isStart }
    });
  }

  function pinSelectedNodeSelection() {
    if (sceneFlowSelection?.type === "node" && sceneFlowSelection.id) {
      pinnedNodeSelectionId = sceneFlowSelection.id;
      pinnedNodeSelectionRevision = sceneFlow?.revision ?? null;
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
      superNodeId: sceneFlow?.superNodeId,
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
      superNodeId: sceneFlow?.superNodeId,
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
      superNodeId: sceneFlow?.superNodeId,
      edgeId: selectedEdge.id,
      fields
    });
    if (response) {
      edgeDraftId = "";
    }
  }

  async function deleteSceneFlowSelection() {
    console.log("[DELETE] deleteSceneFlowSelection called");
    console.log("[DELETE] selectedProjectId:", selectedProjectId);
    console.log("[DELETE] sceneFlowBusy:", sceneFlowBusy);
    if (!selectedProjectId || sceneFlowBusy) {
      console.log("[DELETE] Early return: no project or busy");
      return;
    }
    const selectionList = sceneFlowSelectionList();
    console.log("[DELETE] selectionList:", JSON.stringify(selectionList));
    if (!selectionList.length) {
      console.log("[DELETE] Early return: empty selection");
      return;
    }
    const selection = sceneFlowSelection;
    console.log("[DELETE] current selection:", JSON.stringify(selection));
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    const nodeIds = selectionList.filter((item) => item.type === "node").map((item) => item.id);
    const commentIds = selectionList.filter((item) => item.type === "comment").map((item) => item.id);
    const edgeIds = selectionList.filter((item) => item.type === "edge").map((item) => item.id);
    console.log("[DELETE] nodeIds:", nodeIds, "commentIds:", commentIds, "edgeIds:", edgeIds);

    for (const nodeId of nodeIds) {
      console.log("[DELETE] Sending delete request for node:", nodeId);
      await runSceneFlowCommand("SceneFlow.Node.Delete", { projectId: selectedProjectId, superNodeId: sceneFlow?.superNodeId, nodeId });
    }
    for (const commentId of commentIds) {
      await runSceneFlowCommand("SceneFlow.Comment.Delete", { projectId: selectedProjectId, commentId });
    }
    if (!nodeIds.length) {
      for (const edgeId of edgeIds) {
        const edge = sceneFlow?.edges?.find((entry) => entry.id === edgeId);
        const payload = { projectId: selectedProjectId, superNodeId: sceneFlow?.superNodeId, edgeId };
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

  function handleDialogEscape() {
    if (cmdDialogOpen) {
      closeCmdDialog();
      return true;
    }
    if (typeDefDraft) {
      closeTypeDefDialog();
      return true;
    }
    if (varDefDraft) {
      closeVarDefDialog();
      return true;
    }
    if (monitorDialogOpen) {
      closeMonitorDialog();
      return true;
    }
    if (prefsDialogOpen) {
      closePrefsDialog();
      return true;
    }
    if (projectConfigDialogOpen) {
      closeProjectConfigDialog();
      return true;
    }
    if (missingAgentDialogOpen) {
      closeMissingAgentDialog();
      return true;
    }
    if (recentFailureOpen) {
      closeRecentFailureDialog();
      return true;
    }
    if (saveAsDialogOpen) {
      closeSaveAsDialog();
      return true;
    }
    if (loadConfirmOpen) {
      cancelLoadConfirm();
      return true;
    }
    return false;
  }

  function handleGlobalKeydown(event) {
    if (!event) return;
    const key = event.key;
    if (key === "Escape" && handleDialogEscape()) {
      event.preventDefault();
      return;
    }
    if (isEditableTarget(event.target)) return;
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
    if (key === "Delete" || key === "Backspace") {
      const selList = sceneFlowSelectionList();
      console.log("[KEYDOWN] Delete/Backspace pressed, selectionList:", JSON.stringify(selList));
      if (selList.length) {
        event.preventDefault();
        deleteSceneFlowSelection();
        return;
      }
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

<main class:editor-view={showEditor}>
  {#if !showEditor}
    <header class="hero">
      <div class="hero-brand">
        <img class="hero-logo" src="/images/vsm_logo.svg" alt="Visual SceneMaker" />
        <div>
          <h1>Visual SceneMaker Web</h1>
        <p>
          Version <span title={infoRevision}>{infoRevisionSlug}</span>&nbsp;•&nbsp;Build date {infoBuildDate}
        </p>
        </div>
      </div>
      <div class="hero-status">
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
      </div>
    </header>

    {#if showTokenSection}
      <section class="panel connect">
        <header class="panel-title">
            <h2>Server Connection</h2>
        </header>
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
          <button type="button" class="ghost" on:click={openConnectDialog}>
            {isRemoteConnection ? "Change Server" : "Connect to Remote"}
          </button>
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
          <div class="project-row">
            <button
              type="button"
              class:selected={project.projectId === selectedProjectId}
              on:click={() => openProjectFromLanding(project)}
            >
              <span>{project.name}</span>
              <span class="meta">
                {project.dirty ? "*" : ""} {project.runtimeState}
              </span>
            </button>
            {#if !showEditor && project.projectId === selectedProjectId}
              <button
                type="button"
                class="ghost danger project-row-close"
                aria-label="Close project"
                title="Close project"
                on:click|stopPropagation={requestReturnToLanding}
              >
                Close
              </button>
            {/if}
          </div>
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

      <form class="stack" on:submit|preventDefault={() => openProject(openPath)}>
        <label for="open-path">Open by path</label>
        <div class="row">
          <input
            id="open-path"
            placeholder="/abs/path/to/project"
            bind:this={openPathInput}
            bind:value={openPath}
            on:input={() => (openPathError = "")}
            on:dragover|preventDefault
            on:drop={handleProjectDrop}
          />
          <button type="submit" disabled={!openPath || !openPath.trim()}>Open</button>
          <button type="button" class="ghost" on:click={browseForProjectDir}>Browse…</button>
        </div>
        <input
          class="sr-only"
          type="file"
          webkitdirectory
          directory
          bind:this={openPathPickerInput}
          on:change={handleDirectoryInputChange}
          tabindex="-1"
          aria-hidden="true"
        />
        {#if openPathError}
          <p class="error">{openPathError}</p>
        {/if}
      </form>

      <form class="stack" on:submit|preventDefault={createProject}>
        <label for="new-project-name">New project name</label>
        <input
          id="new-project-name"
          placeholder="Project name"
          bind:this={newProjectNameInput}
          bind:value={newName}
          on:input={() => (createProjectError = "")}
        />
        <label for="new-project-base">Base dir (optional)</label>
        <input
          id="new-project-base"
          placeholder="Base dir (optional)"
          bind:value={newBaseDir}
          on:input={() => (createProjectError = "")}
        />
        <button type="submit" disabled={!newName || !newName.trim()}>Create</button>
        {#if createProjectError}
          <p class="error">{createProjectError}</p>
        {/if}
      </form>

    </section>
    <section class="panel">
        <header class="panel-title">
          <h2>Recent Projects</h2>
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
      <div class="sceneflow-controls-panel">
        <header class="panel-title">
          <h2>
            VSM Web Project <span class="project-name-accent">{selectedProject?.name || ""}{headerDirty ? " *" : ""}</span>
          </h2>
          <div class="panel-title-right">
            {#if autoSaving || autoSaveStatus}
              <span
                class={`autosave-status ${autoSaving ? "saving" : ""} ${autoSaveStatus.includes("failed") ? "error" : ""}`}
                aria-live="polite"
              >
                {autoSaveStatus}
              </span>
            {/if}
            {#if headerDirty}
              <span class="unsaved-indicator" aria-live="polite">Unsaved</span>
            {/if}
            {#if projectRequiresSaveAs}
              <button
                type="button"
                class="ghost panel-save"
                on:click={openSaveAsDialog}
                disabled={!selectedProject || projectSaving}
              >
                Save As
              </button>
            {:else}
              <button
                type="button"
                class="ghost panel-save autosave-toggle"
                class:active={autoSaveEnabled}
                on:click={toggleAutoSave}
                disabled={!selectedProject || projectSaving || autoSaving}
                title={autoSaveEnabled ? "Disable autosave" : "Enable autosave"}
              >
                Autosave
              </button>
              <button
                type="button"
                class="ghost panel-save"
                on:click={() => saveProject(selectedProjectId)}
                disabled={!selectedProject || projectSaving}
              >
                Save
              </button>
            {/if}
            <button
              type="button"
              class="ghost icon-only"
              on:click={() => loadSceneFlow(selectedProjectId)}
              disabled={!selectedProject || sceneFlowLoading}
              aria-label="Reload SceneFlow"
              title="Reload SceneFlow"
            >
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                <path stroke-linecap="round" stroke-linejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0 3.181 3.183a8.25 8.25 0 0 0 13.803-3.7M4.031 9.865a8.25 8.25 0 0 1 13.803-3.7l3.181 3.182m0-4.991v4.99" />
              </svg>
            </button>
            <button
              type="button"
              class="panel-close"
              on:click={exitEditorView}
              disabled={!selectedProject || projectSaving}
              aria-label="Close project"
              title="Close Project"
            >
              <svg viewBox="0 0 24 24" aria-hidden="true">
                <path stroke-linecap="round" stroke-linejoin="round" d="M6 18 18 6M6 6l12 12" />
              </svg>
            </button>
          </div>
        </header>
        <div class="sceneflow-toolbar">
        {#if selectedProject}
          <div class="main-toolbar-row">
            <div class="sceneflow-edit-cluster">
              <button
                type="button"
                class="ghost icon-button danger flat"
                on:click={deleteSceneFlowSelection}
                disabled={!sceneFlowSelection || sceneFlowBusy}
                aria-label="Delete"
                title="Delete"
              >
                <svg viewBox="0 0 24 24" class="icon" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    d="m14.74 9-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 0 1-2.244 2.077H8.084a2.25 2.25 0 0 1-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 0 0-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 0 1 3.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 0 0-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 0 0-7.5 0"
                  />
                </svg>
              </button>
              <button
                type="button"
                class="ghost icon-button flat"
                on:click={undoSceneFlow}
                disabled={!wsConnected || sceneFlowBusy || !(sceneFlow?.undoState?.canUndo ?? sceneFlowCanUndo)}
                aria-label="Undo"
                title="Undo"
              >
                <svg viewBox="0 0 24 24" class="icon" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M9 15 3 9m0 0 6-6M3 9h12a6 6 0 0 1 0 12h-3" />
                </svg>
              </button>
              <button
                type="button"
                class="ghost icon-button flat"
                on:click={redoSceneFlow}
                disabled={!wsConnected || sceneFlowBusy || !(sceneFlow?.undoState?.canRedo ?? sceneFlowCanRedo)}
                aria-label="Redo"
                title="Redo"
              >
                <svg viewBox="0 0 24 24" class="icon" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="m15 15 6-6m0 0-6-6m6 6H9a6 6 0 0 0 0 12h3" />
                </svg>
              </button>
              <button
                type="button"
                class="ghost icon-button flat"
                on:click={straightenAllEdges}
                disabled={!wsConnected || sceneFlowBusy}
                aria-label="Relayout edges"
                title="Relayout edges"
              >
                <svg viewBox="0 0 24 24" class="icon" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                </svg>
              </button>
              <button
                type="button"
                class="ghost icon-button flat"
                on:click={downloadSceneFlowSnapshot}
                disabled={!sceneFlowRef || !sceneFlow}
                aria-label="Download snapshot"
                title="Download snapshot"
              >
                <svg viewBox="0 0 24 24" class="icon" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M6.827 6.175A2.31 2.31 0 0 1 5.186 7.23c-.38.054-.757.112-1.134.175C2.999 7.58 2.25 8.507 2.25 9.574V18a2.25 2.25 0 0 0 2.25 2.25h15A2.25 2.25 0 0 0 21.75 18V9.574c0-1.067-.75-1.994-1.802-2.169a47.865 47.865 0 0 0-1.134-.175 2.31 2.31 0 0 1-1.64-1.055l-.822-1.316a2.192 2.192 0 0 0-1.736-1.039 48.774 48.774 0 0 0-5.232 0 2.192 2.192 0 0 0-1.736 1.039l-.821 1.316Z" />
                  <path stroke-linecap="round" stroke-linejoin="round" d="M16.5 12.75a4.5 4.5 0 1 1-9 0 4.5 4.5 0 0 1 9 0ZM18.75 10.5h.008v.008h-.008V10.5Z" />
                </svg>
              </button>
            </div>
            <div class="sceneflow-nav-cluster">
              <button
                type="button"
                class="sceneflow-gear flat"
                on:click={openProjectConfigDialog}
                disabled={!selectedProject || !wsConnected}
                aria-label="Open project modules"
                title="Project modules"
              >
                <IconPuzzle className="icon" />
              </button>
              <button
                type="button"
                class="sceneflow-gear flat"
                on:click={openPrefsDialog}
                disabled={!selectedProject || !wsConnected}
                aria-label="Open preferences"
                title="Preferences"
              >
                <IconGear className="icon" />
              </button>
              {#if sceneFlowBreadcrumbNodes.length}
                <nav class="sceneflow-breadcrumbs" aria-label="SceneFlow path">
                  {#each sceneFlowBreadcrumbNodes as node, idx}
                    {#if idx > 0}
                      <span class="crumb-sep">/</span>
                    {/if}
                    {#if idx < sceneFlowBreadcrumbNodes.length - 1}
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
                  <span class="muted">Path: {(sceneFlow?.path || []).join(" / ")}</span>
                </div>
              {/if}
              <button
                type="button"
                class="sceneflow-gear flat"
                on:click={openMonitorDialog}
                disabled={!selectedProject || !wsConnected}
                aria-label="Open runtime monitor"
                title="Runtime monitor"
              >
                <IconMonitor className="icon" />
              </button>
            </div>
            <div class="sceneflow-runtime-cluster">
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
        {/if}
        </div>
      </div>
      {#if !selectedProject}
        <p class="muted">Select a project to view the SceneFlow graph.</p>
      {:else if sceneFlow}
        <div class="sceneflow-layout" style={sceneFlowLayoutStyle}>
          {#if sceneFlowShowBlocks}
            <aside
              class="sceneflow-blocks"
              class:agents-collapsed={agentsCollapsed}
              class:scenes-collapsed={scenesCollapsed}
            >
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.EEDGE}
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.PEDGE}
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.FEDGE}
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.CEDGE}
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.TEDGE}
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
                  disabled={!sceneFlow || !wsConnected || sceneFlowBusy || edgeTypeDisabledMap.IEDGE}
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
            <div class="blocks-section blocks-section--agents" class:collapsed={agentsCollapsed}>
              <div class="block-section-header">
                <div class="block-section-title">Agents</div>
                <button
                  type="button"
                  class="ghost icon-button block-section-toggle"
                  aria-pressed={!agentsCollapsed}
                  aria-label={agentsCollapsed ? "Expand agents" : "Collapse agents"}
                  title={agentsCollapsed ? "Expand" : "Collapse"}
                  on:click={() => (agentsCollapsed = !agentsCollapsed)}
                >
                  {#if agentsCollapsed}
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.5v15m7.5-7.5h-15" />
                    </svg>
                  {:else}
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                    </svg>
                  {/if}
                </button>
              </div>
              {#if !agentsCollapsed}
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
              {/if}
            </div>
            <div class="blocks-section blocks-section--scenes" class:collapsed={scenesCollapsed}>
              <div class="block-section-header">
                <div class="block-section-title">Scenes</div>
                <button
                  type="button"
                  class="ghost icon-button block-section-toggle"
                  aria-pressed={!scenesCollapsed}
                  aria-label={scenesCollapsed ? "Expand scenes" : "Collapse scenes"}
                  title={scenesCollapsed ? "Expand" : "Collapse"}
                  on:click={() => (scenesCollapsed = !scenesCollapsed)}
                >
                  {#if scenesCollapsed}
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.5v15m7.5-7.5h-15" />
                    </svg>
                  {:else}
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                    </svg>
                  {/if}
                </button>
              </div>
              {#if !scenesCollapsed}
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
                                title={group?.params?.length ? `${group.name} (${group.params.join(", ")})` : group.name}
                                use:fitMiddleEllipsis={{ text: group?.params?.length ? `${group.name} (${group.params.join(", ")})` : group.name }}
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
              {/if}
            </div>
            <div class="blocks-filler" aria-hidden="true"></div>
            </aside>
          {/if}
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
                runtimeValues={runtimeValues}
                runtimeState={runtimeState}
                onNavigate={navigateSceneFlow}
                onNodeMove={moveSceneFlowNode}
                onNodeGroupMove={moveSceneFlowNodeGroup}
                onCommentUpdate={updateSceneFlowComment}
                onEdgeControlUpdate={updateSceneFlowEdgeControl}
                onEdgeRetarget={retargetSceneFlowEdge}
                onDeleteSelection={deleteSceneFlowSelection}
                onUndo={undoSceneFlow}
                onRedo={redoSceneFlow}
                nodeSnapToGrid={sceneFlowNodeSnap}
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
                class:active={sceneFlowNodeSnap}
                on:click={() => (sceneFlowNodeSnap = !sceneFlowNodeSnap)}
                aria-pressed={sceneFlowNodeSnap}
                disabled={!sceneFlow}
              >
                node snap
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowShowVars}
                on:click={toggleVarBadges}
                aria-pressed={sceneFlowShowVars}
                disabled={!sceneFlow}
              >
                vars
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowShowCmdText}
                on:click={() => (sceneFlowShowCmdText = !sceneFlowShowCmdText)}
                aria-pressed={sceneFlowShowCmdText}
              >
                cmds
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowShowBlocks}
                on:click={() => (sceneFlowShowBlocks = !sceneFlowShowBlocks)}
                aria-pressed={sceneFlowShowBlocks}
                disabled={!sceneFlow}
              >
                blocks
              </button>
              <button
                type="button"
                class="sceneflow-toggle"
                class:active={sceneFlowShowInspector}
                on:click={() => (sceneFlowShowInspector = !sceneFlowShowInspector)}
                aria-pressed={sceneFlowShowInspector}
                disabled={!sceneFlow}
              >
                inspector
              </button>
            </div>
            {#if sceneFlowShowVars}
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
                    {:else if displayGlobalVarList.length === 0}
                      <span class="muted">No variables.</span>
                    {:else}
                      {#each displayGlobalVarList as variable}
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
                      {:else if displayLocalVarList.length === 0}
                        <span class="muted">No local variables.</span>
                      {:else}
                        {#each displayLocalVarList as variable}
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
          {#if sceneFlowShowInspector}
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
              {#if selectionNodes.length}
                <div class="definition-section">
                  <header class="definition-header">
                    <h4>Arrange</h4>
                    <span class="muted">{selectionNodes.length} nodes</span>
                  </header>
                  <div class="stack">
                    <div class="row">
                      <span class="arrange-label">Align X</span>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("left")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Left
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("center")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Center
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("right")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Right
                      </button>
                    </div>
                    <div class="row">
                      <span class="arrange-label">Align Y</span>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("top")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Top
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("middle")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Middle
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => alignSelectedNodes("bottom")}
                        disabled={!selectionHasMovableNodes || !wsConnected || sceneFlowBusy}
                      >
                        Bottom
                      </button>
                    </div>
                    <div class="row">
                      <span class="arrange-label">Distribute</span>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => distributeSelectedNodes("x")}
                        disabled={!selectionCanDistribute || !wsConnected || sceneFlowBusy}
                      >
                        Horizontal
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => distributeSelectedNodes("y")}
                        disabled={!selectionCanDistribute || !wsConnected || sceneFlowBusy}
                      >
                        Vertical
                      </button>
                    </div>
                    <div class="row">
                      <span class="arrange-label">Start</span>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => setSelectedNodesStart(true)}
                        disabled={!selectionCanToggleStart || !wsConnected || sceneFlowBusy}
                      >
                        Set
                      </button>
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => setSelectedNodesStart(false)}
                        disabled={!selectionCanToggleStart || !wsConnected || sceneFlowBusy}
                      >
                        Clear
                      </button>
                    </div>
                  </div>
                </div>
              {/if}
              {#if selectionEdges.length > 1}
                <div class="definition-section">
                  <header class="definition-header">
                    <h4>Edges</h4>
                    <span class="muted">{selectionEdges.length} edges</span>
                  </header>
                  <div class="actions">
                    <button
                      type="button"
                      class="ghost"
                      on:click={normalizeSelectedEdges}
                      disabled={!wsConnected || sceneFlowBusy}
                    >
                      Normalize
                    </button>
                    <button
                      type="button"
                      class="ghost"
                      on:click={straightenSelectedEdges}
                      disabled={!wsConnected || sceneFlowBusy}
                    >
                      Relayout
                    </button>
                  </div>
                </div>
              {/if}
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
                <button
                  type="button"
                  class="ghost"
                  on:click={normalizeSelectedEdge}
                  disabled={!wsConnected || sceneFlowBusy}
                >
                  Normalize
                </button>
                <button
                  type="button"
                  class="ghost"
                  on:click={straightenSelectedEdge}
                  disabled={!wsConnected || sceneFlowBusy}
                >
                  Relayout
                </button>
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
              {:else}
                <h3 class="inspector-title">{currentSuperName}</h3>
              {/if}
              <div class="definition-section">
                <header class="definition-header">
                  <h4>Start nodes</h4>
                  <span class="muted">{startNodes.length}/{superNodeChildren.length}</span>
                </header>
                <div class="def-table start-node-table">
                  <div class="def-list">
                    {#if superNodeStartList.length === 0}
                      <div class="def-empty">No nodes yet.</div>
                    {:else}
                      {#each superNodeStartList as node}
                        <button
                          type="button"
                          class="def-row"
                          class:selected={startListSelectedId === node.id}
                          on:click={() => (startListSelectedId = node.id)}
                          aria-pressed={startListSelectedId === node.id}
                        >
                          <span class="def-line">
                            <span class={node.isStart ? "start-node" : "start-node-inactive"}>
                              {displayNodeName(node)}
                            </span>
                            <span class="muted">({node.type === "Super" ? "Super" : "Basic"})</span>
                          </span>
                        </button>
                      {/each}
                    {/if}
                  </div>
                  <div class="def-actions">
                    <button
                      type="button"
                      class="icon-button start-toggle"
                      class:active={startListSelectedNode?.isStart}
                      on:click={() => startListSelectedNode && toggleChildStart(startListSelectedNode)}
                      aria-pressed={!!startListSelectedNode?.isStart}
                      aria-label="Toggle start node"
                      title={startListSelectedNode?.isStart ? "Unset start node" : "Set start node"}
                      disabled={!startListSelectedNode || !wsConnected || sceneFlowBusy}
                    >
                      <IconStart className="icon" />
                    </button>
                  </div>
                </div>
              </div>
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
                          on:dblclick={() => startVarDefEdit(index)}
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
          {/if}
        </div>
      {:else}
        <p class="muted">No SceneFlow data loaded yet.</p>
      {/if}
      {#if selectedProject}
        <div class="scenescript">
          <div class="script-toolbar">
            <button type="button" class="ghost" on:click={() => loadScript(selectedProjectId)} disabled={!selectedProject}>
              Reload
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
          </div>
          <div class="script-editor" class:has-error={!scriptParseOk}>
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
          </div>
          {#if scriptStatus}
            <p class="status">{scriptStatus}</p>
          {/if}
          {#if scriptError}
            <p class="error">{scriptError}</p>
          {/if}
        </div>
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

    {/if}
  </div>

  <!-- Phase 8.4: Connect to Remote Server Dialog -->
  {#if showConnectDialog}
    <div
      class="modal-backdrop"
      on:click|self={closeConnectDialog}
      role="presentation"
    >
      <div class="modal" role="dialog" aria-modal="true" aria-labelledby="connect-dialog-title" tabindex="-1">
        <h3 id="connect-dialog-title">Connect to Runtime Server</h3>
        <div class="modal-body">
          <p class="connect-dialog-hint">Enter the address of a remote runtime server to connect for execution and monitoring.</p>
          <label class="connect-input-label">
            Server URL
            <input
              type="text"
              bind:value={remoteServerInput}
              placeholder="localhost:8091 or 192.168.1.10:8091"
              disabled={remoteConnecting}
              on:keydown={(e) => e.key === "Enter" && connectToRemoteServer(remoteServerInput)}
            />
          </label>
          {#if remoteConnectionError}
            <p class="connect-error">{remoteConnectionError}</p>
          {/if}
          {#if isRemoteConnection}
            <p class="connect-current">Currently connected to: <strong>{connectedServerName}</strong></p>
          {/if}
        </div>
        <div class="row">
          <button type="button" class="ghost" on:click={closeConnectDialog} disabled={remoteConnecting}>Cancel</button>
          {#if isRemoteConnection}
            <button type="button" class="ghost" on:click={disconnectFromRemote} disabled={remoteConnecting}>Disconnect</button>
          {/if}
          <button
            type="button"
            class="primary"
            on:click={() => connectToRemoteServer(remoteServerInput)}
            disabled={remoteConnecting || !remoteServerInput.trim()}
          >
            {remoteConnecting ? "Connecting..." : "Connect"}
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if loadConfirmOpen}
    <div
      class="modal-backdrop"
      on:click|self={cancelLoadConfirm}
      role="presentation"
    >
      <div class="modal" bind:this={loadConfirmDialogEl} role="dialog" aria-modal="true" aria-labelledby="load-confirm-title" tabindex="-1">
        <h3 id="load-confirm-title">Close Project?</h3>
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
          <button
            type="button"
            class="primary"
            on:click={confirmSaveAndClose}
            disabled={!selectedProject || projectSaving || projectRequiresSaveAs}
          >
            Save and Close
          </button>
          <button type="button" class="danger" on:click={confirmReturnToLanding}>Close</button>
        </div>
      </div>
    </div>
  {/if}

  {#if saveAsDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeSaveAsDialog}
      role="presentation"
    >
      <div class="modal" bind:this={saveAsDialogEl} role="dialog" aria-modal="true" aria-labelledby="save-as-title" tabindex="-1">
        <h3 id="save-as-title">Save project as</h3>
        <form class="modal-body" on:submit|preventDefault={confirmSaveAs}>
          <label for="save-as-path">Save to path</label>
          <input
            id="save-as-path"
            placeholder="/abs/path/to/project"
            bind:this={saveAsInputEl}
            bind:value={saveAsPath}
          />
          <p class="muted">Choose a new folder for this project.</p>
          {#if saveAsError}
            <p class="error">{saveAsError}</p>
          {/if}
          <div class="row">
            <button type="button" class="ghost" on:click={closeSaveAsDialog}>Cancel</button>
            <button
              type="submit"
              class="primary"
              disabled={!saveAsPath || !saveAsPath.trim()}
            >
              Save As
            </button>
          </div>
        </form>
      </div>
    </div>
  {/if}

  {#if recentFailureOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeRecentFailureDialog}
      role="presentation"
    >
      <div class="modal" bind:this={recentFailureDialogEl} role="dialog" aria-modal="true" aria-labelledby="recent-failure-title" tabindex="-1">
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
    </div>
  {/if}

  {#if missingAgentDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeMissingAgentDialog}
      role="presentation"
    >
      <div class="modal missing-agent-modal" bind:this={missingAgentDialogEl} role="dialog" aria-modal="true" aria-labelledby="missing-agent-title" tabindex="-1">
        <h3 id="missing-agent-title">Missing Agent Device Configuration</h3>
        <div class="modal-body">
          <p>
            The scene script references agents that are not configured in this project. Map each one to a device or run
            anyway and abort with Stop if needed. If the device is not present, cancel and add the needed device.
          </p>
          <div class="missing-agent-table">
            <div class="missing-agent-header">
              <span>Agent</span>
              <span>Device</span>
            </div>
            {#each missingAgentDrafts as draft, index}
              <div class="missing-agent-row">
                <div class="missing-agent-name">{draft.name}</div>
                <select
                  value={draft.device}
                  on:change={(event) => updateMissingAgentDraft(index, "device", event.target.value)}
                  disabled={missingAgentDeviceOptions.length === 0}
                >
                  <option value="">Select device</option>
                  {#each missingAgentDeviceOptions as option}
                    <option value={option.value}>{option.label}</option>
                  {/each}
                </select>
              </div>
            {/each}
          </div>
          {#if availableDevicesLoading}
            <p class="muted">Loading devices...</p>
          {/if}
          {#if availableDevicesError}
            <p class="error">{availableDevicesError}</p>
          {/if}
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={closeMissingAgentDialog} disabled={missingAgentBusy}>
            Cancel
          </button>
          <button
            type="button"
            class="ghost"
            on:click={async () => {
              closeMissingAgentDialog();
              await runRuntimeCommand("Runtime.Play", { skipMissingAgentCheck: true });
            }}
            disabled={missingAgentBusy}
          >
            Run anyway
          </button>
          <button
            type="button"
            class="primary"
            on:click={applyMissingAgentsAndRun}
            disabled={!wsConnected || missingAgentBusy || missingAgentDrafts.length === 0}
          >
            Configure & Run
          </button>
        </div>
        {#if missingAgentError}
          <p class="error">{missingAgentError}</p>
        {/if}
      </div>
    </div>
  {/if}

  {#if projectConfigDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeProjectConfigDialog}
      role="presentation"
    >
      <div class="modal project-config-modal" bind:this={projectConfigDialogEl} role="dialog" aria-modal="true" aria-labelledby="project-config-title" tabindex="-1">
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
            {#if !projectConfigError && !projectConfigDirty}
              <span class="muted">No pending changes</span>
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
              disabled={!selectedProject || !wsConnected || !projectConfigDraft || !projectConfigDirty}
            >
              Apply
            </button>
            <button type="button" class="ghost" on:click={closeProjectConfigDialog}>Close</button>
          </div>
        </div>
      </div>
    </div>
  {/if}

  {#if prefsDialogOpen && prefsDialogDraft}
    <div
      class="modal-backdrop"
      on:click|self={closePrefsDialog}
      role="presentation"
    >
      <div class="modal prefs-modal" bind:this={prefsDialogEl} role="dialog" aria-modal="true" aria-labelledby="prefs-dialog-title" tabindex="-1">
        <div class="prefs-header">
          <div class="prefs-title">
            <span class="prefs-title-icon">
              <IconGear className="icon" />
            </span>
            <div>
              <h3 id="prefs-dialog-title">Preferences</h3>
              <span class="prefs-subtitle">
                Preferences of VSM Project {selectedProject ? selectedProject.name : "the active project"}
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
          <section class="prefs-card">
            <header class="prefs-card-header">
              <h4>Sceneflow syntax</h4>
              <span class="muted">XML namespace and schema references.</span>
            </header>
            <div class="prefs-group">
              <div class="prefs-rows">
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-xmlns">Namespace</label>
                    <span class="prefs-help">Used as the sceneflow XML namespace.</span>
                  </div>
                  <div class="prefs-control">
                    <input id="pref-xmlns" class="prefs-input" bind:value={prefsDialogDraft.sceneflowNamespace} />
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-xmlns-xsi">Instance</label>
                    <span class="prefs-help">XML schema instance namespace.</span>
                  </div>
                  <div class="prefs-control">
                    <input id="pref-xmlns-xsi" class="prefs-input" bind:value={prefsDialogDraft.sceneflowInstance} />
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-xsd-location">XSD location</label>
                    <span class="prefs-help">Schema location used when exporting sceneflow.</span>
                  </div>
                  <div class="prefs-control">
                    <input id="pref-xsd-location" class="prefs-input" bind:value={prefsDialogDraft.sceneflowSchema} />
                  </div>
                </div>
              </div>
            </div>
          </section>
          <section class="prefs-card">
            <header class="prefs-card-header">
              <h4>Undo/Redo Management</h4>
              <span class="muted">History depth for this project.</span>
            </header>
            <div class="prefs-group">
              <div class="prefs-rows">
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-autosave">Autosave</label>
                    <span class="prefs-help">Automatically save project changes.</span>
                  </div>
                  <div class="prefs-control">
                    <label class="toggle">
                      <input id="pref-autosave" type="checkbox" bind:checked={prefsDialogDraft.autoSaveEnabled} />
                      {prefsDialogDraft.autoSaveEnabled ? "On" : "Off"}
                    </label>
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-undo-depth">Undo history depth</label>
                    <span class="prefs-help">Maximum undo steps kept on disk.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-undo-depth"
                        type="number"
                        min={PREF_UNDO_MIN}
                        max={PREF_UNDO_MAX}
                        step="10"
                        bind:value={prefsDialogDraft.undoMaxDepth}
                      />
                      <span>steps</span>
                    </div>
                  </div>
                </div>
                <div class="prefs-row">
                  <div class="prefs-field">
                    <label for="pref-command-log-max">Command log max</label>
                    <span class="prefs-help">Maximum command entries stored on disk.</span>
                  </div>
                  <div class="prefs-control">
                    <div class="prefs-number">
                      <input
                        id="pref-command-log-max"
                        type="number"
                        min={PREF_COMMAND_LOG_MIN}
                        max={PREF_COMMAND_LOG_MAX}
                        step="100"
                        bind:value={prefsDialogDraft.commandLogMax}
                      />
                      <span>entries</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
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
    </div>
  {/if}

  {#if monitorDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeMonitorDialog}
      role="presentation"
    >
      <div class="modal monitor-modal" bind:this={monitorDialogEl} role="dialog" aria-modal="true" aria-labelledby="monitor-dialog-title" tabindex="-1">
        <div class="monitor-header">
          <div>
            <h3 id="monitor-dialog-title">Runtime Monitor</h3>
            <span class="muted">State: {runtimeStateLabel}</span>
          </div>
          <button type="button" class="ghost" on:click={closeMonitorDialog}>Close</button>
        </div>
        <div class="monitor-body">
          <div class="monitor-lists">
            <section class="monitor-section">
              <header>
                <h4>Global variables</h4>
              </header>
              {#if monitorGlobals.length === 0}
                <p class="muted">No global variables.</p>
              {:else}
                <div class="monitor-table" role="list">
                  {#each monitorGlobals as variable}
                    {@const key = monitorVarKey("global", variable.name)}
                    {@const details = monitorVarValue(variable)}
                    <button
                      type="button"
                      class="monitor-row"
                      class:selected={monitorSelectedKey === key}
                      on:click={() => selectMonitorVar("global", variable)}
                    >
                      <span class="monitor-name">{variable.name}</span>
                      <span class="monitor-value">
                        {details.displayValue}
                        {#if details.showInitial}
                          <span class="monitor-initial">({details.initial})</span>
                        {/if}
                      </span>
                    </button>
                  {/each}
                </div>
              {/if}
            </section>
            <section class="monitor-section">
              <header>
                <h4>Local variables</h4>
              </header>
              {#if monitorLocals.length === 0}
                <p class="muted">No local variables.</p>
              {:else}
                <div class="monitor-table" role="list">
                  {#each monitorLocals as variable}
                    {@const key = monitorVarKey("local", variable.name)}
                    {@const details = monitorVarValue(variable)}
                    <button
                      type="button"
                      class="monitor-row"
                      class:selected={monitorSelectedKey === key}
                      on:click={() => selectMonitorVar("local", variable)}
                    >
                      <span class="monitor-name">{variable.name}</span>
                      <span class="monitor-value">
                        {details.displayValue}
                        {#if details.showInitial}
                          <span class="monitor-initial">({details.initial})</span>
                        {/if}
                      </span>
                    </button>
                  {/each}
                </div>
              {/if}
            </section>
          </div>
          <section class="monitor-editor">
            <div class="monitor-editor-header">
              <h4>Update variable</h4>
              <span class="muted">
                {monitorSelectedVar
                  ? `${monitorSelectedVar.name} (${monitorSelectedKey.startsWith("local") ? "local" : "global"})`
                  : "Select a variable"}
              </span>
            </div>
            <label for="monitor-value-input">Value expression</label>
            <input
              id="monitor-value-input"
              value={monitorValueDraft}
              on:input={(event) => (monitorValueDraft = event.target.value)}
              on:keydown={(event) => {
                if (event.key === "Enter") {
                  applyMonitorValue();
                }
              }}
              placeholder='e.g. 42, "text", true'
              disabled={!monitorCanEdit || !monitorSelectedVar}
            />
            <button
              type="button"
              class="primary"
              on:click={applyMonitorValue}
              disabled={!monitorCanEdit || !monitorSelectedVar}
            >
              Apply value
            </button>
            <div class="monitor-query">
              <label for="monitor-query-input">Run Prolog query</label>
              <input
                id="monitor-query-input"
                value={monitorQueryDraft}
                on:input={(event) => (monitorQueryDraft = event.target.value)}
                on:keydown={(event) => {
                  if (event.key === "Enter") {
                    runMonitorQuery();
                  }
                }}
                placeholder="query(...)"
                disabled={!monitorCanEdit}
              />
              <button type="button" class="ghost" on:click={runMonitorQuery} disabled={!monitorCanEdit}>
                Run query
              </button>
            </div>
          </section>
        </div>
        {#if monitorError || monitorStatus}
          <div class="monitor-footer">
            {#if monitorError}
              <span class="error">{monitorError}</span>
            {:else}
              <span class="muted">{monitorStatus}</span>
            {/if}
          </div>
        {/if}
      </div>
    </div>
  {/if}

  {#if typeDefDraft}
    <div
      class="modal-backdrop"
      on:click|self={closeTypeDefDialog}
      role="presentation"
    >
      <div class="modal" bind:this={typeDefDialogEl} role="dialog" aria-modal="true" aria-labelledby="type-def-dialog-title" tabindex="-1">
        <h3 id="type-def-dialog-title">{typeDefEditIndex >= 0 ? "Edit type definition" : "Add type definition"}</h3>
        <div class="modal-body">
          <label for="type-def-name">Name</label>
          <input id="type-def-name" bind:this={typeDefNameInputEl} bind:value={typeDefDraft.name} />
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
          <button type="button" class="ghost" on:click={closeTypeDefDialog}>Cancel</button>
        </div>
        {#if typeDefError}
          <p class="error">{typeDefError}</p>
        {/if}
      </div>
    </div>
  {/if}

  {#if cmdDialogOpen}
    <div class="modal-backdrop cmd-modal-backdrop">
      <div class="modal cmd-modal" bind:this={cmdDialogEl} role="dialog" aria-modal="true" aria-labelledby="cmd-dialog-title" tabindex="-1">
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
                      data-cmd-index={index}
                      on:focus={(event) => (cmdInlineInputEls[index] = event.currentTarget)}
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
                class="ghost icon-button"
                on:click={openCmdHelper}
                disabled={!wsConnected || sceneFlowBusy}
                aria-label="Open command helper"
                title="Command helper"
              >
                <IconPuzzle className="icon" />
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
          {#if cmdHelperOpen}
            <div class="cmd-helper">
              <div class="cmd-helper-header">
                <h4>Command helper</h4>
                <button type="button" class="ghost" on:click={closeCmdHelper}>Close</button>
              </div>
              <label for="cmd-helper-type">Command</label>
              <select id="cmd-helper-type" bind:value={cmdHelperType} on:change={updateCmdHelperType}>
                <option value="PlayScene">PlayScene</option>
                <option value="PlayAction">PlayAction</option>
                <option value="Assign">Assign variable</option>
                <option value="Inc">Increase variable</option>
                <option value="Dec">Decrease variable</option>
              </select>
              {#if cmdHelperType === "PlayScene"}
                <label for="cmd-helper-scene">Scene</label>
                <select id="cmd-helper-scene" bind:value={cmdHelperScene}>
                  {#each helperScenes as sceneName}
                    <option value={sceneName}>{sceneName}</option>
                  {/each}
                </select>
                {#if helperScenes.length === 0}
                  <p class="muted">No scenes loaded.</p>
                {/if}
                {#if (helperSceneIndex.get(cmdHelperScene) || []).length}
                  <div class="cmd-helper-args">
                    <div class="cmd-helper-args-header">
                      <span>Scene variables</span>
                    </div>
                    {#each helperSceneIndex.get(cmdHelperScene) || [] as param}
                      <div class="cmd-helper-arg-row">
                        <span>{param}</span>
                        <select
                          value={cmdHelperSceneBindings?.[param] || ""}
                          on:change={(event) => {
                            cmdHelperSceneBindings = {
                              ...cmdHelperSceneBindings,
                              [param]: event.target.value
                            };
                          }}
                        >
                          <option value="">Select variable</option>
                          {#each helperVarCandidates as variable}
                            <option value={variable.name}>{variable.name}</option>
                          {/each}
                        </select>
                      </div>
                    {/each}
                  </div>
                {/if}
              {:else if cmdHelperType === "PlayAction"}
                <label for="cmd-helper-agent">Agent</label>
                <input id="cmd-helper-agent" bind:value={cmdHelperAgent} placeholder="Agent name" />
                <label for="cmd-helper-action">Action</label>
                <input id="cmd-helper-action" bind:value={cmdHelperAction} list="cmd-helper-action-list" placeholder="Action name" />
                <datalist id="cmd-helper-action-list">
                  {#each scriptElements.acticon as action}
                    <option value={action?.name || action?.script}>{action?.name || action?.script}</option>
                  {/each}
                </datalist>
                {#if !scriptElements.acticon.length}
                  <p class="muted">No actions loaded.</p>
                {/if}
                <div class="cmd-helper-args">
                  <div class="cmd-helper-args-header">
                    <span>Arguments</span>
                    <button type="button" class="ghost icon-button" on:click={addCmdHelperArg} aria-label="Add argument">
                      <IconPlus className="icon" />
                    </button>
                  </div>
                  {#if cmdHelperArgs.length === 0}
                    <p class="muted">No arguments.</p>
                  {:else}
                    {#each cmdHelperArgs as arg, argIndex}
                      <div class="cmd-helper-arg-row">
                        <input
                          placeholder="key"
                          value={arg.key}
                          on:input={(event) => updateCmdHelperArg(argIndex, "key", event.target.value)}
                        />
                        <input
                          placeholder="value"
                          value={arg.value}
                          on:input={(event) => updateCmdHelperArg(argIndex, "value", event.target.value)}
                        />
                        <button
                          type="button"
                          class="ghost icon-button danger"
                          on:click={() => removeCmdHelperArg(argIndex)}
                          aria-label="Remove argument"
                        >
                          <IconTrash className="icon" />
                        </button>
                      </div>
                    {/each}
                  {/if}
                </div>
              {:else}
                <label for="cmd-helper-var">Variable</label>
                <input
                  id="cmd-helper-var"
                  bind:value={cmdHelperVarName}
                  list="cmd-helper-var-list"
                  placeholder="Variable name"
                  class:input-warning={!cmdHelperVarExists && cmdHelperVarName.trim().length}
                />
                {#if !cmdHelperVarExists && cmdHelperVarName.trim().length}
                  <p class="muted">Variable not found. It will be created if you insert.</p>
                {/if}
                <datalist id="cmd-helper-var-list">
                  {#each helperVarCandidates as variable}
                    <option value={variable.name}>{variable.type ? `${variable.name} (${variable.type})` : variable.name}</option>
                  {/each}
                </datalist>
                {#if !cmdHelperVarExists}
                  <label for="cmd-helper-var-scope">Create in</label>
                  <select id="cmd-helper-var-scope" bind:value={cmdHelperVarScope}>
                    <option value="global">Global (top-level)</option>
                    <option value="parent">Parent supernode</option>
                    <option value="local">Local node</option>
                  </select>
                  <label for="cmd-helper-var-type">Variable type (if new)</label>
                  <select id="cmd-helper-var-type" bind:value={cmdHelperVarType}>
                    {#each nodeEditorTypeOptions as option}
                      <option value={option}>{option}</option>
                    {/each}
                  </select>
                {/if}
                {#if cmdHelperType === "Assign"}
                  <label for="cmd-helper-expr">Expression</label>
                  <input
                    id="cmd-helper-expr"
                    bind:value={cmdHelperVarExpr}
                    placeholder={varExpressionHint(cmdHelperVarType)}
                  />
                {:else}
                  <label for="cmd-helper-step">Step</label>
                  <input id="cmd-helper-step" bind:value={cmdHelperVarStep} placeholder="1" />
                {/if}
              {/if}
              <div class="actions cmd-helper-actions">
                <button type="button" class="primary" on:click={applyCmdHelperInsert}>Insert</button>
                <button type="button" class="ghost" on:click={closeCmdHelper}>Cancel</button>
              </div>
            </div>
          {/if}
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
    <div
      class="modal-backdrop"
      on:click|self={closeVarDefDialog}
      role="presentation"
    >
      <div class="modal" bind:this={varDefDialogEl} role="dialog" aria-modal="true" aria-labelledby="var-def-dialog-title" tabindex="-1">
        <h3 id="var-def-dialog-title">{varDefEditIndex >= 0 ? "Edit variable definition" : "Add variable definition"}</h3>
        <div class="modal-body">
          <label for="var-def-name">Name</label>
          <input
            id="var-def-name"
            bind:this={varDefNameInputEl}
            bind:value={varDefDraft.name}
            on:keydown={handleVarDefKeydown}
          />
          <label for="var-def-type">Type</label>
          <select id="var-def-type" bind:value={varDefDraft.type} on:change={updateVarDefType}>
            {#each nodeEditorTypeOptions as option}
              <option value={option}>{option}</option>
            {/each}
          </select>
          <label for="var-def-exp">Expression</label>
          <input
            id="var-def-exp"
            bind:value={varDefDraft.expression}
            placeholder={varExpressionHint(varDefDraft.type)}
            on:keydown={handleVarDefKeydown}
          />
        </div>
        <div class="actions">
          <button type="button" class="primary" on:click={applyVarDefEdit} disabled={!wsConnected || sceneFlowBusy}>
            {varDefEditIndex >= 0 ? "Save" : "Add"}
          </button>
          <button type="button" class="ghost" on:click={closeVarDefDialog}>Cancel</button>
        </div>
        {#if varDefError}
          <p class="error">{varDefError}</p>
        {/if}
      </div>
    </div>
  {/if}
</main>
