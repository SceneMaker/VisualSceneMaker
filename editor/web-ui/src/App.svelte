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
  import IconDocument from "./icons/IconDocument.svelte";
  import IconSearch from "./icons/IconSearch.svelte";
  import IconPuzzle from "./icons/IconPuzzle.svelte";
  import IconBlocks from "./icons/IconBlocks.svelte";
  import PluginDashboard from "./PluginDashboard.svelte";
  import IconPause from "./icons/IconPause.svelte";
  import IconStart from "./icons/IconStart.svelte";
  import IconStop from "./icons/IconStop.svelte";
  import IconTrash from "./icons/IconTrash.svelte";
  import IconMonitor from "./icons/IconMonitor.svelte";
  import VarBadge from './VarBadge.svelte';

  // sessionStorage is tab-specific and survives page reloads (including force-reload)
  // but NOT tab close+reopen. This ensures each browser tab has a distinct identity,
  // so sharing a link in the same browser correctly creates a guest (not a second owner).
  const clientId = (() => {
    const existing = sessionStorage.getItem("vsm_client_id");
    if (existing) return existing;
    const generated =
      (window.crypto && window.crypto.randomUUID && window.crypto.randomUUID()) ||
      `client-${Date.now()}`;
    sessionStorage.setItem("vsm_client_id", generated);
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
  const SCENEFLOW_MINIMAP_KEY = "vsm_scene_flow_minimap";
  const NEW_PROJECT_BASE_DIR_KEY = "vsm_new_project_base_dir";
  const NEW_PROJECT_BASE_DIR_PANEL_KEY = "vsm_new_project_base_suggestions_open";
  const RECENT_PINNED_PATHS_KEY = "vsm_recent_pinned_paths";
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
    global: { x: 16, y: 12, w: 240, h: 150, expanded: true },
    local: { x: 16, y: 190, w: 240, h: 150, expanded: true }
  };
  const DEFAULT_SCENEFLOW_TOGGLES = {
    nodeSnap: true,
    showInfo: true,
    showVars: true,
    showBlocks: true,
    showInspector: true
  };
  const AUTOSAVE_DELAY_MS = 5000;
  const VAR_BADGE_COOKIE = "vsm_var_badges";
  const VAR_BADGE_MIN_WIDTH = 180;
  const VAR_BADGE_MIN_HEIGHT = 90;
  const ACTIVITY_NODE_MIN_HIGHLIGHT_MS = 200;
  const EVENT_OVERPROD_WINDOW_MS = 2000;
  const EVENT_OVERPROD_TOTAL_THRESHOLD = 2500;
  const EVENT_OVERPROD_FLOW_THRESHOLD = 1200;
  const EVENT_OVERPROD_REQUIRED_WINDOWS = 2;
  const RUNTIME_VIZ_RATE_DEFAULT = 1500;
  const RUNTIME_VIZ_RATE_MIN = 100;
  const RUNTIME_VIZ_RATE_MAX = 20000;
  const RUNTIME_VIZ_BURST_DEFAULT = 3000;
  const RUNTIME_VIZ_BURST_MIN = 200;
  const RUNTIME_VIZ_BURST_MAX = 40000;
  const PLUGIN_BADGE_STORAGE_KEY_PREFIX = 'vsm_plugin_badges_';
  const PLUGIN_BADGE_DEFAULT_X = 270;
  const PLUGIN_BADGE_DEFAULT_Y = 12;
  const PLUGIN_BADGE_Y_STEP = 40;
  const PLUGIN_BADGE_DEFAULT_W = 200;
  const PLUGIN_BADGE_MIN_W = 120;
  const PLUGIN_BADGE_DEFAULT_H = 120;
  const PLUGIN_BADGE_MIN_H = 60;
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
      showInfo: state?.showInfo !== undefined ? !!state.showInfo : DEFAULT_SCENEFLOW_TOGGLES.showInfo,
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
    const expanded = rect?.expanded !== undefined ? !!rect.expanded : (fallback.expanded ?? true);
    return {
      x,
      y,
      w: Math.max(VAR_BADGE_MIN_WIDTH, w),
      h: Math.max(VAR_BADGE_MIN_HEIGHT, h),
      expanded
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

  function loadPluginBadgeState(projectId) {
    if (!projectId) return {};
    try { return JSON.parse(localStorage.getItem(PLUGIN_BADGE_STORAGE_KEY_PREFIX + projectId) || '{}'); }
    catch { return {}; }
  }

  async function fetchPluginBadgeStateFromServer(projectId) {
    if (!projectId) return null;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/ui-prefs`);
      const badges = data?.pluginBadges;
      if (badges && typeof badges === 'object') return badges;
      return null;
    } catch {
      return null;
    }
  }

  function persistPluginBadgeState(projectId, state) {
    if (!projectId) return;
    localStorage.setItem(PLUGIN_BADGE_STORAGE_KEY_PREFIX + projectId, JSON.stringify(state));
    apiPut(`/api/v1/projects/${projectId}/ui-prefs`, { uiPrefs: { pluginBadges: state } }).catch(() => {});
  }

  function getPluginBadgePos(className, index) {
    if (pluginBadgeState[className]) return pluginBadgeState[className];
    return { x: PLUGIN_BADGE_DEFAULT_X, y: PLUGIN_BADGE_DEFAULT_Y + index * PLUGIN_BADGE_Y_STEP, w: PLUGIN_BADGE_DEFAULT_W, h: PLUGIN_BADGE_DEFAULT_H, expanded: true };
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

  function toggleVarBadge(key) {
    const cur = varBadgeState[key];
    if (!cur) return;
    const next = { ...varBadgeState, [key]: { ...cur, expanded: !cur.expanded } };
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

  function startPluginBadgeDrag(event, className) {
    if (!isPrimaryPointer(event)) return;
    event.preventDefault();
    event.stopPropagation();
    pluginBadgeDrag = { className, lastClientX: event.clientX, lastClientY: event.clientY };
  }

  function handlePluginBadgePointerMove(event) {
    if (!pluginBadgeDrag) return;
    event.preventDefault();
    const { className } = pluginBadgeDrag;
    const cur = pluginBadgeState[className] || getPluginBadgePos(className, 0);
    const dx = event.clientX - pluginBadgeDrag.lastClientX;
    const dy = event.clientY - pluginBadgeDrag.lastClientY;
    pluginBadgeDrag.lastClientX = event.clientX;
    pluginBadgeDrag.lastClientY = event.clientY;
    pluginBadgeState = { ...pluginBadgeState, [className]: { ...cur, x: cur.x + dx, y: cur.y + dy } };
  }

  function handlePluginBadgePointerUp() {
    if (!pluginBadgeDrag) return;
    pluginBadgeDrag = null;
    persistPluginBadgeState(selectedProjectId, pluginBadgeState);
  }

  function togglePluginBadge(className) {
    const cur = pluginBadgeState[className] || getPluginBadgePos(className, 0);
    pluginBadgeState = { ...pluginBadgeState, [className]: { ...cur, expanded: !cur.expanded } };
    persistPluginBadgeState(selectedProjectId, pluginBadgeState);
  }

  let shareCopied = false;
  let shareNoLan = false;
  async function shareSession() {
    if (!selectedProjectId) return;
    // When on localhost and LAN access is enabled, replace host with the real
    // LAN address so the link is usable from other machines.
    // If LAN access is disabled (server bound to 127.0.0.1 only), keep localhost
    // and show a one-time hint about --allow-lan.
    let base = window.location.href;
    const lanEnabled = info?.allowExternal === true;
    if (isLocalHost() && lanEnabled && info?.lanAddress) {
      const current = new URL(window.location.href);
      current.hostname = info.lanAddress;
      base = current.toString();
    }
    const url = new URL(base);
    url.searchParams.set("session", selectedProjectId);
    if (isLocalHost() && !lanEnabled) {
      shareNoLan = true;
      setTimeout(() => { shareNoLan = false; }, 6000);
    }
    try {
      await navigator.clipboard.writeText(url.toString());
      shareCopied = true;
      setTimeout(() => { shareCopied = false; }, 2000);
    } catch (_) {
      // Fallback: select a temporary input
      const inp = document.createElement("input");
      inp.value = url.toString();
      document.body.appendChild(inp);
      inp.select();
      document.execCommand("copy");
      document.body.removeChild(inp);
      shareCopied = true;
      setTimeout(() => { shareCopied = false; }, 2000);
    }
  }

  function startPluginBadgeResize(event, className) {
    if (!isPrimaryPointer(event)) return;
    event.preventDefault();
    event.stopPropagation();
    pluginBadgeResize = { className, lastClientX: event.clientX, lastClientY: event.clientY };
  }

  function handlePluginBadgeResizeMove(event) {
    if (!pluginBadgeResize) return;
    event.preventDefault();
    const { className } = pluginBadgeResize;
    const cur = pluginBadgeState[className] || getPluginBadgePos(className, 0);
    const dx = event.clientX - pluginBadgeResize.lastClientX;
    const dy = event.clientY - pluginBadgeResize.lastClientY;
    pluginBadgeResize.lastClientX = event.clientX;
    pluginBadgeResize.lastClientY = event.clientY;
    pluginBadgeState = { ...pluginBadgeState, [className]: {
      ...cur,
      w: Math.max(PLUGIN_BADGE_MIN_W, (cur.w || PLUGIN_BADGE_DEFAULT_W) + dx),
      h: Math.max(PLUGIN_BADGE_MIN_H, (cur.h || PLUGIN_BADGE_DEFAULT_H) + dy)
    } };
  }

  function handlePluginBadgeResizeUp() {
    if (!pluginBadgeResize) return;
    pluginBadgeResize = null;
    persistPluginBadgeState(selectedProjectId, pluginBadgeState);
  }

  onMount(() => {
    const moveHandler = (event) => {
      handleVarBadgePointerMove(event);
      handlePluginBadgePointerMove(event);
      handlePluginBadgeResizeMove(event);
    };
    const upHandler = (event) => {
      handleVarBadgePointerUp(event);
      handlePluginBadgePointerUp();
      handlePluginBadgeResizeUp();
    };
    document.addEventListener("mousemove", moveHandler, true);
    document.addEventListener("mouseup", upHandler, true);
    document.addEventListener("pointermove", moveHandler, true);
    document.addEventListener("pointerup", upHandler, true);
    document.addEventListener("pointercancel", upHandler, true);
    return () => {
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
  const _urlSessionParam = new URLSearchParams(window.location.search).get("session") || "";
  let selectedProjectId = _urlSessionParam || localStorage.getItem("vsm_project_id") || "";
  // Track whether this page load is a URL-based session join (invite link)
  let joiningViaUrl = !!_urlSessionParam;
  let recent = [];
  let recentLoaded = false;
  let recentLoading = false;
  let recentError = "";
  let recentFailureOpen = false;
  let recentFailureProject = null;
  let recentFailureMessage = "";
  let recentSearchQuery = "";
  let recentFilterMode = "all";
  let filteredRecent = [];
  let recentHeaderCountLabel = "0";
  function normalizeRecentPath(path) {
    return String(path || "").trim().replace(/[\\/]+$/, "");
  }
  let recentPinnedProjects = (() => {
    try {
      const raw = localStorage.getItem(RECENT_PINNED_PATHS_KEY);
      const parsed = raw ? JSON.parse(raw) : [];
      if (!Array.isArray(parsed)) return [];
      const out = [];
      const seen = new Set();
      for (const entry of parsed) {
        const normalizedPath = normalizeRecentPath(typeof entry === "string" ? entry : entry?.path);
        if (!normalizedPath || seen.has(normalizedPath)) continue;
        seen.add(normalizedPath);
        const pinnedAt = Number.isFinite(Number(entry?.pinnedAt)) ? Number(entry.pinnedAt) : 0;
        out.push({ path: normalizedPath, pinnedAt });
      }
      return out;
    } catch {
      return [];
    }
  })();
  let tutorials = [];
  let tutorialsLoading = false;
  let tutorialsError = "";

  let openPath = "";
  let newName = "";
  let newBaseDir = localStorage.getItem(NEW_PROJECT_BASE_DIR_KEY) || "";
  let baseDirSuggestionsExpanded = localStorage.getItem(NEW_PROJECT_BASE_DIR_PANEL_KEY) === "true";
  let suggestedBaseDirs = [];
  let openPathError = "";
  let createProjectError = "";
  let saveAsPath = "";
  let saveAsName = "";
  let saveAsDialogOpen = false;
  let saveAsError = "";
  let saveButtonHovered = false;
  let shiftDown = false;

  let openPathInput;
  let openPathPickerInput;
  let newProjectNameInput;
  let saveAsNameInputEl;
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
  let projectConfigPrevBodyOverflow = "";
  let projectConfig = null;
  let projectConfigDraft = null;
  let projectConfigLoading = false;
  let projectConfigError = "";
  let projectConfigSaved = null;
  let projectConfigPending = false;
  let projectConfigApplyTimer = null;
  let projectConfigSelection = { type: "project" };
  let projectConfigGeneralExpanded = true;
  let projectConfigLlmExpanded = true;
  let projectConfigDevicesExpanded = true;
  let projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
  let projectConfigNewAgent = { name: "", device: "" };
  let projectConfigNewFeature = { key: "", value: "" };
  let runtimeVizRateDraft = String(RUNTIME_VIZ_RATE_DEFAULT);
  let runtimeVizBurstDraft = String(RUNTIME_VIZ_BURST_DEFAULT);
  let runtimeVizApplyTimer = null;
  let runtimeVizBusy = false;
  let runtimeVizError = "";
  let runtimeVizCalibrationBusy = false;
  let runtimeVizCalibrationStatus = "";
  let llmExpandedIndex = -1;
  let llmNewName = "";
  let llmModels = {};
  let llmModelsLoading = {};
  let llmTestResult = {};
  let deviceConfigExpanded = true;
  let deviceBehaviorExpanded = true;
  let agentConfigExpanded = true;
  let availableDevices = [];
  let availableDevicesLoading = false;
  let availableDevicesError = "";
  let pluginInterfaces = [];
  let pluginInterfacesLoading = false;
  let pluginInterfacesError = "";
  let lastPluginInterfacesProjectId = "";
  let androidCompatibleDeviceKeys = new Set();
  let selectableAvailableDevices = [];
  let exportableKeyCache = {};
  let exportableKeyLoading = {};
  let exportableKeyError = {};
  let prefsDialogOpen = false;
  let prefsDialogDraft = null;

  let pluginDashboardOpen = false;
  let pluginDashboardPrevBodyOverflow = "";
  let prefsDialogError = "";
  let prefsDialogBusy = false;
  let prefsDialogApplyTimer = null;
  let prefsDialogFingerprintValue = "";
  let prefsDialogPrevBodyOverflow = "";
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
  let scriptLiveTimer = null;
  let scriptLiveLast = "";
  let lastScriptProjectId = "";
  let scriptEditorRef;
  let semanticDoc = null;
  let semanticAnnotations = [];
  let semanticMode = "full";
  let semanticPanelOpen = false;
  let semanticLoading = false;
  let semanticAnalyzeBusy = false;
  let semanticStatus = "";
  let semanticError = "";
  let semanticSourceText = "";
  let semanticStale = false;
  let semanticEditorHighlights = { marks: [], lines: [] };
  let semanticDebug = null;
  let semanticDebugOpen = false;
  let semanticDebugEnabled = false;
  let semanticUdDebug = [];
  let semanticPreviewTimer = null;
  let semanticPreviewRunId = 0;
  let semanticDirty = false;
  let semanticAnalyzeSvo = true;
  let semanticAnalyzeDaTr = true;
  let semanticLLMIndex = 0;
  let semanticSystemPrompt = "You are a multilingual discourse annotation engine for dialogue utterances.";
  let semanticPromptTemplate = `Analyze exactly one utterance sentence and return JSON only (no markdown).
Language can be German or English; handle umlauts correctly (ä/ö/ü and ae/oe/ue variants).
Treat placeholders like $user as normal mentions.
Ignore bracketed stage/action tags (e.g. [wave]).
Classify ONLY this sentence (no cross-sentence inference).

Focus on:
1) dialogueAct.label + dialogueAct.confidence
2) themeRheme.theme + themeRheme.rheme + themeRheme.confidence

Return object fields: version (number), annotations (array with exactly one item).
Annotation fields: id, line, speaker, text, dialogueAct, themeRheme.
Do not output basic subject/verb/object unless explicitly requested by layers.

Dialogue act guideline:
- Use short labels (e.g. greeting, question, inform, request, confirm, reject, thanks, apology, directive, commissive).

Theme-rheme guideline:
- theme = given/topic part (what the sentence is about)
- rheme = new/focus part (what is said about the theme)
- Keep both close to original wording.

Output must be valid JSON.

Layers: {{layers}}
Speaker: {{speaker}}
Line: {{line}}
Sentence:
{{script}}`;

  // Scene execution tracking state
  let activeScenes = [];   // [{ sceneName, language, lower, upper }]
  let activeTurns = [];    // [{ speaker, lower, upper }]
  let sceneHistory = [];   // [{ timestamp, sceneName, language, lower, upper }]

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
  let scriptSearchOpen = false;
  let scriptSearchQuery = "";
  let scriptSearchInputEl;
  let sceneAgentNames = [];
  let sceneFlowPlayActionAgentNames = [];
  let deviceAgentNames = [];
  let agentGroups = { input: [], processing: [], output: [] };
  let missingAgentItems = [];
  let missingAgentNames = [];
  let missingAgentDialogOpen = false;
  let missingAgentDrafts = [];
  let missingAgentDeviceOptions = [];
  let missingAgentError = "";
  let missingAgentBusy = false;
  let eventOverprodDialogOpen = false;
  let eventOverprodMessage = "";
  let eventOverprodRate = "";
  let eventOverprodFlowLabel = "";
  let eventOverprodFlowRate = "";
  let eventOverprodMutedForRun = false;
  let missingVarDialogEl;
  let missingVarDialogOpen = false;
  let missingVarItems = [];
  let preflightModalOpen = false;
  let preflightData = null;
  let pendingPreflightCommand = null;
  let varRenameDialogEl;
  let varRenameDialogOpen = false;
  let varRenameOldName = "";
  let varRenameNewName = "";
  let varRenameUsageCount = 0;
  let renameSceneDialogEl;
  let renameSceneDialogOpen = false;
  let renameSceneOldName = "";
  let renameSceneNewName = "";
  let renameSceneMatches = [];
  let renameSceneBusy = false;
  let renameSceneError = "";
  let renameSceneRequestId = 0;
  let danglingSceneDialogEl;
  let danglingSceneDialogOpen = false;
  let danglingSceneMatches = [];
  let danglingSceneRemoved = [];
  let danglingSceneReplacements = [];
  let danglingSceneBusy = false;
  let danglingSceneError = "";
  let danglingSceneRequestId = 0;
  let danglingSceneCanApply = false;
  let embeddingsAvailable = null;
  let embeddingsChecking = false;
  let embeddingsLastChecked = 0;
  let embeddingsStartAttempted = false;
  let embeddingsStarting = false;
  let embeddingsReady = false;
  let embeddingsModel = "";
  let embeddingsHealthError = "";
  const EMBEDDINGS_URL = "http://127.0.0.1:4050";
  let sceneTitleSuggestions = new Map();
  let sceneTitleSuggestBusy = false;
  let sceneTitleSuggestError = "";
  let sceneTitleSuggestMessage = "";
  const SCENE_TITLE_CLUSTER_THRESHOLD = 0.78;
  let sceneNamesReady = false;
  let previousSceneNames = new Set();
  let previousSceneTextByName = new Map();
  const SELECTION_PREVIEW_LIMIT = 6;

  // Generate panel state
  let generatePanelOpen = false;
  let generateLLMIndex = 0;
  let generateLanguage = "en";
  let generateSceneName = "new_scene";
  let generateSceneCount = 1;
  let generateActors = "";
  let generateFormatPrompt = "";
  let generateActionPrompt = "";
  let generateActionLibrary = [];
  let generateResult = "";
  let generateLoading = false;
  let generateError = "";
  let generateShowFormatPrompt = false;

  const DEFAULT_FORMAT_PROMPT = `You generate {{number}} dialogue scene(s) for a virtual agent system. Each scene starts with a header line:

scene {{language}} {{scene_name}}

Followed by one or more dialogue turns:

{{actor_name}}: Utterance text with punctuation.

Inline actions can be enclosed in square brackets, like [Smile] or [Wave].

The available actors are: {{actors}}

Generate only the scene text. Do not include explanations, markdown formatting, or code blocks.`;

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
  let minimapVisible = localStorage.getItem(SCENEFLOW_MINIMAP_KEY) !== "false";
  $: localStorage.setItem(SCENEFLOW_MINIMAP_KEY, String(minimapVisible));
  let sceneFlowWorldBox = null;
  let sceneFlowViewBox = null;
  // Presence
  let peerPresence = new Map();
  let myPresenceUserId = null;
  let presenceViewportDebounceTimer = null;
  let lastPresenceProjectId = "";
  let sceneFlowSelection = null;
  let sceneFlowMultiSelection = [];
  let nodeEditorTypeOptions = ["Int", "Bool", "Float", "String", "Event"];
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
  let sceneFlowShowInfo = sceneFlowToggleState.showInfo;
  let sceneFlowShowVars = sceneFlowToggleState.showVars;
  let sceneFlowShowBlocks = sceneFlowToggleState.showBlocks;
  let sceneFlowShowInspector = sceneFlowToggleState.showInspector;
  let agentsCollapsed = false;
  let scenesCollapsed = false;
  let typeDefsCollapsed = true;
  let varDefsCollapsed = false;
  let cmdExecCollapsed = false;
  let inspectorDefGridStyle = "";
  let sceneFlowBusy = false;
  let runtimeInfo = null;
  let runtimeError = "";
  let runtimeLoading = false;
  let runtimeLoaded = false;
  let monitorDialogOpen = false;
  let monitorDialogPrevBodyOverflow = "";
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
  let runtimeStopRequested = false;
  let activityNodeCounts = new Map();
  let activityNodeDecayTokens = new Map();
  let activityNodeHoldUntil = new Map();
  let commandActivityHeldNodeIds = new Set();
  let commandActivityKindByNodeId = new Map();
  let playSceneHeldNodeQueue = [];
  let playSceneHoldBySceneKey = new Map();
  let recentActiveNodeQueue = [];
  let recentStoppedNodeQueue = [];
  let pendingPlaySceneStartQueue = [];
  let activityEdgeHits = new Map();
  let activityNodeIds = [];
  let activityEdgeList = [];
  let timeoutEdgeRuns = new Map();
  let overprodWindowStart = 0;
  let overprodWindowTotal = 0;
  let overprodWindowByKey = new Map();
  let overprodWindowMeta = new Map();
  let overprodStreak = 0;
  let overprodNotifiedForRun = false;
  let timeoutEdgeList = [];
  let varBadgeState = loadVarBadgeState();
  let varBadgeDrag = null;
  let pluginBadgeState = {};    // { [className]: { x, y, w, h, expanded } }
  let pluginBadgeDrag = null;   // { className, lastClientX, lastClientY } | null
  let pluginBadgeResize = null; // { className, lastClientX, lastClientY } | null
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
  let timeoutSliderOpen = false;
  let timeoutSliderEdgeId = "";
  let timeoutSliderLastSent = null;
  let timeoutSliderQueuedMs = null;
  let timeoutSliderQueuedEdgeId = "";
  let timeoutSliderSending = false;
  let timeoutSliderMax = 60000;
  let timeoutSliderStep = 1;
  let timeoutInspectorApplyTimer = null;
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
  let typeDefPrevBodyOverflow = "";
  let varDefDraft = null;
  let varDefEditIndex = null;
  let varDefError = "";
  let varDefSelectedIndex = null;
  let varDefPrevBodyOverflow = "";
  let cmdDraft = "";
  let cmdEditIndex = null;
  let cmdError = "";
  let cmdSelectedIndex = null;
  let cmdEditingIndex = null;
  let startListSelectedId = "";
  let cmdDialogOpen = false;
  let cmdInlineDrafts = [];
  let cmdDialogNodeId = "";
  let cmdInlineInputEls = [];
  let cmdInlineWarnings = [];
  let cmdHelperTab = "PlayAction";
  let cmdHelperVarOp = "Assign";
  let cmdHelperSyncing = false;
  let cmdHelperDetectedTab = null;
  let cmdHelperScene = "";
  let cmdHelperAgent = "";
  let cmdHelperAction = "";
  let cmdHelperPlayMode = "blocking";
  let cmdHelperArgs = [];
  let cmdHelperAgentCommands = [];
  let cmdHelperPluginCommands = [];
  let cmdHelperPluginWrites = [];
  let cmdHelperPluginReads = [];
  let cmdHelperPluginConfig = [];
  let cmdHelperPluginCommandsList = [];
  let cmdHelperPluginWritesList = [];
  let cmdHelperPluginReadsList = [];
  let cmdHelperPluginConfigList = [];
  let cmdHelperVarName = "";
  let cmdHelperVarType = "Int";
  let cmdHelperVarExpr = "";
  let cmdHelperVarStep = "1";
  let cmdHelperVarSuggestOpen = false;
  let cmdHelperVarSuggestIndex = 0;
  let cmdHelperVarInputEl;
  let edgeConditionSuggestOpen = false;
  let edgeConditionSuggestIndex = 0;
  let edgeConditionInputEl;
  let edgeConditionApplyTimer = null;
  let edgeConditionSending = false;
  let edgeConditionQueuedDraft = null;
  let edgeConditionQueuedEdgeId = "";
  let lastCmdHelperAction = "";
  let cmdHelperActionDescriptor = null;
  let cmdHelperDescriptor = null;
  let cmdHelperWarnings = [];
  let cmdHelperSceneBindings = {};
  let cmdHelperVarScope = "global";
  let cmdHelperShowWrites = false;
  let cmdHelperShowReads = false;
  let cmdHelperShowConfig = false;
  let lastCmdHelperDescriptorKey = "";
  let cmdAcItems = [];
  let cmdAcSelectedIdx = 0;
  let cmdAcVisible = false;
  let cmdAcReplace = null;
  let cmdAcPrefix = "";
  let cmdAcPos = { left: 0, top: 0, width: 0 };
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
  $: inspectorDefGridStyle = [
    typeDefsCollapsed ? "auto" : "minmax(0, 1fr)",
    varDefsCollapsed ? "auto" : "minmax(0, 1fr)",
    cmdExecCollapsed ? "auto" : "minmax(0, 1fr)"
  ].join(" ");
  $: filteredPrefs = filterKeyValues(prefDraft, prefFilter);
  $: projectConfigView = normalizeProjectConfig(projectConfigDraft || projectConfig || {});
  $: headerDirty = !!(selectedProject?.dirty || sceneFlowDirty || scriptDirty || projectConfigDirty || semanticDirty);
  $: projectConfigPlugins = projectConfigView.plugins;
  $: projectConfigAgents = projectConfigView.agents;
  $: projectConfigLLMs = projectConfigView.llms;
  $: projectConfigLLMPrompts = projectConfigView.llmPrompts;
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
  $: if (!Array.isArray(projectConfigLLMs) || projectConfigLLMs.length === 0) {
    semanticLLMIndex = 0;
  } else if (semanticLLMIndex < 0 || semanticLLMIndex >= projectConfigLLMs.length) {
    semanticLLMIndex = 0;
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
  $: androidCompatibleDeviceKeys = new Set(
    (Array.isArray(pluginInterfaces) ? pluginInterfaces : [])
      .filter((entry) => entry?.plugin?.androidCompatible === true)
      .flatMap((entry) => {
        const className = (entry?.plugin?.className || "").trim();
        const simple = simpleClassName(className);
        return [className, simple];
      })
      .map((value) => String(value || "").trim().toLowerCase())
      .filter(Boolean)
  );
  $: selectableAvailableDevices =
    projectConfigView?.androidProject === true
      ? availableDevices.filter((device) => isAndroidCompatibleDeviceClass(device?.className))
      : availableDevices;
  $: if (
    projectConfigView?.androidProject === true &&
    projectConfigNewPlugin.className &&
    !isAndroidCompatibleDeviceClass(projectConfigNewPlugin.className)
  ) {
    projectConfigNewPlugin = { ...projectConfigNewPlugin, className: "" };
  }
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

  function buildSceneHighlights(scenes, turns, history) {
    const highlights = [];
    for (const h of history) {
      highlights.push({ lower: h.lower, upper: h.upper, type: "played" });
    }
    for (const s of scenes) {
      highlights.push({ lower: s.lower, upper: s.upper, type: "active" });
    }
    for (const t of turns) {
      highlights.push({ lower: t.lower, upper: t.upper, type: "activeTurn" });
    }
    return highlights;
  }

  function truncateSemanticText(text, maxLen = 42) {
    const value = String(text || "").trim();
    if (!value) return "";
    if (value.length <= maxLen) return value;
    return `${value.slice(0, Math.max(3, maxLen - 3)).trimEnd()}...`;
  }

  function semanticLineOffsets(text) {
    const source = String(text || "");
    const starts = [0];
    for (let i = 0; i < source.length; i += 1) {
      if (source[i] === "\n") {
        starts.push(i + 1);
      }
    }
    return starts;
  }

  function semanticFindSpanInLine(spanText, lineText, lineStart, docLength) {
    const needle = String(spanText || "").trim();
    if (!needle || !lineText) return null;
    let idx = lineText.indexOf(needle);
    if (idx < 0) {
      idx = lineText.toLowerCase().indexOf(needle.toLowerCase());
    }
    if (idx < 0) return null;
    const from = Math.max(0, Math.min(docLength, lineStart + idx));
    const to = Math.max(from, Math.min(docLength, from + needle.length));
    if (to <= from) return null;
    return { from, to, text: needle };
  }

  function semanticUtteranceContext(lineText, lineStart, annText = "") {
    const raw = String(lineText || "");
    const colon = raw.indexOf(":");
    if (colon < 0) {
      return null;
    }
    const utteranceRaw = raw.slice(colon + 1);
    const utterance = utteranceRaw.trim();
    let lead = utteranceRaw.indexOf(utterance);
    if (lead < 0) lead = 0;
    const annNeedle = String(annText || "").trim();
    if (annNeedle && utterance && annNeedle.length <= utteranceRaw.length) {
      const inRaw = utteranceRaw.toLowerCase().indexOf(annNeedle.toLowerCase());
      if (inRaw >= 0) {
        lead = inRaw;
      }
    }
    return {
      utteranceStart: lineStart + colon + 1 + Math.max(0, lead),
      utteranceRaw,
      utterance
    };
  }

  function semanticGetByAliases(source, aliases) {
    if (!source || typeof source !== "object") return null;
    for (const key of aliases) {
      if (Object.prototype.hasOwnProperty.call(source, key) && source[key] != null) {
        return source[key];
      }
    }
    return null;
  }

  function semanticArrayByAliases(source, aliases) {
    const value = semanticGetByAliases(source, aliases);
    if (Array.isArray(value)) return value.filter((entry) => entry != null);
    if (value == null) return [];
    return [value];
  }

  function semanticRoleFromArray(items, aliases) {
    if (!Array.isArray(items)) return null;
    const lowered = aliases.map((v) => String(v).toLowerCase());
    for (const item of items) {
      if (!item || typeof item !== "object") continue;
      const role = String(item.role || item.type || item.kind || "").toLowerCase();
      if (lowered.includes(role)) {
        return item;
      }
    }
    return null;
  }

  function semanticExtractBasicSpans(ann) {
    const basic = ann?.basic;
    const subjectAliases = ["subject", "subj", "s", "agent"];
    const verbAliases = ["verb", "predicate", "pred", "v"];
    const objectAliases = ["object", "obj", "o", "patient"];
    const predicateAliases = ["predicate", "subjectComplement", "complement", "predicative"];
    const addressAliases = ["address", "vocative", "voc", "addressee"];
    const addressPhraseAliases = ["addressPhrase", "address_phrase"];
    const subjectModifiersAliases = ["subjectModifiers", "subject_modifiers", "subjectMods"];
    const objectModifiersAliases = ["objectModifiers", "object_modifiers", "objectMods"];
    const predicateModifiersAliases = ["predicateModifiers", "predicate_modifiers", "predicateMods"];
    const addressModifiersAliases = ["addressModifiers", "address_modifiers", "addressMods"];

    if (Array.isArray(basic)) {
      return {
        subject: semanticRoleFromArray(basic, subjectAliases),
        verb: semanticRoleFromArray(basic, verbAliases),
        object: semanticRoleFromArray(basic, objectAliases),
        predicate: semanticRoleFromArray(basic, predicateAliases),
        address: semanticRoleFromArray(basic, addressAliases),
        addressHead: null,
        subjectModifiers: [],
        objectModifiers: [],
        predicateModifiers: [],
        addressModifiers: []
      };
    }
    if (basic && typeof basic === "object") {
      const addressPhraseRaw = semanticGetByAliases(basic, addressPhraseAliases);
      const addressPhrase = addressPhraseRaw && typeof addressPhraseRaw === "object" ? addressPhraseRaw : null;
      const phraseAnchor = addressPhrase ? semanticGetByAliases(addressPhrase, ["anchor", "address", "pronoun"]) : null;
      const phraseHead = addressPhrase ? semanticGetByAliases(addressPhrase, ["head", "noun"]) : null;
      const phraseModifiers = addressPhrase ? semanticArrayByAliases(addressPhrase, ["modifiers", "attributes"]) : [];
      const explicitAddressModifiers = semanticArrayByAliases(basic, addressModifiersAliases);
      return {
        subject: semanticGetByAliases(basic, subjectAliases),
        verb: semanticGetByAliases(basic, verbAliases),
        object: semanticGetByAliases(basic, objectAliases),
        predicate: semanticGetByAliases(basic, predicateAliases),
        address: semanticGetByAliases(basic, addressAliases) || phraseAnchor,
        addressHead: phraseHead,
        subjectModifiers: semanticArrayByAliases(basic, subjectModifiersAliases),
        objectModifiers: semanticArrayByAliases(basic, objectModifiersAliases),
        predicateModifiers: semanticArrayByAliases(basic, predicateModifiersAliases),
        addressModifiers: [...phraseModifiers, ...explicitAddressModifiers]
      };
    }
    return {
      subject: null,
      verb: null,
      object: null,
      predicate: null,
      address: null,
      addressHead: null,
      subjectModifiers: [],
      objectModifiers: [],
      predicateModifiers: [],
      addressModifiers: []
    };
  }

  function normalizeSemanticSpan(span, ann, scriptText, docLength, lineStarts, lines) {
    if (!span) return null;
    const spanObj = typeof span === "object" ? span : { text: String(span) };
    const text = String(spanObj.text || "").trim();
    const hasAbsolute = Number.isFinite(spanObj.from) && Number.isFinite(spanObj.to);
    if (hasAbsolute) {
      const from = Math.max(0, Math.min(docLength, Number(spanObj.from)));
      const to = Math.max(from, Math.min(docLength, Number(spanObj.to)));
      if (to > from) {
        const slice = scriptText.slice(from, to);
        if (!text || slice.toLowerCase().includes(text.toLowerCase()) || text.toLowerCase().includes(slice.toLowerCase())) {
          return { from, to, text: text || slice };
        }
      }
    }
    const lineNo = Number(ann?.line);
    if (Number.isFinite(lineNo) && lineNo > 0 && lineNo <= lines.length) {
      const lineIdx = Math.floor(lineNo) - 1;
      const lineText = lines[lineIdx] || "";
      const lineStart = lineStarts[lineIdx] ?? 0;
      const utteranceCtx = semanticUtteranceContext(lineText, lineStart, ann?.text || "");
      if (!utteranceCtx) {
        return null;
      }
      const inLine = semanticFindSpanInLine(text, utteranceCtx.utteranceRaw || lineText, utteranceCtx.utteranceStart, docLength)
        || semanticFindSpanInLine(text, lineText, lineStart, docLength);
      if (inLine) return inLine;
      if (hasAbsolute && Number(spanObj.from) >= 0 && Number(spanObj.to) > Number(spanObj.from)) {
        const relFrom = Math.max(0, Number(spanObj.from));
        const relTo = Math.max(relFrom, Number(spanObj.to));
        const utteranceLen = (utteranceCtx.utteranceRaw || "").length;
        if (relTo <= utteranceLen) {
          const from = Math.max(0, Math.min(docLength, utteranceCtx.utteranceStart + relFrom));
          const to = Math.max(from, Math.min(docLength, utteranceCtx.utteranceStart + relTo));
          if (to > from) {
            return { from, to, text: text || (utteranceCtx.utteranceRaw || "").slice(relFrom, relTo) };
          }
        }
      }
    }
    return null;
  }

  function semanticUtteranceRangeForLine(lineNo, lineStarts, lines, docLength) {
    const ln = Number(lineNo);
    if (!Number.isFinite(ln) || ln <= 0 || ln > lines.length) return null;
    const lineIdx = Math.floor(ln) - 1;
    const lineText = lines[lineIdx] || "";
    const lineStart = lineStarts[lineIdx] ?? 0;
    const ctx = semanticUtteranceContext(lineText, lineStart, "");
    if (!ctx || !ctx.utterance || !ctx.utterance.length) return null;
    const from = Math.max(0, Math.min(docLength, ctx.utteranceStart));
    const to = Math.max(from, Math.min(docLength, from + ctx.utterance.length));
    if (to <= from) return null;
    return { from, to };
  }

  function clampSemanticSpanToRange(span, range, scriptText, docLength) {
    if (!span || !range) return null;
    const from = Math.max(0, Math.min(docLength, Number(span.from)));
    const to = Math.max(from, Math.min(docLength, Number(span.to)));
    if (to <= from) return null;
    if (to <= range.from || from >= range.to) return null;
    const clampedFrom = Math.max(from, range.from);
    const clampedTo = Math.min(to, range.to);
    if (clampedTo <= clampedFrom) return null;
    const slice = scriptText.slice(clampedFrom, clampedTo);
    return {
      from: clampedFrom,
      to: clampedTo,
      text: String(span.text || slice || "").trim() || slice
    };
  }

  function detectPreviewDialogueAct(utterance) {
    const value = String(utterance || "").trim();
    const lower = value.toLowerCase();
    if (value.endsWith("?")) return "question";
    if (lower.startsWith("please ") || lower.startsWith("can you") || lower.startsWith("could you")) return "request";
    if (lower.startsWith("hi ") || lower.startsWith("hello")) return "greeting";
    if (lower.startsWith("thanks") || lower.startsWith("thank you")) return "thank";
    return "inform";
  }

  function createSemanticPreviewAnnotations(text, includeMeta) {
    const annotations = [];
    const script = String(text || "");
    const lines = script.split("\n");
    const starts = semanticLineOffsets(script);
    const tokenPattern = /[A-Za-z][A-Za-z0-9_'-]*/g;
    for (let idx = 0; idx < lines.length; idx += 1) {
      const line = lines[idx];
      const colon = line.indexOf(":");
      if (colon <= 0 || colon >= line.length - 1) continue;
      const speaker = line.slice(0, colon).trim();
      const utteranceRaw = line.slice(colon + 1);
      const utterance = utteranceRaw.trim();
      if (!speaker || !utterance) continue;
      const utteranceLead = utteranceRaw.indexOf(utterance);
      const utteranceStart = (starts[idx] ?? 0) + colon + 1 + Math.max(0, utteranceLead);
      const tokens = [];
      let match = tokenPattern.exec(utterance);
      while (match) {
        tokens.push({
          text: match[0],
          from: utteranceStart + match.index,
          to: utteranceStart + match.index + match[0].length
        });
        match = tokenPattern.exec(utterance);
      }
      const ann = {
        id: `preview-${idx}`,
        line: idx + 1,
        speaker,
        text: utterance,
        basic: {}
      };
      if (tokens[0]) ann.basic.subject = tokens[0];
      if (tokens[1]) ann.basic.verb = tokens[1];
      if (tokens[2]) {
        ann.basic.object = {
          text: utterance.slice(tokens[2].from - utteranceStart),
          from: tokens[2].from,
          to: utteranceStart + utterance.length
        };
      }
      if (includeMeta) {
        ann.dialogueAct = { label: detectPreviewDialogueAct(utterance), confidence: 0.5 };
        const theme = tokens[0]?.text || "";
        ann.themeRheme = {
          theme,
          rheme: theme ? utterance.replace(new RegExp(`^${theme}\\s*`, "i"), "") : utterance,
          confidence: 0.45
        };
      }
      annotations.push(ann);
    }
    return annotations;
  }

  function isSemanticSpeakerCandidate(rawTag) {
    const tag = String(rawTag || "").trim();
    if (!tag) return false;
    const lower = tag.toLowerCase();
    if (lower === "scene" || lower === "title" || lower.startsWith("scene ") || lower.startsWith("#")) {
      return false;
    }
    if (
      tag.includes("(") || tag.includes(")") ||
      tag.includes("[") || tag.includes("]") ||
      tag.includes("{") || tag.includes("}") ||
      tag.includes("=")
    ) {
      return false;
    }
    if (tag.length > 40) return false;
    return true;
  }

  function extractSemanticSentenceUnits(script) {
    const text = String(script || "");
    const lineStarts = semanticLineOffsets(text);
    const lines = text.split("\n");
    const units = [];
    const sentencePattern = /[^.!?]+[.!?]+|[^.!?]+$/g;
    const sceneHeaderPattern = /^\s*scene\s+([A-Za-z][A-Za-z0-9_-]*)\b/i;
    let currentSceneLanguage = "";
    for (let idx = 0; idx < lines.length; idx += 1) {
      const line = lines[idx] || "";
      const sceneMatch = line.match(sceneHeaderPattern);
      if (sceneMatch) {
        currentSceneLanguage = String(sceneMatch[1] || "").trim().toLowerCase();
      }
      const colon = line.indexOf(":");
      if (colon <= 0 || colon >= line.length - 1) continue;
      const speaker = line.slice(0, colon).trim();
      if (!isSemanticSpeakerCandidate(speaker)) continue;
      const utteranceRaw = line.slice(colon + 1);
      const utterance = utteranceRaw.trim();
      if (!speaker || !utterance) continue;
      let lead = utteranceRaw.indexOf(utterance);
      if (lead < 0) lead = 0;
      const utteranceStart = (lineStarts[idx] ?? 0) + colon + 1 + lead;
      sentencePattern.lastIndex = 0;
      let match = sentencePattern.exec(utterance);
      while (match) {
        const segmentRaw = match[0] || "";
        const sentence = segmentRaw.trim();
        if (sentence) {
          let segLead = segmentRaw.indexOf(sentence);
          if (segLead < 0) segLead = 0;
          units.push({
            line: idx + 1,
            speaker,
            text: sentence,
            startOffset: utteranceStart + match.index + segLead,
            language: currentSceneLanguage
          });
        }
        match = sentencePattern.exec(utterance);
      }
    }
    return units;
  }

  function shiftOffsetsDeep(value, delta) {
    if (Array.isArray(value)) {
      return value.map((entry) => shiftOffsetsDeep(entry, delta));
    }
    if (!value || typeof value !== "object") {
      return value;
    }
    const out = {};
    for (const [key, entry] of Object.entries(value)) {
      out[key] = shiftOffsetsDeep(entry, delta);
    }
    if (Number.isFinite(out.from)) {
      out.from = Number(out.from) + delta;
    }
    if (Number.isFinite(out.to)) {
      out.to = Number(out.to) + delta;
    }
    return out;
  }

  function normalizeSentenceAnnotations(annotations, unit) {
    if (!Array.isArray(annotations)) return [];
    return annotations.map((ann, idx) => {
      const source = ann && typeof ann === "object" ? ann : {};
      const mapped = {
        ...source,
        id: source.id || `ann-${unit.line}-${idx}`,
        line: unit.line,
        speaker: source.speaker || unit.speaker,
        text: source.text || unit.text
      };
      if (source.basic != null) {
        mapped.basic = shiftOffsetsDeep(source.basic, unit.startOffset);
      }
      return mapped;
    });
  }

  function mergeSentenceAnnotationLayers(syntaxAnnotations, metaAnnotations, unit) {
    const syntax = Array.isArray(syntaxAnnotations) ? syntaxAnnotations : [];
    const meta = Array.isArray(metaAnnotations) ? metaAnnotations : [];
    const count = Math.max(syntax.length, meta.length);
    const out = [];
    for (let idx = 0; idx < count; idx += 1) {
      const syn = syntax[idx] && typeof syntax[idx] === "object" ? syntax[idx] : {};
      const prag = meta[idx] && typeof meta[idx] === "object" ? meta[idx] : {};
      const merged = {
        ...syn,
        ...prag,
        id: prag.id || syn.id || `ann-${unit.line}-${idx}`,
        line: unit.line,
        speaker: prag.speaker || syn.speaker || unit.speaker || "",
        text: prag.text || syn.text || unit.text || ""
      };
      if (syn.basic != null) {
        merged.basic = syn.basic;
      } else if (prag.basic != null) {
        merged.basic = prag.basic;
      }
      if (prag.dialogueAct != null) {
        merged.dialogueAct = prag.dialogueAct;
      } else if (syn.dialogueAct != null) {
        merged.dialogueAct = syn.dialogueAct;
      }
      if (prag.themeRheme != null) {
        merged.themeRheme = prag.themeRheme;
      } else if (syn.themeRheme != null) {
        merged.themeRheme = syn.themeRheme;
      }
      const synProv = syn?.provenance && typeof syn.provenance === "object" ? syn.provenance : null;
      const pragProv = prag?.provenance && typeof prag.provenance === "object" ? prag.provenance : null;
      if (synProv || pragProv) {
        const synLayers = synProv?.layers && typeof synProv.layers === "object" ? synProv.layers : {};
        const pragLayers = pragProv?.layers && typeof pragProv.layers === "object" ? pragProv.layers : {};
        merged.provenance = {
          ...(synProv || {}),
          ...(pragProv || {}),
          layers: {
            ...synLayers,
            ...pragLayers
          }
        };
      }
      out.push(merged);
    }
    return out;
  }

  function stopSemanticPreview() {
    semanticPreviewRunId += 1;
    if (semanticPreviewTimer) {
      clearInterval(semanticPreviewTimer);
      semanticPreviewTimer = null;
    }
  }

  function startSemanticPreview(text, includeMeta) {
    stopSemanticPreview();
    const preview = createSemanticPreviewAnnotations(text, includeMeta);
    semanticAnnotations = [];
    semanticSourceText = text || "";
    if (!preview.length) {
      semanticStatus = "Analyzing semantic info...";
      return;
    }
    const runId = semanticPreviewRunId + 1;
    semanticPreviewRunId = runId;
    let shown = 0;
    semanticStatus = `Analyzing semantic info... ${shown}/${preview.length}`;
    semanticPreviewTimer = setInterval(() => {
      if (runId !== semanticPreviewRunId) {
        clearInterval(semanticPreviewTimer);
        semanticPreviewTimer = null;
        return;
      }
      const step = Math.max(1, Math.ceil(preview.length / 24));
      shown = Math.min(preview.length, shown + step);
      semanticAnnotations = preview.slice(0, shown);
      semanticStatus = `Analyzing semantic info... ${shown}/${preview.length}`;
      if (shown >= preview.length) {
        clearInterval(semanticPreviewTimer);
        semanticPreviewTimer = null;
      }
    }, 70);
  }

  function buildSemanticEditorHighlights(annotations, mode, text) {
    const out = { marks: [], lines: [] };
    const debug = {
      mode,
      annotations: Array.isArray(annotations) ? annotations.length : 0,
      docLength: String(text || "").length,
      basicPresent: 0,
      spansProvided: { subject: 0, verb: 0, object: 0, predicate: 0, address: 0 },
      spansResolved: { subject: 0, verb: 0, object: 0, predicate: 0, address: 0 },
      missingLine: 0,
      unresolved: []
    };
    if (mode === "off" || !Array.isArray(annotations) || !annotations.length) {
      return { highlights: out, debug };
    }
    const scriptText = String(text || "");
    const docLength = scriptText.length;
    const lineStarts = semanticLineOffsets(scriptText);
    const lines = scriptText.split("\n");
    const includeBasic = mode === "basic" || mode === "full";
    const includeMeta = mode === "full";

    for (const ann of annotations) {
      if (!ann || typeof ann !== "object") continue;
      if (includeBasic && ann.basic != null) {
        debug.basicPresent += 1;
        const basic = semanticExtractBasicSpans(ann);
        const hasLine = Number.isFinite(Number(ann?.line)) && Number(ann.line) > 0 && Number(ann.line) <= lines.length;
        if (!hasLine) {
          debug.missingLine += 1;
        }
        if (basic.subject != null) debug.spansProvided.subject += 1;
        if (basic.verb != null) debug.spansProvided.verb += 1;
        if (basic.object != null) debug.spansProvided.object += 1;
        if (basic.predicate != null) debug.spansProvided.predicate += 1;
        if (basic.address != null) debug.spansProvided.address += 1;

        const utteranceRange = semanticUtteranceRangeForLine(ann?.line, lineStarts, lines, docLength);
        const subject = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.subject, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const verb = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.verb, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const object = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.object, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const predicate = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.predicate, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const address = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.address, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const addressHead = clampSemanticSpanToRange(
          normalizeSemanticSpan(basic.addressHead, ann, scriptText, docLength, lineStarts, lines),
          utteranceRange,
          scriptText,
          docLength
        );
        const modifierGroups = [
          { role: "subject", spans: basic.subjectModifiers || [] },
          { role: "object", spans: basic.objectModifiers || [] },
          { role: "predicate", spans: basic.predicateModifiers || [] },
          { role: "address", spans: basic.addressModifiers || [] }
        ];
        if (subject) out.marks.push({ ...subject, kind: "subject" });
        if (verb) out.marks.push({ ...verb, kind: "verb" });
        if (object) out.marks.push({ ...object, kind: "object" });
        if (predicate) out.marks.push({ ...predicate, kind: "predicate" });
        if (address) out.marks.push({ ...address, kind: "address" });
        if (addressHead) out.marks.push({ ...addressHead, kind: "address-head" });
        for (const group of modifierGroups) {
          const spans = Array.isArray(group.spans) ? group.spans : [];
          for (const raw of spans) {
            const normalized = clampSemanticSpanToRange(
              normalizeSemanticSpan(raw, ann, scriptText, docLength, lineStarts, lines),
              utteranceRange,
              scriptText,
              docLength
            );
            if (!normalized) continue;
            const pos = String(raw?.pos || raw?.kind || raw?.type || "").toLowerCase();
            const kind = pos.includes("compar")
              ? `${group.role}-comparison`
              : pos.includes("adv")
                ? `${group.role}-adverb`
                : `${group.role}-adjective`;
            out.marks.push({ ...normalized, kind });
          }
        }
        if (subject) {
          debug.spansResolved.subject += 1;
        } else if (basic.subject != null && debug.unresolved.length < 24) {
          debug.unresolved.push({
            line: ann?.line ?? null,
            kind: "subject",
            span: basic.subject
          });
        }
        if (verb) {
          debug.spansResolved.verb += 1;
        } else if (basic.verb != null && debug.unresolved.length < 24) {
          debug.unresolved.push({
            line: ann?.line ?? null,
            kind: "verb",
            span: basic.verb
          });
        }
        if (object) {
          debug.spansResolved.object += 1;
        } else if (basic.object != null && debug.unresolved.length < 24) {
          debug.unresolved.push({
            line: ann?.line ?? null,
            kind: "object",
            span: basic.object
          });
        }
        if (predicate) {
          debug.spansResolved.predicate += 1;
        } else if (basic.predicate != null && debug.unresolved.length < 24) {
          debug.unresolved.push({
            line: ann?.line ?? null,
            kind: "predicate",
            span: basic.predicate
          });
        }
        if (address) {
          debug.spansResolved.address += 1;
        } else if (basic.address != null && debug.unresolved.length < 24) {
          debug.unresolved.push({
            line: ann?.line ?? null,
            kind: "address",
            span: basic.address
          });
        }
      }
      if (includeMeta) {
        const badgeParts = [];
        const daLabel = String(ann?.dialogueAct?.label || "").trim();
        if (daLabel) {
          badgeParts.push(`DA ${truncateSemanticText(daLabel, 22)}`);
        }
        const theme = String(ann?.themeRheme?.theme || "").trim();
        const rheme = String(ann?.themeRheme?.rheme || "").trim();
        if (theme || rheme) {
          const themeShort = truncateSemanticText(theme, 18);
          const rhemeShort = truncateSemanticText(rheme, 24);
          badgeParts.push(`T/R ${themeShort}${themeShort && rhemeShort ? " -> " : ""}${rhemeShort}`);
        }
        const line = Number(ann.line);
        if (badgeParts.length && Number.isFinite(line) && line > 0) {
          out.lines.push({ line: Math.floor(line), badge: badgeParts.join(" | ") });
        }
      }
    }
    debug.renderedMarks = out.marks.length;
    debug.renderedLineBadges = out.lines.length;
    return { highlights: out, debug };
  }

  $: scriptClean = !scriptDirty;
  $: sceneHighlights = scriptClean
    ? buildSceneHighlights(activeScenes, activeTurns, sceneHistory)
    : [];
  $: semanticStale = semanticAnnotations.length > 0 && semanticSourceText !== scriptDraft;
  $: {
    if (semanticStale || semanticMode === "off") {
      semanticEditorHighlights = { marks: [], lines: [] };
      semanticDebug = {
        mode: semanticMode,
        annotations: Array.isArray(semanticAnnotations) ? semanticAnnotations.length : 0,
        spansProvided: { subject: 0, verb: 0, object: 0, predicate: 0, address: 0 },
        spansResolved: { subject: 0, verb: 0, object: 0, predicate: 0, address: 0 },
        unresolved: [],
        renderedMarks: 0,
        renderedLineBadges: 0,
        ud: semanticUdDebug
      };
    } else {
      const semanticComputed = buildSemanticEditorHighlights(semanticAnnotations, semanticMode, scriptDraft);
      semanticEditorHighlights = semanticComputed.highlights;
      semanticDebug = {
        ...semanticComputed.debug,
        ud: semanticUdDebug
      };
    }
  }
  $: if (
    semanticDebugEnabled &&
    semanticDebug &&
    !semanticAnalyzeBusy &&
    ((semanticDebug.spansProvided?.subject || 0) > 0 ||
      (semanticDebug.spansProvided?.verb || 0) > 0 ||
      (semanticDebug.spansProvided?.object || 0) > 0 ||
      (semanticDebug.spansProvided?.predicate || 0) > 0 ||
      (semanticDebug.spansProvided?.address || 0) > 0) &&
    (semanticDebug.renderedMarks || 0) === 0
  ) {
    semanticDebugOpen = true;
  }

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
    if (scriptDirty) {
      console.log("[canAutoApply]", canAutoApply, {showEditor, autoSaveEnabled, wsConnected, scriptDirty, scriptParseOk, scriptError: !!scriptError, diagLen: scriptDiagnostics.length, scriptLoading, projectSaving, scriptDiagTimer: !!scriptDiagTimer});
    }
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
  $: selectedNode =
    sceneFlowSelection?.type === "node"
      ? sceneFlow?.nodes?.find((node) => node.id === sceneFlowSelection.id)
      : sceneFlowSelection?.type === "command"
        ? sceneFlow?.nodes?.find((node) => node.id === sceneFlowSelection.nodeId)
        : null;
  $: selectedCommand =
    sceneFlowSelection?.type === "command"
      ? (() => {
          const node = sceneFlow?.nodes?.find((entry) => entry.id === sceneFlowSelection.nodeId);
          const commands = Array.isArray(node?.commands) ? node.commands : [];
          const index = Number(sceneFlowSelection.index);
          return Number.isInteger(index) && index >= 0 && index < commands.length
            ? { node, command: commands[index], index }
            : null;
        })()
      : null;
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
  $: edgeAltStartChildNodes = Array.isArray(selectedEdgeTarget?.childNodes)
    ? selectedEdgeTarget.childNodes.filter((node) => node && !node.isHistory)
    : [];
  $: edgeAltStartStartNodes = edgeAltStartChildNodes.filter((node) => node.isStart);
  $: edgeAltStartSelections = edgeDraft?.altStartSelections || {};
  $: edgeAltStartSelectorMuted = !edgeAltStartEnabled || edgeAltStartChildNodes.length === 0;
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
      : sceneFlowSelection?.type === "command"
        ? selectedCommand?.node || null
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
    const base = ["Int", "Bool", "Float", "String", "Event"];
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
    const currentSuperScope = sceneFlow?.superNodeData?.isRoot === true ? "global" : "supernode";
    (sceneFlowVarDefs || []).forEach((def) => addVar(def, currentSuperScope));
    const ancestors = Array.isArray(sceneFlowPathNodes) ? sceneFlowPathNodes.slice(0, -1).reverse() : [];
    ancestors.forEach((node, index) => {
      const depth = index + 1;
      const scope = node?.isRoot ? "global" : (depth === 1 ? "parent" : `ancestor-${depth}`);
      (Array.isArray(node?.varDefs) ? node.varDefs : []).forEach((def) => addVar(def, scope));
    });
    return list;
  })();
  $: cmdHelperVarSuggestions = (() => {
    const prefix = String(cmdHelperVarName || "").trim().toLowerCase();
    if (!prefix) return [];
    return helperVarCandidates.filter((entry) => String(entry.name || "").toLowerCase().startsWith(prefix));
  })();
  $: edgeConditionToken = edgeConditionCurrentToken(edgeDraft?.condition ?? "", edgeConditionInputEl);
  $: edgeConditionSuggestions = (() => {
    const prefix = String(edgeConditionToken || "").trim().toLowerCase();
    if (!prefix) return [];
    return helperVarCandidates.filter((entry) => String(entry.name || "").toLowerCase().startsWith(prefix));
  })();
  $: if (cmdHelperVarSuggestIndex >= cmdHelperVarSuggestions.length) {
    cmdHelperVarSuggestIndex = cmdHelperVarSuggestions.length > 0 ? 0 : -1;
  }
  $: if (!cmdHelperVarSuggestions.length) {
    cmdHelperVarSuggestOpen = false;
  }
  $: if (edgeConditionSuggestIndex >= edgeConditionSuggestions.length) {
    edgeConditionSuggestIndex = edgeConditionSuggestions.length > 0 ? 0 : -1;
  }
  $: if (!edgeConditionSuggestions.length) {
    edgeConditionSuggestOpen = false;
  }
  $: if (cmdHelperVarInputEl && getContentEditableValue(cmdHelperVarInputEl) !== String(cmdHelperVarName || "")) {
    setContentEditableValue(cmdHelperVarInputEl, cmdHelperVarName || "");
  }
  $: if (edgeConditionInputEl && getContentEditableValue(edgeConditionInputEl) !== String(edgeDraft?.condition || "")) {
    setContentEditableValue(edgeConditionInputEl, edgeDraft?.condition || "");
  }
  $: cmdHelperVarExists = (() => {
    const name = (cmdHelperVarName || "").trim();
    if (!name) return false;
    return helperVarCandidates.some((entry) => entry.name === name);
  })();
  $: sceneFlowFrameColor = superNodeFrameColor(sceneFlow);
  $: sceneFlowFrameStyle = `--sf-frame-color:${sceneFlowFrameColor};`;
  $: sceneFlowLayoutStyle = "";
  $: if (varBadgeState.visible !== sceneFlowShowVars) {
    varBadgeState = { ...varBadgeState, visible: sceneFlowShowVars };
  }
  $: persistSceneFlowToggles({
    nodeSnap: sceneFlowNodeSnap,
    showInfo: sceneFlowShowInfo,
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
  $: pluginBadgeVarNames = new Set(
    pluginBadgeDescriptors.flatMap((b) => b.variables.map((v) => v.name))
  );
  $: displayGlobalVarList = (() => {
    const merged = [];
    const seen = new Set();
    runtimeDisplayGlobals.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name || pluginBadgeVarNames.has(name)) return;
      seen.add(name);
      merged.push(def);
    });
    sceneFlowVarDefs.forEach((def) => {
      const name = (def?.name || "").trim();
      if (!name || seen.has(name) || pluginBadgeVarNames.has(name)) return;
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
  $: infoBuildYear = (() => {
    const raw = String(infoBuildDate || "");
    const matchedYear = raw.match(/\b(19|20)\d{2}\b/);
    if (matchedYear) return matchedYear[0];
    const parsed = Date.parse(raw);
    if (!Number.isNaN(parsed)) return String(new Date(parsed).getFullYear());
    return String(new Date().getFullYear());
  })();
  $: projectRequiresSaveAs = (() => {
    if (!selectedProject) return false;
    if (selectedProject.saveAsOnly !== undefined) {
      return selectedProject.saveAsOnly === true;
    }
    return !selectedProject.path || selectedProject.pending === true;
  })();
  $: saveButtonActsAsSaveAs = !projectRequiresSaveAs && saveButtonHovered && shiftDown;
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
  $: {
    const currentSceneTextMap = buildSceneTextMapFromScript(scriptDraft || "");
    const currentNames = sceneNameSetFromGroups(scriptScenesLive);
    if (!sceneNamesReady) {
      previousSceneNames = currentNames;
      previousSceneTextByName = currentSceneTextMap;
      sceneNamesReady = true;
    } else if (!setsEqual(currentNames, previousSceneNames)) {
      const removed = Array.from(previousSceneNames).filter((name) => !currentNames.has(name));
      const added = Array.from(currentNames).filter((name) => !previousSceneNames.has(name));
      const removedTextMap = previousSceneTextByName;
      previousSceneNames = currentNames;
      previousSceneTextByName = currentSceneTextMap;
      void handleSceneListChange(removed, added, removedTextMap);
    } else {
      previousSceneTextByName = currentSceneTextMap;
    }
  }
  $: filteredScriptElements = filterScriptElements(scriptElements, scriptElementsFilter);
  $: cmdHelperAgentCommands = (pluginInterfaces, projectConfigView, pluginCommandsForAgent(cmdHelperAgent));
  $: cmdHelperDescriptor = (pluginInterfaces, projectConfigView, pluginInterfaceForAgent(cmdHelperAgent));
  $: cmdHelperActionDescriptor =
    (cmdHelperAgentCommands || []).find((entry) => entry?.name === cmdHelperAction) || null;
  $: cmdHelperPluginCommands = Array.isArray(cmdHelperDescriptor?.commands) ? cmdHelperDescriptor.commands : [];
  $: cmdHelperPluginWrites = Array.isArray(cmdHelperDescriptor?.writes) ? cmdHelperDescriptor.writes : [];
  $: cmdHelperPluginReads = Array.isArray(cmdHelperDescriptor?.reads) ? cmdHelperDescriptor.reads : [];
  $: cmdHelperPluginConfig = Array.isArray(cmdHelperDescriptor?.config) ? cmdHelperDescriptor.config : [];
  $: cmdHelperPluginCommandsList = cmdHelperPluginCommands;
  $: cmdHelperPluginWritesList = cmdHelperPluginWrites;
  $: cmdHelperPluginReadsList = cmdHelperPluginReads;
  $: cmdHelperPluginConfigList = cmdHelperPluginConfig;
  $: if (selectedProjectId) {
    pluginBadgeState = loadPluginBadgeState(selectedProjectId);
    const _pid = selectedProjectId;
    fetchPluginBadgeStateFromServer(_pid).then((serverState) => {
      if (serverState && selectedProjectId === _pid) {
        pluginBadgeState = serverState;
        localStorage.setItem(PLUGIN_BADGE_STORAGE_KEY_PREFIX + _pid, JSON.stringify(serverState));
      }
    });
  }
  $: pluginBadgeDescriptors = buildPluginBadgeDescriptors(projectConfigView, pluginInterfaces);
  $: {
    const normalize = (value) => String(value || "").trim().toLowerCase();
    const isUnknown = (value) => !value || normalize(value) === "unknown";
    const isDynamic = (value) => String(value || "").trim().startsWith("<");
    const writesUnclear = cmdHelperPluginWritesList.length > 0 &&
      cmdHelperPluginWritesList.every((entry) => isDynamic(entry?.var) && isUnknown(entry?.type));
    const readsUnclear = cmdHelperPluginReadsList.length > 0 &&
      cmdHelperPluginReadsList.every((entry) => isDynamic(entry?.var) && isUnknown(entry?.type));
    const configUnclear = cmdHelperPluginConfigList.length > 0 &&
      cmdHelperPluginConfigList.every((entry) => !entry?.key && isUnknown(entry?.type));
    const descriptorKey = [
      cmdHelperAgent,
      cmdHelperDescriptor?.plugin?.id,
      cmdHelperDescriptor?.plugin?.name,
      cmdHelperDescriptor?.plugin?.className
    ]
      .filter(Boolean)
      .join("|");
    if (descriptorKey !== lastCmdHelperDescriptorKey) {
      lastCmdHelperDescriptorKey = descriptorKey;
      cmdHelperShowWrites = cmdHelperPluginWritesList.length > 0 && !writesUnclear;
      cmdHelperShowReads = cmdHelperPluginReadsList.length > 0 && !readsUnclear;
      cmdHelperShowConfig = cmdHelperPluginConfigList.length > 0 && !configUnclear;
    }
  }
  $: cmdHelperAgentKnown = cmdHelperAgent && (projectConfigAgents || []).some((a) => a?.name === cmdHelperAgent);
  $: cmdHelperActionKnown = cmdHelperAction && cmdHelperAgentCommands.some((c) => c?.name === cmdHelperAction);
  $: cmdHelperWarnings =
    cmdHelperTab === "PlayAction"
      ? playActionWarnings(cmdHelperAgent, cmdHelperAction, cmdHelperArgs, pluginInterfaceForAgent(cmdHelperAgent))
      : [];
  $: sceneAgentNames = extractSceneAgents(scriptDraft);
  $: sceneFlowPlayActionAgentNames = extractSceneFlowPlayActionAgents(sceneFlow);
  $: deviceAgentNames = extractDeviceAgents(projectConfigAgents);
  $: agentGroups = buildAgentGroups(
    mergeAgentNames(sceneAgentNames, sceneFlowPlayActionAgentNames),
    deviceAgentNames,
    pluginInterfaces,
    projectConfigView
  );
  $: missingAgentItems = extractMissingAgentsDetailed(
    mergeAgentNames(sceneAgentNames, sceneFlowPlayActionAgentNames),
    projectConfigAgents,
    sceneAgentNames,
    sceneFlowPlayActionAgentNames
  );
  $: missingAgentNames = missingAgentItems.map((entry) => entry.name);
  $: missingAgentDeviceOptions = buildMissingAgentDeviceOptions(projectConfigPlugins);
  $: monitorSelectedVar = findMonitorVar(monitorSelectedKey);
  $: prefsPreviewStyle = buildPrefsPreviewStyle(prefsDialogDraft);
  $: if (prefsDialogOpen && prefsDialogDraft) {
    const nextFingerprint = prefsDialogFingerprint(prefsDialogDraft);
    if (nextFingerprint && nextFingerprint !== prefsDialogFingerprintValue) {
      prefsDialogFingerprintValue = nextFingerprint;
      schedulePrefsApply();
    }
  }
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
    typeDefsCollapsed = true;
    varDefsCollapsed = nodeEditorTarget?.isRoot ? false : true;
    cmdExecCollapsed = false;
    closeTypeDefDialog();
    closeVarDefDialog();
    if (cmdDialogOpen) {
      syncCmdInlineDrafts();
      cmdSelectedIndex = null;
      cmdEditingIndex = null;
    } else {
      resetCmdEditor();
    }
  } else if (!nodeEditorTargetId && lastNodeDefsId) {
    lastNodeDefsId = "";
    typeDefsCollapsed = true;
    varDefsCollapsed = true;
    cmdExecCollapsed = false;
    resetTypeDefEditor();
    resetVarDefEditor();
    if (cmdDialogOpen) {
      syncCmdInlineDrafts();
      cmdSelectedIndex = null;
      cmdEditingIndex = null;
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
      cmdEditingIndex = null;
    }
  }
  $: if (cmdEditingIndex !== null && cmdEditingIndex !== cmdSelectedIndex) {
    cmdEditingIndex = null;
  }

  $: if (cmdDialogOpen && cmdSelectedIndex !== null && !cmdHelperSyncing) {
    syncHelperFromSelection();
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
    const timeoutMode = timeoutModeFromEdge(selectedEdge);
    edgeDraftId = selectedEdge.id;
    edgeDraft = {
      condition: selectedEdge.condition ?? "",
      probability: selectedEdge.probability !== undefined ? String(selectedEdge.probability) : "",
      timeoutSpec: edgeTimeoutSpec(selectedEdge),
      timeoutMinSpec: timeoutMode === "interval" ? String(selectedEdge.timeoutMinMs) : "",
      timeoutMaxSpec: timeoutMode === "interval" ? String(selectedEdge.timeoutMaxMs) : "",
      timeoutMode,
      altStartText: formatAltStartMap(selectedEdge),
      altStartSelections: altStartSelectionsFromEdge(selectedEdge, selectedEdgeTarget)
    };
    edgeEditError = "";
  } else if (!selectedEdge) {
    edgeDraftId = "";
    edgeDraft = null;
    edgeEditError = "";
  }
  $: if ((selectedEdge?.id || "") !== edgeConditionQueuedEdgeId) {
    edgeConditionQueuedEdgeId = selectedEdge?.id || "";
    edgeConditionSuggestOpen = false;
    edgeConditionSuggestIndex = 0;
    if (edgeConditionApplyTimer) {
      clearTimeout(edgeConditionApplyTimer);
      edgeConditionApplyTimer = null;
    }
    edgeConditionSending = false;
    edgeConditionQueuedDraft = null;
  }
  $: if ((selectedEdge?.id || "") !== timeoutSliderEdgeId) {
    timeoutSliderEdgeId = selectedEdge?.id || "";
    if (timeoutInspectorApplyTimer) {
      clearTimeout(timeoutInspectorApplyTimer);
      timeoutInspectorApplyTimer = null;
    }
    timeoutSliderOpen = false;
    timeoutSliderLastSent = null;
    timeoutSliderQueuedMs = null;
    timeoutSliderQueuedEdgeId = "";
    timeoutSliderSending = false;
    timeoutSliderMax = 60000;
    timeoutSliderStep = 1;
  }

  $: nodeDirty =
    !!selectedNode &&
    !!nodeDraft &&
    ((!selectedNode.isHistory && nodeDraft.name !== (selectedNode.name ?? "")) ||
      nodeDraft.comment !== (selectedNode.comment ?? "") ||
      !!nodeDraft.isStart !== !!selectedNode.isStart);
  $: superNodeStartLocked = !!(superNodeDraft && nodeEditorTarget?.isRoot);
  $: rootSceneFlowCommandEditingLocked = !!nodeEditorTarget?.isRoot;
  $: if (rootSceneFlowCommandEditingLocked) {
    varDefsCollapsed = false;
    cmdExecCollapsed = true;
  }
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
      JSON.stringify(normalizeAltStartSelections(edgeDraft.altStartSelections, selectedEdgeTarget)) !==
        JSON.stringify(normalizeAltStartSelections(altStartSelectionsFromEdge(selectedEdge, selectedEdgeTarget), selectedEdgeTarget));
    if (selectedEdge.type === "CEDGE" || selectedEdge.type === "IEDGE") {
      return (edgeDraft.condition ?? "") !== (selectedEdge.condition ?? "") || altDirty;
    }
    if (selectedEdge.type === "PEDGE") {
      return altDirty;
    }
    if (selectedEdge.type === "TEDGE") {
      const mode = edgeDraft.timeoutMode || "fixed";
      if (mode !== timeoutModeFromEdge(selectedEdge)) return true;
      if (mode === "interval") {
        const minNow = String(edgeDraft.timeoutMinSpec ?? "").trim();
        const maxNow = String(edgeDraft.timeoutMaxSpec ?? "").trim();
        const minEdge = String(selectedEdge.timeoutMinMs ?? "").trim();
        const maxEdge = String(selectedEdge.timeoutMaxMs ?? "").trim();
        return minNow !== minEdge || maxNow !== maxEdge || altDirty;
      }
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
  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastPluginInterfacesProjectId) {
    lastPluginInterfacesProjectId = selectedProjectId;
    loadPluginInterfaces(selectedProjectId);
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastScriptProjectId) {
    lastScriptProjectId = selectedProjectId;
    scriptDiagRequestId += 1;
    if (scriptDiagTimer) {
      clearTimeout(scriptDiagTimer);
      scriptDiagTimer = null;
    }
    clearScriptAutoApplyTimer();
    sceneNamesReady = false;
    previousSceneNames = new Set();
    loadScript(selectedProjectId);
    loadScriptScenes(selectedProjectId);
    loadScriptElements(selectedProjectId);
  }

  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastSceneFlowProjectId) {
    lastSceneFlowProjectId = selectedProjectId;
    loadSceneFlow(selectedProjectId);
  }
  $: if (sessionReady && !embeddingsStartAttempted) {
    embeddingsStartAttempted = true;
    void checkEmbeddingsService();
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

  // Presence: subscribe to the selected project whenever it changes
  let isSessionOwner = false;
  $: if (sessionReady && selectedProjectId && selectedProjectId !== lastPresenceProjectId) {
    lastPresenceProjectId = selectedProjectId;
    peerPresence = new Map();
    myPresenceUserId = null;
    isSessionOwner = false;
    sendCommand("Session.Subscribe", { projectId: selectedProjectId, clientToken: clientId }).then((result) => {
      myPresenceUserId = result.myUserId || null;
      isSessionOwner = result.isOwner === true;
      const list = result.presence || [];
      const next = new Map();
      for (const p of list) {
        if (p.userId && p.userId !== myPresenceUserId) {
          next.set(p.userId, p);
        }
      }
      peerPresence = next;
    }).catch(() => {});
  }
  $: if (!selectedProjectId) {
    lastPresenceProjectId = "";
    peerPresence = new Map();
    myPresenceUserId = null;
    isSessionOwner = false;
  }

  // Presence: send viewport updates when our view changes (debounced 200 ms)
  $: if (sessionReady && selectedProjectId && sceneFlowViewBox?.width > 0 && sceneFlowViewBox?.height > 0) {
    if (presenceViewportDebounceTimer) clearTimeout(presenceViewportDebounceTimer);
    presenceViewportDebounceTimer = setTimeout(() => {
      sendCommand("Presence.Update", {
        projectId: selectedProjectId,
        viewport: {
          x: sceneFlowViewBox.x,
          y: sceneFlowViewBox.y,
          width: sceneFlowViewBox.width,
          height: sceneFlowViewBox.height
        }
      }).catch(() => {});
    }, 200);
  }

  $: if (!selectedProjectId) {
    lastProjectConfigProjectId = "";
    lastPluginInterfacesProjectId = "";
    projectConfig = null;
    projectConfigDraft = null;
    projectConfigError = "";
    projectConfigLoading = false;
    projectConfigSaved = null;
    projectConfigPending = false;
    pluginInterfaces = [];
    pluginInterfacesError = "";
    pluginInterfacesLoading = false;
    scriptText = "";
    scriptDraft = "";
    scriptVersion = null;
    scriptStatus = "";
    scriptError = "";
    scriptParseOk = true;
    scriptDiagnostics = [];
    semanticDoc = null;
    semanticAnnotations = [];
    semanticLoading = false;
    semanticAnalyzeBusy = false;
    semanticStatus = "";
    semanticError = "";
    semanticSourceText = "";
    semanticDirty = false;
    scriptLiveLast = "";
    scriptScenes = [];
    scriptScenesError = "";
    scriptScenesLoading = false;
    scriptElements = { acticon: [], gesticon: [], visicon: [] };
    scriptElementsError = "";
    scriptElementsLoading = false;
    sceneNamesReady = false;
    previousSceneNames = new Set();
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
    runtimeStopRequested = false;
    activityNodeCounts = new Map();
    activityEdgeHits = new Map();
    runtimeVizRateDraft = String(RUNTIME_VIZ_RATE_DEFAULT);
    runtimeVizBurstDraft = String(RUNTIME_VIZ_BURST_DEFAULT);
    runtimeVizError = "";
    runtimeVizCalibrationStatus = "";
    runtimeVizCalibrationBusy = false;
    runtimeVizBusy = false;
    if (runtimeVizApplyTimer) {
      clearTimeout(runtimeVizApplyTimer);
      runtimeVizApplyTimer = null;
    }
    resetEventOverproductionMonitoring({ resetMute: true });
    eventOverprodMessage = "";
    eventOverprodRate = "";
    eventOverprodFlowLabel = "";
    eventOverprodFlowRate = "";
    resetProjectLoadState();
    projectLoadAttempted = false;
    projectLoadProjectId = "";
    pluginBadgeState = {};
    pluginBadgeDrag = null;
    pluginBadgeResize = null;
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
      if (joiningViaUrl) {
        joiningViaUrl = false;
        window.history.replaceState({}, "", window.location.pathname);
      }
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
    stopSemanticPreview();
    if (timeoutInspectorApplyTimer) {
      clearTimeout(timeoutInspectorApplyTimer);
      timeoutInspectorApplyTimer = null;
    }
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

  function hasRecentPlugins(project) {
    return Array.isArray(project?.stats?.plugins) && project.stats.plugins.length > 0;
  }

  function isRecentPinned(path) {
    const value = normalizeRecentPath(path);
    return !!value && recentPinnedProjects.some((entry) => entry.path === value);
  }

  function toggleRecentPinned(path) {
    const value = normalizeRecentPath(path);
    if (!value) return;
    if (isRecentPinned(value)) {
      recentPinnedProjects = recentPinnedProjects.filter((entry) => entry.path !== value);
    } else {
      recentPinnedProjects = [
        { path: value, pinnedAt: Date.now() },
        ...recentPinnedProjects.filter((entry) => entry.path !== value)
      ];
    }
  }

  function formatRecentScenesStats(project) {
    const total = Number(project?.stats?.scenes ?? 0);
    const entries = Array.isArray(project?.stats?.sceneLanguages)
      ? project.stats.sceneLanguages
          .map((entry) => ({
            language: String(entry?.language ?? "").trim(),
            count: Number(entry?.count ?? 0)
          }))
          .filter((entry) => entry.count > 0)
      : [];
    if (!entries.length) {
      return `Scenes ${total}`;
    }
    if (entries.length === 1) {
      const label = entries[0].language || "?";
      return `Scenes ${total} (${label})`;
    }
    const parts = entries.map((entry) => `${entry.count} (${entry.language || "?"})`);
    return `Scenes ${total} ・ ${parts.join(" ・ ")}`;
  }

  function formatRecentRelativeDate(dateText) {
    const raw = String(dateText || "").trim();
    if (!raw) return "";
    const parsed = Date.parse(raw);
    if (!Number.isFinite(parsed)) return raw;
    const now = Date.now();
    const diffMs = now - parsed;
    const future = diffMs < 0;
    const absMs = Math.abs(diffMs);
    const minute = 60 * 1000;
    const hour = 60 * minute;
    const day = 24 * hour;
    if (absMs < minute) return future ? "in <1m" : "just now";
    if (absMs < hour) {
      const value = Math.floor(absMs / minute);
      return future ? `in ${value}m` : `${value}m ago`;
    }
    if (absMs < day) {
      const value = Math.floor(absMs / hour);
      return future ? `in ${value}h` : `${value}h ago`;
    }
    if (absMs < 30 * day) {
      const value = Math.floor(absMs / day);
      return future ? `in ${value}d` : `${value}d ago`;
    }
    return new Date(parsed).toLocaleDateString();
  }

  function formatTutorialMeta(project) {
    const level = String(project?.level || project?.difficulty || "").trim();
    const duration = String(project?.duration || project?.estimatedDuration || "").trim();
    const tags = Array.isArray(project?.tags) ? project.tags.map((tag) => String(tag || "").trim()).filter(Boolean).slice(0, 3) : [];
    const parts = [];
    if (level) parts.push(level);
    if (duration) parts.push(duration);
    if (tags.length) parts.push(tags.join(", "));
    return parts.join(" · ");
  }

  async function loadTutorials() {
    tutorialsLoading = true;
    tutorialsError = "";
    try {
      const data = await apiGet("/api/v1/projects/tutorials");
      tutorials = data.projects || [];
    } catch (err) {
      tutorialsError = err?.message || "Failed to load tutorials.";
      tutorials = [];
    } finally {
      tutorialsLoading = false;
    }
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

  function parentDirectory(pathValue) {
    const raw = String(pathValue || "").trim();
    if (!raw) return "";
    const normalized = raw.replace(/[\\/]+$/, "");
    const lastSeparator = Math.max(normalized.lastIndexOf("/"), normalized.lastIndexOf("\\"));
    if (lastSeparator < 0) return "";
    if (/^[A-Za-z]:[\\/]/.test(normalized) && lastSeparator === 2) {
      return normalized.slice(0, 3);
    }
    if (lastSeparator === 0) return normalized.startsWith("/") ? "/" : "";
    return normalized.slice(0, lastSeparator);
  }

  function useSuggestedBaseDir(path) {
    if (!path) return;
    newBaseDir = path;
    createProjectError = "";
  }

  function useSuggestedSaveAsBaseDir(path) {
    if (!path) return;
    saveAsPath = path;
    saveAsError = "";
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

  $: suggestedBaseDirs = (() => {
    const dirs = [];
    const seen = new Set();
    for (const project of recent) {
      const baseDir = parentDirectory(project?.path);
      if (!baseDir || seen.has(baseDir)) continue;
      seen.add(baseDir);
      dirs.push(baseDir);
      if (dirs.length >= 8) break;
    }
    return dirs;
  })();

  $: filteredRecent = (() => {
    const needle = (recentSearchQuery || "").trim().toLowerCase();
    const pinnedOrder = recentPinnedProjects
      .slice()
      .sort((a, b) => (b.pinnedAt || 0) - (a.pinnedAt || 0))
      .map((entry) => entry.path);
    const pinnedSet = new Set(pinnedOrder);
    const matching = recent.filter((project) => {
      const path = normalizeRecentPath(project?.path);
      if (recentFilterMode === "android" && !(project?.androidProject === true || project?.stats?.androidProject === true)) {
        return false;
      }
      if (recentFilterMode === "plugins" && !hasRecentPlugins(project)) {
        return false;
      }
      if (recentFilterMode === "pinned" && !isRecentPinned(path)) {
        return false;
      }
      if (!needle) {
        return true;
      }
      const pluginText = hasRecentPlugins(project)
        ? project.stats.plugins
            .map((plugin) => `${plugin?.name || ""} ${plugin?.className || ""}`)
            .join(" ")
        : "";
      const haystack = `${project?.name || ""} ${project?.path || ""} ${pluginText}`.toLowerCase();
      return haystack.includes(needle);
    });
    if (recentFilterMode === "pinned") {
      const byPath = new Map(matching.map((project) => [normalizeRecentPath(project?.path), project]));
      const orderedPinned = [];
      for (const path of pinnedOrder) {
        const project = byPath.get(path);
        if (project) orderedPinned.push(project);
      }
      return orderedPinned;
    }
    if (recentFilterMode !== "all") {
      return matching;
    }
    const byPath = new Map(matching.map((project) => [normalizeRecentPath(project?.path), project]));
    const orderedPinned = [];
    for (const path of pinnedOrder) {
      const project = byPath.get(path);
      if (project) orderedPinned.push(project);
    }
    const regular = matching.filter((project) => !pinnedSet.has(normalizeRecentPath(project?.path)));
    return [...orderedPinned, ...regular];
  })();

  $: recentHeaderCountLabel =
    filteredRecent.length === recent.length ? `${recent.length}` : `${filteredRecent.length} / ${recent.length}`;

  $: localStorage.setItem(NEW_PROJECT_BASE_DIR_KEY, newBaseDir || "");
  $: localStorage.setItem(NEW_PROJECT_BASE_DIR_PANEL_KEY, String(baseDirSuggestionsExpanded));
  $: localStorage.setItem(RECENT_PINNED_PATHS_KEY, JSON.stringify(recentPinnedProjects));

  async function saveProject(projectId, { skipScriptApply = false, saveSemantic = true } = {}) {
    if (!projectId || projectSaving) return false;
    let ok = false;
    projectSaving = true;
    try {
      if (!skipScriptApply && scriptDirty && scriptParseOk && !scriptError && scriptDiagnostics.length === 0) {
        const applied = await applyScript();
        if (!applied) {
          projectSaving = false;
          return false;
        }
      }
      if (saveSemantic) {
        await saveSemanticDraft(projectId);
      }
      await persistSemanticAnalysisSettings();
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

  async function saveSemanticDraft(projectId) {
    if (!projectId || !semanticDirty) {
      return true;
    }
    const payload = {
      semantic: {
        version: semanticDoc?.version ?? 1,
        annotations: Array.isArray(semanticAnnotations) ? semanticAnnotations : []
      }
    };
    await apiPut(`/api/v1/projects/${projectId}/semantic`, payload);
    semanticDoc = payload.semantic;
    semanticDirty = false;
    return true;
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
    const ok = await saveProject(selectedProjectId, { skipScriptApply: true, saveSemantic: true });
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

  async function removeProjectFromRuntime(projectId) {
    if (!projectId) return;
    await apiPost(`/api/v1/projects/${projectId}/close`, {});
    projects = projects.filter((project) => project.projectId !== projectId);
    if (selectedProjectId === projectId) {
      selectedProjectId = "";
    }
  }

  function resetProjectSelectionTracking() {
    projectLoadAttempted = false;
    projectLoadProjectId = "";
    lastConfigProjectId = "";
    lastProjectConfigProjectId = "";
    lastScriptProjectId = "";
    lastSceneFlowProjectId = "";
    lastRuntimeProjectId = "";
    lastRuntimeSuperNodeId = "";
    resetProjectLoadState();
  }

  async function reopenProjectAfterFirstSave(projectId, projectPath) {
    if (!projectId || !projectPath) return false;
    if (selectedProjectId === projectId && runtimeState !== "stopped") {
      await runRuntimeCommand("Runtime.Stop");
    }
    await removeProjectFromRuntime(projectId);
    resetProjectSelectionTracking();
    recentLoaded = false;
    showEditor = false;
    await tick();
    const reopened = await openProject(projectPath, { surfaceError: false });
    if (!reopened?.ok) {
      saveAsError = reopened?.error || "Project was saved, but reopening failed.";
      await loadProjects();
      await loadRecent();
      return false;
    }
    return true;
  }

  async function saveAsProject(projectId, overridePath, overrideName) {
    const targetPath = (overridePath || saveAsPath || "").trim();
    const targetName = (overrideName || saveAsName || "").trim();
    if (!projectId || !targetPath || projectSaving) return false;
    if (!targetName) {
      saveAsError = "Project name is required.";
      return false;
    }
    projectSaving = true;
    const firstSave = projectRequiresSaveAs;
    try {
      await saveSemanticDraft(projectId);
      await persistSemanticAnalysisSettings();
      const response = await apiPost(`/api/v1/projects/${projectId}/save-as`, {
        path: targetPath,
        name: targetName
      });
      saveAsPath = "";
      saveAsName = "";
      saveAsError = "";
      if (firstSave) {
        const reopened = await reopenProjectAfterFirstSave(projectId, response?.path || "");
        if (!reopened) {
          return false;
        }
      } else {
        await loadProjects();
        await loadRecent();
      }
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

  function clampNumber(value, min, max) {
    const parsed = Number(value);
    if (!Number.isFinite(parsed)) return min;
    return Math.max(min, Math.min(max, parsed));
  }

  function readRuntimeVizProjectInt(configLike, key, fallback) {
    const raw = configLike?.semanticServices?.[key];
    const text = String(raw ?? "").trim();
    if (!text) return fallback;
    const parsed = Number.parseInt(text, 10);
    return Number.isFinite(parsed) ? parsed : fallback;
  }

  function syncRuntimeVizGuardFromProjectConfig(configLike = projectConfigDraft || projectConfig || {}) {
    const cfg = normalizeProjectConfig(configLike || {});
    const rate = clampNumber(
      readRuntimeVizProjectInt(cfg, "runtimeVizRate", RUNTIME_VIZ_RATE_DEFAULT),
      RUNTIME_VIZ_RATE_MIN,
      RUNTIME_VIZ_RATE_MAX
    );
    const burst = clampNumber(
      readRuntimeVizProjectInt(cfg, "runtimeVizBurst", RUNTIME_VIZ_BURST_DEFAULT),
      RUNTIME_VIZ_BURST_MIN,
      RUNTIME_VIZ_BURST_MAX
    );
    runtimeVizRateDraft = String(Math.round(rate));
    runtimeVizBurstDraft = String(Math.round(burst));
  }

  function hasRuntimeVizGuardInProjectConfig(configLike = projectConfigDraft || projectConfig || {}) {
    const services = configLike?.semanticServices || {};
    const rateRaw = String(services.runtimeVizRate ?? "").trim();
    const burstRaw = String(services.runtimeVizBurst ?? "").trim();
    if (!rateRaw || !burstRaw) {
      return false;
    }
    const rate = Number.parseInt(rateRaw, 10);
    const burst = Number.parseInt(burstRaw, 10);
    return (
      Number.isFinite(rate) &&
      Number.isFinite(burst) &&
      rate >= RUNTIME_VIZ_RATE_MIN &&
      rate <= RUNTIME_VIZ_RATE_MAX &&
      burst >= RUNTIME_VIZ_BURST_MIN &&
      burst <= RUNTIME_VIZ_BURST_MAX
    );
  }

  function scheduleRuntimeVizGuardApply() {
    if (!projectConfigDialogOpen || !selectedProjectId || runtimeVizBusy || runtimeVizCalibrationBusy) {
      return;
    }
    if (runtimeVizApplyTimer) {
      clearTimeout(runtimeVizApplyTimer);
    }
    runtimeVizApplyTimer = setTimeout(() => {
      runtimeVizApplyTimer = null;
      applyRuntimeVizGuardConfig();
    }, 350);
  }

  function updateRuntimeVizGuardField(field, value) {
    if (field === "rate") {
      runtimeVizRateDraft = value;
    } else {
      runtimeVizBurstDraft = value;
    }
    runtimeVizError = "";
    runtimeVizCalibrationStatus = "";
    scheduleRuntimeVizGuardApply();
  }

  async function applyRuntimeVizGuardConfig({ silentNoChange = false } = {}) {
    if (!selectedProjectId || runtimeVizBusy) return false;
    const rate = Number.parseInt(String(runtimeVizRateDraft || "").trim(), 10);
    const burst = Number.parseInt(String(runtimeVizBurstDraft || "").trim(), 10);
    if (!Number.isFinite(rate) || rate < RUNTIME_VIZ_RATE_MIN || rate > RUNTIME_VIZ_RATE_MAX) {
      runtimeVizError = `Rate must be between ${RUNTIME_VIZ_RATE_MIN} and ${RUNTIME_VIZ_RATE_MAX}.`;
      return false;
    }
    if (!Number.isFinite(burst) || burst < RUNTIME_VIZ_BURST_MIN || burst > RUNTIME_VIZ_BURST_MAX) {
      runtimeVizError = `Burst must be between ${RUNTIME_VIZ_BURST_MIN} and ${RUNTIME_VIZ_BURST_MAX}.`;
      return false;
    }
    const base = normalizeProjectConfig(projectConfigDraft || projectConfig || {});
    const currentRate = readRuntimeVizProjectInt(base, "runtimeVizRate", RUNTIME_VIZ_RATE_DEFAULT);
    const currentBurst = readRuntimeVizProjectInt(base, "runtimeVizBurst", RUNTIME_VIZ_BURST_DEFAULT);
    if (rate === currentRate && burst === currentBurst) {
      runtimeVizError = "";
      return true;
    }
    runtimeVizBusy = true;
    runtimeVizError = "";
    try {
      const nextConfig = {
        ...base,
        semanticServices: {
          ...(base.semanticServices || {}),
          runtimeVizRate: String(rate),
          runtimeVizBurst: String(burst)
        }
      };
      const response = await sendCommand("ProjectConfig.Update", {
        projectId: selectedProjectId,
        config: nextConfig
      });
      projectConfig = normalizeProjectConfig(response.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      syncLLMSelectionsFromConfig(projectConfig);
      syncSemanticAnalysisSettingsFromConfig(projectConfig);
      syncRuntimeVizGuardFromProjectConfig(projectConfig);
      projectConfigSaved = response.saved ?? null;
      projectConfigPending = response.pending === true;
      if (response.pending === true) {
        projects = projects.map((project) =>
          project.projectId === selectedProjectId ? { ...project, dirty: true } : project
        );
      }
      if (!silentNoChange) {
        runtimeVizCalibrationStatus = runtimeVizCalibrationStatus || "Runtime visualization guard updated.";
      }
      return true;
    } catch (err) {
      runtimeVizError = err.message || "Failed to update runtime visualization guard.";
      return false;
    } finally {
      runtimeVizBusy = false;
    }
  }

  async function calibrateRuntimeVizGuard({ automated = false } = {}) {
    if (runtimeVizCalibrationBusy || runtimeVizBusy) return false;
    runtimeVizCalibrationBusy = true;
    runtimeVizCalibrationStatus = "";
    runtimeVizError = "";
    try {
      const start = performance.now();
      const durationMs = 1400;
      const end = start + durationMs;
      const scratch = new Map();
      let ops = 0;
      while (performance.now() < end) {
        for (let i = 0; i < 2500; i += 1) {
          const idx = ops & 255;
          const key = `n${idx}`;
          scratch.set(key, (scratch.get(key) || 0) + 1);
          if ((ops & 31) === 0) {
            scratch.delete(`n${(idx + 67) & 255}`);
          }
          ops += 1;
        }
        await new Promise((resolve) => requestAnimationFrame(resolve));
      }
      const elapsedSec = Math.max(0.2, (performance.now() - start) / 1000);
      const opsPerSec = ops / elapsedSec;
      const rate = Math.round(clampNumber(opsPerSec * 0.00035, RUNTIME_VIZ_RATE_MIN, RUNTIME_VIZ_RATE_MAX));
      const burst = Math.round(clampNumber(rate * 2, RUNTIME_VIZ_BURST_MIN, RUNTIME_VIZ_BURST_MAX));
      runtimeVizRateDraft = String(rate);
      runtimeVizBurstDraft = String(burst);
      runtimeVizCalibrationStatus = `Calibrated for this computer: ${Math.round(opsPerSec)} synthetic ops/s.`;
      const applied = await applyRuntimeVizGuardConfig({ silentNoChange: automated });
      if (applied && automated) {
        statusMessage = "Runtime visualization guard auto-calibrated for this computer. Save project to persist values.";
      }
      return applied;
    } catch (err) {
      runtimeVizError = err.message || "Calibration failed.";
      return false;
    } finally {
      runtimeVizCalibrationBusy = false;
    }
  }

  async function loadProjectConfig(projectId) {
    if (!projectId) return;
    projectConfigLoading = true;
    projectConfigError = "";
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/project-config`);
      projectConfig = normalizeProjectConfig(data.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      syncLLMSelectionsFromConfig(projectConfig);
      syncSemanticAnalysisSettingsFromConfig(projectConfig);
      syncRuntimeVizGuardFromProjectConfig(projectConfig);
      projectConfigSaved = data.saved ?? null;
      projectConfigPending = data.pending === true;
    } catch (err) {
      projectConfigError = err.message || "Failed to load project config.";
    } finally {
      projectConfigLoading = false;
    }
  }

  async function loadPluginInterfaces(projectId) {
    if (!projectId) return;
    pluginInterfacesLoading = true;
    pluginInterfacesError = "";
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/plugin-interfaces`);
      pluginInterfaces = Array.isArray(data.interfaces) ? data.interfaces : [];
    } catch (err) {
      pluginInterfacesError = err.message || "Failed to load plugin interfaces.";
      pluginInterfaces = [];
    } finally {
      pluginInterfacesLoading = false;
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
          className: device?.className ?? "",
          displayName: simpleClassName(device?.className ?? "")
        }))
        .filter((device) => device.className)
        .sort((a, b) => {
          const byDisplay = String(a.displayName || "").localeCompare(String(b.displayName || ""), undefined, {
            sensitivity: "base"
          });
          if (byDisplay !== 0) return byDisplay;
          return String(a.className || "").localeCompare(String(b.className || ""), undefined, {
            sensitivity: "base"
          });
        });
    } catch (err) {
      availableDevicesError = err.message || "Failed to load device list.";
    } finally {
      availableDevicesLoading = false;
    }
  }

  function simpleClassName(value) {
    const text = String(value || "").trim();
    if (!text) return "";
    const parts = text.split(".");
    return parts[parts.length - 1] || text;
  }

  function isAndroidCompatibleDeviceClass(className) {
    const full = String(className || "").trim().toLowerCase();
    if (!full) return false;
    const simple = simpleClassName(full).toLowerCase();
    return androidCompatibleDeviceKeys.has(full) || androidCompatibleDeviceKeys.has(simple);
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
      return;
    }
    try {
      const response = await sendCommand("ProjectConfig.Update", {
        projectId: selectedProjectId,
        config: projectConfigDraft
      });
      projectConfig = normalizeProjectConfig(response.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      syncLLMSelectionsFromConfig(projectConfig);
      syncSemanticAnalysisSettingsFromConfig(projectConfig);
      syncRuntimeVizGuardFromProjectConfig(projectConfig);
      projectConfigSaved = response.saved ?? null;
      projectConfigPending = response.pending === true;
      // Mark project as dirty (has unsaved changes) but NOT pending (which would require save-as)
      if (response.pending === true) {
        projects = projects.map((project) =>
          project.projectId === selectedProjectId ? { ...project, dirty: true } : project
        );
      }
    } catch (err) {
      projectConfigError = err.message || "Failed to update project config.";
    }
  }

  function scheduleProjectConfigApply() {
    if (
      !projectConfigDialogOpen ||
      !selectedProjectId ||
      !wsConnected ||
      !projectConfigDraft ||
      projectConfigLoading
    ) {
      return;
    }
    if (projectConfigApplyTimer) {
      clearTimeout(projectConfigApplyTimer);
    }
    projectConfigApplyTimer = setTimeout(() => {
      projectConfigApplyTimer = null;
      applyProjectConfig();
    }, 350);
  }

  function openProjectConfigDialog() {
    if (!selectedProjectId) return;
    rememberFocus();
    projectConfigDialogOpen = true;
    projectConfigPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
    projectConfigSelection = { type: "devices" };
    projectConfigGeneralExpanded = true;
    projectConfigLlmExpanded = true;
    projectConfigDevicesExpanded = true;
    projectConfigError = "";
    projectConfigSaved = null;
    projectConfigPending = false;
    runtimeVizError = "";
    runtimeVizCalibrationStatus = "";
    syncRuntimeVizGuardFromProjectConfig();
    loadProjectConfig(selectedProjectId);
    loadConfig(selectedProjectId);
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
    projectConfigSelection = { type: "devices" };
    projectConfigGeneralExpanded = true;
    projectConfigLlmExpanded = true;
    projectConfigDevicesExpanded = true;
    projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
    projectConfigNewAgent = { name: "", device: "" };
    projectConfigNewFeature = { key: "", value: "" };
    if (projectConfigApplyTimer) {
      clearTimeout(projectConfigApplyTimer);
      projectConfigApplyTimer = null;
    }
    if (runtimeVizApplyTimer) {
      clearTimeout(runtimeVizApplyTimer);
      runtimeVizApplyTimer = null;
    }
    runtimeVizError = "";
    runtimeVizCalibrationStatus = "";
    document.body.style.overflow = projectConfigPrevBodyOverflow;
    restoreFocus();
  }

  function projectConfigSectionStyle(section) {
    const visibleCount =
      (projectConfigGeneralExpanded ? 1 : 0) +
      (projectConfigLlmExpanded ? 1 : 0) +
      (projectConfigDevicesExpanded ? 1 : 0);
    if (visibleCount <= 1) {
      return "flex:1 1 0; min-height:0;";
    }
    const weights = {
      general: 3,
      llm: 2,
      devices: 5
    };
    const weight = weights[section] || 1;
    return `flex:${weight} 1 0; min-height:0;`;
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
    scheduleProjectConfigApply();
  }

  function updateAndroidProject(value) {
    projectConfigDraft = {
      ...projectConfigDraft,
      androidProject: value === true
    };
    scheduleProjectConfigApply();
  }

  function updateSceneTitleConcepts(value) {
    const entries = String(value || "")
      .split(/\r?\n/)
      .map((line) => line.trim())
      .filter(Boolean);
    projectConfigDraft = {
      ...projectConfigDraft,
      sceneTitleConcepts: entries
    };
    scheduleProjectConfigApply();
  }

  function updateSemanticServiceField(field, value) {
    projectConfigDraft = {
      ...projectConfigDraft,
      semanticServices: {
        ...(projectConfigDraft?.semanticServices || projectConfigView?.semanticServices || {}),
        [field]: value
      }
    };
    scheduleProjectConfigApply();
  }

  function resolveAvailableDeviceClass(name) {
    const needle = (name || "").trim().toLowerCase();
    if (!needle || !availableDevices?.length) return "";
    const match =
      availableDevices.find((device) => (device?.name || "").trim().toLowerCase() === needle) ||
      availableDevices.find((device) => (device?.className || "").trim().toLowerCase() === needle) ||
      availableDevices.find((device) => (device?.displayName || "").trim().toLowerCase() === needle);
    return match?.className || "";
  }

  function deriveDeviceNameFromClass(className) {
    return simpleClassName(className);
  }

  function updatePluginField(index, field, value) {
    const plugins = projectConfigPlugins.map((plugin, idx) =>
      idx === index ? { ...plugin, [field]: value } : plugin
    );
    projectConfigDraft = {
      ...projectConfigDraft,
      plugins
    };
    scheduleProjectConfigApply();
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
    scheduleProjectConfigApply();
  }

  function updateAgentField(index, field, value) {
    const agents = projectConfigAgents.map((agent, idx) =>
      idx === index ? { ...agent, [field]: value } : agent
    );
    projectConfigDraft = {
      ...projectConfigDraft,
      agents
    };
    scheduleProjectConfigApply();
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
    scheduleProjectConfigApply();
  }

  function updateAgentFeature(agentIndex, featureIndex, field, value) {
    const agents = [...projectConfigAgents];
    const agent = agents[agentIndex];
    if (!agent) return;
    const features = updateFeatureList(agent.features, featureIndex, field, value);
    agents[agentIndex] = { ...agent, features };
    projectConfigDraft = { ...projectConfigDraft, agents };
    scheduleProjectConfigApply();
  }

  function updatePlayerFeature(featureIndex, field, value) {
    const features = updateFeatureList(projectConfigPlayer.features, featureIndex, field, value);
    projectConfigDraft = {
      ...projectConfigDraft,
      player: { features }
    };
    scheduleProjectConfigApply();
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
    scheduleProjectConfigApply();
  }

  function removePluginFeature(pluginIndex, featureIndex) {
    const plugins = [...projectConfigPlugins];
    const plugin = plugins[pluginIndex];
    if (!plugin) return;
    const features = plugin.features.filter((_, idx) => idx !== featureIndex);
    plugins[pluginIndex] = { ...plugin, features };
    projectConfigDraft = { ...projectConfigDraft, plugins };
    scheduleProjectConfigApply();
  }

  function removeAgentFeature(agentIndex, featureIndex) {
    const agents = [...projectConfigAgents];
    const agent = agents[agentIndex];
    if (!agent) return;
    const features = agent.features.filter((_, idx) => idx !== featureIndex);
    agents[agentIndex] = { ...agent, features };
    projectConfigDraft = { ...projectConfigDraft, agents };
    scheduleProjectConfigApply();
  }

  function removePlayerFeature(featureIndex) {
    const features = projectConfigPlayer.features.filter((_, idx) => idx !== featureIndex);
    projectConfigDraft = {
      ...projectConfigDraft,
      player: { features }
    };
    scheduleProjectConfigApply();
  }

  async function addPlugin() {
    const name = (projectConfigNewPlugin.name || "").trim();
    const className =
      (projectConfigNewPlugin.className || "").trim() || resolveAvailableDeviceClass(name);
    const type = (projectConfigNewPlugin.type || "device").trim() || "device";
    if (!name || !className) {
      projectConfigError = "Module and name are required.";
      return;
    }
    if (projectConfigPlugins.some((plugin) => plugin.name === name)) {
      projectConfigError = "Device name already exists.";
      return;
    }

    // Request plugin template with pre-populated required keys from backend
    let next = {
      name,
      className,
      type,
      load: projectConfigNewPlugin.load !== false,
      features: []
    };
    let sceneflowVarsToCreate = [];
    let hasTemplates = false;
    try {
      const response = await sendCommand("ProjectConfig.Plugin.Create", {
        name,
        className,
        type,
        load: projectConfigNewPlugin.load !== false
      });
      if (response?.plugin) {
        // Use the backend-provided plugin with pre-populated features
        next = {
          name: response.plugin.name || name,
          className: response.plugin.className || className,
          type: response.plugin.type || type,
          load: response.plugin.load !== false,
          features: Array.isArray(response.plugin.features)
            ? response.plugin.features.map((f) => ({ key: f.key || "", value: f.value || "" }))
            : []
        };
      }
      // Collect sceneflow variables to create
      if (Array.isArray(response?.sceneflowVars)) {
        sceneflowVarsToCreate = response.sceneflowVars;
      }
      // Check if plugin has templates to install
      if (response?.templates) {
        hasTemplates = true;
      }
    } catch (err) {
      // Fallback to empty features if backend call fails
      console.warn("Failed to get plugin template:", err);
    }

    const plugins = [...projectConfigPlugins, next];
    projectConfigDraft = { ...projectConfigDraft, plugins };
    projectConfigNewPlugin = { name: "", className: "", type: "device", load: true };
    selectProjectConfig({ type: "plugin", pluginIndex: plugins.length - 1 });
    scheduleProjectConfigApply();

    // Auto-create SceneFlow variables defined by the plugin
    if (sceneflowVarsToCreate.length > 0 && selectedProjectId) {
      for (const varDef of sceneflowVarsToCreate) {
        await createSceneFlowVariableIfNotExists(varDef.name, varDef.type);
      }
    }

    // Install template files if the plugin has any
    if (hasTemplates && selectedProjectId) {
      await installPluginTemplates(className);
    }
  }

  async function installPluginTemplates(className) {
    if (!className || !selectedProjectId) return;
    try {
      const response = await sendCommand("Project.Templates.Install", {
        projectId: selectedProjectId,
        className: className
      });
      if (response?.createdFiles?.length > 0) {
        console.log("Created template files:", response.createdFiles);
      }
      if (response?.skippedFiles?.length > 0) {
        console.log("Skipped existing files:", response.skippedFiles);
      }
    } catch (err) {
      console.warn("Failed to install plugin templates:", err);
    }
  }

  async function createSceneFlowVariableIfNotExists(varName, varType) {
    if (!varName || !varType || !selectedProjectId) return;

    // Check if variable already exists in current sceneFlow (at root level)
    const existingVars = sceneFlow?.variables || [];
    const exists = existingVars.some((v) => v.name === varName);
    if (exists) {
      console.log(`SceneFlow variable '${varName}' already exists, skipping creation`);
      return;
    }

    // Create variable at root SceneFlow level (nodeId empty = root)
    try {
      await runSceneFlowCommand("SceneFlow.Node.VarDef.Add", {
        projectId: selectedProjectId,
        nodeId: "",  // Root SceneFlow
        superNodeId: sceneFlow?.superNodeId || "",
        varDef: {
          name: varName,
          type: varType
          // No expression needed for Event types; for others, backend provides default
        }
      });
      console.log(`Created SceneFlow variable '${varName}' of type '${varType}'`);
    } catch (err) {
      console.warn(`Failed to create SceneFlow variable '${varName}':`, err);
    }
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
    scheduleProjectConfigApply();
  }

  function removePlugin(index) {
    if (!projectConfigDraft) {
      projectConfigDraft = cloneProjectConfig(projectConfigView);
    }
    const plugin = projectConfigPlugins[index];
    if (!plugin) return;
    const plugins = projectConfigPlugins.filter((_, idx) => idx !== index);
    const agents = projectConfigAgents.filter((agent) => agent.device !== plugin.name);
    projectConfigDraft = { ...projectConfigDraft, plugins, agents };
    if (projectConfigSelection.type === "plugin" && projectConfigSelection.pluginIndex === index) {
      projectConfigSelection = { type: "project" };
    }
    scheduleProjectConfigApply();
  }

  function removeAgent(index) {
    if (!projectConfigDraft) {
      projectConfigDraft = cloneProjectConfig(projectConfigView);
    }
    const agents = projectConfigAgents.filter((_, idx) => idx !== index);
    projectConfigDraft = { ...projectConfigDraft, agents };
    if (projectConfigSelection.type === "agent" && projectConfigSelection.agentIndex === index) {
      projectConfigSelection = { type: "project" };
    }
    scheduleProjectConfigApply();
  }

  // --- LLM config functions ---

  function getLLMFeature(llm, key, fallback) {
    const f = (llm?.features || []).find((f) => f.key === key);
    return f ? f.value : (fallback ?? "");
  }

  function setLLMFeature(llmIndex, key, value) {
    const llms = [...projectConfigLLMs];
    const llm = llms[llmIndex];
    if (!llm) return;
    const features = llm.features.filter((f) => f.key !== key);
    if (value !== "" && value != null) {
      features.push({ key, value });
    }
    llms[llmIndex] = { ...llm, features };
    projectConfigDraft = { ...projectConfigDraft, llms };
    scheduleProjectConfigApply();
  }

  function addLLM() {
    const name = (llmNewName || "").trim();
    if (!name) {
      projectConfigError = "LLM name is required.";
      return;
    }
    if (projectConfigLLMs.some((l) => l.name.toLowerCase() === name.toLowerCase())) {
      projectConfigError = "LLM name already exists.";
      return;
    }
    const entry = {
      name,
      features: [
        { key: "baseUrl", value: "http://localhost:8234/v1/" },
        { key: "temperature", value: "0.7" },
        { key: "timeout", value: "30" }
      ]
    };
    const llms = [...projectConfigLLMs, entry];
    projectConfigDraft = { ...projectConfigDraft, llms };
    llmNewName = "";
    llmExpandedIndex = llms.length - 1;
    scheduleProjectConfigApply();
  }

  function removeLLM(index) {
    const llms = projectConfigLLMs.filter((_, idx) => idx !== index);
    projectConfigDraft = { ...projectConfigDraft, llms };
    if (llmExpandedIndex === index) {
      llmExpandedIndex = -1;
    } else if (llmExpandedIndex > index) {
      llmExpandedIndex--;
    }
    scheduleProjectConfigApply();
  }

  function updateLLMName(index, value) {
    const llms = [...projectConfigLLMs];
    llms[index] = { ...llms[index], name: value };
    projectConfigDraft = { ...projectConfigDraft, llms };
    scheduleProjectConfigApply();
  }

  async function fetchLLMModels(index) {
    const llm = projectConfigLLMs[index];
    if (!llm) return;
    const baseUrl = getLLMFeature(llm, "baseUrl", "").trim();
    if (!baseUrl) {
      llmTestResult = { ...llmTestResult, [index]: { ok: false, error: "Base URL is empty" } };
      return;
    }
    llmModelsLoading = { ...llmModelsLoading, [index]: true };
    llmTestResult = { ...llmTestResult, [index]: null };
    try {
      const data = await apiPost("/api/v1/llm/models", {
        baseUrl,
        apiKey: getLLMFeature(llm, "apiKey", "") || null
      });
      llmModels = { ...llmModels, [index]: data.models || [] };
      llmModelsLoading = { ...llmModelsLoading, [index]: false };
    } catch (err) {
      llmModels = { ...llmModels, [index]: [] };
      llmModelsLoading = { ...llmModelsLoading, [index]: false };
      llmTestResult = { ...llmTestResult, [index]: { ok: false, error: err.message || "Failed" } };
    }
  }

  async function testLLMConnection(index) {
    const llm = projectConfigLLMs[index];
    if (!llm) return;
    const baseUrl = getLLMFeature(llm, "baseUrl", "").trim();
    if (!baseUrl) {
      llmTestResult = { ...llmTestResult, [index]: { ok: false, error: "Base URL is empty" } };
      return;
    }
    llmTestResult = { ...llmTestResult, [index]: null };
    try {
      const data = await apiPost("/api/v1/llm/test", {
        baseUrl,
        apiKey: getLLMFeature(llm, "apiKey", "") || null
      });
      llmTestResult = { ...llmTestResult, [index]: data };
    } catch (err) {
      llmTestResult = { ...llmTestResult, [index]: { ok: false, error: err.message || "Failed" } };
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

  function prefsDialogFingerprint(draft) {
    try {
      return JSON.stringify(draft || {});
    } catch (err) {
      return "";
    }
  }

  function schedulePrefsApply() {
    if (!prefsDialogOpen || !prefsDialogDraft || !wsConnected) {
      return;
    }
    if (prefsDialogApplyTimer) {
      clearTimeout(prefsDialogApplyTimer);
    }
    prefsDialogApplyTimer = setTimeout(() => {
      prefsDialogApplyTimer = null;
      if (!prefsDialogBusy) {
        applyPrefsDialog();
      } else {
        schedulePrefsApply();
      }
    }, 350);
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
      scriptFontSize: String(readConfigInt("scriptfonsize", PREF_SCRIPT_FONT_SIZE_DEFAULT))
    };
    prefsDialogError = "";
    prefsDialogOpen = true;
    prefsDialogPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
    prefsDialogFingerprintValue = prefsDialogFingerprint(prefsDialogDraft);
    focusDialog(prefsDialogEl);
  }

  function closePrefsDialog() {
    prefsDialogOpen = false;
    prefsDialogDraft = null;
    prefsDialogError = "";
    if (prefsDialogApplyTimer) {
      clearTimeout(prefsDialogApplyTimer);
      prefsDialogApplyTimer = null;
    }
    document.body.style.overflow = prefsDialogPrevBodyOverflow;
    restoreFocus();
  }

  function openPluginDashboard() {
    if (!selectedProjectId) return;
    rememberFocus();
    pluginDashboardPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
    pluginDashboardOpen = true;
  }

  function closePluginDashboard() {
    pluginDashboardOpen = false;
    document.body.style.overflow = pluginDashboardPrevBodyOverflow;
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
    if (!Object.keys(configChanges).length && !Object.keys(prefChanges).length) {
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
      const hasDirtyDraft = scriptLoaded && scriptDraft !== scriptText;
      scriptText = data.text || "";
      if (!hasDirtyDraft) {
        scriptDraft = scriptText;
      } else {
        // Preserve user's in-progress edits; reschedule auto-apply with updated version.
        clearScriptAutoApplyTimer();
        scriptAutoApplyTimer = setTimeout(runScriptAutoApply, 200);
      }
      scriptVersion = data.version ?? null;
      scriptDiagnostics = data.parseErrors || [];
      scriptParseOk = data.parseOk !== false;
      scriptLoaded = true;
      await loadSemanticAnnotations(projectId);
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

  async function loadSemanticAnnotations(projectId) {
    if (!projectId) return;
    stopSemanticPreview();
    semanticLoading = true;
    semanticError = "";
    semanticStatus = "";
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/semantic`);
      if (projectId !== selectedProjectId) {
        return;
      }
      semanticDoc = data || null;
      semanticAnnotations = Array.isArray(data?.annotations) ? data.annotations : [];
      semanticSourceText = scriptDraft || "";
      semanticDirty = false;
    } catch (err) {
      if (projectId !== selectedProjectId) {
        return;
      }
      semanticDoc = null;
      semanticAnnotations = [];
      semanticSourceText = "";
      semanticDirty = false;
      semanticError = err.message || "Failed to load semantic annotations.";
    } finally {
      if (projectId === selectedProjectId) {
        semanticLoading = false;
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

  function toggleScriptSearchPanel() {
    scriptSearchOpen = !scriptSearchOpen;
    if (scriptSearchOpen) {
      if (!scriptSearchQuery) {
        scriptSearchQuery = "";
      }
      scriptEditorRef?.setSearchQuery(scriptSearchQuery);
      tick().then(() => scriptSearchInputEl?.focus());
    }
  }

  function updateScriptSearchQuery(value) {
    scriptSearchQuery = value;
    scriptEditorRef?.setSearchQuery(scriptSearchQuery);
  }

  function runScriptSearchNext() {
    scriptEditorRef?.findNext();
  }

  function runScriptSearchPrevious() {
    scriptEditorRef?.findPrevious();
  }

  function openGeneratePanel() {
    generatePanelOpen = true;
    generateResult = "";
    generateError = "";
    generateLoading = false;
    generateShowFormatPrompt = false;
    // Pre-fill actors from project agents
    const agents = (projectConfigView?.agents || []).map((a) => a.name).filter(Boolean);
    generateActors = agents.join(", ");
    // Load format prompt from config or default
    const stored = projectConfigLLMPrompts?.formatPrompt;
    generateFormatPrompt = stored || DEFAULT_FORMAT_PROMPT;
    // Load action prompt library from config
    generateActionLibrary = Array.isArray(projectConfigLLMPrompts?.actionPrompts)
      ? [...projectConfigLLMPrompts.actionPrompts]
      : [];
    // Restore selected LLM from project config (fallback: first LLM)
    generateLLMIndex = llmIndexByName(projectConfigView?.llmSelections?.generate);
  }

  function toggleGeneratePanel() {
    if (generatePanelOpen) {
      closeGeneratePanel();
    } else {
      openGeneratePanel();
    }
  }

  function closeGeneratePanel() {
    generatePanelOpen = false;
    generateResult = "";
    generateError = "";
    generateLoading = false;
    generateActionPrompt = "";
  }

  function toggleSemanticPanel() {
    semanticPanelOpen = !semanticPanelOpen;
  }

  function llmNameByIndex(index, llmList = projectConfigLLMs) {
    const list = Array.isArray(llmList) ? llmList : [];
    if (list.length === 0) {
      return "";
    }
    const idx = Number.isFinite(Number(index)) ? Number(index) : 0;
    if (idx < 0 || idx >= list.length) return "";
    return String(list[idx]?.name || "").trim();
  }

  function llmIndexByName(name, llmList = projectConfigLLMs) {
    const list = Array.isArray(llmList) ? llmList : [];
    if (list.length === 0) {
      return 0;
    }
    const needle = String(name || "").trim().toLowerCase();
    if (!needle) return 0;
    const idx = list.findIndex((llm) => String(llm?.name || "").trim().toLowerCase() === needle);
    return idx >= 0 ? idx : 0;
  }

  function syncLLMSelectionsFromConfig(configLike) {
    const cfg = configLike || projectConfigView || {};
    const llms = Array.isArray(cfg?.llms) ? cfg.llms : projectConfigLLMs;
    const selections = cfg?.llmSelections || {};
    generateLLMIndex = llmIndexByName(selections.generate, llms);
    semanticLLMIndex = llmIndexByName(selections.semantic, llms);
  }

  function boolFromSemanticService(value, fallback = true) {
    if (typeof value === "boolean") return value;
    const text = String(value ?? "").trim().toLowerCase();
    if (!text) return fallback;
    if (["true", "1", "yes", "on"].includes(text)) return true;
    if (["false", "0", "no", "off"].includes(text)) return false;
    return fallback;
  }

  function syncSemanticAnalysisSettingsFromConfig(configLike) {
    const cfg = configLike || projectConfigView || {};
    const services = cfg?.semanticServices || {};
    const analyzeSyntaxRaw = services.analyzeSyntax ?? services.analyzeSvo;
    semanticAnalyzeSvo = boolFromSemanticService(analyzeSyntaxRaw, true);
    semanticAnalyzeDaTr = boolFromSemanticService(services.analyzeDaTr, true);
    const llms = Array.isArray(cfg?.llms) ? cfg.llms : projectConfigLLMs;
    const daTrLlm = String(services.daTrLlm || "").trim();
    if (daTrLlm) {
      semanticLLMIndex = llmIndexByName(daTrLlm, llms);
    }
    const systemPrompt = String(services.systemPrompt || "").trim();
    const promptTemplate = String(services.promptTemplate || "").trim();
    if (systemPrompt) {
      semanticSystemPrompt = systemPrompt;
    }
    if (promptTemplate) {
      semanticPromptTemplate = promptTemplate;
    }
  }

  function writeSemanticAnalysisSettingsToDraft() {
    const baseDraft = projectConfigDraft ? cloneProjectConfig(projectConfigDraft) : cloneProjectConfig(projectConfigView);
    const semanticLlm = llmNameByIndex(semanticLLMIndex);
    const nextSemanticServices = {
      ...(baseDraft.semanticServices || projectConfigView?.semanticServices || {}),
      analyzeSyntax: String(!!semanticAnalyzeSvo),
      analyzeSvo: String(!!semanticAnalyzeSvo),
      analyzeDaTr: String(!!semanticAnalyzeDaTr),
      daTrLlm: semanticLlm,
      systemPrompt: semanticSystemPrompt || "",
      promptTemplate: semanticPromptTemplate || ""
    };
    const nextLlmSelections = {
      ...(baseDraft.llmSelections || projectConfigView?.llmSelections || {}),
      semantic: semanticLlm
    };
    projectConfigDraft = {
      ...baseDraft,
      semanticServices: nextSemanticServices,
      llmSelections: nextLlmSelections
    };
  }

  async function persistSemanticAnalysisSettings() {
    if (!selectedProjectId || !wsConnected) return;
    writeSemanticAnalysisSettingsToDraft();
    await applyProjectConfig();
  }

  function stageSemanticAnalysisSettings() {
    writeSemanticAnalysisSettingsToDraft();
  }

  async function persistLLMSelections() {
    if (!selectedProjectId || !wsConnected) return;
    const baseDraft = projectConfigDraft ? cloneProjectConfig(projectConfigDraft) : cloneProjectConfig(projectConfigView);
    const semanticLlm = llmNameByIndex(semanticLLMIndex);
    const nextLlmSelections = {
      ...(baseDraft.llmSelections || {}),
      generate: llmNameByIndex(generateLLMIndex),
      semantic: semanticLlm
    };
    const nextSemanticServices = {
      ...(baseDraft.semanticServices || projectConfigView?.semanticServices || {}),
      daTrLlm: semanticLlm
    };
    projectConfigDraft = {
      ...baseDraft,
      llmSelections: nextLlmSelections,
      semanticServices: nextSemanticServices
    };
    await applyLLMPromptsConfig();
  }

  function handleGenerateLLMSelectionChange(event) {
    if (event?.target && event.target.value !== undefined) {
      generateLLMIndex = Number(event.target.value);
    }
    void persistLLMSelections();
  }

  function handleSemanticLLMSelectionChange(event) {
    if (event?.target && event.target.value !== undefined) {
      semanticLLMIndex = Number(event.target.value);
    }
    stageSemanticAnalysisSettings();
  }

  function toggleSemanticAnalyzeSyntax() {
    if (semanticAnalyzeBusy) return;
    semanticAnalyzeSvo = !semanticAnalyzeSvo;
    stageSemanticAnalysisSettings();
  }

  function toggleSemanticAnalyzeDaTr() {
    if (semanticAnalyzeBusy) return;
    semanticAnalyzeDaTr = !semanticAnalyzeDaTr;
    stageSemanticAnalysisSettings();
  }

  function toggleSemanticDebug() {
    semanticDebugEnabled = !semanticDebugEnabled;
  }

  function resetSemanticPrompts() {
    semanticSystemPrompt = "You are a multilingual discourse annotation engine for dialogue utterances.";
    semanticPromptTemplate = `Analyze exactly one utterance sentence and return JSON only (no markdown).
Language can be German or English; handle umlauts correctly (ä/ö/ü and ae/oe/ue variants).
Treat placeholders like $user as normal mentions.
Ignore bracketed stage/action tags (e.g. [wave]).
Classify ONLY this sentence (no cross-sentence inference).

Focus on:
1) dialogueAct.label + dialogueAct.confidence
2) themeRheme.theme + themeRheme.rheme + themeRheme.confidence

Return object fields: version (number), annotations (array with exactly one item).
Annotation fields: id, line, speaker, text, dialogueAct, themeRheme.
Do not output basic subject/verb/object unless explicitly requested by layers.

Dialogue act guideline:
- Use short labels (e.g. greeting, question, inform, request, confirm, reject, thanks, apology, directive, commissive).

Theme-rheme guideline:
- theme = given/topic part (what the sentence is about)
- rheme = new/focus part (what is said about the theme)
- Keep both close to original wording.

Output must be valid JSON.

Layers: {{layers}}
Speaker: {{speaker}}
Line: {{line}}
Sentence:
{{script}}`;
    stageSemanticAnalysisSettings();
  }

  function semanticPreferredLanguage() {
    const navLang = String(typeof navigator !== "undefined" ? (navigator.language || "") : "").toLowerCase();
    if (navLang.startsWith("de")) return "de";
    if (navLang.startsWith("en")) return "en";
    return "de";
  }

  async function generateScene() {
    const llm = projectConfigLLMs[generateLLMIndex];
    if (!llm) {
      generateError = "No LLM service selected.";
      return;
    }
    const baseUrl = getLLMFeature(llm, "baseUrl", "").trim();
    const apiKey = getLLMFeature(llm, "apiKey", "") || null;
    const model = getLLMFeature(llm, "model", "").trim();
    const temperature = parseFloat(getLLMFeature(llm, "temperature", "0.7")) || 0.7;
    const timeout = parseInt(getLLMFeature(llm, "timeout", "60"), 10) || 60;
    if (!baseUrl) {
      generateError = "LLM base URL is not configured.";
      return;
    }
    if (!model) {
      generateError = "No model selected for this LLM service.";
      return;
    }
    if (!generateActionPrompt.trim()) {
      generateError = "Please enter an action prompt.";
      return;
    }
    // Substitute placeholders in format prompt
    let resolvedFormat = generateFormatPrompt
      .replace(/\{\{number\}\}/g, String(generateSceneCount || 1))
      .replace(/\{\{language\}\}/g, generateLanguage || "en")
      .replace(/\{\{scene_name\}\}/g, generateSceneName || "new_scene")
      .replace(/\{\{actors\}\}/g, generateActors || "agent");
    generateLoading = true;
    generateError = "";
    generateResult = "";
    try {
      const data = await apiPost("/api/v1/llm/generate", {
        baseUrl,
        apiKey,
        model,
        temperature,
        timeout,
        formatPrompt: resolvedFormat,
        actionPrompt: generateActionPrompt.trim()
      });
      if (data.error) {
        generateError = data.error;
      } else {
        generateResult = data.text || "";
      }
    } catch (err) {
      generateError = err.message || "Generation failed.";
    } finally {
      generateLoading = false;
    }
  }

  function insertGeneratedScene() {
    if (!generateResult) return;
    scriptEditorRef?.insertText("\n\n" + generateResult);
    generateResult = "";
  }

  async function applyLLMPromptsConfig() {
    // Apply config changes from generate panel (doesn't require dialog to be open)
    if (!selectedProjectId || !projectConfigDraft || !wsConnected) {
      console.log("[Generate] applyLLMPromptsConfig skipped:", { selectedProjectId, hasDraft: !!projectConfigDraft, wsConnected });
      return;
    }
    console.log("[Generate] applyLLMPromptsConfig sending:", projectConfigDraft?.llmPrompts);
    try {
      const response = await sendCommand("ProjectConfig.Update", {
        projectId: selectedProjectId,
        config: projectConfigDraft
      });
      console.log("[Generate] applyLLMPromptsConfig response:", response?.status, response?.pending);
      projectConfig = normalizeProjectConfig(response.config || {});
      projectConfigDraft = cloneProjectConfig(projectConfig);
      syncLLMSelectionsFromConfig(projectConfig);
      // Mark project as dirty (has unsaved changes) but NOT pending (which would require save-as)
      if (response.pending === true) {
        projects = projects.map((project) =>
          project.projectId === selectedProjectId ? { ...project, dirty: true } : project
        );
      }
    } catch (err) {
      console.error("[Generate] Failed to save LLM prompts config:", err);
    }
  }

  function saveActionPromptToLibrary() {
    const text = generateActionPrompt.trim();
    if (!text) return;
    if (generateActionLibrary.includes(text)) return;
    generateActionLibrary = [...generateActionLibrary, text];
    // Save to project config
    if (!projectConfigDraft) {
      projectConfigDraft = cloneProjectConfig(projectConfigView);
    }
    projectConfigDraft.llmPrompts = {
      ...projectConfigDraft.llmPrompts,
      actionPrompts: [...generateActionLibrary]
    };
    applyLLMPromptsConfig();
  }

  function removeActionPromptFromLibrary(index) {
    generateActionLibrary = generateActionLibrary.filter((_, i) => i !== index);
    if (!projectConfigDraft) {
      projectConfigDraft = cloneProjectConfig(projectConfigView);
    }
    projectConfigDraft.llmPrompts = {
      ...projectConfigDraft.llmPrompts,
      actionPrompts: [...generateActionLibrary]
    };
    applyLLMPromptsConfig();
  }

  function saveFormatPrompt() {
    if (!projectConfigDraft) {
      projectConfigDraft = cloneProjectConfig(projectConfigView);
    }
    projectConfigDraft.llmPrompts = {
      ...projectConfigDraft.llmPrompts,
      formatPrompt: generateFormatPrompt
    };
    applyLLMPromptsConfig();
  }

  async function runSemanticAnalysis() {
    if (!selectedProjectId || semanticAnalyzeBusy) return;
    semanticAnalyzeBusy = true;
    stopSemanticPreview();
    semanticError = "";
    semanticStatus = "";
    try {
      const includeSvo = !!semanticAnalyzeSvo;
      const includeDaTr = !!semanticAnalyzeDaTr;
      const llmLayersText = `basic:false, dialogueAct:${includeDaTr}, themeRheme:${includeDaTr}`;
      const language = semanticPreferredLanguage();
      const units = extractSemanticSentenceUnits(scriptDraft || "");
      semanticAnnotations = [];
      semanticDoc = null;
      semanticSourceText = scriptDraft || "";
      semanticDirty = true;
      semanticUdDebug = [];
      if (!includeSvo && !includeDaTr) {
        semanticStatus = "Enable at least one analysis layer (Syntax or DA/TR).";
        return;
      }
      if (!units.length) {
        semanticStatus = "No utterance sentences found for semantic analysis.";
        return;
      }
      let failures = 0;
      const failureMessages = [];
      const merged = [];
      for (let i = 0; i < units.length; i += 1) {
        const unit = units[i];
        semanticStatus = `Analyzing sentence ${i + 1}/${units.length}...`;
        const unitLanguage = String(unit.language || language || "de").trim().toLowerCase() || "de";
        let syntaxNormalized = [];
        let metaNormalized = [];
        if (includeSvo) {
          try {
            const syntaxResponse = await apiPost(`/api/v1/projects/${selectedProjectId}/semantic/syntax`, {
              text: unit.text,
              persist: false,
              language: unitLanguage,
              debug: semanticDebugEnabled
            });
            syntaxNormalized = normalizeSentenceAnnotations(syntaxResponse?.annotations, unit);
            if (semanticDebugEnabled && syntaxResponse?.debug) {
              semanticUdDebug = [
                ...semanticUdDebug,
                {
                  line: unit.line,
                  sentence: unit.text,
                  language: unitLanguage,
                  trace: syntaxResponse.debug
                }
              ];
            }
          } catch (err) {
            failures += 1;
            if (failureMessages.length < 5) {
              const message = err?.message ? String(err.message) : "syntax analysis call failed";
              failureMessages.push(`line ${unit.line}: ${message}`);
            }
          }
        }
        if (includeDaTr) {
          const prompt = String(semanticPromptTemplate || "")
            .replace(/\{\{layers\}\}/g, llmLayersText)
            .replace(/\{\{script\}\}/g, unit.text)
            .replace(/\{\{line\}\}/g, String(unit.line))
            .replace(/\{\{speaker\}\}/g, unit.speaker || "");
          try {
            const response = await apiPost(`/api/v1/projects/${selectedProjectId}/semantic/analyze`, {
              text: unit.text,
              useLlm: true,
              persist: false,
              basicProvider: "ud",
              language: unitLanguage,
              llmIndex: semanticLLMIndex,
              systemPrompt: semanticSystemPrompt || "",
              prompt,
              layers: {
                basic: false,
                dialogueAct: includeDaTr,
                themeRheme: includeDaTr
              }
            });
            metaNormalized = normalizeSentenceAnnotations(response?.annotations, unit);
          } catch (err) {
            failures += 1;
            if (failureMessages.length < 5) {
              const message = err?.message ? String(err.message) : "dialogue/thema analysis call failed";
              failureMessages.push(`line ${unit.line}: ${message}`);
            }
          }
        }
        const sentenceAnnotations = mergeSentenceAnnotationLayers(syntaxNormalized, metaNormalized, unit);
        if (sentenceAnnotations.length) {
          merged.push(...sentenceAnnotations);
          semanticAnnotations = [...merged];
          semanticSourceText = scriptDraft || "";
        } else {
          failures += 1;
          if (failureMessages.length < 5) {
            failureMessages.push(`line ${unit.line}: no annotations returned`);
          }
        }
      }
      stopSemanticPreview();
      const finalDoc = {
        version: 2,
        schema: {
          id: "vsm.semantic.annotations",
          version: 2
        },
        annotations: merged
      };
      semanticDoc = finalDoc;
      semanticAnnotations = merged;
      semanticSourceText = scriptDraft || "";
      semanticDirty = true;
      semanticStatus = failures > 0
        ? `Semantic analysis updated (${merged.length} annotations, ${failures} sentence errors).`
        : `Semantic analysis updated (${merged.length} annotations).`;
      semanticError = failures > 0
        ? `Sentence analysis errors:\n${failureMessages.join("\n")}`
        : "";
    } catch (err) {
      stopSemanticPreview();
      semanticError = err.message || "Semantic analysis failed.";
    } finally {
      semanticAnalyzeBusy = false;
    }
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
      console.log("[applyScript] sending version:", payload.version, "textLen:", payload.text?.length);
      const response = await sendCommand("Script.Update", payload);
      console.log("[applyScript] response:", JSON.stringify(response).slice(0, 200));
      if (response.applied) {
        const nextText = response.text ?? scriptDraft;
        scriptText = nextText;
        scriptDraft = nextText;
        scriptVersion = response.version ?? scriptVersion;
        scriptParseOk = response.parseOk !== false;
        scriptDiagnostics = response.parseErrors || [];
        scriptStatus = "Script updated.";
        loadScriptScenes(selectedProjectId);
        return true;
      }
      if (response.reason === "VERSION_MISMATCH") {
        // Update local version/text to what the server has, then retry
        if (response.version !== undefined) scriptVersion = response.version;
        if (response.text !== undefined) scriptText = response.text;
        scriptParseOk = true;
        scriptStatus = "";
        clearScriptAutoApplyTimer();
        scriptAutoApplyTimer = setTimeout(runScriptAutoApply, 100);
        return false;
      }
      if (response.reason === "PARSE_FAILED") {
        scriptParseOk = false;
        scriptDiagnostics = response.parseErrors || [];
        scriptError = "Script parse failed. Check syntax.";
        return false;
      }
      scriptParseOk = response.parseOk !== false;
      scriptDiagnostics = response.parseErrors || [];
      scriptStatus = "Script update not applied.";
      return false;
    } catch (err) {
      scriptError = err.message || "Failed to update script.";
      return false;
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

  async function ensureRuntimeVizGuardBeforeFirstRun() {
    if (!selectedProjectId) return false;
    if (!projectConfig || lastProjectConfigProjectId !== selectedProjectId) {
      await loadProjectConfig(selectedProjectId);
    }
    const cfg = normalizeProjectConfig(projectConfigDraft || projectConfig || {});
    if (hasRuntimeVizGuardInProjectConfig(cfg)) {
      syncRuntimeVizGuardFromProjectConfig(cfg);
      return true;
    }
    return calibrateRuntimeVizGuard({ automated: true });
  }

  async function checkAndShowPreflight(command) {
    try {
      const res = await fetch(`/api/v1/projects/${selectedProjectId}/preflight`);
      if (!res.ok) return true;
      const data = await res.json();
      if (data.firstRunOnMachine && data.machineSpecificConfig && data.machineSpecificConfig.length > 0) {
        preflightData = data;
        pendingPreflightCommand = command;
        preflightModalOpen = true;
        return false;
      }
    } catch (e) { /* ignore — let execution proceed */ }
    return true;
  }

  async function confirmPreflight() {
    preflightModalOpen = false;
    const cmd = pendingPreflightCommand;
    pendingPreflightCommand = null;
    preflightData = null;
    if (cmd) await executeRuntimeCommand(cmd);
  }

  function cancelPreflight() {
    preflightModalOpen = false;
    pendingPreflightCommand = null;
    preflightData = null;
  }

  function openPluginDashboardFromPreflight() {
    preflightModalOpen = false;
    pendingPreflightCommand = null;
    preflightData = null;
    openPluginDashboard();
  }

  async function runRuntimeCommand(command, options = {}) {
    if (!selectedProjectId) return;
    if (command === "Runtime.Stop") {
      runtimeStopRequested = true;
      clearSceneFlowActivity();
      resetEventOverproductionMonitoring({ resetMute: true });
    } else if (command === "Runtime.Play" || command === "Runtime.Start" || command === "Runtime.Resume") {
      runtimeStopRequested = false;
    }
    if (command === "Runtime.Play") {
      const missingVars = await checkUndefinedVariables();
      if (missingVars.length) {
        openMissingVarDialog(missingVars);
        return;
      }
      if (!options.skipMissingAgentCheck && missingAgentNames.length) {
        openMissingAgentDialog();
        return;
      }
      const guardReady = await ensureRuntimeVizGuardBeforeFirstRun();
      if (!guardReady) {
        runtimeError = runtimeVizError || projectConfigError || "Runtime visualization guard calibration failed.";
        return;
      }
      const preflightOk = await checkAndShowPreflight(command);
      if (!preflightOk) return;
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
    activityNodeHoldUntil = new Map();
    commandActivityHeldNodeIds = new Set();
    commandActivityKindByNodeId = new Map();
    playSceneHeldNodeQueue = [];
    playSceneHoldBySceneKey = new Map();
    recentActiveNodeQueue = [];
    recentStoppedNodeQueue = [];
    pendingPlaySceneStartQueue = [];
    activityEdgeHits = new Map();
    timeoutEdgeRuns = new Map();
  }

  function resetEventOverproductionMonitoring({ resetMute = false } = {}) {
    overprodWindowStart = 0;
    overprodWindowTotal = 0;
    overprodWindowByKey = new Map();
    overprodWindowMeta = new Map();
    overprodStreak = 0;
    overprodNotifiedForRun = false;
    if (resetMute) {
      eventOverprodMutedForRun = false;
    }
    eventOverprodDialogOpen = false;
  }

  function closeEventOverprodDialog() {
    eventOverprodDialogOpen = false;
  }

  function muteEventOverprodDialogForRun() {
    eventOverprodMutedForRun = true;
    eventOverprodDialogOpen = false;
  }

  function formatFlowNodeLabel(nodeId) {
    if (!nodeId || !sceneFlow?.nodes?.length) return nodeId || "";
    const node = sceneFlow.nodes.find((entry) => entry.id === nodeId);
    if (!node) return nodeId;
    const name = String(node.name || "").trim();
    return name ? `${name} (${nodeId})` : nodeId;
  }

  function makeOverprodFlowLabel(meta) {
    if (!meta) return "unknown flow";
    if (meta.sourceId && meta.targetId) {
      return `${formatFlowNodeLabel(meta.sourceId)} -> ${formatFlowNodeLabel(meta.targetId)}`;
    }
    if (meta.nodeId) {
      return formatFlowNodeLabel(meta.nodeId);
    }
    return meta.key || "unknown flow";
  }

  function evaluateEventOverproductionWindow(now) {
    if (!overprodWindowStart || overprodWindowTotal <= 0) {
      overprodWindowStart = now;
      return;
    }
    const elapsedMs = Math.max(1, now - overprodWindowStart);
    const elapsedSec = elapsedMs / 1000;
    const totalRate = overprodWindowTotal / elapsedSec;
    let topKey = "";
    let topCount = 0;
    for (const [key, count] of overprodWindowByKey.entries()) {
      if (count > topCount) {
        topKey = key;
        topCount = count;
      }
    }
    const topRate = topCount / elapsedSec;
    const topMeta = overprodWindowMeta.get(topKey) || null;
    const zeroTimeoutHotspot = !!topMeta?.zeroTimeout;
    const configuredRateLimit = readRuntimeVizProjectInt(
      projectConfigDraft || projectConfig || {},
      "runtimeVizRate",
      RUNTIME_VIZ_RATE_DEFAULT
    );
    const dynamicTotalThreshold = Math.max(80, Math.round(configuredRateLimit * 0.85));
    const dynamicFlowThreshold = Math.max(50, Math.round(dynamicTotalThreshold * 0.6));
    const overloaded =
      totalRate >= dynamicTotalThreshold &&
      (topRate >= dynamicFlowThreshold || zeroTimeoutHotspot);
    overprodStreak = overloaded ? (overprodStreak + 1) : 0;
    if (
      overloaded &&
      overprodStreak >= EVENT_OVERPROD_REQUIRED_WINDOWS &&
      !overprodNotifiedForRun &&
      !eventOverprodMutedForRun
    ) {
      eventOverprodMessage = zeroTimeoutHotspot
        ? "Realtime parallel visualization is overloaded by an ultra-fast timeout loop (0 ms). Consider redesigning this subflow to an event-based approach instead of continuous immediate transitions."
        : "Realtime parallel visualization is overloaded by a very high event rate in one subflow. Consider redesigning this subflow to an event-based approach.";
      eventOverprodFlowLabel = makeOverprodFlowLabel(topMeta);
      eventOverprodRate = `${Math.round(totalRate)} runtime events/s`;
      eventOverprodFlowRate = `${Math.round(topRate)} events/s in this subflow`;
      openEventOverprodDialog();
      overprodNotifiedForRun = true;
    }
    overprodWindowStart = now;
    overprodWindowTotal = 0;
    overprodWindowByKey = new Map();
    overprodWindowMeta = new Map();
  }

  function recordRuntimeEventForOverproduction(eventName, payload = {}) {
    if (!selectedProjectId) return;
    const now = Date.now();
    if (!overprodWindowStart) {
      overprodWindowStart = now;
    } else if (now - overprodWindowStart >= EVENT_OVERPROD_WINDOW_MS) {
      evaluateEventOverproductionWindow(now);
    }

    let key = "";
    let meta = null;
    if (eventName === "runtime.edgeActive" || eventName === "runtime.timeoutProgress") {
      const edgeType = normalizeProtocolEdgeType(payload.edgeType);
      const sourceId = String(payload.sourceId || "").trim();
      const targetId = String(payload.targetId || "").trim();
      if (sourceId && targetId) {
        key = `edge:${edgeType}:${sourceId}->${targetId}`;
        meta = { key, sourceId, targetId, edgeType };
      }
      if (eventName === "runtime.timeoutProgress") {
        const timeoutMs = Number(payload.timeoutMs);
        if (meta && Number.isFinite(timeoutMs) && timeoutMs <= 0) {
          meta.zeroTimeout = true;
        }
      }
    } else if (eventName === "runtime.nodeActive" || eventName === "runtime.nodeStopped") {
      const nodeId = resolveActivityNodeId(payload);
      if (nodeId) {
        key = `node:${nodeId}`;
        meta = { key, nodeId };
      }
    }
    if (!key) return;

    overprodWindowTotal += 1;
    const next = new Map(overprodWindowByKey);
    next.set(key, (next.get(key) || 0) + 1);
    overprodWindowByKey = next;
    if (meta) {
      const metaMap = new Map(overprodWindowMeta);
      const current = metaMap.get(key);
      if (current) {
        metaMap.set(key, { ...current, ...meta, zeroTimeout: current.zeroTimeout || meta.zeroTimeout });
      } else {
        metaMap.set(key, meta);
      }
      overprodWindowMeta = metaMap;
    }
  }

  function activityProjectMatches(payload) {
    const projectId = payload?.projectId;
    // When both sides have an ID they must match; fall back to accepting
    // projectId-less events only in single-project / legacy mode.
    if (selectedProjectId && projectId) return projectId === selectedProjectId;
    return !projectId;
  }

  function resolveActivityNodeId(payload) {
    if (!sceneFlow?.nodes) return "";
    const nodeId = (payload?.nodeId || "").trim();
    const visible = new Set(sceneFlow.nodes.map((node) => node.id));
    if (nodeId && visible.has(nodeId)) return nodeId;
    // Walk full ancestor chain (ancestorIds: [parentId, grandParentId, ...])
    const ancestors = Array.isArray(payload?.ancestorIds)
      ? payload.ancestorIds
      : (payload?.parentId ? [payload.parentId] : []);
    for (const aid of ancestors) {
      if (aid && visible.has(aid)) return aid;
    }
    return "";
  }

  // Returns the primary resolved node plus any canonical/alias counterparts visible at this level.
  function resolveAllActivityNodeIds(payload) {
    const primary = resolveActivityNodeId(payload);
    if (!primary || !sceneFlow?.nodes) return primary ? [primary] : [];
    const result = new Set([primary]);
    for (const node of sceneFlow.nodes) {
      if (node.type === "Alias" && node.refId === primary) {
        result.add(node.id); // primary is canonical → also activate its aliases
      }
      if (node.id === primary && node.type === "Alias" && node.refId) {
        result.add(node.refId); // primary is alias → also activate canonical (if visible)
      }
    }
    return Array.from(result);
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
    return match?.type === "Super" || match?.type === "Alias";
  }

  function isEpsilonOnlyNode(nodeId) {
    if (!nodeId || !sceneFlow?.edges?.length) return false;
    const outgoing = sceneFlow.edges.filter((edge) => edge.sourceId === nodeId);
    return outgoing.length === 1 && (outgoing[0]?.type || "") === "EEDGE";
  }

  function clearActivityNodeDecay(nodeId) {
    if (!nodeId) return;
    const existing = activityNodeDecayTokens.get(nodeId);
    if (existing) {
      clearTimeout(existing);
      const nextTokens = new Map(activityNodeDecayTokens);
      nextTokens.delete(nodeId);
      activityNodeDecayTokens = nextTokens;
    }
  }

  function scheduleActivityNodeDecay(nodeId) {
    if (!nodeId) return;
    clearActivityNodeDecay(nodeId);
    const now = Date.now();
    const holdUntil = Number(activityNodeHoldUntil.get(nodeId) || 0);
    const holdDelay = holdUntil > now ? (holdUntil - now + 20) : 0;
    const delay = Math.max(0, holdDelay);
    const token = setTimeout(() => {
      const current = activityNodeDecayTokens.get(nodeId);
      if (current === token) {
        const nextTokens = new Map(activityNodeDecayTokens);
        nextTokens.delete(nodeId);
        activityNodeDecayTokens = nextTokens;
        const currentHold = Number(activityNodeHoldUntil.get(nodeId) || 0);
        if (currentHold > Date.now()) {
          scheduleActivityNodeDecay(nodeId);
          return;
        }
        const nextHold = new Map(activityNodeHoldUntil);
        nextHold.delete(nodeId);
        activityNodeHoldUntil = nextHold;
        clearActivityNode(nodeId);
      }
    }, delay);
    const nextTokens = new Map(activityNodeDecayTokens);
    nextTokens.set(nodeId, token);
    activityNodeDecayTokens = nextTokens;
  }

  function holdActivityNodeUntil(nodeId, untilTs) {
    if (!nodeId) return;
    const until = Number(untilTs);
    if (!Number.isFinite(until) || until <= Date.now()) return;
    const prev = Number(activityNodeHoldUntil.get(nodeId) || 0);
    if (until > prev) {
      const next = new Map(activityNodeHoldUntil);
      next.set(nodeId, until);
      activityNodeHoldUntil = next;
    }
    if (activityNodeCounts.has(nodeId)) {
      scheduleActivityNodeDecay(nodeId);
    }
  }

  function clearActivityNode(nodeId) {
    if (!nodeId) return;
    const next = new Map(activityNodeCounts);
    next.delete(nodeId);
    activityNodeCounts = next;
  }

  function forceClearNodeActivity(nodeId) {
    if (!nodeId) return;
    clearActivityNodeDecay(nodeId);
    clearActivityNode(nodeId);
    const holdNext = new Map(activityNodeHoldUntil);
    holdNext.delete(nodeId);
    activityNodeHoldUntil = holdNext;
    if (commandActivityHeldNodeIds.has(nodeId)) {
      const held = new Set(commandActivityHeldNodeIds);
      held.delete(nodeId);
      commandActivityHeldNodeIds = held;
    }
    if (commandActivityKindByNodeId.has(nodeId)) {
      const kinds = new Map(commandActivityKindByNodeId);
      kinds.delete(nodeId);
      commandActivityKindByNodeId = kinds;
    }
    if (playSceneHeldNodeQueue.length) {
      playSceneHeldNodeQueue = playSceneHeldNodeQueue.filter((id) => id !== nodeId);
    }
  }

  function incrementActivityNode(nodeId) {
    if (!nodeId) return;
    holdActivityNodeUntil(nodeId, Date.now() + ACTIVITY_NODE_MIN_HIGHLIGHT_MS);
    clearActivityNodeDecay(nodeId);
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
    if (count > 1) {
      next.set(nodeId, count - 1);
      activityNodeCounts = next;
      return;
    }
    const holdUntil = Number(activityNodeHoldUntil.get(nodeId) || 0);
    if (holdUntil > Date.now()) {
      next.set(nodeId, 1);
      activityNodeCounts = next;
      scheduleActivityNodeDecay(nodeId);
      return;
    }
    next.delete(nodeId);
    activityNodeCounts = next;
    const holdNext = new Map(activityNodeHoldUntil);
    holdNext.delete(nodeId);
    activityNodeHoldUntil = holdNext;
    clearActivityNodeDecay(nodeId);
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

  function longRunningCommandKindForNode(nodeId) {
    if (!nodeId || !sceneFlow?.nodes?.length) return null;
    const node = sceneFlow.nodes.find((entry) => entry?.id === nodeId);
    if (!node) return null;
    const commands = Array.isArray(node.commands) ? node.commands : [];
    for (const cmd of commands) {
      const text =
        typeof cmd === "string"
          ? cmd
          : String(cmd?.text || cmd?.syntax || cmd?.name || cmd?.type || cmd?.label || "");
      const normalized = text.trim();
      if (/\bPlayScene\b/i.test(normalized)) {
        return "playScene";
      }
    }
    return null;
  }

  function holdCommandActivityNode(nodeId, forcedKind = null) {
    if (!nodeId) return;
    const kind = forcedKind || longRunningCommandKindForNode(nodeId);
    if (!kind) return;
    // Cancel any pending min-highlight decay so command-driven activity stays visible.
    clearActivityNodeDecay(nodeId);
    const held = new Set(commandActivityHeldNodeIds);
    held.add(nodeId);
    commandActivityHeldNodeIds = held;
    const kinds = new Map(commandActivityKindByNodeId);
    kinds.set(nodeId, kind);
    commandActivityKindByNodeId = kinds;
    if (kind === "playScene" && !playSceneHeldNodeQueue.includes(nodeId)) {
      playSceneHeldNodeQueue = [...playSceneHeldNodeQueue, nodeId];
    }
    const nextCounts = new Map(activityNodeCounts);
    const current = Number(nextCounts.get(nodeId) || 0);
    if (current < 1) {
      nextCounts.set(nodeId, 1);
      activityNodeCounts = nextCounts;
    }
  }

  function releaseCommandActivityNode(nodeId) {
    if (!nodeId || !commandActivityHeldNodeIds.has(nodeId)) return;
    const held = new Set(commandActivityHeldNodeIds);
    held.delete(nodeId);
    commandActivityHeldNodeIds = held;
    const kinds = new Map(commandActivityKindByNodeId);
    kinds.delete(nodeId);
    commandActivityKindByNodeId = kinds;
    if (playSceneHeldNodeQueue.length) {
      playSceneHeldNodeQueue = playSceneHeldNodeQueue.filter((id) => id !== nodeId);
    }
    decrementActivityNode(nodeId);
  }

  function releaseNextPlaySceneHeldNode() {
    if (!playSceneHeldNodeQueue.length) return;
    const queue = [...playSceneHeldNodeQueue];
    while (queue.length) {
      const nodeId = queue.shift();
      if (nodeId && commandActivityHeldNodeIds.has(nodeId) && commandActivityKindByNodeId.get(nodeId) === "playScene") {
        playSceneHeldNodeQueue = queue;
        releaseCommandActivityNode(nodeId);
        return;
      }
    }
    playSceneHeldNodeQueue = [];
  }

  function pushRecentStoppedNode(nodeId) {
    if (!nodeId) return;
    const now = Date.now();
    const windowMs = 4000;
    const next = recentStoppedNodeQueue
      .filter((entry) => entry && entry.nodeId && (now - Number(entry.ts || 0)) <= windowMs)
      .filter((entry) => entry.nodeId !== nodeId);
    next.push({ nodeId, ts: now });
    recentStoppedNodeQueue = next.slice(-24);
  }

  function pushRecentActiveNode(nodeId) {
    if (!nodeId) return;
    const now = Date.now();
    const windowMs = 4000;
    const next = recentActiveNodeQueue
      .filter((entry) => entry && entry.nodeId && (now - Number(entry.ts || 0)) <= windowMs)
      .filter((entry) => entry.nodeId !== nodeId);
    next.push({ nodeId, ts: now });
    recentActiveNodeQueue = next.slice(-24);
  }

  function bindPlaySceneToRecentActiveNode() {
    if (!recentActiveNodeQueue.length) return null;
    const now = Date.now();
    const windowMs = 4000;
    const queue = [...recentActiveNodeQueue];
    while (queue.length) {
      const entry = queue.pop();
      if (!entry?.nodeId) continue;
      if ((now - Number(entry.ts || 0)) > windowMs) {
        continue;
      }
      recentActiveNodeQueue = queue;
      holdCommandActivityNode(entry.nodeId, "playScene");
      return entry.nodeId;
    }
    recentActiveNodeQueue = [];
    return null;
  }

  function bindPlaySceneToRecentStoppedNode() {
    if (!recentStoppedNodeQueue.length) return null;
    const now = Date.now();
    const windowMs = 4000;
    const queue = [...recentStoppedNodeQueue];
    while (queue.length) {
      const entry = queue.shift();
      if (!entry?.nodeId) continue;
      if ((now - Number(entry.ts || 0)) > windowMs) {
        continue;
      }
      recentStoppedNodeQueue = queue;
      holdCommandActivityNode(entry.nodeId, "playScene");
      return entry.nodeId;
    }
    recentStoppedNodeQueue = [];
    return null;
  }

  function queuePendingPlaySceneStart() {
    const now = Date.now();
    const windowMs = 4000;
    const next = pendingPlaySceneStartQueue
      .filter((ts) => Number.isFinite(Number(ts)) && (now - Number(ts)) <= windowMs);
    next.push(now);
    pendingPlaySceneStartQueue = next.slice(-24);
  }

  function consumePendingPlaySceneStart() {
    if (!pendingPlaySceneStartQueue.length) return false;
    const now = Date.now();
    const windowMs = 4000;
    const next = pendingPlaySceneStartQueue
      .filter((ts) => Number.isFinite(Number(ts)) && (now - Number(ts)) <= windowMs);
    if (!next.length) {
      pendingPlaySceneStartQueue = [];
      return false;
    }
    next.shift();
    pendingPlaySceneStartQueue = next;
    return true;
  }

  function sceneEventKey(payload) {
    const sceneName = String(payload?.sceneName || "");
    const language = String(payload?.language || "");
    const lower = String(payload?.lower || "");
    const upper = String(payload?.upper || "");
    return `${sceneName}|${language}|${lower}|${upper}`;
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

  function scheduleScriptLive() {
    if (!wsConnected || !selectedProjectId) return;
    if (scriptLiveTimer) clearTimeout(scriptLiveTimer);
    scriptLiveTimer = setTimeout(() => {
      scriptLiveTimer = null;
      if (scriptDraft === scriptLiveLast) return;
      scriptLiveLast = scriptDraft;
      if (!ws || ws.readyState !== 1 || !selectedProjectId) return;
      try {
        ws.send(JSON.stringify({
          method: "Script.Live",
          params: { projectId: selectedProjectId, text: scriptDraft }
        }));
      } catch (e) {}
    }, 100);
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
        if (message.event === "script.snapshot" || message.event === "project.dirty") {
          console.log("[WS recv event]", message.event, JSON.stringify(message).slice(0, 200));
        }
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
    // Fall back to the message itself when the server puts data at the top level
    // (no explicit payload wrapper). Wrapped messages take precedence.
    const payload = message.payload || message;
    const eventName = message.event;
    if (!eventName) return;

    // Presence events
    if (eventName === "presence.joined" || eventName === "presence.update") {
      const userId = payload.userId;
      if (userId && userId !== myPresenceUserId) {
        const next = new Map(peerPresence);
        next.set(userId, payload);
        peerPresence = next;
      }
      return;
    }
    if (eventName === "presence.left") {
      const userId = payload.userId;
      if (userId && peerPresence.has(userId)) {
        const next = new Map(peerPresence);
        next.delete(userId);
        peerPresence = next;
      }
      return;
    }

    if (eventName === "system.preferences" && payload?.preferences) {
      preferences = payload.preferences;
      prefDraft = { ...preferences };
      return;
    }
    if (eventName === "project.dirty") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) return;
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
        syncLLMSelectionsFromConfig(projectConfig);
        syncSemanticAnalysisSettingsFromConfig(projectConfig);
        projectConfigLoading = false;
        projectConfigError = "";
      }
      return;
    }
    if (eventName === "script.snapshot") {
      // Broadcasts put snapshot at top level (no payload wrapper); fall back to message.
      applyScriptSnapshot(message.payload != null ? message.payload : message);
      return;
    }
    if (eventName === "script.live") {
      const liveProjectId = message.projectId || payload.projectId;
      if (liveProjectId && liveProjectId !== selectedProjectId) return;
      // Only update if the local user is not currently editing (no unpublished draft).
      if (!scriptDirty && message.text !== undefined) {
        scriptText = message.text;
        scriptDraft = message.text;
      }
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
      // Broadcasts may be flat (snapshot/projectId at top level) or wrapped in payload.
      // Use payload if present; otherwise fall back to the raw message.
      applySceneFlowSnapshot(message.payload != null ? message.payload : message);
      return;
    }
    if (eventName === "sceneflow.edgeUpdated") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) return;
      if (selectedProjectId) {
        loadSceneFlow(selectedProjectId, sceneFlow?.superNodeId || "");
      }
      return;
    }
    if (eventName === "sceneflow.selection") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) return;
      applyProtocolSelection(payload.selection);
      return;
    }
    if (eventName === "runtime.state") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) {
        return;
      }
      const status = (payload.status || payload.state || "").toLowerCase();
      // Immediately reflect new state so play/pause/stop buttons update in all windows
      // without waiting for the async loadRuntime() REST round-trip.
      if (status) {
        runtimeInfo = { ...(runtimeInfo || {}), state: status };
      }
      if (status === "running") {
        runtimeStopRequested = false;
        activeScenes = [];
        activeTurns = [];
        sceneHistory = [];
        resetEventOverproductionMonitoring();
      }
      if (status === "paused") {
        runtimeStopRequested = false;
      }
      if (status === "stopped") {
        runtimeStopRequested = false;
        clearSceneFlowActivity();
        resetEventOverproductionMonitoring({ resetMute: true });
      }
      if (selectedProjectId) {
        loadRuntime(selectedProjectId);
      }
      return;
    }
    if (eventName === "runtime.nodeActive") {
      if (!activityProjectMatches(payload)) return;
      if (runtimeStopRequested) return;
      recordRuntimeEventForOverproduction(eventName, payload);
      const nodeIds = resolveAllActivityNodeIds(payload);
      for (const nodeId of nodeIds) {
        pushRecentActiveNode(nodeId);
        incrementActivityNode(nodeId);
      }
      return;
    }
    if (eventName === "runtime.nodeStopped") {
      if (!activityProjectMatches(payload)) return;
      if (runtimeStopRequested) return;
      recordRuntimeEventForOverproduction(eventName, payload);
      const nodeIds = resolveAllActivityNodeIds(payload);
      for (const nodeId of nodeIds) {
        pushRecentStoppedNode(nodeId);
        if (commandActivityHeldNodeIds.has(nodeId)) {
          const heldKind = commandActivityKindByNodeId.get(nodeId);
          if (heldKind === "playScene") {
            // Keep command-driven highlight while long-running scene playback is still active.
          } else {
            // Defensive cleanup: do not keep stale non-scene command holds.
            releaseCommandActivityNode(nodeId);
          }
        } else if (consumePendingPlaySceneStart()) {
          holdCommandActivityNode(nodeId, "playScene");
        } else {
          decrementActivityNode(nodeId);
        }
        clearTimeoutEdgesForNode(nodeId);
      }
      return;
    }
    if (eventName === "runtime.edgeActive") {
      if (!activityProjectMatches(payload)) return;
      if (runtimeStopRequested) return;
      recordRuntimeEventForOverproduction(eventName, payload);
      const edgeType = normalizeProtocolEdgeType(payload.edgeType);
      const sourceNodeIds = resolveAllActivityNodeIds({
        nodeId: payload.sourceId,
        parentId: payload.sourceParentId,
        ancestorIds: payload.sourceAncestorIds
      });
      // Once an outgoing edge fires, the source node is no longer executing.
      // Clear any stale min-highlight, timeout, or command-driven holds.
      for (const sourceNodeId of sourceNodeIds) {
        forceClearNodeActivity(sourceNodeId);
        clearTimeoutEdgesForNode(sourceNodeId);
      }
      const edgeId = resolveActivityEdgeId({ ...payload, edgeType });
      if (edgeId) {
        if (edgeType === "TEDGE") {
          // Timeout edges may execute without timeout metadata in this event.
          // Ensure a visible edge pulse at execution time.
          registerEdgeActivity(edgeId);
          registerTimeoutEdge(edgeId, payload.startedAt, payload.timeoutMs);
        } else {
          registerEdgeActivity(edgeId);
        }
      }
      return;
    }
    if (eventName === "runtime.timeoutProgress") {
      if (!activityProjectMatches(payload)) return;
      if (runtimeStopRequested) return;
      recordRuntimeEventForOverproduction(eventName, payload);
      const edgeType = normalizeProtocolEdgeType(payload.edgeType);
      const timeoutMs = Number(payload.timeoutMs);
      const startedAt = Number.isFinite(Number(payload.startedAt))
        ? Number(payload.startedAt)
        : (Number.isFinite(Number(payload.elapsedMs)) ? Date.now() - Number(payload.elapsedMs) : Date.now());
      const sourceNodeId = resolveActivityNodeId({
        nodeId: payload.sourceId,
        parentId: payload.sourceParentId,
        ancestorIds: payload.sourceAncestorIds
      });
      if (sourceNodeId && isSuperNodeId(sourceNodeId) && Number.isFinite(timeoutMs) && timeoutMs > 0) {
        holdActivityNodeUntil(sourceNodeId, startedAt + timeoutMs);
      }
      const edgeId = resolveActivityEdgeId({ ...payload, edgeType });
      if (edgeId) {
        registerTimeoutEdge(edgeId, startedAt, payload.timeoutMs);
      }
      return;
    }
    if (eventName === "runtime.scene.playing") {
      if (!activityProjectMatches(payload)) return;
      let boundNodeId = resolveActivityNodeId(payload);
      if (boundNodeId) {
        holdCommandActivityNode(boundNodeId, "playScene");
      } else {
        boundNodeId = bindPlaySceneToRecentActiveNode();
        if (!boundNodeId) {
          boundNodeId = bindPlaySceneToRecentStoppedNode();
        }
        if (!boundNodeId) {
          queuePendingPlaySceneStart();
        }
      }
      if (boundNodeId) {
        const next = new Map(playSceneHoldBySceneKey);
        next.set(sceneEventKey(payload), boundNodeId);
        playSceneHoldBySceneKey = next;
      }
      activeScenes = [...activeScenes, {
        sceneName: payload.sceneName, language: payload.language,
        lower: payload.lower, upper: payload.upper
      }];
      return;
    }
    if (eventName === "runtime.scene.turn") {
      if (!activityProjectMatches(payload)) return;
      activeTurns = [...activeTurns, {
        speaker: payload.speaker, lower: payload.lower, upper: payload.upper
      }];
      return;
    }
    if (eventName === "runtime.scene.turnDone") {
      if (!activityProjectMatches(payload)) return;
      activeTurns = activeTurns.filter(
        t => !(t.lower === payload.lower && t.upper === payload.upper));
      return;
    }
    if (eventName === "runtime.scene.done") {
      if (!activityProjectMatches(payload)) return;
      const doneKey = sceneEventKey(payload);
      const directNodeId = resolveActivityNodeId(payload);
      const mappedNodeId = playSceneHoldBySceneKey.get(doneKey);
      const releaseNodeId = mappedNodeId || directNodeId || "";
      if (releaseNodeId) {
        releaseCommandActivityNode(releaseNodeId);
      } else {
        releaseNextPlaySceneHeldNode();
      }
      if (mappedNodeId) {
        const next = new Map(playSceneHoldBySceneKey);
        next.delete(doneKey);
        playSceneHoldBySceneKey = next;
      }
      activeScenes = activeScenes.filter(
        s => !(s.lower === payload.lower && s.upper === payload.upper));
      sceneHistory = [...sceneHistory, {
        timestamp: Date.now(), sceneName: payload.sceneName,
        language: payload.language, lower: payload.lower, upper: payload.upper
      }];
      return;
    }
    if (eventName === "vars.updated") {
      if (payload?.projectId && payload.projectId !== selectedProjectId) return;
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
            : currentSelection?.type === "command"
              ? snapshot.nodes.some((node) =>
                  node.id === currentSelection.nodeId &&
                  Array.isArray(node.commands) &&
                  currentSelection.index >= 0 &&
                  currentSelection.index < node.commands.length
                )
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
    console.log("[applyScriptSnapshot] called, snapshot keys:", snapshot ? Object.keys(snapshot) : null, "text len:", snapshot?.text?.length, "version:", snapshot?.version);
    if (!snapshot) return;
    if (snapshot.projectId && snapshot.projectId !== selectedProjectId) return;
    if (snapshot.text !== undefined) {
      const newText = snapshot.text || "";
      // Preserve a dirty draft (user has unpublished edits) so their changes
      // are not overwritten by a concurrent snapshot from another session.
      // The next auto-apply will pick up the updated scriptVersion and retry.
      const hasDirtyDraft = scriptLoaded && scriptDraft !== scriptText;
      scriptText = newText;
      if (!hasDirtyDraft) {
        scriptDraft = newText;
      } else {
        // Reschedule auto-apply so the pending draft is retried with the
        // correct (just-updated) version.
        clearScriptAutoApplyTimer();
        scriptAutoApplyTimer = setTimeout(runScriptAutoApply, 200);
      }
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
    monitorDialogPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
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
    document.body.style.overflow = monitorDialogPrevBodyOverflow;
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

  async function apiPut(path, body) {
    return apiFetch(path, {
      method: "PUT",
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
    if (semanticDirty) {
      reasons.push("Semantic analysis: results are not saved.");
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
    const currentName = String(selectedProject?.name || "").trim();
    const currentPath = String(selectedProject?.path || "").trim();
    saveAsName = currentName;
    saveAsPath = currentPath ? parentDirectory(currentPath) : "";
    saveAsError = "";
    rememberFocus();
    saveAsDialogOpen = true;
    focusDialog(saveAsDialogEl, saveAsNameInputEl || saveAsInputEl);
  }

  function openDuplicateSaveAsDialog() {
    const currentName = String(selectedProject?.name || "").trim();
    const currentPath = String(selectedProject?.path || "").trim();
    saveAsName = currentName;
    saveAsPath = currentPath ? parentDirectory(currentPath) : "";
    saveAsError = "";
    rememberFocus();
    saveAsDialogOpen = true;
    focusDialog(saveAsDialogEl, saveAsNameInputEl || saveAsInputEl);
  }

  function closeSaveAsDialog() {
    saveAsDialogOpen = false;
    saveAsName = "";
    saveAsError = "";
    restoreFocus();
  }

  async function confirmSaveAs() {
    const name = (saveAsName || "").trim();
    const target = (saveAsPath || "").trim();
    if (!name) {
      saveAsError = "Project name is required.";
      await tick();
      saveAsNameInputEl?.focus();
      return;
    }
    if (!target) {
      saveAsError = "Path is required.";
      await tick();
      saveAsInputEl?.focus();
      return;
    }
    const ok = await saveAsProject(selectedProjectId, target, name);
    if (ok) {
      saveAsDialogOpen = false;
      restoreFocus();
    } else {
      await tick();
      (saveAsError.includes("name") ? saveAsNameInputEl : saveAsInputEl)?.focus();
    }
  }

  function handlePrimarySaveClick() {
    if (!selectedProjectId || projectSaving) return;
    if (saveButtonActsAsSaveAs) {
      openDuplicateSaveAsDialog();
      return;
    }
    saveProject(selectedProjectId);
  }

  async function removeRecentProject(path) {
    if (!path) return;
    await apiPost("/api/v1/projects/recent/remove", { path });
    const normalizedPath = normalizeRecentPath(path);
    recentPinnedProjects = recentPinnedProjects.filter((entry) => entry.path !== normalizedPath);
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
      androidProject: safe.androidProject === true,
      path: safe.path ?? "",
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
      llms: Array.isArray(safe.llms)
        ? safe.llms.map((llm) => ({
            name: llm?.name ?? "",
            features: normalizeConfigFeatures(llm?.features)
          }))
        : [],
      player: {
        features: normalizeConfigFeatures(safe.player?.features)
      },
      llmPrompts: {
        formatPrompt: safe.llmPrompts?.formatPrompt ?? "",
        actionPrompts: Array.isArray(safe.llmPrompts?.actionPrompts) ? [...safe.llmPrompts.actionPrompts] : []
      },
      llmSelections: {
        generate: safe.llmSelections?.generate ?? "",
        semantic: safe.llmSelections?.semantic ?? ""
      },
      semanticServices: {
        basicProvider: safe.semanticServices?.basicProvider ?? "ud",
        udUrl: safe.semanticServices?.udUrl ?? "",
        udTimeoutMs: safe.semanticServices?.udTimeoutMs ?? "",
        analyzeSyntax: safe.semanticServices?.analyzeSyntax ?? safe.semanticServices?.analyzeSvo ?? "true",
        analyzeSvo: safe.semanticServices?.analyzeSvo ?? safe.semanticServices?.analyzeSyntax ?? "true",
        analyzeDaTr: safe.semanticServices?.analyzeDaTr ?? "true",
        daTrLlm: safe.semanticServices?.daTrLlm ?? "",
        systemPrompt: safe.semanticServices?.systemPrompt ?? "",
        promptTemplate: safe.semanticServices?.promptTemplate ?? "",
        runtimeVizRate: safe.semanticServices?.runtimeVizRate ?? "",
        runtimeVizBurst: safe.semanticServices?.runtimeVizBurst ?? ""
      },
      sceneTitleConcepts: Array.isArray(safe.sceneTitleConcepts) ? [...safe.sceneTitleConcepts] : []
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
    const pluginSpecific = Array.isArray(keys.pluginSpecific)
      ? keys.pluginSpecific.map((entry) => ({ ...entry, kind: "pluginSpecific" }))
      : [];
    return [...required, ...optional, ...pluginSpecific];
  }

  function keyHintLabel(entry) {
    if (!entry) return "";
    const desc = (entry.description || "").trim();
    let prefix = "optional";
    if (entry.kind === "required") prefix = "required";
    else if (entry.kind === "pluginSpecific") prefix = "behavior";
    return desc ? `${prefix}: ${desc}` : prefix;
  }

  function findKeyHintDefault(keyOptions, keyName) {
    if (!keyOptions || !keyName) return null;
    const entry = keyOptions.find((opt) => opt.name === keyName);
    if (entry && entry.default !== undefined && entry.default !== null) {
      return String(entry.default);
    }
    return null;
  }

  function isKeyReadonly(keyOptions, keyName) {
    if (!keyOptions || !keyName) return false;
    const entry = keyOptions.find((opt) => opt.name === keyName);
    return entry?.readonly === true;
  }

  function isKeyPluginSpecific(keyOptions, keyName) {
    if (!keyOptions || !keyName) return false;
    const entry = keyOptions.find((opt) => opt.name === keyName);
    return entry?.kind === "pluginSpecific";
  }

  function getKeyType(keyOptions, keyName) {
    if (!keyOptions || !keyName) return "string";
    const entry = keyOptions.find((opt) => opt.name === keyName);
    return entry?.type || "string";
  }

  function getKeyDescription(keyOptions, keyName) {
    if (!keyOptions || !keyName) return "";
    const entry = keyOptions.find((opt) => opt.name === keyName);
    return entry?.description || "";
  }

  function handleNewFeatureKeyInput(event) {
    const key = event.target.value;
    projectConfigNewFeature.key = key;
    // Pre-fill default value if available and value is currently empty
    if (key && !projectConfigNewFeature.value) {
      const defaultVal = findKeyHintDefault(pluginKeyOptions, key);
      if (defaultVal !== null) {
        projectConfigNewFeature.value = defaultVal;
      }
    }
  }

  function handleNewAgentFeatureKeyInput(event) {
    const key = event.target.value;
    projectConfigNewFeature.key = key;
    // Pre-fill default value if available and value is currently empty
    if (key && !projectConfigNewFeature.value) {
      const defaultVal = findKeyHintDefault(agentKeyOptions, key);
      if (defaultVal !== null) {
        projectConfigNewFeature.value = defaultVal;
      }
    }
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

  function altStartSelectionsFromEdge(edge, targetNode) {
    const selections = {};
    const startIds = Array.isArray(targetNode?.startNodeIds)
      ? targetNode.startNodeIds
      : Array.isArray(targetNode?.childNodes)
        ? targetNode.childNodes.filter((node) => node?.isStart && !node?.isHistory).map((node) => node.id)
        : [];
    startIds.forEach((startId) => {
      selections[startId] = "";
    });
    for (const entry of edge?.altStartMap || []) {
      const startId = String(entry?.startId || "").trim();
      const altStartId = String(entry?.altStartId || "").trim();
      if (!startId) continue;
      selections[startId] = altStartId;
    }
    return selections;
  }

  function normalizeAltStartSelections(selections, targetNode) {
    const validIds = new Set(
      (Array.isArray(targetNode?.childNodes) ? targetNode.childNodes : [])
        .filter((node) => node && !node.isHistory)
        .map((node) => node.id)
    );
    return Object.entries(selections || {})
      .map(([startId, altStartId]) => ({
        startId: String(startId || "").trim(),
        altStartId: String(altStartId || "").trim()
      }))
      .filter((entry) => entry.startId && entry.altStartId && validIds.has(entry.altStartId))
      .sort((a, b) => a.startId.localeCompare(b.startId));
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

  function varBadgeLine(def) {
    if (!def) return "";
    const name = (def.name || "").trim();
    const expr = (def.expr ?? def.expression ?? "").trim();
    const hasLiveValue = Object.prototype.hasOwnProperty.call(runtimeValues, name);
    const value = normalizeRuntimeValue(hasLiveValue ? runtimeValues[name] : def.value);
    const capturedInitial = normalizeRuntimeValue(runtimeInitialValues[name]);
    const initial = capturedInitial || expr;
    const showInitial = hasLiveValue && initial !== "" && value !== initial;
    if (value) {
      return showInitial ? `${name} = ${value} (${initial})` : `${name} = ${value}`;
    }
    if (!expr) return name;
    return `${name} = ${expr}`;
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
    if (hasTimeoutInterval(edge)) {
      return `${edge.timeoutMinMs}-${edge.timeoutMaxMs}`;
    }
    const expr = (edge.timeoutExpr ?? "").trim();
    if (expr) return expr;
    if (edge.timeoutMs !== undefined && edge.timeoutMs !== null) {
      return String(edge.timeoutMs);
    }
    return "";
  }

  function hasTimeoutInterval(edge) {
    if (!edge) return false;
    const minRaw = edge.timeoutMinMs;
    const maxRaw = edge.timeoutMaxMs;
    if (minRaw === null || minRaw === undefined || maxRaw === null || maxRaw === undefined) return false;
    const minText = String(minRaw).trim().toLowerCase();
    const maxText = String(maxRaw).trim().toLowerCase();
    if (!minText || !maxText || minText === "null" || maxText === "null") return false;
    const min = Number(minRaw);
    const max = Number(maxRaw);
    return Number.isFinite(min) && Number.isFinite(max) && min >= 0 && max >= min;
  }

  function timeoutModeFromEdge(edge) {
    if (!edge) return "fixed";
    if (hasTimeoutInterval(edge)) return "interval";
    const expr = String(edge.timeoutExpr ?? "").trim();
    if (expr) return "var";
    return "fixed";
  }

  function isTimeoutNumber(value) {
    return /^\d+$/.test(String(value || "").trim());
  }

  function parseTimeoutMs(value) {
    const raw = String(value ?? "").trim();
    if (!/^\d+$/.test(raw)) return null;
    const parsed = Number.parseInt(raw, 10);
    return Number.isFinite(parsed) && parsed >= 0 ? parsed : null;
  }

  function timeoutSliderConfig(value) {
    const current = parseTimeoutMs(value);
    if (!Number.isFinite(current)) return null;
    return { min: 0, max: timeoutSliderMax, step: timeoutSliderStep, value: current };
  }

  function isTimeoutVarName(value) {
    const name = String(value || "").trim();
    if (!name) return false;
    return sceneFlowIntVarNames.includes(name);
  }

  function openTimeoutSlider() {
    if (selectedEdge?.type !== "TEDGE") return;
    if ((edgeDraft?.timeoutMode || "fixed") !== "fixed") return;
    timeoutSliderOpen = true;
  }

  function scheduleTimeoutInspectorApply(task, delayMs = 140) {
    if (timeoutInspectorApplyTimer) {
      clearTimeout(timeoutInspectorApplyTimer);
      timeoutInspectorApplyTimer = null;
    }
    timeoutInspectorApplyTimer = setTimeout(() => {
      timeoutInspectorApplyTimer = null;
      task?.();
    }, delayMs);
  }

  function patchTimeoutEdgeInSceneFlow(edgeId, fields) {
    if (!sceneFlow || !edgeId || !fields) return;
    const nextEdges = (sceneFlow.edges || []).map((edge) => {
      if (!edge || edge.id !== edgeId) return edge;
      const next = { ...edge };
      if (fields.timeoutMs !== undefined) {
        next.timeoutMs = fields.timeoutMs;
        next.timeoutExpr = "";
        delete next.timeoutMinMs;
        delete next.timeoutMaxMs;
      }
      if (fields.timeoutExpr !== undefined) {
        next.timeoutExpr = fields.timeoutExpr;
        delete next.timeoutMinMs;
        delete next.timeoutMaxMs;
      }
      if (fields.timeoutMinMs !== undefined && fields.timeoutMaxMs !== undefined) {
        next.timeoutMinMs = fields.timeoutMinMs;
        next.timeoutMaxMs = fields.timeoutMaxMs;
        next.timeoutExpr = "";
        next.timeoutMs = fields.timeoutMinMs;
      }
      return next;
    });
    sceneFlow = { ...sceneFlow, edges: nextEdges };
  }

  async function sendTimeoutSliderValue(timeoutMs, edgeId = selectedEdge?.id) {
    if (!selectedProjectId || !edgeId) return;
    if (!Number.isFinite(timeoutMs) || timeoutMs < 0) return;
    patchTimeoutEdgeInSceneFlow(edgeId, { timeoutMs });
    if (timeoutSliderSending) {
      timeoutSliderQueuedMs = timeoutMs;
      timeoutSliderQueuedEdgeId = edgeId;
      return;
    }
    timeoutSliderSending = true;
    try {
      await runSceneFlowCommand("SceneFlow.Edge.Update", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId,
        edgeId,
        fields: {
          timeoutMs,
          timeoutExpr: ""
        }
      });
      timeoutSliderLastSent = timeoutMs;
    } finally {
      timeoutSliderSending = false;
      const queued = timeoutSliderQueuedMs;
      const queuedEdgeId = timeoutSliderQueuedEdgeId;
      timeoutSliderQueuedMs = null;
      timeoutSliderQueuedEdgeId = "";
      if (Number.isFinite(queued) && queued !== timeoutSliderLastSent) {
        await sendTimeoutSliderValue(queued, queuedEdgeId || selectedEdge?.id);
      }
    }
  }

  function handleTimeoutSliderInput(event) {
    if (!edgeDraft) return;
    const timeoutMs = parseTimeoutMs(event?.currentTarget?.value);
    if (!Number.isFinite(timeoutMs)) return;
    edgeDraft = { ...edgeDraft, timeoutMode: "fixed", timeoutSpec: String(timeoutMs) };
    edgeEditError = "";
    sendTimeoutSliderValue(timeoutMs, selectedEdge?.id);
  }

  function applyTimeoutEdgeModeDraft() {
    if (!selectedEdge || selectedEdge.type !== "TEDGE" || !edgeDraft) return;
    const mode = edgeDraft.timeoutMode || "fixed";
    if (mode === "fixed") {
      const timeoutMs = parseTimeoutMs(edgeDraft.timeoutSpec);
      if (!Number.isFinite(timeoutMs)) return;
      edgeEditError = "";
      sendTimeoutSliderValue(timeoutMs, selectedEdge.id);
      return;
    }
    if (mode === "var") {
      const raw = String(edgeDraft.timeoutSpec ?? "").trim();
      if (!raw || !isTimeoutVarName(raw) || !selectedProjectId || sceneFlowBusy) return;
      edgeEditError = "";
      runSceneFlowCommand("SceneFlow.Edge.Update", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId,
        edgeId: selectedEdge.id,
        fields: { timeoutExpr: raw }
      });
      return;
    }
    const min = parseTimeoutMs(edgeDraft.timeoutMinSpec);
    const max = parseTimeoutMs(edgeDraft.timeoutMaxSpec);
    if (!Number.isFinite(min) || !Number.isFinite(max) || max <= min) return;
    if (!selectedProjectId || sceneFlowBusy) return;
    edgeEditError = "";
    runSceneFlowCommand("SceneFlow.Edge.Update", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      edgeId: selectedEdge.id,
      fields: { timeoutMinMs: min, timeoutMaxMs: max }
    });
  }

  function handleTimeoutModeInspectorChange() {
    edgeEditError = "";
    timeoutSliderOpen = edgeDraft?.timeoutMode === "fixed";
    scheduleTimeoutInspectorApply(() => applyTimeoutEdgeModeDraft());
  }

  function handleTimeoutFixedInspectorInput(event) {
    if (!edgeDraft) return;
    const raw = String(event?.currentTarget?.value ?? "");
    edgeDraft = { ...edgeDraft, timeoutMode: "fixed", timeoutSpec: raw };
    edgeEditError = "";
    const timeoutMs = parseTimeoutMs(raw);
    if (!Number.isFinite(timeoutMs)) return;
    sendTimeoutSliderValue(timeoutMs, selectedEdge?.id);
  }

  function handleTimeoutVarInspectorInput() {
    edgeEditError = "";
    scheduleTimeoutInspectorApply(() => applyTimeoutEdgeModeDraft());
  }

  function handleTimeoutIntervalInspectorInput() {
    edgeEditError = "";
    scheduleTimeoutInspectorApply(() => applyTimeoutEdgeModeDraft());
  }

  function handleCanvasTimeoutEdgeUpdate(edgeId, timeoutMs) {
    const parsed = Number(timeoutMs);
    if (!edgeId || !Number.isFinite(parsed) || parsed < 0) return;
    if (selectedEdge?.id === edgeId && edgeDraft) {
      edgeDraft = { ...edgeDraft, timeoutMode: "fixed", timeoutSpec: String(Math.floor(parsed)) };
      edgeEditError = "";
    }
    sendTimeoutSliderValue(Math.floor(parsed), edgeId);
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
    const sourceNode = (snapshot?.nodes || []).find((node) => String(node?.id || "").trim() === sourceKey) || null;
    const isSuperNode = sourceNode?.type === "Super";
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
    const hasSelfLoopT = edges.some((edge) => {
      if (normalizeEdgeType(edge?.type) !== "TEDGE") return false;
      const targetId = String(edge?.targetId ?? "").trim();
      return targetId === sourceKey;
    });
    if (hasP) return new Set(["PEDGE"]);
    if (hasI) {
      // Interruptive edges may coexist with a timeout default edge.
      // The data model still allows only one default edge (E/T), so this
      // does not create multiple timeout defaults.
      const allowed = new Set(["IEDGE", "TEDGE"]);
      return allowed;
    }
    if (hasF) return new Set(["FEDGE"]);
    if (hasC) {
      const allowed = new Set(["CEDGE"]);
      if (!hasD) {
        allowed.add("EEDGE");
        allowed.add("TEDGE");
      }
      if (hasSelfLoopT || isSuperNode) {
        allowed.add("IEDGE");
      }
      return allowed;
    }
    if (hasD) {
      const allowed = new Set(["CEDGE"]);
      if (hasSelfLoopT || isSuperNode) {
        allowed.add("IEDGE");
      }
      return allowed;
    }
    return new Set(ALL_EDGE_TYPES);
  }

  function edgeTypeAllowedForSource(type, nodeId) {
    const sourceId = String(nodeId || "").trim();
    if (!sourceId) return true;
    const allowed = sourceId === edgeRestrictionNodeId
      ? edgeRestrictionAllowed
      : allowedEdgeTypesForSource(sourceId, sceneFlow);
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

  function commandEntryText(entry) {
    if (typeof entry === "string") return entry;
    if (!entry || typeof entry !== "object") return "";
    return String(entry.cmd || entry.text || entry.syntax || entry.name || entry.type || entry.label || "");
  }

  function extractSceneFlowPlayActionAgents(flow) {
    if (!flow || typeof flow !== "object") return [];
    const map = new Map();
    const collectFromCommands = (commands) => {
      if (!Array.isArray(commands)) return;
      for (const entry of commands) {
        const parsed = parsePlayActionCommand(commandEntryText(entry));
        const name = String(parsed?.agent || "").trim();
        if (!name) continue;
        const key = name.toLowerCase();
        if (!map.has(key)) {
          map.set(key, name);
        }
      }
    };
    for (const node of Array.isArray(flow.nodes) ? flow.nodes : []) {
      collectFromCommands(node?.commands);
    }
    for (const edge of Array.isArray(flow.edges) ? flow.edges : []) {
      collectFromCommands(edge?.commands);
    }
    return Array.from(map.values()).sort((a, b) => a.localeCompare(b));
  }

  function mergeAgentNames(...groups) {
    const map = new Map();
    for (const group of groups) {
      if (!Array.isArray(group)) continue;
      for (const rawName of group) {
        const name = String(rawName || "").trim();
        if (!name) continue;
        const key = name.toLowerCase();
        if (!map.has(key)) {
          map.set(key, name);
        }
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

  function buildAgentSourceMap(sceneAgents, playActionAgents) {
    const map = new Map();
    const markSource = (names, sourceKey) => {
      if (!Array.isArray(names)) return;
      for (const rawName of names) {
        const name = String(rawName || "").trim();
        if (!name) continue;
        const key = name.toLowerCase();
        const entry = map.get(key) || { name, inScript: false, inPlayAction: false };
        entry.name = entry.name || name;
        if (sourceKey === "script") entry.inScript = true;
        if (sourceKey === "playAction") entry.inPlayAction = true;
        map.set(key, entry);
      }
    };
    markSource(sceneAgents, "script");
    markSource(playActionAgents, "playAction");
    return map;
  }

  function missingAgentSourceLabel(source) {
    const inScript = source?.inScript === true;
    const inPlayAction = source?.inPlayAction === true;
    if (inScript && inPlayAction) return "Scene script and PlayAction";
    if (inScript) return "Scene script";
    if (inPlayAction) return "PlayAction";
    return "Unknown";
  }

  function extractMissingAgentsDetailed(allUsedAgents, agents, sceneAgents = [], playActionAgents = []) {
    if (!Array.isArray(allUsedAgents) || !allUsedAgents.length) return [];
    const configured = new Set();
    if (Array.isArray(agents)) {
      for (const agent of agents) {
        const name = String(agent?.name || "").trim().toLowerCase();
        if (name) configured.add(name);
      }
    }
    const sourceMap = buildAgentSourceMap(sceneAgents, playActionAgents);
    return allUsedAgents
      .filter((name) => {
        const key = String(name || "").trim().toLowerCase();
        return key && !configured.has(key);
      })
      .map((name) => {
        const key = String(name || "").trim().toLowerCase();
        const source = sourceMap.get(key) || { name, inScript: false, inPlayAction: false };
        return {
          name,
          source,
          sourceLabel: missingAgentSourceLabel(source)
        };
      });
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

  function buildAgentGroups(sceneAgents, deviceAgents, interfaces, configView) {
    // Merge all known agent names (from script + from config) into a deduplicated map
    const allAgents = new Map();
    for (const name of sceneAgents || []) {
      const key = String(name).toLowerCase();
      if (!allAgents.has(key)) allAgents.set(key, { name, inScript: true, inConfig: false });
      else allAgents.get(key).inScript = true;
    }
    for (const name of deviceAgents || []) {
      const key = String(name).toLowerCase();
      if (!allAgents.has(key)) allAgents.set(key, { name, inScript: false, inConfig: true });
      else allAgents.get(key).inConfig = true;
    }

    // Helper to resolve plugin interface categories for an agent name
    const resolveCategories = (agentName) => {
      const descriptor = pluginInterfaceForAgentWithContext(agentName, interfaces, configView);
      if (!descriptor?.categories) return null;
      const primary = String(descriptor.categories.primary || "").toLowerCase();
      const secondary = Array.isArray(descriptor.categories.secondary)
        ? descriptor.categories.secondary.map((s) => String(s || "").toLowerCase()).filter(Boolean)
        : [];
      return { primary, secondary };
    };

    const input = [];
    const processing = [];
    const output = [];
    const seen = { input: new Set(), processing: new Set(), output: new Set() };

    const addTo = (group, seenSet, name, type) => {
      const key = name.toLowerCase();
      if (!seenSet.has(key)) {
        seenSet.add(key);
        group.push({ name, type });
      }
    };

    for (const [, agentInfo] of allAgents) {
      const categories = resolveCategories(agentInfo.name);
      if (categories && categories.primary) {
        // Place in primary category group
        const primary = categories.primary;
        if (primary === "input") addTo(input, seen.input, agentInfo.name, "input");
        else if (primary === "processing") addTo(processing, seen.processing, agentInfo.name, "processing");
        else if (primary === "output") addTo(output, seen.output, agentInfo.name, "output");

        // Place in secondary category groups as well
        for (const sec of categories.secondary) {
          if (sec === "input") addTo(input, seen.input, agentInfo.name, "input");
          else if (sec === "processing") addTo(processing, seen.processing, agentInfo.name, "processing");
          else if (sec === "output") addTo(output, seen.output, agentInfo.name, "output");
        }
      } else {
        // Fallback: script agents → output, config-only agents → processing
        if (agentInfo.inScript) {
          addTo(output, seen.output, agentInfo.name, "output");
        } else {
          addTo(processing, seen.processing, agentInfo.name, "processing");
        }
      }
    }

    input.sort((a, b) => a.name.localeCompare(b.name));
    processing.sort((a, b) => a.name.localeCompare(b.name));
    output.sort((a, b) => a.name.localeCompare(b.name));
    return { input, processing, output };
  }

  function pluginInterfaceForAgentWithContext(agentName, interfaces, configView) {
    const agents = configView?.agents || [];
    const plugins = configView?.plugins || [];
    const agent = agents.find((entry) => entry?.name === agentName);
    if (!agent) return null;
    const deviceName = agent?.device || "";
    const plugin = plugins.find((entry) => entry?.name === deviceName);
    const className = plugin?.className || "";
    const normalizeKey = (value) => String(value || "").trim().toLowerCase();
    const simpleClassName = (value) => {
      const text = String(value || "").trim();
      if (!text) return "";
      const parts = text.split(".");
      return parts[parts.length - 1] || text;
    };
    const matchesDescriptor = (descriptor, key, simpleKey) => {
      const descriptorPlugin = descriptor?.plugin || {};
      const id = normalizeKey(descriptorPlugin.id);
      const name = normalizeKey(descriptorPlugin.name);
      const classKey = normalizeKey(descriptorPlugin.className);
      return [id, name, classKey].some((entry) => entry && (entry === key || (simpleKey && entry === simpleKey)));
    };
    if (className) {
      const classKey = normalizeKey(className);
      const simpleKey = normalizeKey(simpleClassName(className));
      const match = (interfaces || []).find((entry) => matchesDescriptor(entry, classKey, simpleKey));
      if (match) return match;
    }
    if (deviceName) {
      const deviceKey = normalizeKey(deviceName);
      const simpleKey = normalizeKey(simpleClassName(deviceName));
      const match = (interfaces || []).find((entry) => matchesDescriptor(entry, deviceKey, simpleKey));
      if (match) return match;
    }
    return null;
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
    return (names || []).map((entry) => ({
      name: entry?.name || "",
      source: entry?.source || null,
      sourceLabel: entry?.sourceLabel || "Unknown",
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
    missingAgentDrafts = buildMissingAgentDrafts(missingAgentItems, projectConfigPlugins);
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

  function openEventOverprodDialog() {
    rememberFocus();
    eventOverprodDialogOpen = true;
  }

  function closeEventOverprodDialogAndRestoreFocus() {
    closeEventOverprodDialog();
    restoreFocus();
  }

  function muteEventOverprodDialogForRunAndClose() {
    muteEventOverprodDialogForRun();
    restoreFocus();
  }

  async function checkUndefinedVariables() {
    if (!selectedProjectId) return [];
    try {
      const data = await apiGet(`/api/v1/projects/${selectedProjectId}/validate/vars`);
      return Array.isArray(data?.missing) ? data.missing : [];
    } catch (err) {
      console.warn("Failed to validate variables:", err);
      return [];
    }
  }

  function openMissingVarDialog(items) {
    rememberFocus();
    missingVarItems = Array.isArray(items) ? items : [];
    missingVarDialogOpen = true;
    focusDialog(missingVarDialogEl);
  }

  function closeMissingVarDialog() {
    missingVarDialogOpen = false;
    missingVarItems = [];
    restoreFocus();
  }

  function openVarRenameDialog(oldName, newName, usageCount) {
    rememberFocus();
    varRenameOldName = oldName || "";
    varRenameNewName = newName || "";
    varRenameUsageCount = Math.max(0, Number(usageCount) || 0);
    varRenameDialogOpen = true;
    focusDialog(varRenameDialogEl);
  }

  function closeVarRenameDialog() {
    varRenameDialogOpen = false;
    varRenameOldName = "";
    varRenameNewName = "";
    varRenameUsageCount = 0;
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
      syncLLMSelectionsFromConfig(projectConfig);
      syncSemanticAnalysisSettingsFromConfig(projectConfig);
      syncRuntimeVizGuardFromProjectConfig(projectConfig);
      projectConfigSaved = response.saved ?? null;
      projectConfigPending = response.pending === true;
      missingAgentDialogOpen = false;
      missingAgentDrafts = [];
      await runRuntimeCommand("Runtime.Play", { skipMissingAgentCheck: true });
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
      .map((entry) => entry.type === "command"
        ? `${entry.type}:${entry.nodeId}:${entry.index}`
        : `${entry.type}:${entry.id}`)
      .sort()
      .join("|");
  }

  async function copySceneFlowSelection() {
    if (!sceneFlow || !selectedProjectId) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    const commandSelection = selectionList.find((item) => item.type === "command");
    if (commandSelection) {
      const node = (sceneFlow.nodes || []).find((entry) => entry.id === commandSelection.nodeId);
      const commands = Array.isArray(node?.commands) ? node.commands : [];
      const command = commands[commandSelection.index];
      const text = String(command?.text || "").trim();
      if (!node || !text) return;
      sceneFlowClipboard = {
        kind: "commands",
        commands: [{ text }],
        sourceNodeId: node.id,
        origin: { x: node.graphics?.x ?? 0, y: node.graphics?.y ?? 0 }
      };
      sceneFlowPasteIndex = 0;
      return;
    }
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
      kind: "nodes-comments",
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
    if (sceneFlowClipboard.kind === "commands") {
      const selection = sceneFlowSelection;
      let targetNodeId = "";
      let insertIndex = -1;
      if (selection?.type === "command" && selection.nodeId) {
        targetNodeId = selection.nodeId;
        insertIndex = Number.isFinite(selection.index) ? selection.index + 1 : -1;
      } else if (selection?.type === "node" && selection.id) {
        targetNodeId = selection.id;
        const node = (sceneFlow?.nodes || []).find((entry) => entry.id === targetNodeId);
        insertIndex = Array.isArray(node?.commands) ? node.commands.length : -1;
      } else {
        return;
      }
      let nextSelection = null;
      for (let i = 0; i < (sceneFlowClipboard.commands || []).length; i += 1) {
        const entry = sceneFlowClipboard.commands[i];
        const response = await runSceneFlowCommand("SceneFlow.Node.Cmd.Add", {
          projectId: selectedProjectId,
          superNodeId: sceneFlow?.superNodeId || "",
          nodeId: targetNodeId,
          command: { text: entry.text },
          index: insertIndex < 0 ? undefined : insertIndex + i
        });
        if (!response) return;
        nextSelection = {
          type: "command",
          id: `${targetNodeId}:${(insertIndex < 0 ? i : insertIndex + i)}`,
          nodeId: targetNodeId,
          index: insertIndex < 0 ? i : insertIndex + i
        };
      }
      if (nextSelection) {
        sceneFlowSelection = nextSelection;
        sceneFlowMultiSelection = [nextSelection];
      }
      return;
    }
    const newSelections = [];

    if (sceneFlowClipboard.nodeIds?.length) {
      const response = await runSceneFlowCommand("SceneFlow.Selection.Paste", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId || "",
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
      if (Array.isArray(response.warnings) && response.warnings.length > 0) {
        statusMessage = response.warnings[0];
      }
    }

    for (const comment of sceneFlowClipboard.comments || []) {
      const response = await runSceneFlowCommand("SceneFlow.Comment.Create", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId || "",
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
    if (selectionList.some((entry) => entry.type === "command")) return;
    await copySceneFlowSelection();
    await deleteSceneFlowSelection();
  }

  async function duplicateSceneFlowSelection() {
    if (!selectedProjectId || sceneFlowBusy) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    if (selectionList.some((entry) => entry.type === "command")) return;
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

  function sceneNameSetFromGroups(groups) {
    const names = new Set();
    if (!Array.isArray(groups)) return names;
    groups.forEach((lang) => {
      (lang?.groups || []).forEach((group) => {
        if (group?.name) {
          names.add(group.name);
        }
      });
    });
    return names;
  }

  function sceneGroupKey(language, name) {
    return `${language || ""}::${name || ""}`;
  }

  function camelBackTitle(words) {
    const cleaned = words
      .filter(Boolean)
      .slice(0, 3)
      .map((word) => String(word).replace(/[^A-Za-z0-9]+/g, ""))
      .filter(Boolean);
    if (!cleaned.length) return "";
    return cleaned
      .map((word, idx) => {
        const hasInternalCaps = /[A-Z]/.test(word.slice(1));
        const normalized = hasInternalCaps ? word : word.toLowerCase();
        return normalized.charAt(0).toUpperCase() + normalized.slice(1);
      })
      .join("");
  }

  function normalizeSceneTitleConceptCandidates(concepts) {
    const candidates = new Set();
    (concepts || []).forEach((entry) => {
      const words = String(entry || "")
        .trim()
        .split(/[\s,_-]+/)
        .filter(Boolean);
      const title = camelBackTitle(words);
      if (title) {
        candidates.add(title);
      }
    });
    return Array.from(candidates);
  }

  function extractSceneGroupsWithText(text) {
    const groups = new Map();
    if (!text) return [];
    const lines = String(text).split(/\r?\n/);
    let current = null;
    let buffer = [];
    const flush = () => {
      if (current && buffer.length) {
        const key = sceneGroupKey(current.language, current.name);
        const content = buffer.join(" ").trim();
        if (content) {
          if (!groups.has(key)) {
            groups.set(key, { ...current, texts: [] });
          }
          groups.get(key).texts.push(content);
        }
      }
      current = null;
      buffer = [];
    };
    for (const raw of lines) {
      const line = raw.trim();
      if (!line || line.startsWith("//") || line.startsWith("#")) {
        continue;
      }
      const match = line.match(/^scene\s+(\S+)\s+(.+)$/i);
      if (match) {
        flush();
        current = { language: match[1], name: match[2].trim() };
        continue;
      }
      if (current) {
        const stripped = line.replace(/^[^:]+:\s*/, "");
        if (stripped) {
          buffer.push(stripped);
        }
      }
    }
    flush();
    const output = [];
    groups.forEach((value) => {
      const combined = value.texts.join(" ").slice(0, 1200);
      output.push({ language: value.language, name: value.name, text: combined });
    });
    return output;
  }

  function buildSceneTextMapFromScript(text) {
    const groups = extractSceneGroupsWithText(text);
    const map = new Map();
    groups.forEach((group) => {
      const name = (group?.name || "").trim();
      if (!name) return;
      const existing = map.get(name);
      const combined = existing ? `${existing} ${group.text}`.trim() : group.text;
      map.set(name, combined);
    });
    return map;
  }

  function keywordsFromText(text, globalCounts = new Map(), globalSceneCount = 1) {
    if (!text) return [];
    const stop = new Set([
      "the", "and", "for", "with", "that", "this", "have", "has", "are", "was", "were", "you",
      "your", "our", "their", "they", "them", "she", "him", "her", "its", "not", "but", "can",
      "will", "just", "from", "into", "then", "than", "about", "what", "when", "where", "who",
      "why", "how", "say", "says", "said", "im", "i", "me", "my", "we", "us", "a", "an", "to",
      "of", "in", "on", "at", "is", "it", "be", "as", "or", "if", "so", "do", "does", "did",
      "yeah", "okay", "ok", "hello", "hi", "hey", "thanks", "thank", "please",
      "du", "ich", "wir", "ihr", "sie", "er", "sein", "sein", "mein", "dein", "euer", "uns",
      "mich", "dich", "ihn", "ihm", "ihrer", "ihre", "euch", "mir", "dir", "danke",
      "ja", "nein", "bitte", "hallo", "tschuss", "tschüss", "okay", "ok",
      "nicht", "kein", "keine", "keiner", "keines", "nichts",
      "sein", "bin", "bist", "ist", "sind", "seid", "war", "waren", "wurde", "wurden",
      "haben", "habe", "hast", "hat", "habt", "hatte", "hatten",
      "werden", "werde", "wirst", "wird", "werdet",
      "kann", "kannst", "können", "koennen",
      "muss", "musst", "müssen", "muessen",
      "soll", "sollst", "sollen",
      "will", "willst", "wollen",
      "darf", "darfst", "dürfen", "duerfen",
      "geh", "gehe", "gehst", "gehen",
      "komm", "komme", "kommst", "kommen",
      "seh", "sehe", "siehst", "sehen",
      "sag", "sage", "sagst", "sagen",
      "mach", "mache", "machst", "machen",
      "dein", "deine", "deiner", "deines",
      "mein", "meine", "meiner", "meines"
    ]);
    const commonThreshold = Math.max(2, Math.ceil(globalSceneCount * 0.35));
    const verbLike = /(st|est|en|t)$/i;
    const tokens = String(text)
      .toLowerCase()
      .replace(/[^a-z0-9\s]/g, " ")
      .split(/\s+/)
      .filter((word) => word.length >= 4 && !stop.has(word))
      .filter((word) => {
        const count = globalCounts.get(word) || 0;
        if (count >= commonThreshold) return false;
        if (verbLike.test(word)) return false;
        return true;
      });
    const counts = new Map();
    tokens.forEach((word) => counts.set(word, (counts.get(word) || 0) + 1));
    return Array.from(counts.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 8)
      .map(([word]) => word);
  }

  function buildCandidateTitles(text, globalCounts, globalSceneCount) {
    const keywords = keywordsFromText(text, globalCounts, globalSceneCount);
    const single = keywords.map((word) => camelBackTitle([word])).filter(Boolean);
    const candidates = new Set(single);
    if (single.length < 3) {
      for (let i = 0; i < Math.min(4, keywords.length); i += 1) {
        for (let j = i + 1; j < Math.min(6, keywords.length); j += 1) {
          const title = camelBackTitle([keywords[i], keywords[j]]);
          if (title) {
            candidates.add(title);
          }
        }
      }
    }
    return Array.from(candidates);
  }

  async function fetchEmbeddings(texts) {
    const available = await checkEmbeddingsService();
    if (!available) return null;
    try {
      const response = await fetchWithTimeout(
        `${EMBEDDINGS_URL}/embed`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ texts })
        },
        5000
      );
      if (!response.ok) {
        return null;
      }
      const data = await response.json();
      return data?.vectors || null;
    } catch (err) {
      return null;
    }
  }

  function cosineSimilarity(a, b) {
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length || a.length === 0) return 0;
    let dot = 0;
    for (let i = 0; i < a.length; i += 1) {
      dot += a[i] * b[i];
    }
    return dot;
  }

  function clusterScenesBySimilarity(items, vectors) {
    const clusters = [];
    const visited = new Set();
    for (let i = 0; i < items.length; i += 1) {
      if (visited.has(i)) continue;
      const stack = [i];
      const cluster = [];
      visited.add(i);
      while (stack.length) {
        const idx = stack.pop();
        cluster.push(idx);
        for (let j = 0; j < items.length; j += 1) {
          if (visited.has(j) || j === idx) continue;
          const sim = cosineSimilarity(vectors[idx], vectors[j]);
          if (sim >= SCENE_TITLE_CLUSTER_THRESHOLD) {
            visited.add(j);
            stack.push(j);
          }
        }
      }
      clusters.push(cluster);
    }
    return clusters;
  }

  function setsEqual(a, b) {
    if (a === b) return true;
    if (!a || !b || a.size !== b.size) return false;
    for (const item of a) {
      if (!b.has(item)) return false;
    }
    return true;
  }

  function levenshteinDistance(a, b) {
    const s = String(a || "");
    const t = String(b || "");
    const m = s.length;
    const n = t.length;
    if (m === 0) return n;
    if (n === 0) return m;
    const dp = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
    for (let i = 0; i <= m; i += 1) dp[i][0] = i;
    for (let j = 0; j <= n; j += 1) dp[0][j] = j;
    for (let i = 1; i <= m; i += 1) {
      for (let j = 1; j <= n; j += 1) {
        const cost = s[i - 1] === t[j - 1] ? 0 : 1;
        dp[i][j] = Math.min(
          dp[i - 1][j] + 1,
          dp[i][j - 1] + 1,
          dp[i - 1][j - 1] + cost
        );
      }
    }
    return dp[m][n];
  }

  function closestSceneName(target, candidates) {
    const pool = Array.isArray(candidates) ? candidates.filter(Boolean) : [];
    if (!target || pool.length === 0) return "";
    let best = pool[0];
    let bestScore = Infinity;
    const targetLower = target.toLowerCase();
    for (const candidate of pool) {
      const score = levenshteinDistance(targetLower, String(candidate).toLowerCase());
      if (score < bestScore) {
        bestScore = score;
        best = candidate;
      }
    }
    return best;
  }

  async function fetchWithTimeout(url, options = {}, timeoutMs = 1200) {
    const controller = new AbortController();
    const id = setTimeout(() => controller.abort(), timeoutMs);
    try {
      const response = await fetch(url, { ...options, signal: controller.signal });
      return response;
    } finally {
      clearTimeout(id);
    }
  }

  async function checkEmbeddingsService() {
    if (embeddingsChecking) return embeddingsAvailable === true;
    const now = Date.now();
    if (embeddingsAvailable !== null && now - embeddingsLastChecked < 10000) {
      return embeddingsAvailable === true;
    }
    embeddingsChecking = true;
    embeddingsLastChecked = now;
    try {
      const response = await fetchWithTimeout(`${EMBEDDINGS_URL}/health`, {}, 1200);
      embeddingsAvailable = response.ok;
      embeddingsReady = false;
      embeddingsModel = "";
      embeddingsHealthError = "";
      if (response.ok) {
        try {
          const health = await response.json();
          embeddingsReady = health?.ready === true;
          embeddingsModel = health?.model || "";
          embeddingsHealthError = health?.error || "";
        } catch (err) {
          // ignore parse error
        }
      }
      if (!embeddingsAvailable && wsConnected && !embeddingsStarting) {
        embeddingsStartAttempted = true;
        embeddingsStarting = true;
        try {
          const startResp = await sendCommand("Embeddings.Start", {});
          if (startResp?.error) {
            console.warn("[embeddings] start failed:", startResp.error);
          }
        } catch (err) {
          console.warn("[embeddings] start failed:", err);
        }
        embeddingsAvailable = await waitForEmbeddingsHealth(5000);
        embeddingsStarting = false;
      }
      embeddingsChecking = false;
      return embeddingsAvailable === true;
    } catch (err) {
      embeddingsAvailable = false;
      if (wsConnected && !embeddingsStarting) {
        embeddingsStarting = true;
        try {
          const startResp = await sendCommand("Embeddings.Start", {});
          if (startResp?.error) {
            console.warn("[embeddings] start failed:", startResp.error);
          }
        } catch (startErr) {
          console.warn("[embeddings] start failed:", startErr);
        }
        embeddingsAvailable = await waitForEmbeddingsHealth(5000);
        embeddingsStarting = false;
      }
      embeddingsChecking = false;
      return false;
    }
  }

  async function waitForEmbeddingsHealth(timeoutMs) {
    const start = Date.now();
    let delay = 300;
    while (Date.now() - start < timeoutMs) {
      try {
        const resp = await fetchWithTimeout(`${EMBEDDINGS_URL}/health`, {}, 1200);
        if (resp.ok) {
          return true;
        }
      } catch (err) {
        // ignore and retry
      }
      await new Promise((resolve) => setTimeout(resolve, delay));
      delay = Math.min(1200, Math.floor(delay * 1.5));
    }
    return false;
  }

  async function fetchSemanticSuggestions(removed, candidates) {
    const available = await checkEmbeddingsService();
    console.log("[embeddings] semantic check", {
      available,
      ready: embeddingsReady,
      model: embeddingsModel,
      error: embeddingsHealthError,
      removed,
      candidates: candidates?.length || 0
    });
    if (!available) return [];
    try {
      const response = await fetchWithTimeout(
        `${EMBEDDINGS_URL}/similarity`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ query: removed, candidates, topN: candidates.length })
        },
        4000
      );
      if (!response.ok) {
        console.warn("[embeddings] similarity failed:", response.status);
        return [];
      }
      const data = await response.json();
      const results = Array.isArray(data?.results) ? data.results : [];
      if (results.length) {
        console.log("[embeddings] similarity results", results);
      }
      const normalized = results
        .map((result) => ({
          name: String(result?.name || ""),
          score: Number.isFinite(result?.score) ? result.score : null,
          model: data?.model || ""
        }))
        .filter((result) => result.name);
      const top = normalized[0];
      if (top) {
        console.log("[embeddings] similarity result", top);
      }
      return normalized;
    } catch (err) {
      embeddingsAvailable = false;
      console.warn("[embeddings] similarity error:", err);
      return [];
    }
  }

  async function fetchPlaySceneReferences(sceneName) {
    if (!selectedProjectId || !sceneName) return [];
    const requestId = ++renameSceneRequestId;
    try {
      const response = await sendCommand("SceneFlow.PlayScene.Find", {
        projectId: selectedProjectId,
        sceneName
      });
      if (requestId !== renameSceneRequestId) return [];
      return Array.isArray(response?.matches) ? response.matches : [];
    } catch (err) {
      if (requestId === renameSceneRequestId) {
        renameSceneError = err.message || "Failed to scan PlayScene references.";
      }
      return [];
    }
  }

  async function fetchPlaySceneReferencesMany(sceneNames) {
    if (!selectedProjectId || !Array.isArray(sceneNames) || sceneNames.length === 0) return [];
    const requestId = ++danglingSceneRequestId;
    try {
      const response = await sendCommand("SceneFlow.PlayScene.FindMany", {
        projectId: selectedProjectId,
        sceneNames
      });
      if (requestId !== danglingSceneRequestId) return [];
      return Array.isArray(response?.matches) ? response.matches : [];
    } catch (err) {
      if (requestId === danglingSceneRequestId) {
        danglingSceneError = err.message || "Failed to scan PlayScene references.";
      }
      return [];
    }
  }

  function openRenameSceneDialog(oldName, newName, matches) {
    renameSceneOldName = oldName;
    renameSceneNewName = newName;
    renameSceneMatches = Array.isArray(matches) ? matches : [];
    renameSceneError = "";
    renameSceneBusy = false;
    renameSceneDialogOpen = true;
    focusDialog(renameSceneDialogEl);
  }

  function closeRenameSceneDialog() {
    renameSceneDialogOpen = false;
    renameSceneOldName = "";
    renameSceneNewName = "";
    renameSceneMatches = [];
    renameSceneBusy = false;
    renameSceneError = "";
  }

  function openDanglingSceneDialog(removedScenes, matches, removedTextMap = new Map()) {
    danglingSceneRemoved = Array.isArray(removedScenes) ? removedScenes : [];
    danglingSceneMatches = Array.isArray(matches) ? matches : [];
    const candidates = Array.from(sceneNameSetFromGroups(scriptScenesLive));
    danglingSceneReplacements = danglingSceneRemoved.map((name) => {
      const suggestion = closestSceneName(name, candidates);
      const sourceText = removedTextMap?.get(name) || "";
      return {
        name,
        suggestion,
        semantic: false,
        selected: suggestion,
        options: candidates,
        sourceText
      };
    });
    danglingSceneError = "";
    danglingSceneBusy = false;
    danglingSceneDialogOpen = true;
    focusDialog(danglingSceneDialogEl);
    void checkEmbeddingsService();
    void enrichDanglingSuggestionsWithSemantic();
  }

  function closeDanglingSceneDialog() {
    danglingSceneDialogOpen = false;
    danglingSceneRemoved = [];
    danglingSceneMatches = [];
    danglingSceneReplacements = [];
    danglingSceneBusy = false;
    danglingSceneError = "";
  }

  function updateDanglingReplacement(index, value) {
    danglingSceneReplacements = danglingSceneReplacements.map((entry, idx) =>
      idx === index ? { ...entry, selected: value } : entry
    );
  }

  async function enrichDanglingSuggestionsWithSemantic() {
    if (!danglingSceneReplacements.length) return;
    const candidates = Array.from(sceneNameSetFromGroups(scriptScenesLive));
    if (!candidates.length) return;
    const available = await checkEmbeddingsService();
    if (!available) {
      return;
    }
    const candidateTextMap = buildSceneTextMapFromScript(scriptDraft || "");
    const updates = [];
    for (const entry of danglingSceneReplacements) {
      const queryText = (entry?.sourceText || "").trim();
      if (!queryText) continue;
      const texts = [queryText];
      const names = [];
      candidates.forEach((name) => {
        const text = (candidateTextMap.get(name) || "").trim();
        if (!text) return;
        names.push(name);
        texts.push(text);
      });
      if (names.length === 0) continue;
      const vectors = await fetchEmbeddings(texts);
      if (!vectors || vectors.length !== texts.length) continue;
      const queryVector = vectors[0];
      const scored = names
        .map((name, idx) => ({
          name,
          score: cosineSimilarity(queryVector, vectors[idx + 1])
        }))
        .sort((a, b) => b.score - a.score);
      const topOptions = scored.slice(0, 3);
      const result = topOptions[0];
      if (result && result.name) {
        updates.push({
          name: entry.name,
          suggestion: result.name,
          options: topOptions,
          score: result.score,
          model: embeddingsModel || ""
        });
      }
    }
    if (!updates.length) return;
    danglingSceneReplacements = danglingSceneReplacements.map((entry) => {
      const update = updates.find((u) => u.name === entry.name);
      if (!update) return entry;
      return {
        ...entry,
        suggestion: update.suggestion,
        selected: update.suggestion,
        semantic: true,
        semanticOptions: update.options || [],
        semanticScore: update.score,
        semanticModel: update.model
      };
    });
  }

  $: danglingSceneCanApply = danglingSceneReplacements.some(
    (entry) => entry?.selected && entry.selected !== entry.name
  );

  async function applyDanglingReplacements() {
    if (!selectedProjectId) return;
    danglingSceneBusy = true;
    danglingSceneError = "";
    let failures = 0;
    for (const entry of danglingSceneReplacements) {
      const target = (entry?.selected || "").trim();
      if (!entry?.name || !target || entry.name === target) {
        continue;
      }
      const response = await sendCommand("SceneFlow.PlayScene.Rename", {
        projectId: selectedProjectId,
        sceneName: entry.name,
        newName: target,
        superNodeId: sceneFlow?.superNodeId || ""
      });
      if (!response || response.status !== "ok") {
        failures += 1;
      } else if (response.snapshot) {
        sceneFlow = response.snapshot;
        sceneFlowDirty = true;
      }
    }
    danglingSceneBusy = false;
    if (failures > 0) {
      danglingSceneError = `Failed to update ${failures} scene reference${failures === 1 ? "" : "s"}.`;
      return;
    }
    closeDanglingSceneDialog();
  }

  async function generateSceneTitleSuggestions() {
    if (sceneTitleSuggestBusy) return;
    sceneTitleSuggestBusy = true;
    sceneTitleSuggestError = "";
    sceneTitleSuggestMessage = "";
    try {
      const groups = extractSceneGroupsWithText(scriptDraft);
      if (!groups.length) {
        sceneTitleSuggestMessage = "No scenes found in the script.";
        return;
      }
      const conceptCandidates = normalizeSceneTitleConceptCandidates(projectConfigView?.sceneTitleConcepts);
      if (!conceptCandidates.length) {
        sceneTitleSuggestError = "Add scene title concepts in Project Settings to generate suggestions.";
        return;
      }
      const texts = groups.map((group) => group.text);
      const vectors = await fetchEmbeddings(texts);
      if (!vectors || vectors.length !== groups.length) {
        sceneTitleSuggestError = "Semantic model not available.";
        return;
      }
      const clusters = clusterScenesBySimilarity(groups, vectors);
      const suggestions = new Map();
      for (const cluster of clusters) {
        const clusterText = cluster.map((idx) => groups[idx].text).join(" ").slice(0, 1200);
        const results = await fetchSemanticSuggestions(clusterText, conceptCandidates);
        const topResults = results.length
          ? results.slice(0, 3)
          : conceptCandidates.slice(0, 3).map((name) => ({ name, score: null, model: "" }));
        const title = topResults[0]?.name || conceptCandidates[0];
        for (const idx of cluster) {
          const group = groups[idx];
          const key = sceneGroupKey(group.language, group.name);
          suggestions.set(key, {
            current: group.name,
            language: group.language,
            suggestion: title,
            suggestions: topResults,
            semantic: true,
            semanticScore: topResults[0]?.score ?? null,
            semanticModel: topResults[0]?.model ?? ""
          });
        }
      }
      sceneTitleSuggestions = suggestions;
      sceneTitleSuggestMessage = suggestions.size ? "Suggestions ready." : "No suggestions generated.";
    } catch (err) {
      sceneTitleSuggestError = err.message || "Failed to generate suggestions.";
    } finally {
      sceneTitleSuggestBusy = false;
    }
  }

  async function applySceneTitleSuggestion(key, overrideName = "") {
    const suggestion = sceneTitleSuggestions.get(key);
    const target = overrideName || suggestion?.suggestion || "";
    if (!suggestion || !target || target === suggestion.current) {
      sceneTitleSuggestions.delete(key);
      sceneTitleSuggestions = new Map(sceneTitleSuggestions);
      return;
    }
    const currentName = suggestion.current;
    const newName = target;
    const language = suggestion.language;
    const headerRegex = new RegExp(`^(scene\\s+${language.replace(/[-/\\\\^$*+?.()|[\\]{}]/g, "\\$&")}\\s+)${currentName.replace(/[-/\\\\^$*+?.()|[\\]{}]/g, "\\$&")}(\\s*)$`, "gmi");
    scriptDraft = scriptDraft.replace(headerRegex, `$1${newName}$2`);
    await sendCommand("SceneFlow.PlayScene.Rename", {
      projectId: selectedProjectId,
      sceneName: currentName,
      newName,
      superNodeId: sceneFlow?.superNodeId || ""
    });
    sceneTitleSuggestions.delete(key);
    sceneTitleSuggestions = new Map(sceneTitleSuggestions);
  }

  function dismissSceneTitleSuggestion(key) {
    if (!sceneTitleSuggestions.has(key)) return;
    sceneTitleSuggestions.delete(key);
    sceneTitleSuggestions = new Map(sceneTitleSuggestions);
  }

  async function applyAllSceneTitleSuggestions() {
    const keys = Array.from(sceneTitleSuggestions.keys());
    for (const key of keys) {
      await applySceneTitleSuggestion(key);
    }
  }

  function dismissAllSceneTitleSuggestions() {
    sceneTitleSuggestions = new Map();
  }

  async function applyRenameSceneReferences() {
    if (!selectedProjectId || !renameSceneMatches.length) {
      closeRenameSceneDialog();
      return;
    }
    renameSceneBusy = true;
    renameSceneError = "";
    const response = await sendCommand("SceneFlow.PlayScene.Rename", {
      projectId: selectedProjectId,
      sceneName: renameSceneOldName,
      newName: renameSceneNewName,
      superNodeId: sceneFlow?.superNodeId || ""
    });
    renameSceneBusy = false;
    if (!response || response.status !== "ok") {
      renameSceneError = response?.error || "Failed to update PlayScene commands.";
      return;
    }
    if (response.snapshot) {
      sceneFlow = response.snapshot;
      sceneFlowDirty = true;
    }
    closeRenameSceneDialog();
  }

  async function handleSceneListChange(removed, added, removedTextMap) {
    if (!selectedProjectId) return;
    if (renameSceneDialogOpen || danglingSceneDialogOpen) return;
    if (removed.length === 1 && added.length === 1) {
      const matches = await fetchPlaySceneReferences(removed[0]);
      if (matches.length) {
        openRenameSceneDialog(removed[0], added[0], matches);
      }
      return;
    }
    if (!removed.length) return;
    const matches = await fetchPlaySceneReferencesMany(removed);
    if (!matches.length) return;
    openDanglingSceneDialog(removed, matches, removedTextMap);
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
    document.body.style.overflow = typeDefPrevBodyOverflow;
    restoreFocus();
  }

  function closeVarDefDialog() {
    resetVarDefEditor();
    document.body.style.overflow = varDefPrevBodyOverflow;
    restoreFocus();
  }

  function resetCmdEditor() {
    cmdDraft = "";
    cmdEditIndex = null;
    cmdError = "";
    cmdSelectedIndex = null;
    cmdEditingIndex = null;
    cmdInlineDrafts = [];
    cmdDialogNodeId = "";
    cmdInlineInputEls = [];
    cmdHelperVarOp = "Assign";
    cmdHelperSyncing = false;
    cmdHelperDetectedTab = null;
    cmdHelperShowWrites = false;
    cmdHelperShowReads = false;
    cmdHelperShowConfig = false;
    lastCmdHelperDescriptorKey = "";
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
    if (name === "Event") return "";
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
      expression: "",
      eventElementType: "",
      eventCapacity: 0
    };
  }

  function parseEventTypeString(typeStr) {
    const str = (typeStr || "").trim();
    const match = str.match(/^Event\(([^,)]*?)(?:,\s*(\d+))?\)$/i);
    if (!match) return { elementType: "", capacity: 0 };
    const et = (match[1] || "").trim();
    const cap = parseInt(match[2]) || 0;
    return { elementType: et === "*" ? "" : et, capacity: cap };
  }

  function buildEventTypeString(elementType, capacity) {
    const et = (elementType || "").trim();
    const cap = parseInt(capacity) || 0;
    if (!et && cap <= 0) return "Event";
    if (!et && cap > 0) return `Event(*, ${cap})`;
    if (et && cap <= 0) return `Event(${et})`;
    return `Event(${et}, ${cap})`;
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
    typeDefPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
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
    typeDefPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
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
    varDefPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
    focusDialog(varDefDialogEl, varDefNameInputEl);
  }

  function startVarDefEdit(index) {
    const def = nodeEditorVarDefs[index];
    if (!def) return;
    rememberFocus();
    varDefError = "";
    varDefEditIndex = index;
    varDefSelectedIndex = index;
    const rawType = (def.type ?? "").trim();
    const isEvent = rawType.toLowerCase().startsWith("event");
    const eventParsed = isEvent ? parseEventTypeString(rawType) : { elementType: "", capacity: 0 };
    varDefDraft = {
      name: def.name ?? "",
      type: isEvent ? "Event" : (rawType || nodeEditorTypeOptions[0] || "Bool"),
      expression: def.expression ?? "",
      eventElementType: eventParsed.elementType,
      eventCapacity: eventParsed.capacity
    };
    varDefPrevBodyOverflow = document.body.style.overflow || "";
    document.body.style.overflow = "hidden";
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
    if (varDefDraft.type === "Event") {
      varDefDraft.expression = "";
    } else {
      varDefDraft.eventElementType = "";
      varDefDraft.eventCapacity = 0;
    }
  }

  async function applyVarDefEdit() {
    varDefError = "";
    if (!selectedProjectId || !nodeEditorTarget || !varDefDraft) return;
    const previousName =
      varDefEditIndex >= 0 ? String(nodeEditorVarDefs[varDefEditIndex]?.name ?? "").trim() : "";
    const name = (varDefDraft.name ?? "").trim();
    if (!name) {
      varDefError = "Variable name is required.";
      return;
    }
    const baseType = (varDefDraft.type ?? "").trim();
    if (!baseType) {
      varDefError = "Variable type is required.";
      return;
    }
    const type = baseType === "Event"
      ? buildEventTypeString(varDefDraft.eventElementType, varDefDraft.eventCapacity)
      : baseType;
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
    if (
      varDefEditIndex >= 0 &&
      previousName &&
      previousName !== name &&
      (response.renamedReferences ?? 0) > 0
    ) {
      openVarRenameDialog(previousName, name, response.renamedReferences);
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

  async function openCmdDialog(nodeId = null, focusIndex = null) {
    const targetId = nodeId || nodeEditorTarget?.id || "";
    if (!targetId && !nodeEditorTarget) return;
    if (!nodeId && nodeEditorTarget?.isRoot) {
      cmdError = "Command executions are disabled for the top-level SceneFlow.";
      return;
    }
    rememberFocus();
    if (nodeId && (sceneFlowSelection?.type !== "node" || sceneFlowSelection.id !== nodeId)) {
      sceneFlowSelection = { type: "node", id: nodeId };
      sceneFlowMultiSelection = [{ type: "node", id: nodeId }];
      await tick();
    }
    cmdDialogOpen = true;
    cmdInlineInputEls = [];
    initCmdHelper();
    syncCmdInlineDrafts();
    if (focusIndex != null && focusIndex >= 0 && focusIndex < cmdInlineDrafts.length) {
      cmdSelectedIndex = focusIndex;
    } else if (cmdInlineDrafts.length > 0) {
      cmdSelectedIndex = 0;
    }
    focusDialog(cmdDialogEl);
  }

  async function startCmdAdd() {
    if (nodeEditorTarget?.isRoot) return;
    await openCmdDialog();
    cmdError = "";
    const nextIndex = cmdInlineDrafts.length;
    cmdInlineDrafts = [...cmdInlineDrafts, ""];
    cmdSelectedIndex = nextIndex;
    cmdEditingIndex = nextIndex;
  }

  async function startCmdEdit(index) {
    if (nodeEditorTarget?.isRoot) return;
    if (index < 0 || index >= nodeEditorCommands.length) return;
    await openCmdDialog();
    cmdError = "";
    cmdSelectedIndex = index;
  }

  function updateCmdInlineDraft(index, value) {
    cmdError = "";
    cmdInlineDrafts = cmdInlineDrafts.map((entry, idx) => (idx === index ? value : entry));
  }

  function handleCmdInlineBlur(event, index) {
    dismissAutocomplete();
    cmdEditingIndex = null;
    const nextTarget = event?.relatedTarget;
    const preservingAdd =
      nextTarget &&
      typeof nextTarget.closest === "function" &&
      nextTarget.closest("[data-cmd-add-button='true']");
    const raw = cmdInlineDrafts[index] ?? "";
    const text = String(raw).trim();
    const isExisting = index < nodeEditorCommands.length;
    if (preservingAdd && !isExisting && !text) {
      return;
    }
    commitCmdInlineDraft(index);
  }

  function buildPlayActionExampleText(agentName, commandEntry) {
    const agent = (agentName || "").trim();
    const command = (commandEntry?.name || "").trim();
    if (!agent || !command) return "";
    if (Array.isArray(commandEntry?.examples) && commandEntry.examples.length) {
      const example = commandEntry.examples.find((ex) => ex?.playAction) || commandEntry.examples[0];
      const exampleText = (example?.playAction || "").trim();
      if (exampleText) return exampleText;
    }
    let payload = `${agent} ${command}`;
    if (Array.isArray(commandEntry?.params) && commandEntry.params.length) {
      const required = commandEntry.params.filter((param) => param?.required);
      if (required.length) {
        const placeholders = required
          .map((param) => `${param?.name || "param"}=<${param?.type || "value"}>`)
          .join(" ");
        payload = `${payload} ${placeholders}`;
      }
    }
    return `PlayAction("[${payload}]")`;
  }

  function applyPlayActionExample(commandEntry) {
    if (cmdSelectedIndex === null || cmdSelectedIndex === undefined) return;
    const text = buildPlayActionExampleText(cmdHelperAgent, commandEntry);
    if (!text) return;
    updateCmdInlineDraft(cmdSelectedIndex, text);
    statusMessage = `Applied: ${text}`;
  }

  function handleCmdInlineKeydown(event, index) {
    if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      dismissAutocomplete();
      commitCmdInlineDraft(index);
      cmdEditingIndex = null;
      event.currentTarget?.blur?.();
      return;
    }
    if (cmdAcVisible && cmdAcItems.length > 0) {
      if (event.key === "ArrowDown") {
        event.preventDefault();
        cmdAcSelectedIdx = (cmdAcSelectedIdx + 1) % cmdAcItems.length;
        return;
      }
      if (event.key === "ArrowUp") {
        event.preventDefault();
        cmdAcSelectedIdx = (cmdAcSelectedIdx - 1 + cmdAcItems.length) % cmdAcItems.length;
        return;
      }
      if (event.key === "Enter" || event.key === "Tab") {
        if (cmdAcSelectedIdx >= 0 && cmdAcSelectedIdx < cmdAcItems.length) {
          event.preventDefault();
          acceptAcItem(event.currentTarget, cmdAcItems[cmdAcSelectedIdx]);
          return;
        }
      }
      if (event.key === "ArrowRight" && cmdAcPrefix.length > 0) {
        if (cmdAcSelectedIdx >= 0 && cmdAcSelectedIdx < cmdAcItems.length) {
          event.preventDefault();
          acceptAcItem(event.currentTarget, cmdAcItems[cmdAcSelectedIdx]);
          return;
        }
      }
    }
    if (event.key === "Escape") {
      event.preventDefault();
      if (cmdAcVisible) {
        dismissAutocomplete();
        return;
      }
      cmdEditingIndex = null;
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
    if (nodeEditorTarget?.isRoot) {
      cmdError = "Command executions are disabled for the top-level SceneFlow.";
      return;
    }
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
          cmdEditingIndex = null;
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
    if (nodeEditorTarget?.isRoot) return null;
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

  async function moveNodeCommand(nodeId, from, to) {
    if (!selectedProjectId || !nodeId) return null;
    if (!Number.isFinite(from) || !Number.isFinite(to) || from < 0 || to < 0 || from === to) {
      return null;
    }
    pinSelectedNodeSelection();
    return await runSceneFlowCommand("SceneFlow.Node.Cmd.Move", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      nodeId,
      from,
      to
    });
  }

  async function deleteCmd(index) {
    if (nodeEditorTarget?.isRoot) return;
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
    if (nodeEditorTarget?.isRoot) return;
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
    if (nodeEditorTarget?.isRoot) return;
    if (cmdSelectedIndex === null) return;
    const index = cmdSelectedIndex;
    if (index >= nodeEditorCommands.length) {
      cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
      cmdSelectedIndex = null;
      cmdEditingIndex = null;
      return;
    }
    const response = await deleteCmd(index);
    if (response && cmdDialogOpen) {
      cmdInlineDrafts = cmdInlineDrafts.filter((_, idx) => idx !== index);
    }
    cmdSelectedIndex = null;
    cmdEditingIndex = null;
  }

  function editSelectedCmd() {
    if (nodeEditorTarget?.isRoot) return;
    if (cmdSelectedIndex === null) return;
    startCmdEdit(cmdSelectedIndex);
  }

  function initCmdHelper() {
    cmdHelperTab = "PlayAction";
    cmdHelperDetectedTab = null;
    cmdHelperScene = helperScenes?.[0] || "";
    cmdHelperAgent = projectConfigAgents?.[0]?.name || "";
    const pluginActionOption = pluginCommandsForAgent(cmdHelperAgent)?.[0] || null;
    const fallbackActionOption = Array.isArray(scriptElements?.acticon) ? scriptElements.acticon[0] : null;
    cmdHelperAction = pluginActionOption?.name || fallbackActionOption?.name || fallbackActionOption?.script || "";
    // Force reactive prefill for the initially selected action params.
    // Without resetting this marker, reopening the dialog can keep the previous
    // action marker and skip arg autoload (e.g., required `time`).
    lastCmdHelperAction = "";
    cmdHelperPlayMode = "blocking";
    cmdHelperArgs = [];
    cmdHelperVarName = helperVarCandidates?.[0]?.name || "";
    cmdHelperVarType = helperVarCandidates?.[0]?.type || "Int";
    cmdHelperVarExpr = "";
    cmdHelperVarStep = "1";
    cmdHelperVarSuggestOpen = false;
    cmdHelperVarSuggestIndex = 0;
    cmdHelperSceneBindings = {};
    cmdHelperVarScope = "global";
    cmdHelperVarOp = "Assign";
  }

  function updateCmdHelperTab() {
    if (cmdHelperTab === "Variable") {
      cmdHelperVarOp = cmdHelperVarOp || "Assign";
      if (cmdHelperVarOp === "Inc" || cmdHelperVarOp === "Dec") {
        cmdHelperVarType = "Int";
        cmdHelperVarStep = cmdHelperVarStep || "1";
      }
    }
    if (cmdHelperTab !== "PlayAction") {
      lastCmdHelperAction = "";
    }
  }

  function updateCmdHelperVarOp() {
    if (cmdHelperVarOp === "Inc" || cmdHelperVarOp === "Dec") {
      cmdHelperVarType = "Int";
      cmdHelperVarStep = cmdHelperVarStep || "1";
      cmdHelperVarExpr = "";
    }
    if (cmdHelperVarOp === "Assign") {
      cmdHelperVarExpr = cmdHelperVarExpr || "";
    }
  }

  function selectCmdHelperVarSuggestion(item) {
    if (!item) return;
    cmdHelperVarName = String(item.name || "");
    if (item.type) {
      cmdHelperVarType = String(item.type || cmdHelperVarType);
    }
    cmdHelperVarSuggestOpen = false;
    cmdHelperVarSuggestIndex = 0;
    tick().then(() => {
      cmdHelperVarInputEl?.focus?.();
      const len = cmdHelperVarName.length;
      setContentEditableCaret(cmdHelperVarInputEl, len);
    });
  }

  function getContentEditableValue(el) {
    if (!el) return "";
    return String(el.textContent || "").replace(/\u00a0/g, " ").replace(/\r/g, "");
  }

  function setContentEditableValue(el, value) {
    if (!el) return;
    const next = String(value || "");
    if (getContentEditableValue(el) === next) return;
    el.textContent = next;
  }

  function getContentEditableCaret(el) {
    if (!el) return 0;
    const sel = window.getSelection?.();
    if (!sel || sel.rangeCount === 0) return getContentEditableValue(el).length;
    const range = sel.getRangeAt(0);
    if (!el.contains(range.startContainer)) return getContentEditableValue(el).length;
    const prefix = range.cloneRange();
    prefix.selectNodeContents(el);
    prefix.setEnd(range.startContainer, range.startOffset);
    return prefix.toString().length;
  }

  function setContentEditableCaret(el, offset) {
    if (!el) return;
    const selection = window.getSelection?.();
    if (!selection) return;
    const target = Math.max(0, Math.min(String(getContentEditableValue(el)).length, Number(offset) || 0));
    const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    let node = walker.nextNode();
    let remaining = target;
    while (node) {
      const length = node.textContent?.length || 0;
      if (remaining <= length) {
        const range = document.createRange();
        range.setStart(node, remaining);
        range.collapse(true);
        selection.removeAllRanges();
        selection.addRange(range);
        return;
      }
      remaining -= length;
      node = walker.nextNode();
    }
    const range = document.createRange();
    range.selectNodeContents(el);
    range.collapse(false);
    selection.removeAllRanges();
    selection.addRange(range);
  }

  function handleCmdHelperVarInput(event) {
    cmdHelperVarName = getContentEditableValue(event?.currentTarget);
    cmdHelperVarSuggestIndex = cmdHelperVarSuggestions.length > 0 ? 0 : -1;
    cmdHelperVarSuggestOpen = cmdHelperVarSuggestions.length > 0;
  }

  function handleCmdHelperVarFocus() {
    cmdHelperVarSuggestOpen = cmdHelperVarSuggestions.length > 0;
    if (cmdHelperVarSuggestOpen && cmdHelperVarSuggestIndex < 0) {
      cmdHelperVarSuggestIndex = 0;
    }
  }

  function handleCmdHelperVarBlur() {
    setTimeout(() => {
      cmdHelperVarSuggestOpen = false;
    }, 120);
  }

  function handleCmdHelperVarKeydown(event) {
    if (!cmdHelperVarSuggestions.length) return;
    if (event.key === "ArrowDown") {
      event.preventDefault();
      cmdHelperVarSuggestOpen = true;
      cmdHelperVarSuggestIndex = (cmdHelperVarSuggestIndex + 1 + cmdHelperVarSuggestions.length) % cmdHelperVarSuggestions.length;
      return;
    }
    if (event.key === "ArrowUp") {
      event.preventDefault();
      cmdHelperVarSuggestOpen = true;
      cmdHelperVarSuggestIndex = (cmdHelperVarSuggestIndex - 1 + cmdHelperVarSuggestions.length) % cmdHelperVarSuggestions.length;
      return;
    }
    if (event.key === "Tab" || event.key === "Enter" || event.key === " ") {
      if (!cmdHelperVarSuggestOpen) return;
      const item = cmdHelperVarSuggestions[cmdHelperVarSuggestIndex] || cmdHelperVarSuggestions[0];
      if (!item) return;
      event.preventDefault();
      selectCmdHelperVarSuggestion(item);
    }
  }

  function edgeConditionCurrentToken(text, input) {
    const value = String(text ?? "");
    const pos = Math.max(0, getContentEditableCaret(input));
    let start = pos;
    while (start > 0 && /[A-Za-z0-9_]/.test(value[start - 1])) {
      start -= 1;
    }
    let end = pos;
    while (end < value.length && /[A-Za-z0-9_]/.test(value[end])) {
      end += 1;
    }
    return value.slice(start, pos) || value.slice(start, end);
  }

  function replaceEdgeConditionToken(text, input, replacement) {
    const value = String(text ?? "");
    const cursor = Math.max(0, getContentEditableCaret(input));
    let start = cursor;
    while (start > 0 && /[A-Za-z0-9_]/.test(value[start - 1])) {
      start -= 1;
    }
    let end = cursor;
    while (end < value.length && /[A-Za-z0-9_]/.test(value[end])) {
      end += 1;
    }
    return {
      value: `${value.slice(0, start)}${replacement}${value.slice(end)}`,
      caret: start + String(replacement || "").length
    };
  }

  function patchConditionEdgeInSceneFlow(edgeId, fields) {
    if (!sceneFlow || !edgeId || !fields) return;
    const nextEdges = (sceneFlow.edges || []).map((edge) => {
      if (!edge || edge.id !== edgeId) return edge;
      const next = { ...edge };
      if (fields.condition !== undefined) {
        next.condition = fields.condition;
      }
      if (fields.altStartMap !== undefined) {
        next.altStartMap = fields.altStartMap;
      }
      return next;
    });
    sceneFlow = { ...sceneFlow, edges: nextEdges };
  }

  function selectEdgeConditionSuggestion(item) {
    if (!item || !edgeDraft) return;
    const next = replaceEdgeConditionToken(edgeDraft.condition ?? "", edgeConditionInputEl, String(item.name || ""));
    edgeDraft = { ...edgeDraft, condition: next.value };
    edgeConditionSuggestOpen = false;
    edgeConditionSuggestIndex = 0;
    edgeEditError = "";
    tick().then(() => {
      edgeConditionInputEl?.focus?.();
      setContentEditableCaret(edgeConditionInputEl, next.caret);
    });
    scheduleConditionEdgeDraftApply();
  }

  function handleEdgeConditionInput(event) {
    if (!edgeDraft) return;
    const value = getContentEditableValue(event?.currentTarget);
    edgeDraft = { ...edgeDraft, condition: value };
    edgeEditError = "";
    edgeConditionSuggestIndex = edgeConditionSuggestions.length > 0 ? 0 : -1;
    edgeConditionSuggestOpen = edgeConditionSuggestions.length > 0;
    scheduleConditionEdgeDraftApply();
  }

  function handleEdgeConditionFocus() {
    edgeConditionSuggestOpen = edgeConditionSuggestions.length > 0;
    if (edgeConditionSuggestOpen && edgeConditionSuggestIndex < 0) {
      edgeConditionSuggestIndex = 0;
    }
  }

  function handleEdgeConditionBlur() {
    setTimeout(() => {
      edgeConditionSuggestOpen = false;
    }, 120);
  }

  function handleEdgeConditionKeydown(event) {
    if (!edgeConditionSuggestions.length) return;
    if (event.key === "ArrowDown") {
      event.preventDefault();
      edgeConditionSuggestOpen = true;
      edgeConditionSuggestIndex = (edgeConditionSuggestIndex + 1 + edgeConditionSuggestions.length) % edgeConditionSuggestions.length;
      return;
    }
    if (event.key === "ArrowUp") {
      event.preventDefault();
      edgeConditionSuggestOpen = true;
      edgeConditionSuggestIndex = (edgeConditionSuggestIndex - 1 + edgeConditionSuggestions.length) % edgeConditionSuggestions.length;
      return;
    }
    if (event.key === "Tab" || event.key === "Enter" || event.key === " ") {
      if (!edgeConditionSuggestOpen) return;
      const item = edgeConditionSuggestions[edgeConditionSuggestIndex] || edgeConditionSuggestions[0];
      if (!item) return;
      event.preventDefault();
      selectEdgeConditionSuggestion(item);
    }
  }

  function scheduleConditionEdgeDraftApply() {
    if (edgeConditionApplyTimer) {
      clearTimeout(edgeConditionApplyTimer);
    }
    edgeConditionApplyTimer = setTimeout(() => {
      edgeConditionApplyTimer = null;
      applyConditionEdgeDraft();
    }, 220);
  }

  function handleEdgeAltStartSelection(startId, event) {
    if (!edgeDraft || !startId) return;
    const altStartId = String(event?.currentTarget?.value ?? "").trim();
    edgeDraft = {
      ...edgeDraft,
      altStartSelections: {
        ...(edgeDraft.altStartSelections || {}),
        [startId]: altStartId
      }
    };
    edgeEditError = "";
    scheduleConditionEdgeDraftApply();
  }

  async function applyConditionEdgeDraft(draft = edgeDraft, edgeId = selectedEdge?.id) {
    if (!selectedProjectId || !selectedEdge || !draft || !edgeId) return;
    if (!(selectedEdge.type === "CEDGE" || selectedEdge.type === "IEDGE")) return;
    const condition = String(draft.condition ?? "").trim();
    if (!condition) {
      edgeEditError = "Condition is required.";
      return;
    }
    const fields = {};
    if (condition !== String(selectedEdge.condition ?? "")) {
      fields.condition = condition;
    }
    if (edgeAltStartEnabled) {
      const nextEntries = normalizeAltStartSelections(draft.altStartSelections, selectedEdgeTarget);
      const currentEntries = normalizeAltStartSelections(
        altStartSelectionsFromEdge(selectedEdge, selectedEdgeTarget),
        selectedEdgeTarget
      );
      if (JSON.stringify(nextEntries) !== JSON.stringify(currentEntries)) {
        fields.altStartMap = nextEntries;
      }
    }
    if (!Object.keys(fields).length) {
      edgeEditError = "";
      return;
    }
    patchConditionEdgeInSceneFlow(edgeId, fields);
    if (edgeConditionSending) {
      edgeConditionQueuedDraft = { ...draft };
      edgeConditionQueuedEdgeId = edgeId;
      return;
    }
    edgeConditionSending = true;
    edgeEditError = "";
    try {
      await runSceneFlowCommand("SceneFlow.Edge.Update", {
        projectId: selectedProjectId,
        superNodeId: sceneFlow?.superNodeId,
        edgeId,
        fields
      });
    } finally {
      edgeConditionSending = false;
      if (edgeConditionQueuedDraft && edgeConditionQueuedEdgeId === edgeId) {
        const queuedDraft = edgeConditionQueuedDraft;
        edgeConditionQueuedDraft = null;
        await applyConditionEdgeDraft(queuedDraft, edgeId);
      } else {
        edgeConditionQueuedDraft = null;
      }
    }
  }

  $: if (cmdDialogOpen && cmdHelperTab === "PlayScene" && !cmdHelperSyncing) {
    const params = helperSceneIndex.get(cmdHelperScene) || [];
    const next = {};
    params.forEach((param) => {
      next[param] = cmdHelperSceneBindings?.[param] || "";
    });
    cmdHelperSceneBindings = next;
  }
  $: if (cmdDialogOpen && cmdHelperTab === "PlayAction" && !cmdHelperSyncing) {
    const options = cmdHelperAgentCommands || [];
    if (cmdHelperAgent && options.length && !options.some((entry) => entry?.name === cmdHelperAction)) {
      cmdHelperAction = options[0]?.name || "";
    }
    const actionName = (cmdHelperAction || "").trim();
    if (actionName && actionName !== lastCmdHelperAction) {
      const action = options.find((entry) => entry?.name === actionName);
      const existing = new Map(cmdHelperArgs.map((entry) => [entry?.key, entry?.value]));
      const params = Array.isArray(action?.params) ? action.params : [];
      cmdHelperArgs = params
        .map((param) => ({
          key: param?.name || "",
          value: normalizeCmdArgRawValue(existing.get(param?.name) || "")
        }))
        .filter((entry) => entry.key);
      lastCmdHelperAction = actionName;
    }
  }

  function addCmdHelperArg() {
    cmdHelperArgs = [...cmdHelperArgs, { key: "", value: "" }];
  }

  function removeCmdHelperArg(index) {
    cmdHelperArgs = cmdHelperArgs.filter((_, idx) => idx !== index);
  }

  function updateCmdHelperArg(index, field, value) {
    const nextValue = field === "value" ? normalizeCmdArgRawValue(value) : value;
    cmdHelperArgs = cmdHelperArgs.map((entry, idx) => (idx === index ? { ...entry, [field]: nextValue } : entry));
  }

  function pluginInterfaceForAgent(agentName) {
    const agent = (projectConfigView?.agents || []).find((entry) => entry?.name === agentName);
    if (!agent) return null;
    const deviceName = agent?.device || "";
    const plugin = (projectConfigView?.plugins || []).find((entry) => entry?.name === deviceName);
    const className = plugin?.className || "";
    const normalizeKey = (value) => String(value || "").trim().toLowerCase();
    const simpleClassName = (value) => {
      const text = String(value || "").trim();
      if (!text) return "";
      const parts = text.split(".");
      return parts[parts.length - 1] || text;
    };
    const matchesDescriptor = (descriptor, key, simpleKey) => {
      const descriptorPlugin = descriptor?.plugin || {};
      const id = normalizeKey(descriptorPlugin.id);
      const name = normalizeKey(descriptorPlugin.name);
      const classKey = normalizeKey(descriptorPlugin.className);
      return [id, name, classKey].some((entry) => entry && (entry === key || (simpleKey && entry === simpleKey)));
    };
    if (className) {
      const classKey = normalizeKey(className);
      const simpleKey = normalizeKey(simpleClassName(className));
      const match = pluginInterfaces.find((entry) => matchesDescriptor(entry, classKey, simpleKey));
      if (match) return match;
    }
    if (deviceName) {
      const deviceKey = normalizeKey(deviceName);
      const simpleKey = normalizeKey(simpleClassName(deviceName));
      const match = pluginInterfaces.find((entry) => matchesDescriptor(entry, deviceKey, simpleKey));
      if (match) return match;
    }
    return null;
  }

  function buildPluginBadgeDescriptors(configView, interfaces) {
    if (!configView?.plugins || !Array.isArray(interfaces)) return [];
    const normalizeKey = (v) => String(v || "").trim().toLowerCase();
    const simpleClass = (v) => {
      const text = String(v || "").trim();
      if (!text) return "";
      const parts = text.split(".");
      return parts[parts.length - 1] || text;
    };

    function findInterface(className) {
      if (!className) return null;
      const classKey = normalizeKey(className);
      const simpleKey = normalizeKey(simpleClass(className));
      return interfaces.find((e) => {
        const p = e?.plugin || {};
        return [normalizeKey(p.id), normalizeKey(p.name), normalizeKey(p.className)]
          .some((k) => k && (k === classKey || (simpleKey && k === simpleKey)));
      }) || null;
    }

    return configView.plugins
      .filter((plugin) => plugin.load)
      .map((plugin) => {
        const iface = findInterface(plugin.className);
        if (!iface) return null;
        const writes = Array.isArray(iface.writes) ? iface.writes : [];
        const reads  = Array.isArray(iface.reads)  ? iface.reads  : [];
        const config = Array.isArray(iface.config) ? iface.config : [];

        function resolveVarName(configKey) {
          return (plugin.features || []).find((f) => f.key === configKey)?.value
              || config.find((c) => c.key === configKey)?.default
              || configKey;
        }

        const seen = new Set();
        const variables = [
          ...writes.map((v) => ({ ...v, name: resolveVarName(v.var), source: 'writes' })),
          ...reads.map((v)  => ({ ...v, name: resolveVarName(v.var), source: 'reads'  }))
        ].filter((v) => v.name && !seen.has(v.name) && seen.add(v.name));

        return {
          key: `plugin_${plugin.className}`,
          className: plugin.className,
          pluginName: iface.plugin?.name || plugin.name || plugin.className,
          category: String(iface.categories?.primary || "").toLowerCase(),
          variables
        };
      })
      .filter(Boolean);
  }

  function mergedCommandsForDescriptor(agentName, descriptor, configView) {
    const staticCmds = Array.isArray(descriptor?.commands) ? descriptor.commands : [];
    const agentSpec = descriptor?.agentSpec;
    if (!agentSpec?.dynamic?.enabled || !agentSpec?.dynamic?.implicitAction) {
      return staticCmds;
    }
    const agent = (configView?.agents || []).find((a) => a?.name === agentName);
    const features = Array.isArray(agent?.features) ? agent.features : [];
    const fixedNames = new Set(
      (Array.isArray(agentSpec.fixed) ? agentSpec.fixed : []).map((f) => f?.name).filter(Boolean)
    );
    const staticNames = new Set(staticCmds.map((c) => c?.name).filter(Boolean));
    const summaryTemplate = agentSpec.dynamic.actionSummary || "";
    const dynamicCmds = features
      .filter((f) => f?.key && f?.value && !fixedNames.has(f.key) && !staticNames.has(f.key))
      .map((f) => ({
        name: f.key,
        type: "action",
        summary: summaryTemplate.replace("${value}", f.value),
        params: [],
        source: "agent-feature"
      }));
    return [...staticCmds, ...dynamicCmds];
  }

  function pluginCommandsForAgent(agentName) {
    const descriptor = pluginInterfaceForAgent(agentName);
    return mergedCommandsForDescriptor(agentName, descriptor, projectConfigView);
  }

  function cmdParamMeta(paramKey) {
    if (!paramKey || !cmdHelperActionDescriptor) return null;
    const params = Array.isArray(cmdHelperActionDescriptor?.params) ? cmdHelperActionDescriptor.params : [];
    return params.find((param) => param?.name === paramKey) || null;
  }

  function cmdParamHint(meta) {
    if (!meta) return "";
    const type = meta.type ? String(meta.type) : "";
    const required = meta.required ? "required" : "optional";
    const enumList = Array.isArray(meta.enum) ? meta.enum.filter(Boolean) : [];
    const enumHint = enumList.length ? `enum: ${enumList.slice(0, 4).join(", ")}${enumList.length > 4 ? "…" : ""}` : "";
    return [required, type, enumHint].filter(Boolean).join(" · ");
  }

  function cmdParamValuePlaceholder(meta) {
    if (!meta) return "value";
    const enumList = Array.isArray(meta.enum) ? meta.enum.filter(Boolean) : [];
    if (enumList.length) {
      return `one of: ${enumList.slice(0, 3).join(", ")}${enumList.length > 3 ? "…" : ""}`;
    }
    return meta.type ? `${meta.type} value` : "value";
  }

  function isQuotedCmdArgValue(value) {
    const text = String(value || "");
    return /^'[\s\S]*'$/.test(text);
  }

  function normalizeCmdArgRawValue(rawValue) {
    let value = String(rawValue ?? "").trim();
    if (!value) return "";
    while (isQuotedCmdArgValue(value) && value.length >= 2) {
      value = value.slice(1, -1).trim();
    }
    return value;
  }

  function formatCmdHelperArgValue(key, rawValue) {
    const normalized = normalizeCmdArgRawValue(rawValue);
    if (!normalized) return "";
    const escaped = normalized.replace(/\\/g, "\\\\").replace(/'/g, "\\'");
    return `'${escaped}'`;
  }

  function pluginWritesForAgent(agentName) {
    const descriptor = pluginInterfaceForAgent(agentName);
    return Array.isArray(descriptor?.writes) ? descriptor.writes : [];
  }

  function normalizeVarType(typeValue) {
    const raw = String(typeValue || "").trim().toLowerCase();
    if (raw === "int" || raw === "integer") return "Int";
    if (raw === "float" || raw === "double" || raw === "number") return "Float";
    if (raw === "bool" || raw === "boolean") return "Bool";
    if (raw === "list") return "List";
    if (raw === "struct") return "Struct";
    if (raw === "string") return "String";
    return "String";
  }

  function pluginWriteExists(writeEntry) {
    const name = (writeEntry?.var || "").trim();
    if (!name || name.startsWith("<")) return false;
    return helperVarCandidates.some((entry) => entry.name === name);
  }

  function playActionWarnings(agentName, actionName, args, descriptor) {
    const warnings = [];
    if (!agentName) {
      warnings.push("Agent name is required.");
      return warnings;
    }
    if (!descriptor) {
      warnings.push("No plugin descriptor; autocompletion disabled.");
      return warnings;
    }
    if (!actionName) {
      warnings.push("Action name is required.");
      return warnings;
    }
    const commands = pluginCommandsForAgent(agentName);
    const command = commands.find((entry) => entry?.name === actionName);
    if (!command) {
      warnings.push(`Unknown action "${actionName}" for ${agentName}.`);
      return warnings;
    }
    const params = Array.isArray(command?.params) ? command.params : [];
    const required = params.filter((param) => param?.required);
    const providedKeys = new Set((args || []).map((entry) => (entry?.key || "").trim()).filter(Boolean));
    required.forEach((param) => {
      if (param?.name && !providedKeys.has(param.name)) {
        warnings.push(`Missing required param: ${param.name}`);
      }
    });
    providedKeys.forEach((key) => {
      if (!params.some((param) => param?.name === key)) {
        warnings.push(`Unknown param: ${key}`);
      }
    });
    return warnings;
  }

  function tokenizeCommandPayload(payload) {
    const tokens = [];
    let current = "";
    let quote = null;
    for (let i = 0; i < payload.length; i += 1) {
      const ch = payload[i];
      if (quote) {
        current += ch;
        if (ch === quote) {
          quote = null;
        }
        continue;
      }
      if (ch === "'" || ch === '"') {
        current += ch;
        quote = ch;
        continue;
      }
      if (/\s/.test(ch)) {
        if (current) {
          tokens.push(current);
          current = "";
        }
        continue;
      }
      current += ch;
    }
    if (current) {
      tokens.push(current);
    }
    return tokens;
  }

  function parsePlayActionCommand(text) {
    if (!text) return null;
    const source = String(text).trim();
    let mode = "blocking";
    let payload = "";
    const defaultMatch = source.match(
      /PlayAction\s*\(\s*["']([\s\S]+?)["']\s*(?:,\s*\{([\s\S]*?)\}\s*)?\)/
    );
    if (defaultMatch) {
      payload = defaultMatch[1].trim();
      mode = "blocking";
      const modeStruct = defaultMatch[2] || "";
      if (/__vsm_mode\s*=\s*["']\s*nonblocking\s*["']/i.test(modeStruct)) {
        mode = "nonblocking";
      }
    } else {
      const concurrentMatch = source.match(/!=\s*["']([\s\S]+?)["']\s*\.\s*$/);
      if (concurrentMatch) {
        payload = concurrentMatch[1].trim();
        mode = "nonblocking";
      } else {
        const sequentialMatch = source.match(/!-\s*["']([\s\S]+?)["']\s*\.\s*$/);
        if (!sequentialMatch) return null;
        payload = sequentialMatch[1].trim();
        mode = "blocking";
      }
    }
    if (payload.startsWith("[") && payload.endsWith("]")) {
      payload = payload.slice(1, -1).trim();
    }
    if (!payload) return null;
    const tokens = tokenizeCommandPayload(payload);
    if (!tokens.length) return null;
    const agent = tokens[0] || "";
    const action = tokens[1] || "";
    const args = tokens.slice(2).map((token) => {
      const idx = token.indexOf("=");
      if (idx === -1) {
        return { key: token, value: "" };
      }
      return { key: token.slice(0, idx), value: token.slice(idx + 1) };
    });
    return { agent, action, args, mode };
  }

  function getCursorTokenContext(inputValue, cursorPos) {
    if (!inputValue) return null;
    const source = String(inputValue);
    const wrapperMatch =
      source.match(/^(PlayAction\s*\(\s*["']\[?)([\s\S]*?)(\]?["'](?:\s*,\s*\{[\s\S]*\})?\s*\))$/) ||
      source.match(/^(!=\s*["']\[?)([\s\S]*?)(\]?["']\s*\.\s*)$/) ||
      source.match(/^(!-\s*["']\[?)([\s\S]*?)(\]?["']\s*\.\s*)$/);
    if (!wrapperMatch) return null;
    const prefixLen = wrapperMatch[1].length;
    const payload = wrapperMatch[2];
    const payloadEnd = prefixLen + payload.length;
    if (cursorPos < prefixLen || cursorPos > payloadEnd) return null;
    const pCursor = cursorPos - prefixLen;
    const tokens = [];
    let current = "";
    let tokenStart = -1;
    let quote = null;
    for (let i = 0; i < payload.length; i += 1) {
      const ch = payload[i];
      if (quote) {
        current += ch;
        if (ch === quote) quote = null;
        continue;
      }
      if (ch === "'" || ch === '"') {
        if (tokenStart === -1) tokenStart = i;
        current += ch;
        quote = ch;
        continue;
      }
      if (/\s/.test(ch)) {
        if (current) {
          tokens.push({ text: current, start: tokenStart, end: i });
          current = "";
          tokenStart = -1;
        }
        continue;
      }
      if (tokenStart === -1) tokenStart = i;
      current += ch;
    }
    if (current) {
      tokens.push({ text: current, start: tokenStart, end: payload.length });
    }
    let activeTokenIdx = -1;
    for (let i = 0; i < tokens.length; i += 1) {
      if (pCursor >= tokens[i].start && pCursor <= tokens[i].end) {
        activeTokenIdx = i;
        break;
      }
    }
    let tokenPosition;
    let prefix = "";
    let replaceStart;
    let replaceEnd;
    if (activeTokenIdx >= 0) {
      tokenPosition = activeTokenIdx;
      const tok = tokens[activeTokenIdx];
      prefix = tok.text.slice(0, pCursor - tok.start);
      replaceStart = prefixLen + tok.start;
      replaceEnd = prefixLen + tok.end;
    } else {
      let lastBefore = -1;
      for (let i = 0; i < tokens.length; i += 1) {
        if (tokens[i].end <= pCursor) lastBefore = i;
      }
      tokenPosition = lastBefore + 1;
      prefix = "";
      replaceStart = cursorPos;
      replaceEnd = cursorPos;
    }
    if (tokenPosition === 0) return null;
    const agent = tokens[0]?.text || "";
    const action = tokens.length > 1 ? tokens[1]?.text || "" : "";
    if (tokenPosition === 1) {
      return { kind: "action", prefix, replaceStart, replaceEnd, agent, action: prefix };
    }
    if (activeTokenIdx >= 0) {
      const activeText = tokens[activeTokenIdx].text;
      const eqIdx = activeText.indexOf("=");
      if (eqIdx >= 0 && (pCursor - tokens[activeTokenIdx].start) > eqIdx) {
        const argKey = activeText.slice(0, eqIdx);
        const valuePrefix = activeText.slice(eqIdx + 1, pCursor - tokens[activeTokenIdx].start);
        return {
          kind: "value", prefix: valuePrefix,
          replaceStart: prefixLen + tokens[activeTokenIdx].start + eqIdx + 1,
          replaceEnd: prefixLen + tokens[activeTokenIdx].end,
          agent, action, argKey
        };
      }
    }
    const usedKeys = new Set();
    for (let i = 2; i < tokens.length; i += 1) {
      if (i === activeTokenIdx) continue;
      const eq = tokens[i].text.indexOf("=");
      if (eq > 0) usedKeys.add(tokens[i].text.slice(0, eq));
      else usedKeys.add(tokens[i].text);
    }
    return { kind: "key", prefix, replaceStart, replaceEnd, agent, action, usedKeys };
  }

  function computeAcItems(context) {
    if (!context) return [];
    const lowerPrefix = (context.prefix || "").toLowerCase();
    if (context.kind === "action") {
      const cmds = pluginCommandsForAgent(context.agent) || [];
      return cmds
        .filter((c) => c?.name && c.name.toLowerCase().startsWith(lowerPrefix))
        .map((c) => ({ label: c.name, detail: c.summary || "", kind: "action" }));
    }
    if (context.kind === "key") {
      const cmds = pluginCommandsForAgent(context.agent) || [];
      const actionCmd = cmds.find((c) => c?.name === context.action);
      if (!actionCmd) return [];
      const params = Array.isArray(actionCmd.params) ? actionCmd.params : [];
      const usedKeys = context.usedKeys || new Set();
      return params
        .filter((p) => p?.name && !usedKeys.has(p.name) && p.name.toLowerCase().startsWith(lowerPrefix))
        .map((p) => ({
          label: p.name,
          detail: [p.required ? "required" : "", p.type || ""].filter(Boolean).join(" "),
          kind: "key"
        }));
    }
    if (context.kind === "value") {
      const cmds = pluginCommandsForAgent(context.agent) || [];
      const actionCmd = cmds.find((c) => c?.name === context.action);
      if (!actionCmd) return [];
      const params = Array.isArray(actionCmd.params) ? actionCmd.params : [];
      const param = params.find((p) => p?.name === context.argKey);
      if (!param || !Array.isArray(param.enum)) return [];
      return param.enum
        .filter((v) => String(v).toLowerCase().startsWith(lowerPrefix))
        .map((v) => ({ label: String(v), detail: "", kind: "value" }));
    }
    return [];
  }

  function updateAutocomplete(inputEl) {
    if (!inputEl) { dismissAutocomplete(); return; }
    const inputValue = inputEl.value;
    const cursorPos = inputEl.selectionStart ?? inputValue.length;
    const context = getCursorTokenContext(inputValue, cursorPos);
    if (!context) { dismissAutocomplete(); return; }
    const items = computeAcItems(context);
    if (!items.length) { dismissAutocomplete(); return; }
    cmdAcItems = items;
    cmdAcSelectedIdx = 0;
    cmdAcVisible = true;
    cmdAcPrefix = context.prefix;
    cmdAcReplace = { start: context.replaceStart, end: context.replaceEnd };
    const rect = inputEl.getBoundingClientRect();
    cmdAcPos = { left: rect.left, top: rect.bottom + 2, width: rect.width };
  }

  async function acceptAcItem(inputEl, item) {
    if (!inputEl || !item || !cmdAcReplace) return;
    let replacement = item.label;
    if (item.kind === "action") replacement += " ";
    else if (item.kind === "key") replacement += "=";
    else if (item.kind === "value") replacement += " ";
    inputEl.setRangeText(replacement, cmdAcReplace.start, cmdAcReplace.end, "end");
    updateCmdInlineDraft(cmdEditingIndex, inputEl.value);
    dismissAutocomplete();
    await tick();
    inputEl.focus();
    updateAutocomplete(inputEl);
  }

  function dismissAutocomplete() {
    cmdAcItems = [];
    cmdAcSelectedIdx = 0;
    cmdAcVisible = false;
    cmdAcReplace = null;
    cmdAcPrefix = "";
  }

  function parsePlaySceneCommand(text) {
    if (!text) return null;
    const match = String(text).match(
      /PlayScene\s*\(\s*["']([\s\S]*?)["'](?:\s*,\s*\{([\s\S]*?)\})?\s*\)/
    );
    if (!match) return null;
    const scene = match[1] || "";
    const bindings = {};
    if (match[2]) {
      const pairs = match[2].split(",");
      pairs.forEach((pair) => {
        const eqIdx = pair.indexOf("=");
        if (eqIdx === -1) return;
        const key = pair.slice(0, eqIdx).trim();
        const value = pair.slice(eqIdx + 1).trim().replace(/^["']|["']$/g, "");
        if (key) bindings[key] = value;
      });
    }
    return { scene, bindings };
  }

  function escapeRegexStr(str) {
    return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  function parseVariableCommand(text) {
    if (!text) return null;
    const trimmed = String(text).trim();
    if (trimmed.startsWith("PlayScene") || trimmed.startsWith("PlayAction")) return null;
    const eqIdx = trimmed.indexOf("=");
    if (eqIdx === -1) return null;
    const name = trimmed.slice(0, eqIdx).trim();
    const expr = trimmed.slice(eqIdx + 1).trim();
    if (!name || !expr) return null;
    const incMatch = expr.match(new RegExp("^" + escapeRegexStr(name) + "\\s*\\+\\s*(.+)$"));
    if (incMatch) {
      return { op: "Inc", name, step: incMatch[1].trim(), expr: "" };
    }
    const decMatch = expr.match(new RegExp("^" + escapeRegexStr(name) + "\\s*-\\s*(.+)$"));
    if (decMatch) {
      return { op: "Dec", name, step: decMatch[1].trim(), expr: "" };
    }
    return { op: "Assign", name, step: "1", expr };
  }

  function detectCommandType(text) {
    if (!text) return null;
    const trimmed = String(text).trim();
    const playAction = parsePlayActionCommand(trimmed);
    if (playAction) return { tab: "PlayAction", data: playAction };
    const playScene = parsePlaySceneCommand(trimmed);
    if (playScene) return { tab: "PlayScene", data: playScene };
    const variable = parseVariableCommand(trimmed);
    if (variable) return { tab: "Variable", data: variable };
    return null;
  }

  function renderCommandTokens(text, agents, interfaces, configView) {
    if (!text) return [{ text: "", type: "plain" }];
    const str = String(text);
    const actionMatch =
      str.match(/^(PlayAction\s*\(\s*["']\[?)(.+?)(\]?["'](?:\s*,\s*\{[\s\S]*\})?\s*\))$/) ||
      str.match(/^(!=\s*["']\[?)(.+?)(\]?["']\s*\.\s*)$/) ||
      str.match(/^(!-\s*["']\[?)(.+?)(\]?["']\s*\.\s*)$/);
    if (!actionMatch) return [{ text: str, type: "plain" }];
    const prefix = actionMatch[1];
    const payload = actionMatch[2];
    const suffix = actionMatch[3];
    const tokens = [];
    tokens.push({ text: prefix, type: "syntax" });
    const parts = payload.split(/(\s+)/);
    let tokenIndex = 0;
    let agentName = "";
    let actionName = "";
    let agentDescriptor = null;
    let actionDescriptor = null;
    for (let i = 0; i < parts.length; i += 1) {
      const part = parts[i];
      if (/^\s+$/.test(part)) {
        tokens.push({ text: part, type: "syntax" });
        continue;
      }
      if (tokenIndex === 0) {
        agentName = part;
        const agentKnown = (agents || []).some((a) => a?.name === agentName);
        tokens.push({ text: part, type: "agent", known: agentKnown });
        if (agentKnown) {
          agentDescriptor = pluginInterfaceForAgentWithContext(agentName, interfaces, configView);
        }
        tokenIndex += 1;
      } else if (tokenIndex === 1) {
        actionName = part;
        const cmds = mergedCommandsForDescriptor(agentName, agentDescriptor, configView);
        const actionKnown = cmds.some((c) => c?.name === actionName);
        tokens.push({ text: part, type: "action", known: actionKnown });
        if (actionKnown) {
          actionDescriptor = cmds.find((c) => c?.name === actionName) || null;
        }
        tokenIndex += 1;
      } else {
        const eqIdx = part.indexOf("=");
        if (eqIdx > 0) {
          const key = part.slice(0, eqIdx);
          const value = part.slice(eqIdx + 1);
          const params = actionDescriptor?.params || [];
          const keyKnown = params.some((p) => p?.name === key);
          tokens.push({ text: key, type: "argKey", known: keyKnown });
          tokens.push({ text: "=", type: "syntax" });
          tokens.push({ text: value, type: "argValue" });
        } else {
          tokens.push({ text: part, type: "plain" });
        }
        tokenIndex += 1;
      }
    }
    tokens.push({ text: suffix, type: "syntax" });
    return tokens;
  }

  function syncHelperFromSelection() {
    if (cmdSelectedIndex === null || cmdSelectedIndex === undefined) {
      cmdHelperDetectedTab = null;
      return;
    }
    const text = (cmdInlineDrafts[cmdSelectedIndex] ?? "").trim();
    if (!text) {
      cmdHelperDetectedTab = null;
      return;
    }
    cmdHelperSyncing = true;
    try {
      const detected = detectCommandType(text);
      if (!detected) {
        cmdHelperDetectedTab = null;
        return;
      }
      cmdHelperDetectedTab = detected.tab;
      cmdHelperTab = detected.tab;
      if (detected.tab === "PlayAction") {
        const { agent, action, args, mode } = detected.data;
        cmdHelperAgent = agent;
        cmdHelperAction = action;
        cmdHelperPlayMode = mode || "blocking";
        cmdHelperArgs = (args || []).map((entry) => ({
          key: entry?.key || "",
          value: normalizeCmdArgRawValue(entry?.value || "")
        }));
        lastCmdHelperAction = action;
        cmdHelperAgentCommands = pluginCommandsForAgent(agent);
        cmdHelperDescriptor = pluginInterfaceForAgent(agent);
      } else if (detected.tab === "PlayScene") {
        const { scene, bindings } = detected.data;
        cmdHelperScene = scene;
        cmdHelperSceneBindings = bindings || {};
      } else if (detected.tab === "Variable") {
        const { op, name, step, expr } = detected.data;
        cmdHelperVarOp = op;
        cmdHelperVarName = name;
        cmdHelperVarStep = step || "1";
        cmdHelperVarExpr = expr || "";
        const known = helperVarCandidates.find((v) => v.name === name);
        if (known) cmdHelperVarType = known.type || "Int";
      }
    } finally {
      cmdHelperSyncing = false;
    }
  }

  function commandFromHelper() {
    if (cmdHelperTab === "PlayScene") {
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
    if (cmdHelperTab === "PlayAction") {
      const agent = (cmdHelperAgent || "").trim();
      const action = (cmdHelperAction || "").trim();
      if (!agent || !action) return "";
      const args = cmdHelperArgs
        .map((entry) => {
          const key = (entry?.key || "").trim();
          const value = formatCmdHelperArgValue(key, entry?.value || "");
          if (!key || !value) return "";
          return `${key}=${value}`;
        })
        .filter(Boolean)
        .join(" ");
      const payload = [agent, action, args].filter(Boolean).join(" ");
      if (cmdHelperPlayMode === "nonblocking") {
        return `PlayAction("[${payload}]", { __vsm_mode = "nonblocking" })`;
      }
      return `PlayAction("[${payload}]")`;
    }
    if (cmdHelperTab === "Variable") {
      if (cmdHelperVarOp === "Assign") {
        const name = (cmdHelperVarName || "").trim();
        const expr = (cmdHelperVarExpr || "").trim();
        if (!name || !expr) return "";
        return `${name} = ${expr}`;
      }
      if (cmdHelperVarOp === "Inc") {
        const name = (cmdHelperVarName || "").trim();
        const step = (cmdHelperVarStep || "").trim() || "1";
        if (!name) return "";
        return `${name} = ${name} + ${step}`;
      }
      if (cmdHelperVarOp === "Dec") {
        const name = (cmdHelperVarName || "").trim();
        const step = (cmdHelperVarStep || "").trim() || "1";
        if (!name) return "";
        return `${name} = ${name} - ${step}`;
      }
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

  async function createHelperVariable(name, type, scope = "global") {
    if (!name || !selectedProjectId) return false;
    if (helperVarCandidates.some((entry) => entry.name === name)) return true;
    let targetNodeId = "";
    if (scope === "local") {
      targetNodeId = nodeEditorTarget?.id || "";
    } else if (scope === "parent") {
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
      const nextIndex = cmdInlineDrafts.length;
      cmdInlineDrafts = [...cmdInlineDrafts, text];
      cmdSelectedIndex = nextIndex;
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
    if (nodeEditorTarget?.isRoot) {
      cmdError = "Command executions are disabled for the top-level SceneFlow.";
      return;
    }
    cmdError = "";
    if (cmdHelperTab === "PlayAction") {
      if (!(cmdHelperAgent || "").trim()) {
        cmdError = "Agent name is required.";
        return;
      }
    }
    if (cmdHelperTab === "Variable") {
      const ok = await ensureHelperVarExists();
      if (!ok) return;
    }
    const text = commandFromHelper();
    if (!text) {
      cmdError = "Helper command is incomplete.";
      return;
    }
    if (cmdSelectedIndex !== null) {
      updateCmdInlineDraft(cmdSelectedIndex, text);
      cmdEditingIndex = null;
    } else {
      const nextIndex = cmdInlineDrafts.length;
      cmdInlineDrafts = [...cmdInlineDrafts, text];
      cmdSelectedIndex = nextIndex;
    }
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

  function sceneFlowNodeColorByFlavour(node) {
    const flavour = (node?.flavour || "").toLowerCase();
    if (flavour === "enode") return SCENEFLOW_FRAME_COLORS.edges.eedge;
    if (flavour === "fnode") return SCENEFLOW_FRAME_COLORS.edges.fedge;
    if (flavour === "tnode") return SCENEFLOW_FRAME_COLORS.edges.tedge;
    if (flavour === "cnode") return SCENEFLOW_FRAME_COLORS.edges.cedge;
    if (flavour === "pnode") return SCENEFLOW_FRAME_COLORS.edges.pedge;
    if (flavour === "inode") return SCENEFLOW_FRAME_COLORS.edges.iedge;
    return SCENEFLOW_FRAME_COLORS.node;
  }

  function commandNodeHintStyle(node) {
    const history = !!node?.isHistory;
    const fill = history ? "#ffffff" : sceneFlowNodeColorByFlavour(node);
    const text = history ? "#000000" : "#ffffff";
    const border = history ? SCENEFLOW_FRAME_COLORS.node : fill;
    return `--cmd-node-fill:${fill};--cmd-node-text:${text};--cmd-node-border:${border};`;
  }

  function commandNodeHintSuperPath(w = 96, h = 96, inset = 2) {
    const power = 5;
    const steps = 32;
    const a = w / 2;
    const b = h / 2;
    const cx = inset + a;
    const cy = inset + b;
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

  function commandNodeHintTitle(node) {
    const raw = String(node?.name || "(unnamed node)").trim();
    if (raw.length <= 18) return raw;
    return `${raw.slice(0, 17)}…`;
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

  // A supernode is a valid alias source when exactly one top-level supernode
  // is selected and the current view is the root level.
  $: canCreateAlias = (() => {
    if (selectedNode?.type !== "Super") return false;
    const sni = sceneFlow?.superNodeId;
    const atRoot = sni == null || sni === "" || sni === "__root__";
    return atRoot;
  })();

  async function createAlias() {
    if (!canCreateAlias || !selectedNode) return;
    const offset = 140;
    await runSceneFlowCommand("SceneFlow.Node.CreateAlias", {
      projectId: selectedProjectId,
      superNodeId: sceneFlow?.superNodeId,
      refId: selectedNode.id,
      x: (selectedNode.graphics?.x ?? 0) + offset,
      y: selectedNode.graphics?.y ?? 0,
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
    const timeoutMode = timeoutModeFromEdge(selectedEdge);
    edgeDraftId = selectedEdge.id;
    edgeDraft = {
      condition: selectedEdge.condition ?? "",
      probability: selectedEdge.probability !== undefined ? String(selectedEdge.probability) : "",
      timeoutSpec: edgeTimeoutSpec(selectedEdge),
      timeoutMinSpec: timeoutMode === "interval" ? String(selectedEdge.timeoutMinMs) : "",
      timeoutMaxSpec: timeoutMode === "interval" ? String(selectedEdge.timeoutMaxMs) : "",
      timeoutMode,
      altStartText: formatAltStartMap(selectedEdge),
      altStartSelections: altStartSelectionsFromEdge(selectedEdge, selectedEdgeTarget)
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
      const mode = edgeDraft.timeoutMode || "fixed";
      if (mode === "fixed") {
        const raw = String(edgeDraft.timeoutSpec ?? "").trim();
        if (!raw) {
          edgeEditError = "Timeout is required.";
          return;
        }
        if (!isTimeoutNumber(raw)) {
          edgeEditError = "Timeout must be a number.";
          return;
        }
        const parsed = Number.parseInt(raw, 10);
        if (!Number.isFinite(parsed) || parsed < 0) {
          edgeEditError = "Timeout must be >= 0.";
          return;
        }
        // Always send fixed timeout explicitly so switching modes and
        // edge-state mismatches cannot skip the timeout assignment.
        fields.timeoutMs = parsed;
        fields.timeoutExpr = "";
      } else if (mode === "var") {
        const raw = String(edgeDraft.timeoutSpec ?? "").trim();
        if (!raw) {
          edgeEditError = "Timeout variable is required.";
          return;
        }
        if (!isTimeoutVarName(raw)) {
          edgeEditError = sceneFlowIntVarNames.length
            ? "Timeout must be an integer sceneflow variable."
            : "No integer sceneflow variables defined.";
          return;
        }
        if (raw !== (selectedEdge.timeoutExpr ?? "")) {
          fields.timeoutExpr = raw;
        }
      } else {
        const minRaw = String(edgeDraft.timeoutMinSpec ?? "").trim();
        const maxRaw = String(edgeDraft.timeoutMaxSpec ?? "").trim();
        if (!isTimeoutNumber(minRaw) || !isTimeoutNumber(maxRaw)) {
          edgeEditError = "Timeout interval must be two numbers.";
          return;
        }
        const min = Number.parseInt(minRaw, 10);
        const max = Number.parseInt(maxRaw, 10);
        if (!Number.isFinite(min) || !Number.isFinite(max) || min < 0 || max < 0) {
          edgeEditError = "Timeout interval values must be >= 0.";
          return;
        }
        if (max <= min) {
          edgeEditError = "Timeout interval must satisfy min < max.";
          return;
        }
        fields.timeoutMinMs = min;
        fields.timeoutMaxMs = max;
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
    if (!selectedProjectId || sceneFlowBusy) return;
    const selectionList = sceneFlowSelectionList();
    if (!selectionList.length) return;
    if (selectionList.some((item) => item.type === "command")) return;
    sceneFlowSelection = null;
    sceneFlowMultiSelection = [];
    const nodeIds = selectionList.filter((item) => item.type === "node").map((item) => item.id);
    const commentIds = selectionList.filter((item) => item.type === "comment").map((item) => item.id);
    const edgeIds = selectionList.filter((item) => item.type === "edge").map((item) => item.id);

    for (const nodeId of nodeIds) {
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
    if (monitorDialogOpen) {
      return true;
    }
    if (missingAgentDialogOpen) {
      closeMissingAgentDialog();
      return true;
    }
    if (missingVarDialogOpen) {
      closeMissingVarDialog();
      return true;
    }
    if (varRenameDialogOpen) {
      closeVarRenameDialog();
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
    if (key === "Shift") {
      shiftDown = true;
    }
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

  function handleGlobalKeyup(event) {
    if (!event) return;
    if (event.key === "Shift") {
      shiftDown = false;
    }
  }

  function handleWindowBlur() {
    shiftDown = false;
    saveButtonHovered = false;
  }
</script>

<svelte:window on:keydown={handleGlobalKeydown} on:keyup={handleGlobalKeyup} on:blur={handleWindowBlur} />

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
    <section class="panel landing-panel landing-panel--start">
      <header class="panel-title">
        <h2>Projects</h2>
      </header>

      <div class="panel-body">
      <div class="project-list">
        {#if projects.length === 0}
          <p class="muted">No open projects.</p>
        {/if}
        {#each projects as project}
          <div class="project-row">
            <button
              type="button"
              class:selected={project.projectId === selectedProjectId}
              class:android-project={project.androidProject === true}
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
          <button type="submit" class="open-project-btn" disabled={!openPath || !openPath.trim()}>Open</button>
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
        <label for="new-project-base">Base directory (optional)</label>
        <input
          id="new-project-base"
          placeholder="/abs/path/to/parent/folder"
          bind:value={newBaseDir}
          on:input={() => (createProjectError = "")}
        />
        <details class="base-dir-suggestions" bind:open={baseDirSuggestionsExpanded}>
          <summary>Suggested base directories</summary>
          {#if suggestedBaseDirs.length === 0}
            <p class="muted">No suggestions yet. Open a project first to build suggestions.</p>
          {:else}
            <div class="base-dir-suggestion-list">
              {#each suggestedBaseDirs as baseDir}
                <button
                  type="button"
                  class="ghost base-dir-suggestion-btn"
                  on:click={() => useSuggestedBaseDir(baseDir)}
                  title={baseDir}
                >
                  <span class="base-dir-suggestion-path">{baseDir}</span>
                  <span>Use</span>
                </button>
              {/each}
            </div>
          {/if}
        </details>
        <button type="submit" disabled={!newName || !newName.trim()}>Create</button>
        {#if createProjectError}
          <p class="error">{createProjectError}</p>
        {/if}
      </form>

    </div>
    </section>
    <section class="panel landing-panel landing-panel--recent">
        <header class="panel-title">
          <h2>Recent Projects ({recentHeaderCountLabel})</h2>
        </header>
        <div class="panel-body">
        <div class="recent-toolbar">
          <input
            type="text"
            class="recent-search"
            placeholder="Search name, path, plugin…"
            bind:value={recentSearchQuery}
            aria-label="Search recent projects"
          />
          <select bind:value={recentFilterMode} aria-label="Filter recent projects">
            <option value="all">All</option>
            <option value="android">Android</option>
            <option value="plugins">With plugins</option>
            <option value="pinned">Pinned</option>
          </select>
        </div>
        <div class="project-list project-list--recent">
          {#if recentLoading}
            <p class="muted">Loading recent projects...</p>
          {:else if recentError}
            <p class="error">{recentError}</p>
          {:else if recent.length === 0}
            <p class="muted">No recent projects.</p>
          {:else if filteredRecent.length === 0}
            <p class="muted">No projects match the current search/filter.</p>
          {:else}
            {#each filteredRecent as project}
              <div
                class="recent-item"
                class:android-project={project.androidProject === true || project?.stats?.androidProject === true}
              >
                <button
                  type="button"
                  class="recent-open-btn"
                  on:click={() => openRecentProject(project)}
                >
                  <div class="project-list-info">
                    <div class="project-list-header">
                      <div class="project-list-name" title={project.name}>{project.name}</div>
                      {#if project.date}
                        <div class="meta project-list-date" title={project.date}>{formatRecentRelativeDate(project.date)}</div>
                      {/if}
                    </div>
                    {#if project.stats}
                      <div class="meta project-list-meta">
                        Supernodes: {project.stats.superNodes ?? 0} · Nodes: {project.stats.nodes ?? 0} · Commands: {project.stats.commands ?? 0}
                      </div>
                      <div class="meta project-list-meta">
                        {formatRecentScenesStats(project)}
                      </div>
                      <div class="meta project-list-meta">
                        Plugins:
                        {#if hasRecentPlugins(project)}
                          {#each project.stats.plugins as plugin, idx}
                            <span class:project-list-plugin-missing={!plugin.present}>
                              {plugin.name || plugin.className || "Unknown"}{idx < project.stats.plugins.length - 1 ? ", " : ""}
                            </span>
                          {/each}
                        {:else}
                          —
                        {/if}
                      </div>
                      <div class="meta project-list-meta">{project.path || "—"}</div>
                    {/if}
                  </div>
                </button>
                <div class="recent-actions">
                  <button
                    type="button"
                    class="ghost recent-action-btn"
                    on:click|stopPropagation={() => openRecentProject(project)}
                    aria-label="Open project"
                    title="Open project"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="m16.862 4.487 1.687-1.688a1.875 1.875 0 1 1 2.652 2.652L6.832 19.82a4.5 4.5 0 0 1-1.897 1.13l-2.685.8.8-2.685a4.5 4.5 0 0 1 1.13-1.897L16.863 4.487Zm0 0L19.5 7.125" />
                    </svg>
                  </button>
                  <button
                    type="button"
                    class="ghost recent-action-btn recent-action-pin"
                    class:active={isRecentPinned(project?.path)}
                    on:click|stopPropagation={() => toggleRecentPinned(project?.path)}
                    aria-pressed={isRecentPinned(project?.path)}
                    aria-label={isRecentPinned(project?.path) ? "Unpin project" : "Pin project"}
                    title={isRecentPinned(project?.path) ? "Unpin project" : "Pin project"}
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" aria-hidden="true">
                      <path d="M235.32,81.37,174.63,20.69a16,16,0,0,0-22.63,0L98.37,74.49c-10.66-3.34-35-7.37-60.4,13.14a16,16,0,0,0-1.29,23.78L85,159.71,42.34,202.34a8,8,0,0,0,11.32,11.32L96.29,171l48.29,48.29A16,16,0,0,0,155.9,224c.38,0,.75,0,1.13,0a15.93,15.93,0,0,0,11.64-6.33c19.64-26.1,17.75-47.32,13.19-60L235.33,104A16,16,0,0,0,235.32,81.37ZM224,92.69h0l-57.27,57.46a8,8,0,0,0-1.49,9.22c9.46,18.93-1.8,38.59-9.34,48.62L48,100.08c12.08-9.74,23.64-12.31,32.48-12.31A40.13,40.13,0,0,1,96.81,91a8,8,0,0,0,9.25-1.51L163.32,32,224,92.68Z"></path>
                    </svg>
                  </button>
                  <button
                    type="button"
                    class="ghost danger recent-action-btn"
                    on:click|stopPropagation={async () => removeRecentProject(project?.path)}
                    aria-label="Remove recent project"
                    title="Remove recent project"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" aria-hidden="true">
                      <path stroke-linecap="round" stroke-linejoin="round" d="m14.74 9-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 0 1-2.244 2.077H8.084a2.25 2.25 0 0 1-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 0 0-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 0 1 3.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 0 0-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 0 0-7.5 0" />
                    </svg>
                  </button>
                </div>
              </div>
            {/each}
          {/if}
        </div>
        </div>
      </section>
    <section class="panel landing-panel landing-panel--tutorials">
        <header class="panel-title">
          <h2>Tutorials</h2>
        </header>
        <div class="panel-body">
        <div class="project-list">
          {#if tutorialsLoading}
            <p class="muted">Loading tutorials...</p>
          {:else if tutorialsError}
            <p class="error">{tutorialsError}</p>
            <button type="button" class="ghost" on:click={loadTutorials}>Reload tutorials</button>
          {:else if tutorials.length === 0}
            <div class="tutorial-empty-state">
              <div class="tutorial-empty-title">No tutorials installed yet</div>
              <p class="muted">
                Tutorials will appear here with level and duration. You can already create or open projects from the Start panel.
              </p>
              <button type="button" class="ghost" on:click={loadTutorials}>Reload tutorials</button>
            </div>
          {:else}
            {#each tutorials as project}
              <button type="button" class="tutorial-card" on:click={() => openProject(project.path)} title={project.path || project.name}>
                <span class="tutorial-title">{project.name}</span>
                {#if formatTutorialMeta(project)}
                  <span class="tutorial-meta">{formatTutorialMeta(project)}</span>
                {/if}
                {#if project.path}
                  <span class="tutorial-path">{project.path}</span>
                {/if}
              </button>
            {/each}
          {/if}
        </div>
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
            {#if embeddingsStarting}
              <span class="autosave-status saving" aria-live="polite">Embeddings starting…</span>
            {:else if embeddingsAvailable}
              <span
                class="autosave-status embeddings-badge embeddings-badge-centered"
                aria-live="polite"
                title="Embedding-based service for text ready"
              >
                <svg
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
                    d="M15.75 15.75V18m-7.5-6.75h.008v.008H8.25v-.008Zm0 2.25h.008v.008H8.25V13.5Zm0 2.25h.008v.008H8.25v-.008Zm0 2.25h.008v.008H8.25V18Zm2.498-6.75h.007v.008h-.007v-.008Zm0 2.25h.007v.008h-.007V13.5Zm0 2.25h.007v.008h-.007v-.008Zm0 2.25h.007v.008h-.007V18Zm2.504-6.75h.008v.008h-.008v-.008Zm0 2.25h.008v.008h-.008V13.5Zm0 2.25h.008v.008h-.008v-.008Zm0 2.25h.008v.008h-.008V18Zm2.498-6.75h.008v.008h-.008v-.008Zm0 2.25h.008v.008h-.008V13.5ZM8.25 6h7.5v2.25h-7.5V6ZM12 2.25c-1.892 0-3.758.11-5.593.322C5.307 2.7 4.5 3.65 4.5 4.757V19.5a2.25 2.25 0 0 0 2.25 2.25h10.5a2.25 2.25 0 0 0 2.25-2.25V4.757c0-1.108-.806-2.057-1.907-2.185A48.507 48.507 0 0 0 12 2.25Z"
                  />
                </svg>
                ready
              </span>
            {/if}
            {#if headerDirty}
              <span class="unsaved-indicator" aria-live="polite">Unsaved</span>
            {/if}
            {#if isSessionOwner}
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
                  on:click={handlePrimarySaveClick}
                  on:mouseenter={() => (saveButtonHovered = true)}
                  on:mouseleave={() => (saveButtonHovered = false)}
                  disabled={!selectedProject || projectSaving}
                  title={saveButtonActsAsSaveAs ? "Save As (Shift)" : "Save"}
                >
                  {saveButtonActsAsSaveAs ? "Save As" : "Save"}
                </button>
              {/if}
            {:else if selectedProject}
              <a
                href={`/api/v1/projects/${selectedProjectId}/export`}
                download
                class="ghost panel-save export-link"
                title="Download a local copy of the SceneFlow"
              >
                Export
              </a>
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
            <div class="share-button-wrap">
              <button
                type="button"
                class="panel-share"
                class:share-copied={shareCopied}
                class:share-no-lan={shareNoLan}
                on:click={shareSession}
                disabled={!selectedProject}
                aria-label="Copy invite link"
                title={shareCopied ? "Link copied!" : shareNoLan ? "LAN access disabled — see note below button" : "Share session link"}
              >
                {#if shareCopied}
                  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
                    <path d="M4.5 12.75l6 6 9-13.5" />
                  </svg>
                {:else}
                  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
                    <circle cx="18" cy="5" r="3"/><circle cx="6" cy="12" r="3"/><circle cx="18" cy="19" r="3"/>
                    <line x1="8.59" x2="15.42" y1="13.51" y2="17.49"/><line x1="15.41" x2="8.59" y1="6.51" y2="10.49"/>
                  </svg>
                {/if}
              </button>
              {#if shareNoLan}
                <div class="share-no-lan-hint" role="alert">
                  LAN access is disabled. The copied link only works on this machine.<br>
                  To share across the network, restart with <code>--allow-lan</code>.
                </div>
              {/if}
            </div>
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
                on:click={createAlias}
                disabled={!wsConnected || sceneFlowBusy || !canCreateAlias}
                aria-label="Create visual copy"
                title="Create visual copy of selected supernode"
              >
                <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="icon" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M16.5 8.25V6a2.25 2.25 0 0 0-2.25-2.25H6A2.25 2.25 0 0 0 3.75 6v8.25A2.25 2.25 0 0 0 6 16.5h2.25m8.25-8.25H18a2.25 2.25 0 0 1 2.25 2.25V18A2.25 2.25 0 0 1 18 20.25h-7.5A2.25 2.25 0 0 1 8.25 18v-1.5m8.25-8.25h-6a2.25 2.25 0 0 0-2.25 2.25v6" />
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
                on:click={openPluginDashboard}
                disabled={!selectedProject || !wsConnected}
                aria-label="Open plugin dashboard"
                title="Plugin Dashboard"
              >
                <IconBlocks className="icon" />
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
              <button
                type="button"
                class="sceneflow-gear flat"
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
        <div
          class="sceneflow-layout"
          class:left-collapsed={!sceneFlowShowBlocks}
          class:right-collapsed={!sceneFlowShowInspector}
          style={sceneFlowLayoutStyle}
        >
          {#if sceneFlowShowBlocks}
            <aside
              class="sceneflow-blocks sceneflow-region-left"
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
              </div>
              <input
                class="search"
                placeholder="Filter scenes"
                bind:value={scriptScenesFilter}
                disabled={!selectedProject}
              />
              {#if !selectedProject}
                <p class="muted">Select a project to view scenes.</p>
              {:else if scriptScenesLoading && scriptScenesLive.length === 0}
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
                              <div class="scene-title-block">
                                <span
                                  class="scene-name"
                                  title={group?.params?.length ? `${group.name} (${group.params.join(", ")})` : group.name}
                                  use:fitMiddleEllipsis={{ text: group?.params?.length ? `${group.name} (${group.params.join(", ")})` : group.name }}
                                ></span>
                                {#if sceneTitleSuggestions.size > 0}
                                  {@const suggestion = sceneTitleSuggestions.get(sceneGroupKey(lang.language, group.name))}
                                  {#if suggestion && suggestion.suggestions && suggestion.suggestions.length}
                                    <div class="scene-title-suggestion-wrap">
                                      <div class="scene-title-suggestion-list">
                                        {#each suggestion.suggestions.slice(0, 3) as option, idx}
                                          <button
                                            type="button"
                                            class="ghost scene-title-suggestion-line"
                                            aria-label={`Accept suggested title ${option.name}`}
                                            on:click={() =>
                                              applySceneTitleSuggestion(sceneGroupKey(lang.language, group.name), option.name)
                                            }
                                          >
                                            <span class="scene-title-suggestion-rank">{idx + 1}.</span>
                                            <span class="scene-title-suggestion-text">{option.name}</span>
                                            {#if idx === 0}
                                              <span class="scene-title-suggestion-badge">top</span>
                                            {/if}
                                          </button>
                                        {/each}
                                      </div>
                                      <button
                                        type="button"
                                        class="ghost icon-button scene-title-suggestion-dismiss"
                                        aria-label="Dismiss suggested titles"
                                        on:click={() => dismissSceneTitleSuggestion(sceneGroupKey(lang.language, group.name))}
                                      >
                                        ×
                                      </button>
                                    </div>
                                  {/if}
                                {/if}
                              </div>
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
          {:else}
            <div class="sceneflow-side-placeholder sceneflow-region-left" aria-hidden="true"></div>
          {/if}
          <button
            type="button"
            class="sceneflow-rail sceneflow-rail-left"
            class:collapsed={!sceneFlowShowBlocks}
            on:click={() => (sceneFlowShowBlocks = !sceneFlowShowBlocks)}
            aria-label={sceneFlowShowBlocks ? "Hide blocks panel" : "Show blocks panel"}
            aria-pressed={sceneFlowShowBlocks}
            disabled={!sceneFlow}
            title={sceneFlowShowBlocks ? "Hide blocks" : "Show blocks"}
          >
            <span class="sceneflow-rail-line" aria-hidden="true"></span>
            <span class="sceneflow-rail-pill" aria-hidden="true">
              {#if sceneFlowShowBlocks}&#8249;{:else}&#8250;{/if}
            </span>
          </button>
          <div class="sceneflow-container sceneflow-region-center" style={sceneFlowFrameStyle} bind:this={sceneFlowContainerEl}>
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
                showInfo={sceneFlowShowInfo}
                onCommandOpen={openCmdDialog}
                onCommandMove={moveNodeCommand}
                onCopySelection={copySceneFlowSelection}
                onPasteSelection={pasteSceneFlowSelection}
                onCutSelection={cutSceneFlowSelection}
                onDuplicateSelection={duplicateSceneFlowSelection}
                onTimeoutEdgeUpdate={handleCanvasTimeoutEdgeUpdate}
              />
            </div>
            {#if sceneFlow?.usedByAliases?.length > 0}
              <div class="alias-notice" role="note">
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true">
                  <circle cx="12" cy="12" r="10" />
                  <line x1="12" y1="8" x2="12" y2="12" />
                  <line x1="12" y1="16" x2="12.01" y2="16" />
                </svg>
                <span>
                  Shared flow — <strong>{sceneFlow.superNode?.name || sceneFlow.superNodeId}</strong> is also used as a visual copy in:
                  {#each sceneFlow.usedByAliases as alias, i}
                    {alias.parentName || 'root'}{i < sceneFlow.usedByAliases.length - 1 ? ', ' : ''}.
                  {/each}
                </span>
              </div>
            {/if}
            <div class="sceneflow-toggles">
              <button
                type="button"
                class="sceneflow-toggle sceneflow-toggle-icon"
                class:active={sceneFlowNodeSnap}
                on:click={() => (sceneFlowNodeSnap = !sceneFlowNodeSnap)}
                aria-pressed={sceneFlowNodeSnap}
                aria-label="Toggle node snap"
                disabled={!sceneFlow}
                title="Toggle node snap"
              >
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  width="16"
                  height="16"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="currentColor"
                  stroke-width="2"
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  class="icon"
                  aria-hidden="true"
                >
                  <path d="m12 15 4 4" />
                  <path d="M2.352 10.648a1.205 1.205 0 0 0 0 1.704l2.296 2.296a1.205 1.205 0 0 0 1.704 0l6.029-6.029a1 1 0 1 1 3 3l-6.029 6.029a1.205 1.205 0 0 0 0 1.704l2.296 2.296a1.205 1.205 0 0 0 1.704 0l6.365-6.367A1 1 0 0 0 8.716 4.282z" />
                  <path d="m5 8 4 4" />
                </svg>
              </button>
              <button
                type="button"
                class="sceneflow-toggle sceneflow-toggle-icon"
                class:active={sceneFlowShowInfo}
                on:click={() => (sceneFlowShowInfo = !sceneFlowShowInfo)}
                aria-pressed={sceneFlowShowInfo}
                aria-label="Toggle info overlays"
                disabled={!sceneFlow}
                title="Toggle info overlays"
              >
                <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" width="16" height="16" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="m11.25 11.25.041-.02a.75.75 0 0 1 1.063.852l-.708 2.836a.75.75 0 0 0 1.063.853l.041-.021M21 12a9 9 0 1 1-18 0 9 9 0 0 1 18 0Zm-9-3.75h.008v.008H12V8.25Z" />
                </svg>
              </button>
            </div>
            {#if sceneFlowShowVars}
              <VarBadge
                title="Variables"
                variables={displayGlobalVarList.map((def) => ({ line: varBadgeLine(def), description: varBadgeLine(def) }))}
                loading={runtimeLoading}
                error={runtimeError}
                expanded={varBadgeState.global?.expanded ?? true}
                x={varBadgeState.global?.x ?? 0}
                y={varBadgeState.global?.y ?? 0}
                w={varBadgeState.global?.w ?? VAR_BADGE_MIN_WIDTH}
                h={varBadgeState.global?.h ?? VAR_BADGE_MIN_HEIGHT}
                color="#edf1f8"
                onDragStart={(e) => startVarBadgeMove(e, "global")}
                onToggle={() => toggleVarBadge("global")}
                onResizeStart={(e) => startVarBadgeResize(e, "global")}
              />
              {#if showLocalVarBadge}
                <VarBadge
                  title="Local variables"
                  subtitle={currentSuperName}
                  variables={displayLocalVarList.map((def) => ({ line: varBadgeLine(def), description: varBadgeLine(def) }))}
                  loading={runtimeLoading}
                  error={runtimeError}
                  expanded={varBadgeState.local?.expanded ?? true}
                  x={varBadgeState.local?.x ?? 0}
                  y={varBadgeState.local?.y ?? 0}
                  w={varBadgeState.local?.w ?? VAR_BADGE_MIN_WIDTH}
                  h={varBadgeState.local?.h ?? VAR_BADGE_MIN_HEIGHT}
                  color="#edf1f8"
                  onDragStart={(e) => startVarBadgeMove(e, "local")}
                  onToggle={() => toggleVarBadge("local")}
                  onResizeStart={(e) => startVarBadgeResize(e, "local")}
                />
              {/if}
            {/if}
            {#if selectedProjectId && pluginBadgeDescriptors.length > 0}
              {#each pluginBadgeDescriptors as badge, i}
                <VarBadge
                  title={badge.pluginName}
                  category={badge.category}
                  variables={badge.variables.map((v) => {
                    const sfDef = sceneFlowVarDefs.find((d) => d.name === v.name);
                    const expr = normalizeRuntimeValue(sfDef?.expr ?? sfDef?.expression ?? "");
                    const captured = normalizeRuntimeValue(runtimeInitialValues[v.name]);
                    const defaultVal = captured || expr;
                    const value = normalizeRuntimeValue(runtimeValues[v.name]);
                    let line;
                    if (value) {
                      const showDefault = defaultVal && value !== defaultVal;
                      line = showDefault ? `${v.name} = ${value} (${defaultVal})` : `${v.name} = ${value}`;
                    } else if (defaultVal) {
                      line = `${v.name} = ${defaultVal}`;
                    } else {
                      line = v.name;
                    }
                    return { line, description: v.description || line };
                  })}
                  expanded={pluginBadgeState[badge.className]?.expanded ?? true}
                  x={pluginBadgeState[badge.className]?.x ?? PLUGIN_BADGE_DEFAULT_X}
                  y={pluginBadgeState[badge.className]?.y ?? (PLUGIN_BADGE_DEFAULT_Y + i * PLUGIN_BADGE_Y_STEP)}
                  w={pluginBadgeState[badge.className]?.w ?? PLUGIN_BADGE_DEFAULT_W}
                  h={pluginBadgeState[badge.className]?.h ?? PLUGIN_BADGE_DEFAULT_H}
                  color="#f8f6f2"
                  onDragStart={(e) => startPluginBadgeDrag(e, badge.className)}
                  onToggle={() => togglePluginBadge(badge.className)}
                  onResizeStart={(e) => startPluginBadgeResize(e, badge.className)}
                />
              {/each}
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
                <button
                  type="button"
                  class="sceneflow-zoom-button"
                  class:active={minimapVisible}
                  on:click={() => (minimapVisible = !minimapVisible)}
                  aria-label={minimapVisible ? "Hide minimap" : "Show minimap"}
                  title={minimapVisible ? "Hide minimap" : "Show minimap"}
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <rect x="3" y="3" width="18" height="18" rx="2" />
                    <rect x="13" y="13" width="7" height="7" rx="1" />
                  </svg>
                </button>
              </div>
              {#if minimapVisible}
                <SceneFlowMiniMap
                  snapshot={sceneFlow}
                  worldBox={sceneFlowWorldBox}
                  viewBox={sceneFlowViewBox}
                  onCenter={(x, y) => sceneFlowRef?.centerOn(x, y)}
                  peers={[...peerPresence.values()]}
                />
              {/if}
            </div>
          </div>
          <button
            type="button"
            class="sceneflow-rail sceneflow-rail-right"
            class:collapsed={!sceneFlowShowInspector}
            on:click={() => (sceneFlowShowInspector = !sceneFlowShowInspector)}
            aria-label={sceneFlowShowInspector ? "Hide inspector panel" : "Show inspector panel"}
            aria-pressed={sceneFlowShowInspector}
            disabled={!sceneFlow}
            title={sceneFlowShowInspector ? "Hide inspector" : "Show inspector"}
          >
            <span class="sceneflow-rail-line" aria-hidden="true"></span>
            <span class="sceneflow-rail-pill" aria-hidden="true">
              {#if sceneFlowShowInspector}&#8250;{:else}&#8249;{/if}
            </span>
          </button>
          {#if sceneFlowShowInspector}
            <aside class="sceneflow-inspector sceneflow-region-right">
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
            {:else if (sceneFlowSelection?.type === "node" || sceneFlowSelection?.type === "command") && selectedNode && nodeDraft}
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
              {#if sceneFlowSelection?.type === "command" && selectedCommand}
                <div class="definition-section">
                  <header class="definition-header">
                    <h4>Selected Command</h4>
                    <span class="muted">#{selectedCommand.index + 1}</span>
                  </header>
                  <div class="stack">
                    <div class="muted mono">{selectedCommand.command?.text || ""}</div>
                    <div class="actions">
                      <button
                        type="button"
                        class="ghost"
                        on:click={() => openCmdDialog(selectedCommand.node?.id, selectedCommand.index)}
                        disabled={!wsConnected || sceneFlowBusy}
                      >
                        Edit Commands
                      </button>
                    </div>
                  </div>
                </div>
              {/if}
              {#if nodeEditError}
                <p class="error">{nodeEditError}</p>
              {/if}
            {:else if sceneFlowSelection?.type === "edge" && selectedEdge && edgeDraft}
              <h3 class="inspector-title">Edge {selectedEdge.sourceId} → {selectedEdge.targetId}</h3>
              <div class="stack">
                {#if selectedEdge.type === "CEDGE" || selectedEdge.type === "IEDGE"}
                  <div class="cmd-field-label">Condition</div>
                  <div class="cmd-helper-var-wrap">
                    <div
                      class="editable-input mono"
                      class:is-empty={!String(edgeDraft.condition || "").length}
                      contenteditable="true"
                      role="textbox"
                      tabindex="0"
                      id="vsm-edge-condition-input"
                      aria-label="SceneFlow edge condition expression"
                      bind:this={edgeConditionInputEl}
                      spellcheck="false"
                      data-placeholder=""
                      on:input={handleEdgeConditionInput}
                      on:focus={handleEdgeConditionFocus}
                      on:blur={handleEdgeConditionBlur}
                      on:keydown={handleEdgeConditionKeydown}
                    ></div>
                    {#if edgeConditionSuggestOpen && edgeConditionSuggestions.length > 0}
                      <div class="cmd-helper-var-dropdown" role="listbox" aria-label="Condition variable suggestions">
                        {#each edgeConditionSuggestions as variable, i}
                          <button
                            type="button"
                            class="cmd-ac-item"
                            class:selected={i === edgeConditionSuggestIndex}
                            role="option"
                            aria-selected={i === edgeConditionSuggestIndex}
                            on:mousedown|preventDefault={() => selectEdgeConditionSuggestion(variable)}
                          >
                            <span class="cmd-ac-label">{variable.name}</span>
                            <span class="cmd-ac-detail">{variable.type || "Var"} • {variable.scope}</span>
                          </button>
                        {/each}
                      </div>
                    {/if}
                  </div>
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
                  <label for="edge-timeout-mode">Timeout mode</label>
                  <select
                    id="edge-timeout-mode"
                    bind:value={edgeDraft.timeoutMode}
                    on:change={handleTimeoutModeInspectorChange}
                  >
                    <option value="fixed">Fixed milliseconds</option>
                    <option value="var">Integer variable</option>
                    <option value="interval">Random interval</option>
                  </select>
                  {#if edgeDraft.timeoutMode === "fixed"}
                    <label for="edge-timeout">Timeout (ms)</label>
                    <input
                      id="edge-timeout"
                      type="text"
                      placeholder="1000"
                      bind:value={edgeDraft.timeoutSpec}
                      on:input={handleTimeoutFixedInspectorInput}
                    />
                  {:else if edgeDraft.timeoutMode === "var"}
                    <label for="edge-timeout">Timeout variable (int)</label>
                    <input
                      id="edge-timeout"
                      type="text"
                      list="edge-timeout-vars"
                      placeholder="timeout_ms"
                      bind:value={edgeDraft.timeoutSpec}
                      on:input={handleTimeoutVarInspectorInput}
                    />
                    <datalist id="edge-timeout-vars">
                      {#each sceneFlowIntVarNames as varName}
                        <option value={varName}></option>
                      {/each}
                    </datalist>
                  {:else}
                    <label for="edge-timeout-min">Timeout interval (ms)</label>
                    <div class="edge-timeout-interval">
                      <input
                        id="edge-timeout-min"
                        type="number"
                        min="0"
                        max="60000"
                        step="1"
                        placeholder="min"
                        bind:value={edgeDraft.timeoutMinSpec}
                        on:input={handleTimeoutIntervalInspectorInput}
                      />
                      <span>to</span>
                      <input
                        id="edge-timeout-max"
                        type="number"
                        min="0"
                        max="60000"
                        step="1"
                        placeholder="max"
                        bind:value={edgeDraft.timeoutMaxSpec}
                        on:input={handleTimeoutIntervalInspectorInput}
                      />
                    </div>
                    <p class="muted">Runtime picks one random timeout between min and max when this node becomes active.</p>
                  {/if}
                {:else}
                  <p class="muted">No editable fields for this edge type yet.</p>
                {/if}
                <div class:muted={!edgeAltStartEnabled} class="edge-alt-start-panel">
                  <p class="muted">
                    Alternative start nodes require a super node target. Below provide a selector of alternative
                    startnodes if the edge is point at a supernode.
                  </p>
                  {#if edgeAltStartEnabled}
                    {#if edgeAltStartStartNodes.length > 0}
                      <div class="stack edge-alt-start-list">
                        {#each edgeAltStartStartNodes as startNode}
                          <div class="edge-alt-start-row">
                            <label for={`edge-alt-start-${startNode.id}`}>{displayNodeName(startNode)}</label>
                            <select
                              id={`edge-alt-start-${startNode.id}`}
                              value={edgeAltStartSelections[startNode.id] || ""}
                              on:change={(event) => handleEdgeAltStartSelection(startNode.id, event)}
                              disabled={edgeAltStartSelectorMuted}
                            >
                              <option value="">Default start node</option>
                              {#each edgeAltStartChildNodes as candidate}
                                <option value={candidate.id}>{displayNodeName(candidate)}</option>
                              {/each}
                            </select>
                          </div>
                        {/each}
                      </div>
                    {:else}
                      <select disabled>
                        <option>
                          {edgeAltStartChildNodes.length === 0 ? "No internal nodes available" : "No start nodes available"}
                        </option>
                      </select>
                    {/if}
                  {:else}
                    <select disabled>
                      <option>Alternative start nodes unavailable</option>
                    </select>
                  {/if}
                </div>
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
                {#if selectedEdge.type !== "TEDGE" && selectedEdge.type !== "CEDGE" && selectedEdge.type !== "IEDGE"}
                  <button type="button" class="primary" on:click={applyEdgeEdits} disabled={!wsConnected || sceneFlowBusy}>
                    Apply
                  </button>
                {/if}
                {#if selectedEdge.type !== "CEDGE" && selectedEdge.type !== "IEDGE"}
                  <button type="button" class="ghost" on:click={resetEdgeDraft} disabled={!edgeDirty}>
                    Reset
                  </button>
                {/if}
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
              <div class="inspector-def-grid" style={`grid-template-rows:${inspectorDefGridStyle};`}>
              <div class="definition-section">
                <header class="definition-header">
                  <h4>Types ({nodeEditorTypeDefs.length})</h4>
                  <button
                    type="button"
                    class="ghost icon-button block-section-toggle"
                    aria-pressed={!typeDefsCollapsed}
                    aria-label={typeDefsCollapsed ? "Expand type definitions" : "Collapse type definitions"}
                    title={typeDefsCollapsed ? "Expand" : "Collapse"}
                    on:click={() => (typeDefsCollapsed = !typeDefsCollapsed)}
                  >
                    {#if typeDefsCollapsed}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.5v15m7.5-7.5h-15" />
                      </svg>
                    {:else}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                      </svg>
                    {/if}
                  </button>
                </header>
                {#if !typeDefsCollapsed}
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
                {/if}
              </div>

              <div class="definition-section">
                <header class="definition-header">
                  <h4>Variables ({nodeEditorVarDefs.length})</h4>
                  <button
                    type="button"
                    class="ghost icon-button block-section-toggle"
                    aria-pressed={!varDefsCollapsed}
                    aria-label={varDefsCollapsed ? "Expand variable definitions" : "Collapse variable definitions"}
                    title={varDefsCollapsed ? "Expand" : "Collapse"}
                    on:click={() => (varDefsCollapsed = !varDefsCollapsed)}
                  >
                    {#if varDefsCollapsed}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.5v15m7.5-7.5h-15" />
                      </svg>
                    {:else}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                      </svg>
                    {/if}
                  </button>
                </header>
                {#if !varDefsCollapsed}
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
                {/if}

              </div>

              <div class="definition-section">
                <header class="definition-header">
                  <h4>Commands ({nodeEditorCommands.length})</h4>
                  <button
                    type="button"
                    class="ghost icon-button block-section-toggle"
                    aria-pressed={!cmdExecCollapsed}
                    aria-label={cmdExecCollapsed ? "Expand command executions" : "Collapse command executions"}
                    title={cmdExecCollapsed ? "Expand" : "Collapse"}
                    disabled={rootSceneFlowCommandEditingLocked}
                    on:click={() => (cmdExecCollapsed = !cmdExecCollapsed)}
                  >
                    {#if cmdExecCollapsed}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M12 4.5v15m7.5-7.5h-15" />
                      </svg>
                    {:else}
                      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M5 12h14" />
                      </svg>
                    {/if}
                  </button>
                </header>
                {#if !cmdExecCollapsed}
                <div class="def-table">
                  <div
                    class="def-list"
                    role="list"
                    aria-label="Command executions"
                    on:dragover={handleSceneDropOver}
                    on:drop={handleCommandSceneDrop}
                  >
                    {#if nodeEditorCommands.length === 0}
                      <div class="def-empty">
                        {rootSceneFlowCommandEditingLocked ? "No commands allowed here." : "No commands yet."}
                      </div>
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
                      disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked}
                      data-cmd-add-button="true"
                      aria-label="Add command"
                      title="Add command"
                    >
                      <IconPlus className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={deleteSelectedCmd}
                      disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked || cmdSelectedIndex === null}
                      aria-label="Remove command"
                      title="Remove command"
                    >
                      <IconTrash className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={editSelectedCmd}
                      disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked || cmdSelectedIndex === null}
                      aria-label="Edit command"
                      title="Edit command"
                    >
                      <IconPencil className="icon" />
                    </button>
                    <button
                      type="button"
                      class="ghost icon-button"
                      on:click={() => moveSelectedCmd(-1)}
                      disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked || cmdSelectedIndex === null || cmdSelectedIndex === 0}
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
                        rootSceneFlowCommandEditingLocked ||
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
                {/if}
              </div>
              </div>
              {/if}
            </aside>
          {:else}
            <div class="sceneflow-side-placeholder sceneflow-region-right" aria-hidden="true"></div>
          {/if}
        </div>
      {:else}
        <p class="muted">No SceneFlow data loaded yet.</p>
      {/if}
      {#if selectedProject}
        <div class="scenescript">
          <div class="script-toolbar">
            <button
              type="button"
              class="panel-save script-search"
              on:click={toggleScriptSearchPanel}
              disabled={!selectedProject}
            >
              <IconSearch className="icon" />
              Search
            </button>
            <button
              type="button"
              class="panel-save script-generate"
              on:click={toggleGeneratePanel}
              disabled={!selectedProject || projectConfigLLMs.length === 0}
              title={projectConfigLLMs.length === 0 ? "Configure an LLM service in Project Settings first" : "Generate scene text using LLM"}
            >
              <IconDocument className="icon" />
              Generate Scenes
            </button>
            <button
              type="button"
              class="panel-save script-generate"
              on:click={generateSceneTitleSuggestions}
              disabled={!selectedProject || sceneTitleSuggestBusy}
              title="Suggest scene titles based on scene content"
            >
              <IconDocument className="icon" />
              Title Generator
            </button>
            <button
              type="button"
              class="panel-save script-semantic"
              on:click={toggleSemanticPanel}
              disabled={!selectedProject || semanticAnalyzeBusy || semanticLoading}
              title="Analyze semantic roles, dialogue acts, and theme-rheme"
            >
              <IconPuzzle className="icon" />
              Semantic Analysis
            </button>
            <label class="script-semantic-mode">
              <span class="muted">View</span>
              <select bind:value={semanticMode} disabled={!selectedProject || semanticAnalyzeBusy}>
                <option value="off">Off</option>
                <option value="basic">Basic</option>
                <option value="full">Full</option>
              </select>
            </label>
            {#if sceneTitleSuggestions.size > 0}
              <button type="button" class="ghost" on:click={applyAllSceneTitleSuggestions}>
                Accept all
              </button>
              <button type="button" class="ghost" on:click={dismissAllSceneTitleSuggestions}>
                Dismiss all
              </button>
            {/if}
            <div class="script-toolbar-spacer"></div>
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
        {#if scriptSearchOpen}
          <div class="script-search-panel">
            <label for="script-search-input">Search</label>
            <input
              id="script-search-input"
              bind:this={scriptSearchInputEl}
              value={scriptSearchQuery}
              on:input={(event) => updateScriptSearchQuery(event.target.value)}
              on:keydown={(event) => {
                if (event.key === "Enter") {
                  runScriptSearchNext();
                }
              }}
              placeholder="Find..."
            />
            <div class="script-search-actions">
              <button type="button" class="ghost" on:click={runScriptSearchPrevious} disabled={!scriptSearchQuery}>
                Prev
              </button>
              <button type="button" class="ghost" on:click={runScriptSearchNext} disabled={!scriptSearchQuery}>
                Next
              </button>
            </div>
          </div>
        {/if}
        {#if generatePanelOpen}
          <div class="generate-panel">
              <div class="generate-panel-header">
                <strong>Generate Scene</strong>
              </div>
              <div class="generate-panel-body">
                <div class="generate-row">
                  <label>
                    LLM Service
                    <select bind:value={generateLLMIndex} on:change={handleGenerateLLMSelectionChange}>
                      {#each projectConfigLLMs as llm, i}
                        <option value={i}>{llm.name || `LLM ${i + 1}`}</option>
                      {/each}
                    </select>
                  </label>
                  <label>
                    Language
                    <input type="text" bind:value={generateLanguage} placeholder="en" />
                  </label>
                  <label>
                    Scene Name
                    <input type="text" bind:value={generateSceneName} placeholder="greeting" />
                  </label>
                  <label>
                    Scenes
                    <input type="number" bind:value={generateSceneCount} min="1" max="10" style="width: 60px;" />
                  </label>
                </div>
                <div class="generate-row">
                  <label class="generate-full">
                    Actors
                    <input type="text" bind:value={generateActors} placeholder="charly, susanne" />
                  </label>
                  <button type="button" class="ghost" on:click={() => generateShowFormatPrompt = !generateShowFormatPrompt}>
                    {generateShowFormatPrompt ? "Hide Format Prompt" : "Show Format Prompt"}
                  </button>
                </div>
                {#if generateShowFormatPrompt}
                  <div class="generate-row">
                    <label class="generate-full">
                      Format Prompt
                      <textarea bind:value={generateFormatPrompt} rows="6"></textarea>
                    </label>
                  </div>
                  <div class="generate-action-row">
                    <button type="button" class="ghost" on:click={saveFormatPrompt}>Save Format Prompt</button>
                  </div>
                {/if}
                <div class="generate-row">
                  <label class="generate-full">
                    Action Prompt
                    <textarea bind:value={generateActionPrompt} rows="3" placeholder="Describe the scene you want to generate..."></textarea>
                  </label>
                </div>
                {#if generateActionLibrary.length > 0}
                  <div class="generate-saved-prompts">
                    <span class="generate-saved-label">Saved Prompts</span>
                    <div class="generate-saved-list">
                      {#each generateActionLibrary as prompt, i}
                        <div class="generate-saved-item">
                          <button
                            type="button"
                            class="generate-saved-text"
                            on:click={() => generateActionPrompt = prompt}
                            title={prompt}
                          >
                            {prompt}
                          </button>
                          <button
                            type="button"
                            class="generate-saved-delete"
                            on:click={() => removeActionPromptFromLibrary(i)}
                            title="Delete this prompt"
                          >
                            ×
                          </button>
                        </div>
                      {/each}
                    </div>
                  </div>
                {/if}
                <div class="generate-actions">
                  <button type="button" on:click={generateScene} disabled={generateLoading || !generateActionPrompt.trim()}>
                    {generateLoading ? "Generating..." : "Generate"}
                  </button>
                  <button type="button" class="ghost" on:click={saveActionPromptToLibrary} disabled={!generateActionPrompt.trim()}>
                    Save Prompt
                  </button>
                </div>
                {#if generateError}
                  <p class="error">{generateError}</p>
                {/if}
                {#if generateResult}
                  <div class="generate-result">
                    <strong>Result</strong>
                    <pre class="generate-preview">{generateResult}</pre>
                    <div class="generate-result-actions">
                      <button type="button" on:click={insertGeneratedScene}>Insert into Script</button>
                      <button type="button" class="ghost" on:click={() => generateResult = ""}>Discard</button>
                    </div>
                  </div>
                {/if}
            </div>
          </div>
        {/if}
        {#if semanticPanelOpen}
          <div class="semantic-panel">
            <div class="semantic-panel-header">
              <strong>Semantic Analysis</strong>
            </div>
            <div class="semantic-panel-body">
              <p class="muted semantic-variable-hint">
                The analysis is designed with the assumption that meaningful placeholders have been used that match sentence semantics,
                e.g. <code>$person</code>, <code>$agent</code>, <code>$object</code>, <code>$location</code>. Generic names like <code>$x</code> reduce syntax quality.
              </p>
              <div class="semantic-config-group">
                <div class="semantic-config-heading">Configuration</div>
                <div class="generate-row">
                  <button
                    type="button"
                    class="semantic-toggle-btn"
                    class:active={semanticAnalyzeSvo}
                    disabled={semanticAnalyzeBusy}
                    on:click={toggleSemanticAnalyzeSyntax}
                  >
                    Syntax Analysis (UD)
                  </button>
                  <button
                    type="button"
                    class="semantic-toggle-btn"
                    class:active={semanticAnalyzeDaTr}
                    disabled={semanticAnalyzeBusy}
                    on:click={toggleSemanticAnalyzeDaTr}
                  >
                    DA/TR Analysis
                  </button>
                  <button
                    type="button"
                    class="semantic-toggle-btn"
                    class:active={semanticDebugEnabled}
                    title="Debug is session-only and not saved to project.xml"
                    on:click={toggleSemanticDebug}
                  >
                    Debug (session only)
                  </button>
                  <select
                    class="semantic-llm-inline"
                    aria-label="LLM Service for DA/TR Analysis"
                    bind:value={semanticLLMIndex}
                    on:change={handleSemanticLLMSelectionChange}
                    disabled={semanticAnalyzeBusy || !semanticAnalyzeDaTr || projectConfigLLMs.length === 0}
                  >
                    {#if projectConfigLLMs.length === 0}
                      <option value={0}>No LLM configured</option>
                    {:else}
                      {#each projectConfigLLMs as llm, i}
                        <option value={i}>{llm.name || `LLM ${i + 1}`}</option>
                      {/each}
                    {/if}
                  </select>
                </div>
              </div>
              <div class="semantic-selections-preview" aria-label="Stored LLM selections">
                <span class="muted">Stored selections (project.xml):</span>
                <code>generate="{projectConfigView?.llmSelections?.generate || ""}"</code>
                <code>semantic="{projectConfigView?.llmSelections?.semantic || ""}"</code>
                <code>syntax="{(projectConfigView?.semanticServices?.basicProvider || "ud") === "ud" ? "ud (stanza)" : (projectConfigView?.semanticServices?.basicProvider || "llm")}"</code>
                <code>udUrl="{projectConfigView?.semanticServices?.udUrl || "http://127.0.0.1:4061/analyze"}"</code>
              </div>
              <div class="generate-row">
                <label class="generate-full">
                  System Prompt
                  <textarea bind:value={semanticSystemPrompt} rows="2" disabled={!semanticAnalyzeDaTr} on:change={stageSemanticAnalysisSettings}></textarea>
                </label>
              </div>
              <div class="generate-row">
                <label class="generate-full">
                  Analysis Prompt
                  <textarea bind:value={semanticPromptTemplate} rows="8" disabled={!semanticAnalyzeDaTr} on:change={stageSemanticAnalysisSettings}></textarea>
                </label>
              </div>
              <div class="generate-actions">
                <button type="button" on:click={runSemanticAnalysis} disabled={semanticAnalyzeBusy || semanticLoading}>
                  {semanticAnalyzeBusy ? "Analyzing..." : "Analyze"}
                </button>
                <button type="button" class="ghost" on:click={resetSemanticPrompts} disabled={semanticAnalyzeBusy}>
                  Reset Prompts
                </button>
                {#if semanticDirty}
                  <span class="muted semantic-unsaved-note">Unsaved semantic results</span>
                {/if}
                <div class="semantic-legend">
                  <span class="semantic-legend-title">Legend</span>
                  <span class="semantic-legend-item"><span class="semantic-legend-swatch subject"></span>Subject</span>
                  <span class="semantic-legend-item"><span class="semantic-legend-swatch verb"></span>Verb</span>
                  <span class="semantic-legend-item"><span class="semantic-legend-swatch object"></span>Object</span>
                  <span class="semantic-legend-item"><span class="semantic-legend-swatch predicate"></span>Predicate</span>
                  <span class="semantic-legend-item"><span class="semantic-legend-swatch address"></span>Address</span>
                  <span class="semantic-legend-note">Address head solid, Adj dashed, Adv dotted, Comp double (same role color)</span>
                </div>
              </div>
            </div>
          </div>
        {/if}
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
                sceneHighlights={sceneHighlights}
                semanticHighlights={semanticEditorHighlights}
                onChange={(value) => {
                  scriptDraft = value;
                  scheduleScriptDiagnostics();
                  scheduleScriptLive();
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
          {#if semanticStatus}
            <p class="status">{semanticStatus}</p>
          {/if}
          {#if semanticError}
            <p class="error">{semanticError}</p>
          {/if}
          {#if semanticStale}
            <p class="muted">Semantic overlays are outdated. Run Semantic Analysis again.</p>
          {/if}
          {#if semanticDebugEnabled && semanticDebug}
            <details class="semantic-debug" bind:open={semanticDebugOpen}>
              <summary>
                Semantic debug: anns {semanticDebug.annotations} | S {semanticDebug.spansResolved?.subject ?? 0}/{semanticDebug.spansProvided?.subject ?? 0}
                | V {semanticDebug.spansResolved?.verb ?? 0}/{semanticDebug.spansProvided?.verb ?? 0}
                | O {semanticDebug.spansResolved?.object ?? 0}/{semanticDebug.spansProvided?.object ?? 0}
                | P {semanticDebug.spansResolved?.predicate ?? 0}/{semanticDebug.spansProvided?.predicate ?? 0}
                | A {semanticDebug.spansResolved?.address ?? 0}/{semanticDebug.spansProvided?.address ?? 0}
              </summary>
              <pre>{JSON.stringify(semanticDebug, null, 2)}</pre>
            </details>
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
      <div class="modal save-as-modal" bind:this={saveAsDialogEl} role="dialog" aria-modal="true" aria-labelledby="save-as-title" tabindex="-1">
        <h3 id="save-as-title">Save project as</h3>
        <form class="modal-body" on:submit|preventDefault={confirmSaveAs}>
          <label for="save-as-name">Project name</label>
          <input
            id="save-as-name"
            placeholder="Project name"
            bind:this={saveAsNameInputEl}
            bind:value={saveAsName}
          />
          <label for="save-as-path">Save to directory</label>
          <input
            id="save-as-path"
            placeholder="/abs/path/to/parent/folder"
            bind:this={saveAsInputEl}
            bind:value={saveAsPath}
          />
          <p class="muted">Choose the parent directory. The project folder will use the project name above.</p>
          <details class="base-dir-suggestions" open>
            <summary>Suggested base directories</summary>
            {#if suggestedBaseDirs.length === 0}
              <p class="muted">No suggestions yet. Open a project first to build suggestions.</p>
            {:else}
              <div class="base-dir-suggestion-list">
                {#each suggestedBaseDirs as baseDir}
                  <button
                    type="button"
                    class="ghost base-dir-suggestion-btn"
                    on:click={() => useSuggestedSaveAsBaseDir(baseDir)}
                    title={baseDir}
                  >
                    <span class="base-dir-suggestion-path">{baseDir}</span>
                    <span>Use</span>
                  </button>
                {/each}
              </div>
            {/if}
          </details>
          {#if saveAsError}
            <p class="error">{saveAsError}</p>
          {/if}
          <div class="row row-end">
            <button type="button" class="ghost" on:click={closeSaveAsDialog}>Cancel</button>
            <button
              type="submit"
              class="primary"
              disabled={!saveAsName || !saveAsName.trim() || !saveAsPath || !saveAsPath.trim()}
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
            The project uses agents that are not configured in this project. This can come from scene script speaker
            labels or from PlayAction commands in the SceneFlow. Map each one to a device or run anyway and abort with
            Stop if needed. If the device is not present, cancel and add the needed device.
          </p>
          <div class="missing-agent-table">
            <div class="missing-agent-header">
              <span>Agent</span>
              <span>Detected In</span>
              <span>Device</span>
            </div>
            {#each missingAgentDrafts as draft, index}
              <div class="missing-agent-row">
                <div class="missing-agent-name">{draft.name}</div>
                <div class="missing-agent-source">{draft.sourceLabel}</div>
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

  {#if eventOverprodDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeEventOverprodDialogAndRestoreFocus}
      role="presentation"
    >
      <div class="modal rename-scene-modal" role="dialog" aria-modal="true" aria-labelledby="event-overprod-title" tabindex="-1">
        <h3 id="event-overprod-title">Realtime Visualization Limited</h3>
        <div class="modal-body">
          <p>{eventOverprodMessage}</p>
          <p><strong>Hot subflow:</strong> {eventOverprodFlowLabel}</p>
          <p><strong>Current rate:</strong> {eventOverprodRate} ({eventOverprodFlowRate})</p>
          <p class="muted">
            Runtime execution continues, but the canvas cannot display every transition in parallel at this speed.
            Hint: avoid 0 ms timeout loops; use a positive timeout (for example 20-100 ms) or redesign this cycle.
          </p>
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={muteEventOverprodDialogForRunAndClose}>
            Don't show again this run
          </button>
          <button type="button" class="primary" on:click={closeEventOverprodDialogAndRestoreFocus}>
            OK
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if missingVarDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeMissingVarDialog}
      role="presentation"
    >
      <div class="modal missing-var-modal" bind:this={missingVarDialogEl} role="dialog" aria-modal="true" aria-labelledby="missing-var-title" tabindex="-1">
        <h3 id="missing-var-title">Undefined Variables</h3>
        <div class="modal-body">
          <p>
            The project uses variables that are not defined in the SceneFlow. Fix these before running the project.
          </p>
          {#if missingVarItems.length}
            <div class="missing-var-list">
              {#each missingVarItems as item}
                <div class="missing-var-row">
                  <div class="missing-var-name">{item.name || "Unknown variable"}</div>
                  {#if item.context}
                    <div class="missing-var-context">{item.context}</div>
                  {/if}
                </div>
              {/each}
            </div>
          {/if}
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={closeMissingVarDialog}>
            Close
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if varRenameDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeVarRenameDialog}
      role="presentation"
    >
      <div class="modal rename-scene-modal" bind:this={varRenameDialogEl} role="dialog" aria-modal="true" aria-labelledby="var-rename-title" tabindex="-1">
        <h3 id="var-rename-title">Variable Name Updated</h3>
        <div class="modal-body">
          <p>
            You changed the variable name from "{varRenameOldName}" to "{varRenameNewName}".
            To avoid broken logic, SceneMaker also updated every matching usage in your scene flow.
          </p>
          <p>
            Updated usages: {varRenameUsageCount}
          </p>
        </div>
        <div class="row row-end">
          <button type="button" class="primary" on:click={closeVarRenameDialog}>
            OK
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if renameSceneDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeRenameSceneDialog}
      role="presentation"
    >
      <div class="modal rename-scene-modal" bind:this={renameSceneDialogEl} role="dialog" aria-modal="true" aria-labelledby="rename-scene-title" tabindex="-1">
        <h3 id="rename-scene-title">Rename PlayScene references?</h3>
        <div class="modal-body">
          <p>
            Scene "{renameSceneOldName}" was renamed to "{renameSceneNewName}". Found {renameSceneMatches.length} PlayScene
            command{renameSceneMatches.length === 1 ? "" : "s"} that still reference the old name.
          </p>
          {#if renameSceneMatches.length}
            <div class="rename-scene-list">
              {#each renameSceneMatches as match}
                <div class="rename-scene-row">
                  <div class="rename-scene-node">
                    {match.scope === "supernode" ? "SuperNode" : match.scope === "edge" ? "Edge" : "Node"}
                    {match.superNodeName ? ` · ${match.superNodeName}` : ""}
                    {match.nodeName ? ` · ${match.nodeName}` : ""}
                    {match.edgeType ? ` · ${match.edgeType}` : ""}
                  </div>
                  <div class="rename-scene-text">{match.commandText || match.text}</div>
                </div>
              {/each}
            </div>
          {/if}
          {#if renameSceneError}
            <p class="error">{renameSceneError}</p>
          {/if}
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={closeRenameSceneDialog} disabled={renameSceneBusy}>
            Ignore
          </button>
          <button
            type="button"
            class="primary"
            on:click={applyRenameSceneReferences}
            disabled={renameSceneBusy || !renameSceneMatches.length}
          >
            {renameSceneBusy ? "Updating..." : "Update Commands"}
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if danglingSceneDialogOpen}
    <div
      class="modal-backdrop"
      on:click|self={closeDanglingSceneDialog}
      role="presentation"
    >
      <div class="modal rename-scene-modal" bind:this={danglingSceneDialogEl} role="dialog" aria-modal="true" aria-labelledby="dangling-scene-title" tabindex="-1">
        <h3 id="dangling-scene-title">Dangling PlayScene references</h3>
        <div class="modal-body">
          <p>
            The following scene names were removed from the script but are still referenced by PlayScene commands.
          </p>
          {#if embeddingsStarting}
            <p class="muted">Starting semantic suggestions service...</p>
          {/if}
          {#if embeddingsAvailable === false && !embeddingsStarting}
            <p class="muted">Semantic suggestions unavailable (service offline).</p>
          {:else if embeddingsAvailable && !embeddingsReady}
            <p class="muted">
              Semantic model not ready{embeddingsHealthError ? `: ${embeddingsHealthError}` : "."}
            </p>
          {/if}
          {#if danglingSceneRemoved.length}
            <div class="rename-scene-tags">
              {#each danglingSceneRemoved as name}
                <span class="rename-scene-tag">{name}</span>
              {/each}
            </div>
          {/if}
          {#if danglingSceneReplacements.length}
            <div class="rename-scene-replacements">
              {#each danglingSceneReplacements as entry, index}
                <div class="rename-scene-replace-row">
                  <div class="rename-scene-replace-label">
                    {entry.name}
                    {#if entry.semantic}
                      <span
                        class="rename-scene-badge"
                        title={entry.semanticScore !== undefined && entry.semanticScore !== null
                          ? `score ${entry.semanticScore.toFixed?.(3) ?? entry.semanticScore}`
                          : entry.semanticModel
                        }
                      >
                        semantic
                      </span>
                    {/if}
                  </div>
                  <div class="rename-scene-replace-controls">
                    {#if entry.semanticOptions && entry.semanticOptions.length}
                      <div class="rename-scene-suggestions">
                        {#each entry.semanticOptions as option, idx}
                          <button
                            type="button"
                            class="ghost rename-scene-suggestion"
                            aria-label={`Use ${option.name}`}
                            on:click={() => updateDanglingReplacement(index, option.name)}
                          >
                            <span class="rename-scene-suggest-rank">{idx + 1}.</span>
                            <span class="rename-scene-suggest-name">{option.name}</span>
                          </button>
                        {/each}
                      </div>
                    {/if}
                    <select
                      value={entry.selected}
                      on:change={(event) => updateDanglingReplacement(index, event.target.value)}
                    >
                      {#each entry.options as option}
                        <option value={option}>{option}</option>
                      {/each}
                    </select>
                  </div>
                </div>
              {/each}
            </div>
          {/if}
          {#if danglingSceneMatches.length}
            <div class="rename-scene-list">
              {#each danglingSceneMatches as match}
                <div class="rename-scene-row">
                  <div class="rename-scene-node">
                    {match.scope === "supernode" ? "SuperNode" : match.scope === "edge" ? "Edge" : "Node"}
                    {match.sceneName ? ` · ${match.sceneName}` : ""}
                    {match.superNodeName ? ` · ${match.superNodeName}` : ""}
                    {match.nodeName ? ` · ${match.nodeName}` : ""}
                    {match.edgeType ? ` · ${match.edgeType}` : ""}
                  </div>
                  <div class="rename-scene-text">{match.commandText || match.text}</div>
                </div>
              {/each}
            </div>
          {/if}
          {#if danglingSceneError}
            <p class="error">{danglingSceneError}</p>
          {/if}
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={closeDanglingSceneDialog} disabled={danglingSceneBusy}>
            Ignore
          </button>
          <button
            type="button"
            class="primary"
            on:click={applyDanglingReplacements}
            disabled={danglingSceneBusy || !danglingSceneCanApply}
          >
            {danglingSceneBusy ? "Updating..." : "Replace References"}
          </button>
        </div>
      </div>
    </div>
  {/if}

  {#if projectConfigDialogOpen}
    <div class="modal-backdrop project-config-backdrop" role="presentation">
      <div class="modal project-config-modal" bind:this={projectConfigDialogEl} role="dialog" aria-modal="true" aria-labelledby="project-config-title" tabindex="-1">
        <div class="project-config-header">
          <div class="project-config-title">
            <span class="project-config-icon">
              <IconPuzzle className="icon" />
            </span>
            <div>
              <h3 id="project-config-title">Project Settings</h3>
            </div>
          </div>
          <div class="project-config-header-toggle-group" role="group" aria-label="Project settings sections">
            <button
              type="button"
              class="ghost panel-save project-config-header-toggle"
              class:active={projectConfigGeneralExpanded}
              on:click={() => (projectConfigGeneralExpanded = !projectConfigGeneralExpanded)}
              aria-pressed={projectConfigGeneralExpanded}
            >
              General
            </button>
            <button
              type="button"
              class="ghost panel-save project-config-header-toggle"
              class:active={projectConfigLlmExpanded}
              on:click={() => (projectConfigLlmExpanded = !projectConfigLlmExpanded)}
              aria-pressed={projectConfigLlmExpanded}
            >
              Llm
            </button>
            <button
              type="button"
              class="ghost panel-save project-config-header-toggle"
              class:active={projectConfigDevicesExpanded}
              on:click={() => (projectConfigDevicesExpanded = !projectConfigDevicesExpanded)}
              aria-pressed={projectConfigDevicesExpanded}
            >
              Devices
            </button>
          </div>
          <button
            type="button"
            class="ghost icon-button project-config-close"
            on:click={closeProjectConfigDialog}
            aria-label="Close project settings"
            title="Close"
          >
            ×
          </button>
        </div>
        <div class="project-config-body">
          {#if projectConfigGeneralExpanded}
          <div class="project-config-overview" style={projectConfigSectionStyle("general")}>
            <div class="project-config-panel project-config-panel--overview">
              <div class="project-config-overview-grid">
                <div class="project-config-overview-label">Description</div>
                <div class="project-config-overview-field">
                  <input
                    id="project-name-input"
                    value={projectConfigView.name}
                    on:input={(event) => updateProjectName(event.target.value)}
                  />
                  <div class="project-config-android-row">
                    <label class="project-config-toggle project-config-toggle--checkbox-end">
                      <span>Android Project</span>
                      <input
                        type="checkbox"
                        checked={projectConfigView.androidProject === true}
                        on:change={(event) => updateAndroidProject(event.target.checked)}
                      />
                    </label>
                    <div class="project-config-meta">
                      <span>{projectConfigPlugins.length} devices</span>
                      <span>{projectConfigAgents.length} agents</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="project-config-panel project-config-panel--overview project-config-panel--concepts">
              <div class="project-config-overview-grid">
                <div class="project-config-overview-label">Scene Title Concepts</div>
                <div class="project-config-overview-field">
                  <textarea
                    class="project-config-concepts-textarea"
                    rows="4"
                    placeholder={"One concept per line\nEnglish, 1-3 words (CamelBack IDs)"}
                    value={(projectConfigView.sceneTitleConcepts || []).join("\n")}
                    on:input={(event) => updateSceneTitleConcepts(event.target.value)}
                  ></textarea>
                  <span class="muted project-config-concepts-help">Used by Scene Title Generator as the semantic candidate list.</span>
                </div>
              </div>
            </div>
            <div class="project-config-panel project-config-panel--overview">
              <div class="project-config-overview-grid">
                <div class="project-config-overview-label">Semantic Services</div>
                <div class="project-config-overview-field">
                  <div class="project-config-grid">
                    <label for="semantic-basic-provider">Syntax Provider</label>
                    <select
                      id="semantic-basic-provider"
                      value={projectConfigView?.semanticServices?.basicProvider || "ud"}
                      on:change={(event) => updateSemanticServiceField("basicProvider", event.target.value)}
                    >
                      <option value="ud">ud</option>
                      <option value="llm">llm</option>
                    </select>
                    <label for="semantic-ud-url">UD URL</label>
                    <input
                      id="semantic-ud-url"
                      value={projectConfigView?.semanticServices?.udUrl || ""}
                      placeholder="http://127.0.0.1:4061/analyze"
                      on:input={(event) => updateSemanticServiceField("udUrl", event.target.value)}
                    />
                    <label for="semantic-ud-timeout">UD Timeout (ms)</label>
                    <input
                      id="semantic-ud-timeout"
                      type="number"
                      min="100"
                      step="100"
                      value={projectConfigView?.semanticServices?.udTimeoutMs || ""}
                      placeholder="6000"
                      on:change={(event) => updateSemanticServiceField("udTimeoutMs", event.target.value)}
                    />
                  </div>
                  <span class="muted project-config-concepts-help">Stored in project.xml as SemanticServices.</span>
                </div>
              </div>
            </div>
            <div class="project-config-panel project-config-panel--overview">
              <div class="project-config-overview-grid">
                <div class="project-config-overview-label">Runtime Visualization Guard</div>
                <div class="project-config-overview-field">
                  <div class="project-config-grid">
                    <label for="runtime-viz-rate">Event rate limit (events/s)</label>
                    <input
                      id="runtime-viz-rate"
                      type="number"
                      min={RUNTIME_VIZ_RATE_MIN}
                      max={RUNTIME_VIZ_RATE_MAX}
                      step="50"
                      value={runtimeVizRateDraft}
                      on:input={(event) => updateRuntimeVizGuardField("rate", event.target.value)}
                      disabled={runtimeVizBusy || runtimeVizCalibrationBusy}
                    />
                    <label for="runtime-viz-burst">Burst capacity (events)</label>
                    <input
                      id="runtime-viz-burst"
                      type="number"
                      min={RUNTIME_VIZ_BURST_MIN}
                      max={RUNTIME_VIZ_BURST_MAX}
                      step="100"
                      value={runtimeVizBurstDraft}
                      on:input={(event) => updateRuntimeVizGuardField("burst", event.target.value)}
                      disabled={runtimeVizBusy || runtimeVizCalibrationBusy}
                    />
                  </div>
                  <div class="project-config-inline project-config-runtime-viz-actions">
                    <button
                      type="button"
                      class="primary"
                      on:click={calibrateRuntimeVizGuard}
                      disabled={runtimeVizBusy || runtimeVizCalibrationBusy}
                    >
                      {runtimeVizCalibrationBusy ? "Calibrating..." : "Calibrate for this computer"}
                    </button>
                  </div>
                  <span class="muted project-config-concepts-help">
                    Limits visualization event flood (e.g. 0 ms loops) so the server and UI stay responsive.
                  </span>
                  {#if !hasRuntimeVizGuardInProjectConfig(projectConfigView)}
                    <span class="muted project-config-concepts-help">
                      Not calibrated yet. Click “Calibrate for this computer” to write the initial values into <code>project.xml</code>.
                    </span>
                  {/if}
                  {#if runtimeVizCalibrationStatus}
                    <span class="muted project-config-concepts-help">{runtimeVizCalibrationStatus}</span>
                  {/if}
                  {#if runtimeVizError}
                    <p class="error">{runtimeVizError}</p>
                  {/if}
                </div>
              </div>
            </div>
          </div>
          {/if}
          {#if projectConfigLlmExpanded}
          <div class="project-config-llm-panel" style={projectConfigSectionStyle("llm")}>
            <div class="project-config-llm-header">
              <h4>LLM Services ({projectConfigLLMs.length})</h4>
              <div class="project-config-llm-add">
                <input
                  placeholder="Name"
                  bind:value={llmNewName}
                  on:keydown={(e) => { if (e.key === "Enter") addLLM(); }}
                />
                <button type="button" class="primary icon-button" on:click={addLLM} aria-label="Add LLM" title="Add LLM">+</button>
              </div>
            </div>
            {#if projectConfigLLMs.length === 0}
              <p class="muted project-config-llm-empty">No LLM services configured.</p>
            {:else}
              <div
                class="project-config-llm-list"
                class:project-config-llm-list--scroll={projectConfigLLMs.length > 2}
              >
                {#each projectConfigLLMs as llm, index}
                  <div class="project-config-llm-entry" class:expanded={llmExpandedIndex === index}>
                    <div
                      role="button"
                      tabindex="0"
                      class="project-config-llm-row"
                      on:click={() => { llmExpandedIndex = llmExpandedIndex === index ? -1 : index; }}
                      on:keydown={(e) => {
                        if (e.key === "Enter" || e.key === " ") {
                          e.preventDefault();
                          llmExpandedIndex = llmExpandedIndex === index ? -1 : index;
                        }
                      }}
                    >
                      <span class="project-config-llm-name">{llm.name || "Unnamed"}</span>
                      <span class="project-config-llm-url">{getLLMFeature(llm, "baseUrl", "—")}</span>
                      <span class="project-config-llm-model">{getLLMFeature(llm, "model", "—")}</span>
                      <button
                        type="button"
                        class="ghost icon-button danger project-config-llm-delete"
                        on:click|stopPropagation={() => removeLLM(index)}
                        aria-label="Remove LLM"
                        title="Remove"
                      >×</button>
                    </div>
                    {#if llmExpandedIndex === index}
                      <div class="project-config-llm-detail">
                        <div class="project-config-llm-grid">
                          <label for={`llm-name-${index}`}>Name</label>
                          <input
                            id={`llm-name-${index}`}
                            value={llm.name}
                            on:input={(e) => updateLLMName(index, e.target.value)}
                          />
                          <label for={`llm-base-url-${index}`}>Base URL</label>
                          <input
                            id={`llm-base-url-${index}`}
                            value={getLLMFeature(llm, "baseUrl")}
                            placeholder="http://localhost:8234/v1/"
                            on:input={(e) => setLLMFeature(index, "baseUrl", e.target.value)}
                          />
                          <label for={`llm-api-key-${index}`}>API Key</label>
                          <input
                            id={`llm-api-key-${index}`}
                            type="password"
                            value={getLLMFeature(llm, "apiKey")}
                            placeholder="Optional"
                            on:input={(e) => setLLMFeature(index, "apiKey", e.target.value)}
                          />
                          <label for={`llm-model-${index}`}>Model</label>
                          <div class="project-config-llm-model-row">
                            <select
                              id={`llm-model-${index}`}
                              value={getLLMFeature(llm, "model")}
                              on:change={(e) => setLLMFeature(index, "model", e.target.value)}
                            >
                              <option value="">Select model</option>
                              {#if llmModels[index]}
                                {#each llmModels[index] as m}
                                  <option value={m.id}>{m.id}</option>
                                {/each}
                              {/if}
                              {#if getLLMFeature(llm, "model") && !(llmModels[index] || []).some(m => m.id === getLLMFeature(llm, "model"))}
                                <option value={getLLMFeature(llm, "model")}>{getLLMFeature(llm, "model")}</option>
                              {/if}
                            </select>
                            <button
                              type="button"
                              class="ghost icon-button"
                              on:click={() => fetchLLMModels(index)}
                              disabled={llmModelsLoading[index]}
                              aria-label="Fetch models"
                              title="Fetch available models"
                            >{llmModelsLoading[index] ? "..." : "↻"}</button>
                          </div>
                          <label for={`llm-temperature-${index}`}>Temperature</label>
                          <input
                            id={`llm-temperature-${index}`}
                            type="number"
                            min="0" max="2" step="0.1"
                            value={getLLMFeature(llm, "temperature", "0.7")}
                            on:change={(e) => setLLMFeature(index, "temperature", e.target.value)}
                          />
                          <label for={`llm-timeout-${index}`}>Timeout (s)</label>
                          <input
                            id={`llm-timeout-${index}`}
                            type="number"
                            step="1"
                            value={getLLMFeature(llm, "timeout", "30")}
                            on:change={(e) => setLLMFeature(index, "timeout", e.target.value || "30")}
                          />
                        </div>
                        <div class="project-config-llm-actions">
                          <button
                            type="button"
                            class="ghost"
                            on:click={() => testLLMConnection(index)}
                          >Test Connection</button>
                          {#if llmTestResult[index]}
                            <span class={llmTestResult[index].ok ? "llm-test-ok" : "llm-test-fail"}>
                              {llmTestResult[index].ok
                                ? `Connected (${llmTestResult[index].modelCount} models)`
                                : llmTestResult[index].error || "Failed"}
                            </span>
                          {/if}
                        </div>
                      </div>
                    {/if}
                  </div>
                {/each}
              </div>
            {/if}
          </div>
          {/if}
          {#if projectConfigDevicesExpanded}
          <div class="project-config-devices-row" style={projectConfigSectionStyle("devices")}>
          <aside class="project-config-tree">
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
              <div class="project-config-main-scroll">
              {#if projectConfigSelection.type === "devices"}
                <div class="project-config-panel">
                  <div class="project-config-panel-header">
                    <h4>Add device</h4>
                    <span class="muted"></span>
                  </div>
                  <div class="project-config-grid">
                    <label for="device-module">Module</label>
                    <select
                      id="device-module"
                      value={projectConfigNewPlugin.className}
                      disabled={availableDevicesLoading || selectableAvailableDevices.length === 0}
                      on:change={(event) => {
                        const className = event.target.value;
                        const derivedName = deriveDeviceNameFromClass(className);
                        projectConfigNewPlugin = {
                          ...projectConfigNewPlugin,
                          className,
                          name: projectConfigNewPlugin.name ? projectConfigNewPlugin.name : derivedName
                        };
                      }}
                    >
                      <option value="">Select module</option>
                      {#each selectableAvailableDevices as device}
                        <option value={device.className}>{device.displayName || device.className}</option>
                      {/each}
                    </select>
                    <label for="device-name">Name</label>
                    <input
                      id="device-name"
                      value={projectConfigNewPlugin.name}
                      on:input={(event) => {
                        const name = event.target.value;
                        projectConfigNewPlugin = { ...projectConfigNewPlugin, name };
                      }}
                    />
                  </div>
                  <div class="project-config-inline">
                    <label class="project-config-toggle project-config-inline-toggle">
                      <span>Load plugin</span>
                      <input
                        type="checkbox"
                        checked={projectConfigNewPlugin.load !== false}
                        on:change={(event) => {
                          projectConfigNewPlugin = { ...projectConfigNewPlugin, load: event.target.checked };
                        }}
                      />
                    </label>
                  </div>
                  {#if availableDevicesError}
                    <p class="error">{availableDevicesError}</p>
                  {/if}
                  <div class="actions">
                    <button
                      type="button"
                      class="primary icon-button"
                      on:click={addPlugin}
                      aria-label="Add device"
                      title="Add device"
                    >
                      <IconPlus className="icon" />
                    </button>
                  </div>
                </div>
              {:else if projectConfigSelection.type === "plugin" && selectedProjectPlugin}
                <div class="project-config-panel project-config-panel--scroll project-config-panel--device">
                  <div class="project-config-panel-header">
                    <div class="project-config-panel-title">
                      <div class="project-config-panel-title-row">
                        <h4>Device - {selectedProjectPlugin.name || "Unnamed"}</h4>
                        <span class="project-config-title-separator">(load at project startup</span>
                        <label class="project-config-module-toggle project-config-title-toggle">
                          <input
                            type="checkbox"
                            checked={selectedProjectPlugin.load}
                            on:change={(event) =>
                              updatePluginField(projectConfigSelection.pluginIndex, "load", event.target.checked)
                            }
                          />
                        </label>)
                      </div>
                      <div class="project-config-panel-subtitle">
                        {simpleClassName(selectedProjectPlugin.className) || selectedProjectPlugin.className || "Unknown"}
                      </div>
                    </div>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={() => removePlugin(projectConfigSelection.pluginIndex)}
                      aria-label="Delete device"
                      title="Delete device"
                    >
                      <IconTrash className="icon" />
                    </button>
                  </div>
                  <div class="project-config-panel-body">
                    <div class="project-config-keylist-panel">
                      <div class="project-config-section-header">
                      <div class="project-config-keylist-title">Configuration ({(selectedProjectPlugin?.features ?? []).length})</div>
                        <button
                          type="button"
                          class="ghost icon-button project-config-section-toggle"
                          aria-label={deviceConfigExpanded ? "Collapse configuration" : "Expand configuration"}
                          on:click={() => (deviceConfigExpanded = !deviceConfigExpanded)}
                        >{deviceConfigExpanded ? "−" : "+"}</button>
                      </div>
                      {#if deviceConfigExpanded}
                        <div class="project-config-table project-config-keylist-group">
                          <div class="project-config-table-body">
                            {#if (selectedProjectPlugin?.features ?? []).length === 0}
                              <div class="project-config-table-empty">No entries yet.</div>
                            {:else}
                              {#each (selectedProjectPlugin?.features ?? []) as feature, featureIndex}
                                {@const readonly = isKeyReadonly(pluginKeyOptions, feature.key)}
                                <div class="project-config-table-row">
                                  <input
                                    list="plugin-key-hints"
                                    value={feature.key}
                                    placeholder="key"
                                    disabled={readonly}
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
                                    disabled={readonly}
                                    on:input={(event) =>
                                      updatePluginFeature(
                                        projectConfigSelection.pluginIndex,
                                        featureIndex,
                                        "value",
                                        event.target.value
                                      )
                                    }
                                  />
                                  {#if readonly}
                                    <span class="icon-button-placeholder"></span>
                                  {:else}
                                    <button
                                      type="button"
                                      class="ghost icon-button danger"
                                      on:click={() => removePluginFeature(projectConfigSelection.pluginIndex, featureIndex)}
                                    >
                                      <IconTrash className="icon" />
                                    </button>
                                  {/if}
                                </div>
                              {/each}
                            {/if}
                          </div>
                          <div class="project-config-table-add">
                            <input list="plugin-key-hints" placeholder="key" value={projectConfigNewFeature.key} on:input={handleNewFeatureKeyInput} />
                            <input placeholder="value" bind:value={projectConfigNewFeature.value} />
                            <button
                              type="button"
                              class="primary icon-button"
                              on:click={addFeatureToSelection}
                              aria-label="Add key value"
                              title="Add key value"
                            >
                              <IconPlus className="icon" />
                            </button>
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
                              <div class="project-config-keygrid">
                                <div>
                                  <div class="project-config-key-title">Required</div>
                                  {#if selectedProjectPluginKeys.required?.length}
                                    <div class="project-config-key-list">
                                      {#each selectedProjectPluginKeys.required as entry}
                                        <div class="project-config-key-item">
                                          <span>
                                            {entry.name}
                                            {#if entry.description}
                                              <span class="project-config-key-inline-desc">({entry.description})</span>
                                            {/if}
                                          </span>
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
                                          <span>
                                            {entry.name}
                                            {#if entry.description}
                                              <span class="project-config-key-inline-desc">({entry.description})</span>
                                            {/if}
                                          </span>
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
                      {/if}
                    </div>
                    {#if selectedProjectPluginKeys?.pluginSpecific?.length}
                      <div class="project-config-keylist project-config-behavior-section">
                        <div class="project-config-section-header">
                          <div class="project-config-keylist-title">Behavior</div>
                          <button
                            type="button"
                            class="ghost icon-button project-config-section-toggle"
                            aria-label={deviceBehaviorExpanded ? "Collapse plugin behavior" : "Expand plugin behavior"}
                            on:click={() => (deviceBehaviorExpanded = !deviceBehaviorExpanded)}
                          >{deviceBehaviorExpanded ? "−" : "+"}</button>
                        </div>
                        {#if deviceBehaviorExpanded}
                          <div class="project-config-behavior-list">
                            {#each selectedProjectPluginKeys.pluginSpecific as entry}
                              {@const currentFeature = selectedProjectPlugin.features.find(f => f.key === entry.name)}
                              {@const currentValue = currentFeature?.value ?? String(entry.default ?? "")}
                              <div class="project-config-behavior-item">
                                {#if entry.type === "boolean"}
                                  <label class="project-config-behavior-checkbox">
                                    <input
                                      type="checkbox"
                                      checked={currentValue === "true"}
                                      on:change={(event) => {
                                        const newValue = event.target.checked ? "true" : "false";
                                        const featureIndex = selectedProjectPlugin.features.findIndex(f => f.key === entry.name);
                                        if (featureIndex >= 0) {
                                          updatePluginFeature(projectConfigSelection.pluginIndex, featureIndex, "value", newValue);
                                        } else {
                                          // Feature doesn't exist yet, add it
                                          const plugins = [...projectConfigPlugins];
                                          const plugin = plugins[projectConfigSelection.pluginIndex];
                                          if (plugin) {
                                            plugins[projectConfigSelection.pluginIndex] = {
                                              ...plugin,
                                              features: [...plugin.features, { key: entry.name, value: newValue }]
                                            };
                                            projectConfigDraft = { ...projectConfigDraft, plugins };
                                            scheduleProjectConfigApply();
                                          }
                                        }
                                      }}
                                    />
                                    <span>
                                      {entry.name}
                                      {#if entry.description}
                                        <span class="project-config-key-inline-desc">({entry.description})</span>
                                      {/if}
                                    </span>
                                  </label>
                                {:else}
                                  <span class="project-config-behavior-label">
                                    {entry.name}
                                    {#if entry.description}
                                      <span class="project-config-key-inline-desc">({entry.description})</span>
                                    {/if}
                                  </span>
                                  <input
                                    value={currentValue}
                                    on:input={(event) => {
                                      const newValue = event.target.value;
                                      const featureIndex = selectedProjectPlugin.features.findIndex(f => f.key === entry.name);
                                      if (featureIndex >= 0) {
                                        updatePluginFeature(projectConfigSelection.pluginIndex, featureIndex, "value", newValue);
                                      } else {
                                        // Feature doesn't exist yet, add it
                                        const plugins = [...projectConfigPlugins];
                                        const plugin = plugins[projectConfigSelection.pluginIndex];
                                        if (plugin) {
                                          plugins[projectConfigSelection.pluginIndex] = {
                                            ...plugin,
                                            features: [...plugin.features, { key: entry.name, value: newValue }]
                                          };
                                          projectConfigDraft = { ...projectConfigDraft, plugins };
                                          scheduleProjectConfigApply();
                                        }
                                      }
                                    }}
                                  />
                                {/if}
                              </div>
                            {/each}
                          </div>
                        {/if}
                      </div>
                    {/if}
                    <div class="project-config-agent-add">
                      <div class="project-config-panel-header">
                          <h4>Add Agent</h4>
                      </div>
                      <div class="project-config-agent-add-row">
                        <input placeholder="Agent name" bind:value={projectConfigNewAgent.name} />
                        <button
                          type="button"
                          class="primary icon-button"
                          on:click={() => addAgent(selectedProjectPlugin.name)}
                          aria-label="Add agent"
                          title="Add agent"
                        >
                          <IconPlus className="icon" />
                        </button>
                      </div>
                    </div>
                  </div>
                </div>
              {:else if projectConfigSelection.type === "agent" && selectedProjectAgent}
                <div class="project-config-panel project-config-panel--scroll project-config-panel--agent">
                  <div class="project-config-panel-header">
                    <div class="project-config-panel-title">
                      <div class="project-config-panel-title-row">
                        <h4>Agent - {selectedProjectAgent.name || "Unnamed"}</h4>
                      </div>
                      <div class="project-config-panel-subtitle">
                        {selectedProjectAgent.device || "Unknown"}
                      </div>
                    </div>
                    <button
                      type="button"
                      class="ghost icon-button danger"
                      on:click={() => removeAgent(projectConfigSelection.agentIndex)}
                      aria-label="Delete agent"
                      title="Delete agent"
                    >
                      <IconTrash className="icon" />
                    </button>
                  </div>
                  <div class="project-config-panel-body">
                  <div class="project-config-keylist-panel">
                    <div class="project-config-section-header">
                      <div class="project-config-keylist-title">Configuration ({(selectedProjectAgent?.features ?? []).length})</div>
                      <button
                        type="button"
                        class="ghost icon-button project-config-section-toggle"
                        aria-label={agentConfigExpanded ? "Collapse configuration" : "Expand configuration"}
                        on:click={() => (agentConfigExpanded = !agentConfigExpanded)}
                      >{agentConfigExpanded ? "−" : "+"}</button>
                    </div>
                    {#if agentConfigExpanded}
                      <div class="project-config-table project-config-keylist-group">
                        <div class="project-config-table-body">
                          {#if (selectedProjectAgent?.features ?? []).length === 0}
                            <div class="project-config-table-empty">No entries yet.</div>
                          {:else}
                            {#each (selectedProjectAgent?.features ?? []) as feature, featureIndex}
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
                        </div>
                        <div class="project-config-table-add">
                          <input list="agent-key-hints" placeholder="key" value={projectConfigNewFeature.key} on:input={handleNewAgentFeatureKeyInput} />
                          <input placeholder="value" bind:value={projectConfigNewFeature.value} />
                          <button
                            type="button"
                            class="primary icon-button"
                            on:click={addFeatureToSelection}
                            aria-label="Add key value"
                            title="Add key value"
                          >
                            <IconPlus className="icon" />
                          </button>
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
                            <div class="project-config-keygrid">
                              <div>
                                <div class="project-config-key-title">Required</div>
                                {#if selectedProjectAgentKeys.required?.length}
                                  <div class="project-config-key-list">
                                    {#each selectedProjectAgentKeys.required as entry}
                                      <div class="project-config-key-item">
                                        <span>
                                          {entry.name}
                                          {#if entry.description}
                                            <span class="project-config-key-inline-desc">({entry.description})</span>
                                          {/if}
                                        </span>
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
                                        <span>
                                          {entry.name}
                                          {#if entry.description}
                                            <span class="project-config-key-inline-desc">({entry.description})</span>
                                          {/if}
                                        </span>
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
                    {/if}
                  </div>
                  </div>
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
                      <button
                        type="button"
                        class="primary icon-button"
                        on:click={addFeatureToSelection}
                        aria-label="Add key value"
                        title="Add key value"
                      >
                        <IconPlus className="icon" />
                      </button>
                    </div>
                  </div>
                </div>
              {/if}
              </div>
            {/if}
          </section>
          </div>
          {/if}
        </div>
        {#if projectConfigError}
          <p class="error">{projectConfigError}</p>
        {/if}
      </div>
    </div>
  {/if}

  {#if prefsDialogOpen && prefsDialogDraft}
    <div class="modal-backdrop prefs-backdrop" role="presentation">
      <div class="modal prefs-modal" bind:this={prefsDialogEl} role="dialog" aria-modal="true" aria-labelledby="prefs-dialog-title" tabindex="-1">
        <div class="prefs-header">
          <div class="prefs-title">
            <span class="prefs-title-icon">
              <IconGear className="icon" />
            </span>
            <div>
              <h3 id="prefs-dialog-title">Editor Preferences</h3>
            </div>
          </div>
          <button
            type="button"
            class="ghost icon-button prefs-close"
            on:click={closePrefsDialog}
            aria-label="Close preferences"
            title="Close"
          >
            ×
          </button>
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
          <section class="prefs-card prefs-card--wide">
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
        {#if prefsDialogError}
          <p class="error">{prefsDialogError}</p>
        {/if}
      </div>
    </div>
  {/if}

  {#if monitorDialogOpen}
    <div class="modal-backdrop monitor-backdrop" role="presentation">
      <div class="modal monitor-modal" bind:this={monitorDialogEl} role="dialog" aria-modal="true" aria-labelledby="monitor-dialog-title" tabindex="-1">
        <div class="monitor-header">
          <div>
            <h3 id="monitor-dialog-title">Runtime Monitor</h3>
            <span class="muted">State: {runtimeStateLabel}</span>
          </div>
          <button
            type="button"
            class="ghost icon-button monitor-close"
            on:click={closeMonitorDialog}
            aria-label="Close runtime monitor"
            title="Close"
          >
            ×
          </button>
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
    <div class="modal-backdrop def-backdrop" role="presentation">
      <div class="modal def-modal type-def-modal" bind:this={typeDefDialogEl} role="dialog" aria-modal="true" aria-labelledby="type-def-dialog-title" tabindex="-1">
        <div class="def-header">
          <h3 id="type-def-dialog-title">{typeDefEditIndex >= 0 ? "Edit type definition" : "Add type definition"}</h3>
          <button
            type="button"
            class="ghost icon-button def-close"
            on:click={closeTypeDefDialog}
            aria-label="Close type definition"
            title="Close"
          >
            ×
          </button>
        </div>
        <div class="modal-body def-body">
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
        <div class="actions def-actions">
          <button type="button" class="primary" on:click={applyTypeDefEdit} disabled={!wsConnected || sceneFlowBusy}>
            {typeDefEditIndex >= 0 ? "Save" : "Add"}
          </button>
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
        {#if !rootSceneFlowCommandEditingLocked && nodeEditorTarget}
          <div
            class="cmd-node-hint"
            class:is-super={nodeEditorTarget?.type === "Super"}
            aria-hidden="true"
            style={commandNodeHintStyle(nodeEditorTarget)}
          >
            <svg class="cmd-node-hint-svg" viewBox="0 0 100 100" preserveAspectRatio="xMidYMid meet">
              {#if nodeEditorTarget?.type === "Super"}
                <path class="cmd-node-hint-shape-fill" d={commandNodeHintSuperPath(96, 96, 2)} />
              {:else}
                <ellipse class="cmd-node-hint-shape-fill" cx="50" cy="50" rx="48" ry="48" />
              {/if}
              <text class="cmd-node-hint-title" x="50" y="46">{commandNodeHintTitle(nodeEditorTarget)}</text>
              <text class="cmd-node-hint-meta" x="50" y="58">[{nodeEditorTarget?.id || ""}]</text>
              {#if nodeEditorTarget?.isStart}
                <polygon class="cmd-node-hint-start" points="83,10 90,23 76,23" />
              {/if}
            </svg>
          </div>
        {/if}
        <div class="cmd-modal-header">
          <h3 id="cmd-dialog-title">Command(s) executed at {nodeEditorTarget?.name || "(unnamed)"}</h3>
          <button
            type="button"
            class="ghost icon-button cmd-modal-close"
            on:click={closeCmdDialog}
            aria-label="Close command dialog"
            title="Close"
          >
            ×
          </button>
        </div>
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
                <div class="def-empty">
                  {rootSceneFlowCommandEditingLocked ? "No commands allowed here." : "No commands yet."}
                </div>
              {:else}
                {#each cmdInlineDrafts as cmdText, index}
                  <div class="cmd-row" class:selected={cmdSelectedIndex === index}>
                    {#if cmdEditingIndex === index}
                      <input
                        class="cmd-inline-input"
                        name={`vsm-cmd-inline-${index}`}
                        value={cmdText}
                        data-cmd-index={index}
                        autocomplete="new-password"
                        autocorrect="off"
                        autocapitalize="off"
                        spellcheck="false"
                        data-lpignore="true"
                        data-1p-ignore="true"
                        data-form-type="other"
                        on:focus={(event) => (cmdInlineInputEls[index] = event.currentTarget)}
                        on:input={(event) => { updateCmdInlineDraft(index, event.target.value); updateAutocomplete(event.target); }}
                        on:blur={(event) => handleCmdInlineBlur(event, index)}
                        on:keydown={(event) => handleCmdInlineKeydown(event, index)}
                        disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked}
                      />
                    {:else}
                      {@const tokens = renderCommandTokens(cmdText, projectConfigAgents, pluginInterfaces, projectConfigView)}
                      <div
                        class="cmd-inline-display"
                        on:click={async () => { cmdSelectedIndex = index; cmdEditingIndex = index; await tick(); const inp = document.querySelector(`.cmd-inline-input[data-cmd-index="${index}"]`); if (inp) inp.focus(); }}
                        on:keydown={(e) => { if (e.key === "Enter" || e.key === " ") { cmdSelectedIndex = index; cmdEditingIndex = index; } }}
                        role="button"
                        tabindex="0"
                      >
                        {#each tokens as token}
                          {#if token.type === "agent" || token.type === "action" || token.type === "argKey"}
                            <span class={token.known ? "cmd-token-known" : "cmd-token-unknown"}>{token.text}</span>
                          {:else}
                            <span>{token.text}</span>
                          {/if}
                        {/each}
                      </div>
                    {/if}
                  </div>
                {/each}
              {/if}
            </div>
            <div class="def-actions">
                <button
                  type="button"
                  class="ghost icon-button"
                  on:click={startCmdAdd}
                  disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked}
                  data-cmd-add-button="true"
                  aria-label="Add command"
                  title="Add command"
                >
                <IconPlus className="icon" />
              </button>
                <button
                  type="button"
                  class="ghost icon-button danger"
                  on:click={deleteSelectedCmd}
                  disabled={!wsConnected || sceneFlowBusy || rootSceneFlowCommandEditingLocked || cmdSelectedIndex === null}
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
                    rootSceneFlowCommandEditingLocked ||
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
                    rootSceneFlowCommandEditingLocked ||
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
          <div class="cmd-helper">
            <div class="cmd-helper-body">
              <div class="cmd-helper-tabs" role="tablist">
                <button
                  type="button"
                  class="cmd-helper-tab"
                  class:active={cmdHelperTab === "PlayAction"}
                  disabled={cmdHelperDetectedTab !== null && cmdHelperDetectedTab !== "PlayAction"}
                  on:click={() => { cmdHelperTab = "PlayAction"; updateCmdHelperTab(); }}
                  role="tab"
                  aria-selected={cmdHelperTab === "PlayAction"}
                >PlayAction</button>
                <button
                  type="button"
                  class="cmd-helper-tab"
                  class:active={cmdHelperTab === "PlayScene"}
                  disabled={cmdHelperDetectedTab !== null && cmdHelperDetectedTab !== "PlayScene"}
                  on:click={() => { cmdHelperTab = "PlayScene"; updateCmdHelperTab(); }}
                  role="tab"
                  aria-selected={cmdHelperTab === "PlayScene"}
                >PlayScene</button>
                <button
                  type="button"
                  class="cmd-helper-tab"
                  class:active={cmdHelperTab === "Variable"}
                  disabled={cmdHelperDetectedTab !== null && cmdHelperDetectedTab !== "Variable"}
                  on:click={() => { cmdHelperTab = "Variable"; updateCmdHelperTab(); }}
                  role="tab"
                  aria-selected={cmdHelperTab === "Variable"}
                >Variable</button>
              </div>

              {#if cmdHelperTab === "PlayAction"}
                <label for="cmd-helper-agent">Agent</label>
                <select
                  id="cmd-helper-agent"
                  bind:value={cmdHelperAgent}
                  autocomplete="off"
                >
                  <option value="">Select agent...</option>
                  {#each projectConfigAgents as agent}
                    <option value={agent.name}>{agent.name}</option>
                  {/each}
                </select>
                <label for="cmd-helper-action">Action{cmdHelperAgentCommands.length ? ` (${cmdHelperAgentCommands.length})` : ""}</label>
                <select
                  id="cmd-helper-action"
                  bind:value={cmdHelperAction}
                  autocomplete="off"
                >
                  <option value="">Select action...</option>
                  {#if cmdHelperAgentCommands.length}
                    {#each cmdHelperAgentCommands as action}
                      <option value={action?.name}>{action?.name}{action?.summary ? ` — ${action.summary}` : ""}</option>
                    {/each}
                  {:else}
                    {#each scriptElements.acticon as action}
                      <option value={action?.name || action?.script}>{action?.name || action?.script}</option>
                    {/each}
                  {/if}
                </select>
                <label for="cmd-helper-playmode">Execution</label>
                <select id="cmd-helper-playmode" bind:value={cmdHelperPlayMode}>
                  <option value="blocking">Blocking (default)</option>
                  <option value="nonblocking">Non-blocking</option>
                </select>
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
                      {@const paramMeta = cmdParamMeta(arg?.key)}
                      <div class="cmd-helper-arg-row">
                        <input
                          name={`vsm-cmd-arg-key-${argIndex}`}
                          placeholder="key"
                          value={arg.key}
                          autocomplete="new-password"
                          autocorrect="off"
                          autocapitalize="off"
                          spellcheck="false"
                          data-lpignore="true"
                          data-1p-ignore="true"
                          data-form-type="other"
                          on:input={(event) => updateCmdHelperArg(argIndex, "key", event.target.value)}
                        />
                        <input
                          name={`vsm-cmd-arg-value-${argIndex}`}
                          placeholder={cmdParamValuePlaceholder(paramMeta)}
                          value={arg.value}
                          autocomplete="new-password"
                          autocorrect="off"
                          autocapitalize="off"
                          spellcheck="false"
                          data-lpignore="true"
                          data-1p-ignore="true"
                          data-form-type="other"
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
                      {#if paramMeta}
                        <div class="cmd-helper-arg-meta">
                          <span>{cmdParamHint(paramMeta)}</span>
                          {#if paramMeta.description}
                            <span class="muted">{paramMeta.description}</span>
                          {/if}
                        </div>
                      {/if}
                    {/each}
                  {/if}
                </div>
                {#if cmdHelperWarnings.length}
                  <div class="cmd-helper-warnings">
                    {#each cmdHelperWarnings as warning}
                      <p class="cmd-helper-warning">{warning}</p>
                    {/each}
                  </div>
                {/if}

              {:else if cmdHelperTab === "PlayScene"}
                <label for="cmd-helper-scene">Scene</label>
                <select id="cmd-helper-scene" bind:value={cmdHelperScene}>
                  <option value="">Select scene...</option>
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

              {:else if cmdHelperTab === "Variable"}
                <div class="cmd-helper-tabs" role="tablist" aria-label="Variable operation">
                  <button
                    type="button"
                    class="cmd-helper-tab"
                    class:active={cmdHelperVarOp === "Assign"}
                    on:click={() => { cmdHelperVarOp = "Assign"; updateCmdHelperVarOp(); }}
                    role="tab"
                    aria-selected={cmdHelperVarOp === "Assign"}
                  >Assign</button>
                  <button
                    type="button"
                    class="cmd-helper-tab"
                    class:active={cmdHelperVarOp === "Inc"}
                    on:click={() => { cmdHelperVarOp = "Inc"; updateCmdHelperVarOp(); }}
                    role="tab"
                    aria-selected={cmdHelperVarOp === "Inc"}
                  >Increment</button>
                  <button
                    type="button"
                    class="cmd-helper-tab"
                    class:active={cmdHelperVarOp === "Dec"}
                    on:click={() => { cmdHelperVarOp = "Dec"; updateCmdHelperVarOp(); }}
                    role="tab"
                    aria-selected={cmdHelperVarOp === "Dec"}
                  >Decrement</button>
                </div>
                <div class="cmd-field-label">Variable</div>
                <div class="cmd-helper-var-wrap">
                  <div
                    class="editable-input"
                    class:input-warning={!cmdHelperVarExists && cmdHelperVarName.trim().length}
                    class:is-empty={!cmdHelperVarName.trim().length}
                    contenteditable="true"
                    role="textbox"
                    tabindex="0"
                    id="vsm-command-symbol-input"
                    aria-label="SceneFlow variable identifier"
                    bind:this={cmdHelperVarInputEl}
                    spellcheck="false"
                    data-placeholder="Variable name"
                    on:input={handleCmdHelperVarInput}
                    on:focus={handleCmdHelperVarFocus}
                    on:blur={handleCmdHelperVarBlur}
                    on:keydown={handleCmdHelperVarKeydown}
                  ></div>
                  {#if cmdHelperVarSuggestOpen && cmdHelperVarSuggestions.length > 0}
                    <div class="cmd-helper-var-dropdown" role="listbox" aria-label="Variable suggestions">
                      {#each cmdHelperVarSuggestions as variable, i}
                        <button
                          type="button"
                          class="cmd-ac-item"
                          class:selected={i === cmdHelperVarSuggestIndex}
                          role="option"
                          aria-selected={i === cmdHelperVarSuggestIndex}
                          on:mousedown|preventDefault={() => selectCmdHelperVarSuggestion(variable)}
                        >
                          <span class="cmd-ac-label">{variable.name}</span>
                          <span class="cmd-ac-detail">{variable.type}{variable.scope ? ` · ${variable.scope}` : ""}</span>
                        </button>
                      {/each}
                    </div>
                  {/if}
                </div>
                {#if !cmdHelperVarExists && cmdHelperVarName.trim().length}
                  <p class="muted">Variable not found. It will be created if you apply.</p>
                {/if}
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
                {#if cmdHelperVarOp === "Assign"}
                  <label for="cmd-helper-expr">Expression</label>
                  <input
                    id="cmd-helper-expr"
                    name="vsm-command-expression"
                    bind:value={cmdHelperVarExpr}
                    placeholder={varExpressionHint(cmdHelperVarType)}
                    autocomplete="new-password"
                    autocorrect="off"
                    autocapitalize="off"
                    spellcheck="false"
                    data-lpignore="true"
                    data-1p-ignore="true"
                    data-form-type="other"
                  />
                {:else}
                  <label for="cmd-helper-step">Step</label>
                  <input
                    id="cmd-helper-step"
                    name="vsm-command-step"
                    bind:value={cmdHelperVarStep}
                    placeholder="1"
                    autocomplete="new-password"
                    autocorrect="off"
                    autocapitalize="off"
                    spellcheck="false"
                    data-lpignore="true"
                    data-1p-ignore="true"
                    data-form-type="other"
                  />
                {/if}
              {/if}
            </div>
            <div class="actions cmd-helper-actions">
              <button type="button" class="primary" on:click={applyCmdHelperInsert} disabled={rootSceneFlowCommandEditingLocked}>
                {cmdSelectedIndex !== null ? "Update" : "Insert"}
              </button>
            </div>
          </div>
          {#if cmdError}
            <p class="error">{cmdError}</p>
          {/if}
        </div>
      </div>
      {#if cmdAcVisible && cmdAcItems.length > 0}
        <div
          class="cmd-ac-dropdown"
          role="listbox"
          aria-label="Autocomplete suggestions"
          style="left:{cmdAcPos.left}px; top:{cmdAcPos.top}px; width:{cmdAcPos.width}px;"
        >
          {#each cmdAcItems as item, i}
            <button
              class="cmd-ac-item"
              class:selected={i === cmdAcSelectedIdx}
              role="option"
              aria-selected={i === cmdAcSelectedIdx}
              on:mousedown|preventDefault={() => {
                const el = cmdEditingIndex !== null ? cmdInlineInputEls[cmdEditingIndex] : null;
                if (el) acceptAcItem(el, item);
              }}
            >
              <span class="cmd-ac-label">{item.label}</span>
              {#if item.detail}
                <span class="cmd-ac-detail">{item.detail}</span>
              {/if}
            </button>
          {/each}
        </div>
      {/if}
    </div>
  {/if}

  {#if varDefDraft}
    <div class="modal-backdrop def-backdrop" role="presentation">
      <div class="modal def-modal var-def-modal" bind:this={varDefDialogEl} role="dialog" aria-modal="true" aria-labelledby="var-def-dialog-title" tabindex="-1">
        <div class="def-header">
          <h3 id="var-def-dialog-title">{varDefEditIndex >= 0 ? "Edit variable definition" : "Add variable definition"}</h3>
          <button
            type="button"
            class="ghost icon-button def-close"
            on:click={closeVarDefDialog}
            aria-label="Close variable definition"
            title="Close"
          >
            ×
          </button>
        </div>
        <div class="modal-body def-body">
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
          {#if varDefDraft.type !== "Event"}
            <label for="var-def-exp">Expression</label>
            <input
              id="var-def-exp"
              bind:value={varDefDraft.expression}
              placeholder={varExpressionHint(varDefDraft.type)}
              on:keydown={handleVarDefKeydown}
            />
          {:else}
            <label for="var-def-event-eltype">Element type</label>
            <select id="var-def-event-eltype" bind:value={varDefDraft.eventElementType}>
              <option value="">Any</option>
              <option value="String">String</option>
              <option value="Int">Int</option>
              <option value="Bool">Bool</option>
              <option value="Float">Float</option>
            </select>
            <label for="var-def-event-cap">Queue capacity</label>
            <input
              id="var-def-event-cap"
              type="number"
              min="0"
              bind:value={varDefDraft.eventCapacity}
              placeholder="0 = unlimited"
              on:keydown={handleVarDefKeydown}
            />
            <p class="hint">FIFO queue. Capacity 0 means unlimited. Oldest events are dropped when full.</p>
          {/if}
        </div>
        <div class="actions def-actions">
          <button type="button" class="primary" on:click={applyVarDefEdit} disabled={!wsConnected || sceneFlowBusy}>
            {varDefEditIndex >= 0 ? "Save" : "Add"}
          </button>
        </div>
        {#if varDefError}
          <p class="error">{varDefError}</p>
        {/if}
      </div>
    </div>
  {/if}

  <PluginDashboard
    open={pluginDashboardOpen}
    projectId={selectedProjectId}
    projectName={selectedProject?.name || selectedProjectId || ""}
    wsConnected={wsConnected}
    serverMode={info?.mode || "FULL_EDITOR"}
    onClose={closePluginDashboard}
    {apiGet}
    {apiPost}
    {apiPut}
    {sendCommand}
  />

  {#if !showEditor}
    <footer class="landing-footer" aria-label="Credits">
      <div class="landing-footer-logos">
        <span class="landing-footer-lead">
          build with
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            stroke="currentColor"
            class="landing-footer-icon"
            aria-hidden="true"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M21 8.25c0-2.485-2.099-4.5-4.688-4.5-1.935 0-3.597 1.126-4.312 2.733-.715-1.607-2.377-2.733-4.313-2.733C5.1 3.75 3 5.765 3 8.25c0 7.22 9 12 9 12s9-4.78 9-12Z"
            />
          </svg>
          and
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            stroke="currentColor"
            class="landing-footer-icon"
            aria-hidden="true"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M8.25 3v1.5M4.5 8.25H3m18 0h-1.5M4.5 12H3m18 0h-1.5m-15 3.75H3m18 0h-1.5M8.25 19.5V21M12 3v1.5m0 15V21m3.75-18v1.5m0 15V21m-9-1.5h10.5a2.25 2.25 0 0 0 2.25-2.25V6.75a2.25 2.25 0 0 0-2.25-2.25H6.75A2.25 2.25 0 0 0 4.5 6.75v10.5a2.25 2.25 0 0 0 2.25 2.25Zm.75-12h9v9h-9v-9Z"
            />
          </svg>
        </span>by
        <a
          class="landing-footer-link landing-footer-link--scaai"
          href="https://scaai.dfki.de"
          target="_blank"
          rel="noopener noreferrer"
        >
          <img class="landing-footer-logo landing-footer-logo--scaai" src="/images/scaai_logo.svg" alt="SCAAI" />
        </a>
        <span class="landing-footer-sep" aria-hidden="true">@</span>
        <span>
          <a
            class="landing-footer-link"
            href="https://www.dfki.de"
            target="_blank"
            rel="noopener noreferrer"
          >dfki</a>, 2003-{infoBuildYear}.
        </span>
      </div>
    </footer>
  {/if}

  {#if preflightModalOpen && preflightData}
    <div
      class="modal-backdrop"
      on:click|self={cancelPreflight}
      role="presentation"
    >
      <div class="modal preflight-modal" role="dialog" aria-modal="true" aria-labelledby="preflight-title" tabindex="-1">
        <h3 id="preflight-title">First run on this machine</h3>
        <div class="modal-body">
          <p>
            This project is being run for the first time on this machine.
            The following plugin settings are marked as machine-specific — please verify
            they are correct before starting.
          </p>
          {#each preflightData.machineSpecificConfig as block}
            <div class="preflight-plugin-block">
              <div class="preflight-plugin-name">{block.pluginDisplayName}</div>
              {#each block.entries as entry}
                <div class="preflight-entry">
                  <div class="preflight-entry-name">{entry.name}</div>
                  <div class="preflight-entry-desc">{entry.description}</div>
                  <div class="preflight-entry-value">
                    Current value: <code>{entry.currentValue || "(empty)"}</code>
                  </div>
                </div>
              {/each}
            </div>
          {/each}
        </div>
        <div class="row row-end">
          <button type="button" class="ghost" on:click={openPluginDashboardFromPreflight}>
            Review in Plugin Dashboard
          </button>
          <button type="button" class="primary" on:click={confirmPreflight}>
            Run Anyway
          </button>
        </div>
      </div>
    </div>
  {/if}
</main>
