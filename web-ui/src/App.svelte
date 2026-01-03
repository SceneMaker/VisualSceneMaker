<script>
  import { tick } from "svelte";
  import SceneFlowMiniMap from "./SceneFlowMiniMap.svelte";
  import SceneFlowView from "./SceneFlowView.svelte";
  import ScriptEditor from "./ScriptEditor.svelte";
  import IconChevronDown from "./icons/IconChevronDown.svelte";
  import IconChevronUp from "./icons/IconChevronUp.svelte";
  import IconPencil from "./icons/IconPencil.svelte";
  import IconPlus from "./icons/IconPlus.svelte";
  import IconStart from "./icons/IconStart.svelte";
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

  const SCENE_DRAG_TYPE = "application/x-vsm-scene";

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
  let scriptScenesError = "";
  let scriptScenesLoading = false;
  let scriptElements = { acticon: [], gesticon: [], visicon: [] };
  let scriptElementsFilter = "";
  let scriptElementsError = "";
  let scriptElementsLoading = false;

  let sceneFlow = null;
  let sceneFlowError = "";
  let sceneFlowLoading = false;
  let lastSceneFlowProjectId = "";
  let sceneFlowRef;
  let sceneFlowZoom = 1;
  let sceneFlowWorldBox = null;
  let sceneFlowViewBox = null;
  let sceneFlowSelection = null;
  let sceneFlowFrameColor = "#7d7d7d";
  let sceneFlowFrameStyle = "";
  let sceneFlowSnap = true;
  let sceneFlowShowCmdText = true;
  let sceneFlowBusy = false;
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
  $: startNodes = sceneFlow?.nodes ? sceneFlow.nodes.filter((node) => node.isStart && !node.isHistory) : [];
  $: sceneFlowFrameColor = superNodeFrameColor(sceneFlow);
  $: sceneFlowFrameStyle = `--sf-frame-color:${sceneFlowFrameColor};`;
  $: filteredScriptScenes = filterSceneLanguages(scriptScenes, scriptScenesFilter);
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
      return String(edgeDraft.probability ?? "") !== String(selectedEdge.probability ?? "") || altDirty;
    }
    if (selectedEdge.type === "TEDGE") {
      return String(edgeDraft.timeoutMs ?? "") !== String(selectedEdge.timeoutMs ?? "") || altDirty;
    }
    return altDirty;
  })();

  $: if (selectedProjectId && selectedProjectId !== localStorage.getItem("vsm_project_id")) {
    localStorage.setItem("vsm_project_id", selectedProjectId);
  }

  $: if (selectedProjectId && selectedProjectId !== lastConfigProjectId) {
    lastConfigProjectId = selectedProjectId;
    loadConfig(selectedProjectId);
  }

  $: if (selectedProjectId && selectedProjectId !== lastScriptProjectId) {
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

  $: if (selectedProjectId && selectedProjectId !== lastSceneFlowProjectId) {
    lastSceneFlowProjectId = selectedProjectId;
    loadSceneFlow(selectedProjectId);
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
    edgeCreateMode = false;
    edgeCreateSourceId = "";
  }

  async function connectAll() {
    error = "";
    statusMessage = "";
    try {
      await loadInfo();
      await Promise.all([loadProjects(), loadPreferences()]);
      connectWs();
      if (selectedProjectId) {
        await Promise.all([
          loadConfig(selectedProjectId),
          loadScript(selectedProjectId),
          loadScriptScenes(selectedProjectId),
          loadScriptElements(selectedProjectId),
          loadSceneFlow(selectedProjectId)
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
    edgeCreateSourceId = "";
    try {
      const query = superNodeId ? `?superNodeId=${encodeURIComponent(superNodeId)}` : "";
      const data = await apiGet(`/api/v1/projects/${projectId}/sceneflow${query}`);
      sceneFlow = data;
    } catch (err) {
      sceneFlowError = err.message || "Failed to load SceneFlow.";
      sceneFlow = null;
    } finally {
      sceneFlowLoading = false;
    }
  }

  async function navigateSceneFlow(superNodeId) {
    if (!selectedProjectId || !superNodeId) return;
    if (sceneFlow?.superNodeId === superNodeId) return;
    sceneFlowError = "";
    sceneFlowLoading = true;
    sceneFlowSelection = null;
    edgeCreateSourceId = "";
    try {
      const data = await apiPost(`/api/v1/projects/${selectedProjectId}/sceneflow/navigate`, { superNodeId });
      sceneFlow = data;
    } catch (err) {
      sceneFlowError = err.message || "Failed to navigate SceneFlow.";
    } finally {
      sceneFlowLoading = false;
    }
  }

  function scheduleScriptDiagnostics() {
    if (!selectedProjectId || !token) return;
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
    if (!token) {
      wsConnected = false;
      return;
    }
    return new Promise((resolve) => {
      const protocol = location.protocol === "https:" ? "wss" : "ws";
      const url = `${protocol}://${location.host}/ws?token=${encodeURIComponent(token)}`;
      ws = new WebSocket(url);
      ws.onopen = () => {
        wsConnected = true;
        resolve();
      };
      ws.onclose = () => {
        wsConnected = false;
      };
      ws.onerror = () => {
        wsError = "WebSocket connection failed.";
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
        entry.reject(new Error(message.name || "Request failed"));
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

  async function handleCommandSceneDrop(event) {
    const payload = parseSceneDrop(event);
    if (!payload?.name || !nodeEditorTarget?.id) return;
    event.preventDefault();
    await addSceneCommandToNode(nodeEditorTarget.id, payload.name);
  }

  function sceneGroupTotal(groups) {
    if (!Array.isArray(groups)) return 0;
    return groups.reduce((total, group) => total + (group?.count ?? 0), 0);
  }

  function filterSceneLanguages(languages, query) {
    if (!Array.isArray(languages)) return [];
    const needle = (query || "").trim().toLowerCase();
    if (!needle) return languages;
    return languages
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
  }

  async function moveVarDef(index, direction) {
    if (!selectedProjectId || !nodeEditorTarget) return null;
    if (!nodeEditorVarDefs[index]) return null;
    const target = index + direction;
    if (target < 0 || target >= nodeEditorVarDefs.length) return null;
    return await runSceneFlowCommand("SceneFlow.Node.VarDef.Move", {
      projectId: selectedProjectId,
      nodeId: nodeEditorTarget.id,
      from: index,
      to: target
    });
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

  async function createSceneFlowNode(nodeType) {
    if (!selectedProjectId) return;
    const center = sceneFlowCenter();
    await runSceneFlowCommand("SceneFlow.Node.Create", {
      projectId: selectedProjectId,
      nodeType,
      x: center.x,
      y: center.y
    });
  }

  async function createSceneFlowComment() {
    if (!selectedProjectId) return;
    const center = sceneFlowCenter();
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
    }
  }

  async function handleEdgePick(nodeId) {
    if (!edgeCreateMode || !nodeId) return;
    if (!edgeCreateSourceId) {
      edgeCreateSourceId = nodeId;
      sceneFlowSelection = { type: "node", id: nodeId };
      return;
    }
    if (edgeCreateSourceId === nodeId) {
      edgeCreateSourceId = "";
      sceneFlowSelection = null;
      return;
    }
    await createSceneFlowEdge(edgeCreateSourceId, nodeId);
    edgeCreateSourceId = "";
    edgeCreateMode = false;
    sceneFlowSelection = null;
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
      const raw = String(edgeDraft.probability ?? "").trim();
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isFinite(parsed)) {
        edgeEditError = "Probability must be a number.";
        return;
      }
      if (parsed < 0 || parsed > 100) {
        edgeEditError = "Probability must be between 0 and 100.";
        return;
      }
      if (parsed !== (selectedEdge.probability ?? 0)) {
        fields.probability = parsed;
      }
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
    if (!selectedProjectId || !sceneFlowSelection) return;
    const { type, id } = sceneFlowSelection;
    if (type === "node") {
      await runSceneFlowCommand("SceneFlow.Node.Delete", { projectId: selectedProjectId, nodeId: id });
      sceneFlowSelection = null;
      return;
    }
    if (type === "comment") {
      await runSceneFlowCommand("SceneFlow.Comment.Delete", { projectId: selectedProjectId, commentId: id });
      sceneFlowSelection = null;
      return;
    }
    if (type === "edge") {
      const edge = sceneFlow?.edges?.find((entry) => entry.id === id);
      const payload = { projectId: selectedProjectId, edgeId: id };
      if (edge?.sourceId) {
        payload.sourceId = edge.sourceId;
      }
      if (edge?.targetId) {
        payload.targetId = edge.targetId;
      }
      await runSceneFlowCommand("SceneFlow.Edge.Delete", payload);
      sceneFlowSelection = null;
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
</script>

<main>
  <header class="hero">
    <div>
      <h1>Visual SceneMaker Web UI</h1>
      <p>Preferences and project dialogs are live. Use the token to connect to the editor.</p>
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
      <button type="button" class="ghost" on:click={loadInfo}>Refresh Info</button>
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

    <section class="panel script-panel">
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

    <section class="panel">
      <header class="panel-title">
        <h2>Scenes</h2>
        <button
          type="button"
          class="ghost"
          on:click={() => loadScriptScenes(selectedProjectId)}
          disabled={!selectedProject}
        >
          Reload
        </button>
      </header>
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
                  <div
                    class="scene-item"
                    role="listitem"
                  >
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
    </section>

    <section class="panel">
      <header class="panel-title">
        <h2>Script Elements</h2>
        <button
          type="button"
          class="ghost"
          on:click={() => loadScriptElements(selectedProjectId)}
          disabled={!selectedProject}
        >
          Reload
        </button>
      </header>
      <input
        class="search"
        placeholder="Filter elements"
        bind:value={scriptElementsFilter}
        disabled={!selectedProject}
      />
      {#if !selectedProject}
        <p class="muted">Select a project to view script elements.</p>
      {:else if scriptElementsLoading}
        <p class="muted">Loading script elements...</p>
      {:else if scriptElementsError}
        <p class="error">{scriptElementsError}</p>
      {:else}
        <div class="element-list">
          <details class="element-category" open>
            <summary>Acticon ({countActicon(filteredScriptElements.acticon)})</summary>
            {#if filteredScriptElements.acticon.length === 0}
              <p class="muted">No actions found.</p>
            {:else}
              <div class="element-items">
                {#each filteredScriptElements.acticon as action}
                  <div class="element-row">
                    <div class="element-main">
                      <span class="element-name">{action?.name || "Action"}</span>
                      {#if action?.script}
                        <span class="element-script" title={action.script}>{action.script}</span>
                      {/if}
                    </div>
                    <button
                      type="button"
                      class="ghost"
                      on:click={() => insertScriptSnippet(action?.script)}
                      disabled={!action?.script}
                    >
                      Insert
                    </button>
                  </div>
                {/each}
              </div>
            {/if}
          </details>
          <details class="element-category">
            <summary>Gesticon ({countGesticon(filteredScriptElements.gesticon)})</summary>
            {#if filteredScriptElements.gesticon.length === 0}
              <p class="muted">No gestures found.</p>
            {:else}
              {#each filteredScriptElements.gesticon as agent}
                <div class="element-group">
                  <div class="element-group-title">
                    <span>{agent?.agent || "Agent"}</span>
                    <span class="scene-count">{agent?.gestures?.length || 0}</span>
                  </div>
                  <div class="element-items">
                    {#each agent?.gestures || [] as gesture}
                      {@const meta = gestureMeta(gesture)}
                      <div class="element-row">
                        <div class="element-main">
                          <span class="element-name">{gestureLabel(gesture)}</span>
                          {#if meta}
                            <span class="element-meta">{meta}</span>
                          {/if}
                        </div>
                        <button
                          type="button"
                          class="ghost"
                          on:click={() => insertScriptSnippet(gesture?.script)}
                          disabled={!gesture?.script}
                        >
                          Insert
                        </button>
                      </div>
                    {/each}
                  </div>
                </div>
              {/each}
            {/if}
          </details>
          <details class="element-category">
            <summary>Visicon ({countVisicon(filteredScriptElements.visicon)})</summary>
            {#if filteredScriptElements.visicon.length === 0}
              <p class="muted">No visemes found.</p>
            {:else}
              {#each filteredScriptElements.visicon as agent}
                <div class="element-group">
                  <div class="element-group-title">
                    <span>{agent?.agent || "Agent"}</span>
                    <span class="scene-count">{agent?.visemes?.length || 0}</span>
                  </div>
                  <div class="element-items">
                    {#each agent?.visemes || [] as viseme}
                      <div class="element-row">
                        <div class="element-main">
                          <span class="element-name">{viseme?.key || "Viseme"}</span>
                          {#if viseme?.value}
                            <span class="element-meta">{viseme.value}</span>
                          {/if}
                        </div>
                      </div>
                    {/each}
                  </div>
                </div>
              {/each}
            {/if}
          </details>
        </div>
      {/if}
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
          on:click={() => createSceneFlowNode("Basic")}
          disabled={!selectedProject || !wsConnected || sceneFlowBusy}
        >
          Add node
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => createSceneFlowNode("Super")}
          disabled={!selectedProject || !wsConnected || sceneFlowBusy}
        >
          Add super
        </button>
        <button
          type="button"
          class="ghost"
          on:click={createSceneFlowComment}
          disabled={!selectedProject || !wsConnected || sceneFlowBusy}
        >
          Add comment
        </button>
        <button
          type="button"
          class="ghost"
          class:active={edgeCreateMode}
          on:click={toggleEdgeCreateMode}
          disabled={!selectedProject || !wsConnected || sceneFlowBusy || !sceneFlow}
        >
          Add edge
        </button>
        <label class="toggle edge-type">
          <span>Edge type</span>
          <select bind:value={edgeCreateType} disabled={!sceneFlow || !wsConnected || sceneFlowBusy}>
            <option value="EEDGE">Epsilon</option>
            <option value="CEDGE">Conditional</option>
            <option value="IEDGE">Interruptive</option>
            <option value="PEDGE">Probability</option>
            <option value="TEDGE">Timeout</option>
            <option value="FEDGE">Fork</option>
          </select>
        </label>
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
        <label class="toggle">
          <input type="checkbox" bind:checked={sceneFlowSnap} disabled={!sceneFlow} />
          <span>Snap</span>
        </label>
        <button
          type="button"
          class="ghost"
          on:click={() => sceneFlowRef?.zoomOut()}
          disabled={!sceneFlow}
        >
          Zoom out
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => sceneFlowRef?.zoomIn()}
          disabled={!sceneFlow}
        >
          Zoom in
        </button>
        <button
          type="button"
          class="ghost"
          on:click={() => sceneFlowRef?.fitToView()}
          disabled={!sceneFlow}
        >
          Fit
        </button>
        {#if sceneFlow}
          <span class="muted">Zoom {Math.round(sceneFlowZoom * 100)}%</span>
        {/if}
        {#if edgeCreateMode}
          <span class="muted">
            Edge {edgeTypeLabel(edgeCreateType)}: {edgeCreateSourceId ? `source ${edgeCreateSourceId} → pick target` : "pick source node"}
          </span>
        {/if}
        {#if sceneFlow?.path?.length}
          <span class="muted">Path: {sceneFlow.path.join(" / ")}</span>
        {/if}
        {#if sceneFlow?.revision}
          <span class="muted">rev {sceneFlow.revision}</span>
        {/if}
        {#if sceneFlowLoading}
          <span class="muted">Loading...</span>
        {/if}
      </div>
      {#if !selectedProject}
        <p class="muted">Select a project to view the SceneFlow graph.</p>
      {:else if sceneFlow}
        <div class="sceneflow-layout">
          <div class="sceneflow-container" style={sceneFlowFrameStyle}>
            <div class="sceneflow-scroll">
              <SceneFlowView
                bind:this={sceneFlowRef}
                bind:zoomLevel={sceneFlowZoom}
                bind:worldBox={sceneFlowWorldBox}
                bind:viewBoxState={sceneFlowViewBox}
                bind:selection={sceneFlowSelection}
                config={configDraft}
                snapshot={sceneFlow}
                onNavigate={navigateSceneFlow}
                onNodeMove={moveSceneFlowNode}
                onCommentUpdate={updateSceneFlowComment}
                onEdgeControlUpdate={updateSceneFlowEdgeControl}
                onDeleteSelection={deleteSceneFlowSelection}
                onUndo={undoSceneFlow}
                onRedo={redoSceneFlow}
                snapToGrid={sceneFlowSnap}
                edgeCreateMode={edgeCreateMode}
                onEdgePick={handleEdgePick}
                onSceneDrop={handleSceneFlowSceneDrop}
                sceneDragType={SCENE_DRAG_TYPE}
                showCommandText={sceneFlowShowCmdText}
                onCommandOpen={openCmdDialog}
              />
            </div>
            <button
              type="button"
              class="sceneflow-cmd-toggle"
              class:active={sceneFlowShowCmdText}
              on:click={() => (sceneFlowShowCmdText = !sceneFlowShowCmdText)}
              aria-pressed={sceneFlowShowCmdText}
            >
              show cmds
            </button>
            <SceneFlowMiniMap
              snapshot={sceneFlow}
              worldBox={sceneFlowWorldBox}
              viewBox={sceneFlowViewBox}
              onCenter={(x, y) => sceneFlowRef?.centerOn(x, y)}
            />
          </div>
          <aside class="sceneflow-inspector">
            {#if sceneFlowSelection?.type === "node" && selectedNode && nodeDraft}
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
                  <label for="edge-probability">Probability (0-100)</label>
                  <input id="edge-probability" type="number" min="0" max="100" bind:value={edgeDraft.probability} />
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

            {#if nodeEditorTarget}
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
          <p class="muted sceneflow-hint">Drag to pan, scroll to zoom, drag nodes/comments to move, double click a super node to drill down.</p>
        </div>
      {:else}
        <p class="muted">No SceneFlow data loaded yet.</p>
      {/if}
      {#if sceneFlowError}
        <p class="error">{sceneFlowError}</p>
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
