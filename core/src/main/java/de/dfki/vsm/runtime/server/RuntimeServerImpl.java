package de.dfki.vsm.runtime.server;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.EdgeExecutedEvent;
import de.dfki.vsm.event.event.NodeExecutedEvent;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.NodeTerminatedEvent;
import de.dfki.vsm.event.event.SceneStoppedEvent;
import de.dfki.vsm.event.event.TimeoutEdgeStartedEvent;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.web.SceneFlowSnapshotBuilder;
import io.javalin.Javalin;
import io.javalin.http.Context;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Headless Runtime Server for SceneMaker.
 * Provides REST API and WebSocket support for runtime control and monitoring.
 * Implements EventListener to broadcast runtime events to connected Web UI clients.
 *
 * This server is designed to run standalone on Android or Desktop Java environments,
 * allowing remote Web UI clients to connect, monitor, and control runtime execution.
 */
public class RuntimeServerImpl implements EventListener {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";

    private Javalin mApp;
    private String mAuthToken;
    private int mPort;
    private String mBindHost;

    // Runtime project management
    private RunTimeProject mRuntimeProject;
    private String mProjectPath;
    private String mProjectId;
    private String mRuntimeState = "stopped";

    // WebSocket sessions for event broadcasting
    private final Set<WsContext> mWsSessions = ConcurrentHashMap.newKeySet();

    public RuntimeServerImpl() {
    }

    /**
     * Starts the runtime server.
     *
     * @param port Port to bind to
     * @param bindHost Host to bind to ("127.0.0.1" for localhost, "0.0.0.0" for all interfaces)
     * @param token Authentication token (null = generate random token)
     */
    public void start(int port, String bindHost, String token) {
        if (mApp != null) {
            sLogger.warning("Runtime server already started");
            return;
        }

        mPort = port;
        mBindHost = bindHost;
        mAuthToken = (token != null) ? token : generateToken();

        Preferences.load();

        mApp = Javalin.create(config -> {
            // Serve the Web UI static files from classpath
            boolean hasWebUi = getClass().getClassLoader().getResource("web-ui/index.html") != null;
            if (hasWebUi) {
                config.addStaticFiles("/web-ui", Location.CLASSPATH);
                config.addSinglePageRoot("/", "/web-ui/index.html", Location.CLASSPATH);
            }
            // Enable CORS for cross-origin requests
            config.enableCorsForAllOrigins();
        }).start(mBindHost, mPort);

        registerRoutes();

        // Register for runtime events
        EventDispatcher.getInstance().register(this);

        sLogger.message("Runtime server started on " + mBindHost + ":" + mPort);
        sLogger.message("Auth token: " + mAuthToken);
    }

    /**
     * Stops the runtime server.
     */
    public void stop() {
        if (mApp != null) {
            EventDispatcher.getInstance().remove(this);

            // Unload runtime if loaded
            if (mRuntimeProject != null && mRuntimeProject.wasExecuted()) {
                mRuntimeProject.unload();
            }

            mApp.stop();
            mApp = null;
            sLogger.message("Runtime server stopped");
        }
    }

    /**
     * Loads a project from the filesystem.
     */
    public boolean loadProject(String path) {
        // Unload existing project if any
        if (mRuntimeProject != null && mRuntimeProject.wasExecuted()) {
            mRuntimeProject.unload();
        }

        try {
            mRuntimeProject = new RunTimeProject(new File(path));
            if (mRuntimeProject.parse(path)) {
                mProjectPath = path;
                mProjectId = UUID.randomUUID().toString();

                if (mRuntimeProject.launch()) {
                    mRuntimeState = "stopped";
                    broadcastRuntimeState();
                    sLogger.message("Loaded project: " + path);
                    return true;
                } else {
                    mRuntimeProject = null;
                    mProjectPath = null;
                    mProjectId = null;
                    sLogger.failure("Failed to launch project: " + path);
                }
            } else {
                mRuntimeProject = null;
                mProjectPath = null;
                mProjectId = null;
                sLogger.failure("Failed to parse project: " + path);
            }
        } catch (Exception e) {
            sLogger.failure("Error loading project: " + e.getMessage());
            mRuntimeProject = null;
            mProjectPath = null;
            mProjectId = null;
        }
        return false;
    }

    /**
     * Starts the runtime execution.
     */
    public boolean startRuntime() {
        if (mRuntimeProject == null) {
            return false;
        }
        if (mRuntimeProject.start()) {
            mRuntimeState = "running";
            broadcastRuntimeState();
            sLogger.message("Runtime started");
            return true;
        }
        return false;
    }

    // ========== Route Registration ==========

    private void registerRoutes() {
        // Public endpoints (no auth required)
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/token", this::handleToken);

        // Web UI compatible endpoints
        mApp.get(API_PREFIX + "/projects", this::handleProjects);
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.get(API_PREFIX + "/projects/samples", ctx -> handleEmptyList(ctx));
        mApp.get(API_PREFIX + "/projects/tutorials", ctx -> handleEmptyList(ctx));
        mApp.get(API_PREFIX + "/preferences", this::handlePreferences);
        mApp.get(API_PREFIX + "/devices", this::handleDevices);
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleProjectSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/runtime", this::handleProjectRuntime);
        mApp.get(API_PREFIX + "/projects/{pid}/config", this::handleProjectEditorConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config", this::handleProjectProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config/keys", this::handleProjectConfigKeys);
        mApp.get(API_PREFIX + "/projects/{pid}/script", this::handleProjectScript);
        mApp.get(API_PREFIX + "/projects/{pid}/script/scenes", this::handleProjectScriptScenes);
        mApp.get(API_PREFIX + "/projects/{pid}/script/elements", this::handleProjectScriptElements);
        mApp.get(API_PREFIX + "/projects/{pid}/history/commands", this::handleCommandLog);
        mApp.post(API_PREFIX + "/projects/{pid}/sceneflow/navigate", this::handleSceneflowNavigate);

        // Runtime control REST endpoints
        mApp.post(API_PREFIX + "/runtime/load", this::handleLoad);
        mApp.post(API_PREFIX + "/runtime/start", this::handleStart);
        mApp.post(API_PREFIX + "/runtime/pause", this::handlePause);
        mApp.post(API_PREFIX + "/runtime/resume", this::handleResume);
        mApp.post(API_PREFIX + "/runtime/stop", this::handleStopRest);
        mApp.post(API_PREFIX + "/runtime/unload", this::handleUnload);
        mApp.get(API_PREFIX + "/runtime/status", this::handleStatus);
        mApp.get(API_PREFIX + "/runtime/variables", this::handleVariables);
        mApp.get(API_PREFIX + "/runtime/sceneflow", this::handleSceneflowLegacy);

        // WebSocket endpoint for real-time event broadcasting and commands
        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                sLogger.message("[WS] Client connected: " + ctx.getSessionId());
                mWsSessions.add(ctx);
            });
            ws.onClose(ctx -> {
                sLogger.message("[WS] Client disconnected: " + ctx.getSessionId());
                mWsSessions.remove(ctx);
            });
            ws.onError(ctx -> {
                mWsSessions.remove(ctx);
            });
            ws.onMessage(ctx -> {
                handleWsMessage(ctx.message(), ctx::send);
            });
        });
    }

    // ========== EventListener Implementation ==========

    @Override
    public void update(EventObject event) {
        if (event == null) {
            return;
        }

        // Handle VariableChangedEvent
        if (event instanceof VariableChangedEvent) {
            VariableChangedEvent varEvent = (VariableChangedEvent) event;
            Tuple<String, String> pair = varEvent.getVarValue();
            if (pair == null || pair.getFirst() == null || pair.getFirst().isBlank()) {
                return;
            }
            JSONObject message = new JSONObject();
            message.put("type", "event");
            message.put("ts", System.currentTimeMillis());
            message.put("channel", "vars");
            message.put("event", "vars.updated");
            JSONObject payload = new JSONObject();
            if (mProjectId != null) {
                payload.put("projectId", mProjectId);
            }
            payload.put("name", pair.getFirst());
            payload.put("value", pair.getSecond() != null ? pair.getSecond() : "");
            message.put("payload", payload);
            broadcast(message.toString());
            return;
        }

        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        JSONObject payload = new JSONObject();
        if (mProjectId != null) {
            payload.put("projectId", mProjectId);
        }

        if (event instanceof NodeStartedEvent) {
            BasicNode node = ((NodeStartedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.nodeActive");

        } else if (event instanceof NodeExecutedEvent || event instanceof NodeTerminatedEvent) {
            BasicNode node = event instanceof NodeExecutedEvent
                    ? ((NodeExecutedEvent) event).getNode()
                    : ((NodeTerminatedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.nodeStopped");

        } else if (event instanceof EdgeExecutedEvent) {
            AbstractEdge edge = ((EdgeExecutedEvent) event).getEdge();
            if (edge == null) return;
            String sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            String targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            payload.put("edgeType", SceneFlowSnapshotBuilder.getEdgeTypeLowercase(edge));
            message.put("channel", "runtime");
            message.put("event", "runtime.edgeActive");

        } else if (event instanceof TimeoutEdgeStartedEvent) {
            TimeoutEdgeStartedEvent te = (TimeoutEdgeStartedEvent) event;
            TimeoutEdge edge = te.getEdge();
            if (edge == null) return;
            String sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            String targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            payload.put("edgeType", "timeout");
            payload.put("timeoutMs", te.getTimeoutMs());
            payload.put("startedAt", te.getStartedAt());
            payload.put("elapsedMs", 0L);
            payload.put("ratio", 0.0);
            message.put("channel", "runtime");
            message.put("event", "runtime.timeoutProgress");

        } else if (event instanceof SceneStoppedEvent || event instanceof TerminationEvent) {
            payload.put("status", "stopped");
            message.put("channel", "runtime");
            message.put("event", "runtime.state");
            mRuntimeState = "stopped";

        } else {
            // Unknown event type, skip
            return;
        }

        message.put("payload", payload);
        broadcast(message.toString());
    }

    // ========== WebSocket Command Handling ==========

    private void handleWsMessage(String raw, java.util.function.Consumer<String> sender) {
        try {
            JSONObject msg = new JSONObject(raw);
            String id = msg.optString("id", "");
            // Support both "method" (editor style) and "name" (web-ui style) for the command name
            String method = msg.optString("method", "");
            if (method.isEmpty()) {
                method = msg.optString("name", "");
            }
            // Support both "params" (editor style) and "payload" (web-ui style) for parameters
            JSONObject params = msg.optJSONObject("params");
            if (params == null) {
                params = msg.optJSONObject("payload");
            }
            if (params == null) {
                params = new JSONObject();
            }

            JSONObject result = dispatchWsCommand(method, params);

            // Send response in the format the Web UI expects
            JSONObject resp = new JSONObject();
            resp.put("type", "response");
            if (!id.isEmpty()) {
                resp.put("id", id);
            }
            resp.put("payload", result);
            resp.put("status", "ok");
            sender.accept(resp.toString());
        } catch (Exception exc) {
            JSONObject resp = new JSONObject();
            resp.put("type", "error");
            JSONObject payload = new JSONObject();
            payload.put("message", exc.getMessage());
            resp.put("payload", payload);
            resp.put("status", "error");
            sender.accept(resp.toString());
        }
    }

    private JSONObject dispatchWsCommand(String method, JSONObject params) {
        switch (method) {
            // Runtime control commands
            case "Runtime.Play":
            case "Runtime.Start":
                return handleWsRuntimePlay(params);
            case "Runtime.Pause":
                return handleWsRuntimePause(params);
            case "Runtime.Stop":
                return handleWsRuntimeStop(params);
            case "Runtime.Variable.Set":
                return handleWsVariableSet(params);
            case "Runtime.Query":
                return handleWsRuntimeQuery(params);

            // Read-only sceneflow queries
            case "SceneFlow.Get":
            case "SceneFlow.Snapshot":
                return handleWsSceneFlowGet(params);

            // Editing commands - rejected in runtime-only mode
            case "SceneFlow.Node.Add":
            case "SceneFlow.Node.Create":
            case "SceneFlow.Node.Update":
            case "SceneFlow.Node.Delete":
            case "SceneFlow.Node.Move":
            case "SceneFlow.Edge.Add":
            case "SceneFlow.Edge.Create":
            case "SceneFlow.Edge.Update":
            case "SceneFlow.Edge.Delete":
            case "SceneFlow.Comment.Add":
            case "SceneFlow.Comment.Create":
            case "SceneFlow.Comment.Update":
            case "SceneFlow.Comment.Delete":
            case "SceneFlow.Undo":
            case "SceneFlow.Redo":
            case "SceneFlow.Node.VarDef.Add":
            case "SceneFlow.Node.VarDef.Update":
            case "SceneFlow.Node.VarDef.Delete":
            case "SceneFlow.Node.VarDef.Move":
            case "SceneFlow.Node.TypeDef.Add":
            case "SceneFlow.Node.TypeDef.Update":
            case "SceneFlow.Node.TypeDef.Delete":
            case "SceneFlow.Node.TypeDef.Move":
            case "SceneFlow.Node.Command.Add":
            case "SceneFlow.Node.Command.Update":
            case "SceneFlow.Node.Command.Delete":
            case "SceneFlow.Node.Command.Move":
            case "Script.Update":
            case "Config.Update":
            case "ProjectConfig.Update":
            case "Preferences.Update": {
                JSONObject error = new JSONObject();
                error.put("error", "EDITING_NOT_SUPPORTED");
                error.put("message", "Editing not supported in runtime-only mode");
                return error;
            }

            // Pass-through for non-editing operations
            case "Project.Save":
            case "Project.SaveAs":
            case "Project.Close": {
                JSONObject ok = new JSONObject();
                ok.put("status", "ok");
                return ok;
            }

            default: {
                JSONObject error = new JSONObject();
                error.put("error", "UNKNOWN_COMMAND");
                error.put("message", "Unknown command: " + method);
                return error;
            }
        }
    }

    private JSONObject handleWsRuntimePlay(JSONObject params) {
        if (mRuntimeProject == null) {
            JSONObject err = new JSONObject();
            err.put("error", "NO_PROJECT");
            err.put("message", "No project loaded");
            return err;
        }

        boolean success = false;
        String newState = mRuntimeState;

        if (mRuntimeProject.isRunning()) {
            if (mRuntimeProject.isPaused()) {
                success = mRuntimeProject.proceed();
                newState = success ? "running" : "paused";
            } else {
                success = true;
                newState = "running";
            }
        } else {
            boolean launched = mRuntimeProject.launch();
            if (launched) {
                success = mRuntimeProject.start();
                newState = success ? "running" : "stopped";
            }
        }

        mRuntimeState = newState;
        broadcastRuntimeState();

        JSONObject result = new JSONObject();
        result.put("state", newState);
        if (mProjectId != null) {
            result.put("projectId", mProjectId);
        }
        return result;
    }

    private JSONObject handleWsRuntimePause(JSONObject params) {
        if (mRuntimeProject == null) {
            JSONObject err = new JSONObject();
            err.put("error", "NO_PROJECT");
            err.put("message", "No project loaded");
            return err;
        }

        String newState = mRuntimeState;
        if (mRuntimeProject.isRunning() && !mRuntimeProject.isPaused()) {
            boolean success = mRuntimeProject.pause();
            newState = success ? "paused" : "running";
        } else {
            newState = mRuntimeProject.isPaused() ? "paused" : (mRuntimeProject.isRunning() ? "running" : "stopped");
        }

        mRuntimeState = newState;
        broadcastRuntimeState();

        JSONObject result = new JSONObject();
        result.put("state", newState);
        if (mProjectId != null) {
            result.put("projectId", mProjectId);
        }
        return result;
    }

    private JSONObject handleWsRuntimeStop(JSONObject params) {
        if (mRuntimeProject == null) {
            JSONObject err = new JSONObject();
            err.put("error", "NO_PROJECT");
            err.put("message", "No project loaded");
            return err;
        }

        if (mRuntimeProject.isRunning()) {
            boolean success = mRuntimeProject.abort();
            if (success) {
                mRuntimeProject.unload();
            }
        }

        mRuntimeState = "stopped";
        broadcastRuntimeState();

        JSONObject result = new JSONObject();
        result.put("state", "stopped");
        if (mProjectId != null) {
            result.put("projectId", mProjectId);
        }
        return result;
    }

    private JSONObject handleWsVariableSet(JSONObject params) {
        if (mRuntimeProject == null) {
            JSONObject err = new JSONObject();
            err.put("error", "NO_PROJECT");
            err.put("message", "No project loaded");
            return err;
        }

        String name = params.optString("name", "");
        String value = params.optString("value", "");
        if (value.isEmpty()) {
            value = params.optString("valueExpr", "");
        }
        if (name.isEmpty()) {
            JSONObject err = new JSONObject();
            err.put("error", "BAD_REQUEST");
            err.put("message", "Missing variable name");
            return err;
        }
        if (value.isEmpty()) {
            JSONObject err = new JSONObject();
            err.put("error", "BAD_REQUEST");
            err.put("message", "Missing value");
            return err;
        }

        try {
            // Use string-based setVariable — RunTimeProject will handle type conversion
            boolean ok = mRuntimeProject.setVariable(name, value);
            if (ok) {
                JSONObject result = new JSONObject();
                result.put("status", "ok");
                result.put("name", name);
                result.put("value", value);
                return result;
            } else {
                JSONObject err = new JSONObject();
                err.put("error", "SET_FAILED");
                err.put("message", "Failed to set variable");
                return err;
            }
        } catch (Exception e) {
            JSONObject err = new JSONObject();
            err.put("error", "SET_FAILED");
            err.put("message", "Failed to set variable: " + e.getMessage());
            return err;
        }
    }

    private JSONObject handleWsRuntimeQuery(JSONObject params) {
        // Return current runtime state info
        JSONObject result = new JSONObject();
        result.put("state", mRuntimeState);
        if (mProjectId != null) {
            result.put("projectId", mProjectId);
        }
        if (mRuntimeProject != null) {
            result.put("isRunning", mRuntimeProject.isRunning());
            result.put("isPaused", mRuntimeProject.isPaused());
        }
        return result;
    }

    private JSONObject handleWsSceneFlowGet(JSONObject params) {
        if (mRuntimeProject == null || mRuntimeProject.getSceneFlow() == null) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            return empty;
        }

        SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", "");
        SuperNode target = SceneFlowSnapshotBuilder.resolveSuperNode(sceneFlow, superNodeId);
        if (target == null) {
            target = sceneFlow;
        }

        return SceneFlowSnapshotBuilder.createSnapshot(
                mProjectId != null ? mProjectId : "", target, sceneFlow, 90, 90, null);
    }

    // ========== Web UI Compatible REST Endpoints ==========

    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Runtime Server");
        info.put("mode", "runtime");
        info.put("port", mPort);
        info.put("tokenRequired", true);
        writeJson(ctx, info);
    }

    private void handleToken(Context ctx) {
        JSONObject token = new JSONObject();
        token.put("token", "dev-token");
        writeJson(ctx, token);
    }

    private void handleProjects(Context ctx) {
        JSONObject response = new JSONObject();
        JSONArray list = new JSONArray();

        if (mRuntimeProject != null && mProjectId != null) {
            JSONObject entry = new JSONObject();
            entry.put("projectId", mProjectId);
            entry.put("name", mRuntimeProject.getProjectName() != null ? mRuntimeProject.getProjectName() : "Runtime Project");
            entry.put("path", mProjectPath != null ? mProjectPath : "");
            entry.put("dirty", false);
            entry.put("pending", false);
            entry.put("runtimeState", mRuntimeState);
            list.put(entry);
        }

        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handleRecentProjects(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("projects", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleEmptyList(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("projects", new JSONArray());
        writeJson(ctx, response);
    }

    private void handlePreferences(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("preferences", new JSONObject());
        writeJson(ctx, response);
    }

    private void handleDevices(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("devices", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleProjectSceneflow(Context ctx) {
        String pid = ctx.pathParam("pid");
        String superNodeIdParam = ctx.queryParam("superNodeId");

        if (mRuntimeProject == null || mProjectId == null || !mProjectId.equals(pid)) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            empty.put("raw", "");
            writeJson(ctx, empty);
            return;
        }

        try {
            SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
            SuperNode targetSuperNode = SceneFlowSnapshotBuilder.resolveSuperNode(sceneFlow, superNodeIdParam);
            if (targetSuperNode == null) {
                targetSuperNode = sceneFlow;
            }

            JSONObject snapshot = SceneFlowSnapshotBuilder.createSnapshot(
                    pid, targetSuperNode, sceneFlow, 90, 90, null);
            writeJson(ctx, snapshot);
        } catch (Exception exc) {
            sLogger.warning("Cannot load sceneflow for pid=" + pid + ": " + exc.getMessage());
            JSONObject error = new JSONObject();
            error.put("nodes", new JSONArray());
            error.put("edges", new JSONArray());
            error.put("comments", new JSONArray());
            error.put("raw", "");
            writeJson(ctx, error);
        }
    }

    private void handleProjectRuntime(Context ctx) {
        String pid = ctx.pathParam("pid");
        JSONObject response = new JSONObject();
        response.put("state", mRuntimeState);

        if (mRuntimeProject != null && mProjectId != null && mProjectId.equals(pid)) {
            response.put("project", mProjectPath != null ? mProjectPath : "");
            SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
            if (sceneFlow != null) {
                // Build type map for typeFlavor resolution
                Map<String, DataTypeDefinition> typeMap = new HashMap<>();
                for (DataTypeDefinition def : sceneFlow.getTypeDefList()) {
                    typeMap.put(def.getName(), def);
                }

                // Global variables from root sceneflow
                JSONArray globals = new JSONArray();
                for (VariableDefinition def : sceneFlow.getVarDefList()) {
                    globals.put(variableToJson(def, typeMap));
                }
                response.put("globalVariables", globals);
                response.put("localVariables", new JSONArray());
            } else {
                response.put("globalVariables", new JSONArray());
                response.put("localVariables", new JSONArray());
            }
        } else {
            response.put("globalVariables", new JSONArray());
            response.put("localVariables", new JSONArray());
        }
        writeJson(ctx, response);
    }

    private void handleProjectEditorConfig(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("config", new JSONObject());
        writeJson(ctx, response);
    }

    private void handleProjectProjectConfig(Context ctx) {
        String pid = ctx.pathParam("pid");
        JSONObject response = new JSONObject();

        if (mRuntimeProject != null && mProjectId != null && mProjectId.equals(pid)
                && mRuntimeProject.getProjectConfig() != null) {
            response.put("config", projectConfigToJson(mRuntimeProject.getProjectConfig()));
        } else {
            JSONObject empty = new JSONObject();
            empty.put("plugins", new JSONArray());
            empty.put("agents", new JSONArray());
            empty.put("player", new JSONObject());
            response.put("config", empty);
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfigKeys(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("keys", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleProjectScript(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("script", "");
        response.put("version", 1);
        writeJson(ctx, response);
    }

    private void handleProjectScriptScenes(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("scenes", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleProjectScriptElements(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("acticon", new JSONArray());
        response.put("gesticon", new JSONArray());
        response.put("visicon", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleCommandLog(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("entries", new JSONArray());
        response.put("lastSeq", 0);
        response.put("count", 0);
        writeJson(ctx, response);
    }

    private void handleSceneflowNavigate(Context ctx) {
        String pid = ctx.pathParam("pid");
        JSONObject body = new JSONObject(ctx.body());
        String superNodeId = body.optString("superNodeId", "");

        if (mRuntimeProject == null || mProjectId == null || !mProjectId.equals(pid)) {
            JSONObject error = new JSONObject();
            error.put("status", "error");
            error.put("message", "Project not found");
            writeJson(ctx, error);
            return;
        }

        SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
        SuperNode target = SceneFlowSnapshotBuilder.resolveSuperNode(sceneFlow, superNodeId);
        if (target == null) {
            target = sceneFlow;
        }

        JSONObject snapshot = SceneFlowSnapshotBuilder.createSnapshot(
                pid, target, sceneFlow, 90, 90, null);
        writeJson(ctx, snapshot);
    }

    // ========== Legacy Runtime REST Endpoints ==========

    private void handleLoad(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("projectPath", "");

        if (path.isEmpty()) {
            ctx.status(400).result("Missing projectPath");
            return;
        }

        if (loadProject(path)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mRuntimeState);
            response.put("projectPath", mProjectPath);
            response.put("projectName", mRuntimeProject.getProjectName());
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to load project");
        }
    }

    private void handleStart(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (startRuntime()) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mRuntimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to start runtime");
        }
    }

    private void handlePause(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (mRuntimeProject.pause()) {
            mRuntimeState = "paused";
            broadcastRuntimeState();
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mRuntimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to pause runtime");
        }
    }

    private void handleResume(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (mRuntimeProject.proceed()) {
            mRuntimeState = "running";
            broadcastRuntimeState();
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mRuntimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to resume runtime");
        }
    }

    private void handleStopRest(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (mRuntimeProject.isRunning()) {
            mRuntimeProject.abort();
        }
        mRuntimeState = "stopped";
        broadcastRuntimeState();
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", mRuntimeState);
        writeJson(ctx, response);
    }

    private void handleUnload(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (mRuntimeProject.wasExecuted()) {
            mRuntimeProject.unload();
        }
        mRuntimeProject = null;
        mProjectPath = null;
        mProjectId = null;
        mRuntimeState = "stopped";
        broadcastRuntimeState();
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", mRuntimeState);
        writeJson(ctx, response);
    }

    private void handleStatus(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("state", mRuntimeState);
        if (mRuntimeProject != null) {
            response.put("projectPath", mProjectPath != null ? mProjectPath : "");
            response.put("projectName", mRuntimeProject.getProjectName());
            response.put("isRunning", mRuntimeProject.isRunning());
            response.put("isPaused", mRuntimeProject.isPaused());
        }
        writeJson(ctx, response);
    }

    private void handleVariables(Context ctx) {
        JSONObject response = new JSONObject();
        if (mRuntimeProject != null && mRuntimeProject.getSceneFlow() != null) {
            try {
                SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
                Map<String, DataTypeDefinition> typeMap = new HashMap<>();
                for (DataTypeDefinition def : sceneFlow.getTypeDefList()) {
                    typeMap.put(def.getName(), def);
                }
                JSONArray vars = new JSONArray();
                for (VariableDefinition def : sceneFlow.getVarDefList()) {
                    vars.put(variableToJson(def, typeMap));
                }
                response.put("variables", vars);
            } catch (Exception e) {
                response.put("variables", new JSONArray());
            }
        } else {
            response.put("variables", new JSONArray());
        }
        writeJson(ctx, response);
    }

    private void handleSceneflowLegacy(Context ctx) {
        if (mRuntimeProject == null || mRuntimeProject.getSceneFlow() == null) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            writeJson(ctx, empty);
            return;
        }

        SceneFlow sceneFlow = mRuntimeProject.getSceneFlow();
        JSONObject snapshot = SceneFlowSnapshotBuilder.createSnapshot(
                mProjectId != null ? mProjectId : "", sceneFlow, sceneFlow, 90, 90, null);
        writeJson(ctx, snapshot);
    }

    // ========== Event Broadcasting ==========

    private void broadcastRuntimeState() {
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        message.put("channel", "runtime");
        message.put("event", "runtime.state");
        JSONObject payload = new JSONObject();
        payload.put("state", mRuntimeState);
        payload.put("status", mRuntimeState);
        if (mProjectId != null) {
            payload.put("projectId", mProjectId);
        }
        message.put("payload", payload);
        broadcast(message.toString());
    }

    private void broadcast(String message) {
        for (WsContext session : mWsSessions) {
            try {
                if (session.session.isOpen()) {
                    session.send(message);
                }
            } catch (Exception e) {
                sLogger.warning("Failed to broadcast to WebSocket client: " + e.getMessage());
            }
        }
    }

    // ========== ProjectConfig Serialization ==========

    private JSONObject projectConfigToJson(ProjectConfig cfg) {
        JSONObject cfgJson = new JSONObject();
        cfgJson.put("name", cfg.getProjectName());
        cfgJson.put("path", mProjectPath != null ? mProjectPath : "");

        JSONArray pluginsJson = new JSONArray();
        Set<String> seenPlugins = new HashSet<>();
        for (PluginConfig plugin : cfg.getPluginConfigList()) {
            String pluginName = plugin.getPluginName() == null ? "" : plugin.getPluginName();
            String pluginKey = pluginName.trim().toLowerCase();
            if (!pluginKey.isEmpty() && !seenPlugins.add(pluginKey)) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("type", plugin.getPluginType());
            entry.put("name", plugin.getPluginName());
            entry.put("className", plugin.getClassName());
            entry.put("load", plugin.isMarkedtoLoad());
            entry.put("features", configFeaturesToJson(plugin.getEntryList()));
            pluginsJson.put(entry);
        }

        JSONArray agentsJson = new JSONArray();
        Set<String> seenAgents = new HashSet<>();
        for (AgentConfig agent : cfg.getAgentConfigList()) {
            String agentName = agent.getAgentName() == null ? "" : agent.getAgentName();
            String agentKey = agentName.trim().toLowerCase();
            if (!agentKey.isEmpty() && !seenAgents.add(agentKey)) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("name", agent.getAgentName());
            entry.put("device", agent.getDeviceName());
            entry.put("features", configFeaturesToJson(agent.getEntryList()));
            agentsJson.put(entry);
        }

        JSONObject playerJson = new JSONObject();
        PlayerConfig player = cfg.getPlayerConfig();
        playerJson.put("features", configFeaturesToJson(player != null ? player.getEntryList() : null));

        cfgJson.put("plugins", pluginsJson);
        cfgJson.put("agents", agentsJson);
        cfgJson.put("player", playerJson);
        return cfgJson;
    }

    private JSONArray configFeaturesToJson(List<ConfigFeature> features) {
        JSONArray list = new JSONArray();
        if (features == null) return list;
        for (ConfigFeature feature : features) {
            JSONObject entry = new JSONObject();
            entry.put("key", feature.getKey() == null ? "" : feature.getKey());
            entry.put("value", feature.getValue() == null ? "" : feature.getValue());
            list.put(entry);
        }
        return list;
    }

    // ========== Variable Serialization ==========

    private JSONObject variableToJson(VariableDefinition def, Map<String, DataTypeDefinition> typeMap) {
        JSONObject json = new JSONObject();
        json.put("name", def.getName());
        json.put("type", def.getType());
        json.put("typeFlavor", resolveTypeFlavor(def.getType(), typeMap));
        json.put("expr", def.getExp() != null ? def.getExp().getConcreteSyntax() : "");
        json.put("scope", "global");
        // Get actual runtime value
        String value = resolveVariableValue(def.getName());
        if (value != null) {
            json.put("value", value);
        }
        return json;
    }

    private String resolveVariableValue(String name) {
        if (mRuntimeProject == null || name == null || name.isBlank()) {
            return null;
        }
        try {
            AbstractValue value = mRuntimeProject.getValueOf(name);
            if (value == null) {
                return null;
            }
            String raw = value.getConcreteSyntax();
            // Remove internal type markers like #s# for string, #i# for int, etc.
            return raw != null ? raw.replaceAll("#[a-zA-Z]#", "") : null;
        } catch (Exception e) {
            return null;
        }
    }

    private String resolveTypeFlavor(String type, Map<String, DataTypeDefinition> typeMap) {
        if (type == null) {
            return "Primitive";
        }
        DataTypeDefinition def = typeMap.get(type);
        if (def != null && def.getFlavour() != null) {
            return def.getFlavour().name();
        }
        if ("Int".equalsIgnoreCase(type)
                || "Float".equalsIgnoreCase(type)
                || "Bool".equalsIgnoreCase(type)
                || "String".equalsIgnoreCase(type)) {
            return "Primitive";
        }
        return "Primitive";
    }

    // ========== Utilities ==========

    private String generateToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }

    public String getLocalUrl() {
        return "http://" + (mBindHost.equals("0.0.0.0") ? "127.0.0.1" : mBindHost) + ":" + mPort;
    }

    public String getAuthToken() {
        return mAuthToken;
    }

    public String getRuntimeState() {
        return mRuntimeState;
    }

    public RunTimeProject getRuntimeProject() {
        return mRuntimeProject;
    }

    public String getProjectId() {
        return mProjectId;
    }
}
