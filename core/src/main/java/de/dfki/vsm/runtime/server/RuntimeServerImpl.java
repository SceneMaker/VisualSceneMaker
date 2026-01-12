package de.dfki.vsm.runtime.server;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.http.Context;
import io.javalin.http.UnauthorizedResponse;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Headless Runtime Server for SceneMaker.
 * Provides REST API and WebSocket support for runtime control and monitoring.
 *
 * This server is designed to run standalone on Android or Desktop Java environments,
 * allowing remote editors to connect and control runtime execution.
 *
 * @author Phase 3 Refactoring - 2026-01-12
 */
public class RuntimeServerImpl {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";

    private Javalin mApp;
    private String mAuthToken;
    private int mPort;
    private String mBindHost;

    // Runtime project management
    private RunTimeProject mRuntimeProject;
    private String mProjectPath;
    private RuntimeState mState = RuntimeState.STOPPED;

    // WebSocket sessions for event broadcasting
    private final Set<WsContext> mWsSessions = ConcurrentHashMap.newKeySet();

    /**
     * Runtime execution states.
     */
    public enum RuntimeState {
        STOPPED,    // Not loaded or stopped
        LOADED,     // Project loaded but not running
        RUNNING,    // Executing
        PAUSED      // Paused
    }

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
            // Serve minimal runtime monitoring UI (if available)
            config.addStaticFiles("/runtime-ui", Location.CLASSPATH);
        }).start(mBindHost, mPort);

        registerRoutes();

        sLogger.message("Runtime server started on " + mBindHost + ":" + mPort);
        sLogger.message("Auth token: " + mAuthToken);
    }

    /**
     * Stops the runtime server.
     */
    public void stop() {
        if (mApp != null) {
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
     * Registers REST and WebSocket routes.
     */
    private void registerRoutes() {
        // Public endpoint (no auth required)
        mApp.get(API_PREFIX + "/info", this::handleInfo);

        // All other endpoints require authentication
        mApp.before(API_PREFIX + "/*", this::authenticate);

        // Runtime control endpoints
        mApp.post(API_PREFIX + "/runtime/load", this::handleLoad);
        mApp.post(API_PREFIX + "/runtime/start", this::handleStart);
        mApp.post(API_PREFIX + "/runtime/pause", this::handlePause);
        mApp.post(API_PREFIX + "/runtime/resume", this::handleResume);
        mApp.post(API_PREFIX + "/runtime/stop", this::handleStop);
        mApp.post(API_PREFIX + "/runtime/unload", this::handleUnload);

        // Status and monitoring endpoints
        mApp.get(API_PREFIX + "/runtime/status", this::handleStatus);
        mApp.get(API_PREFIX + "/runtime/variables", this::handleVariables);
        mApp.get(API_PREFIX + "/runtime/sceneflow", this::handleSceneflow);

        // WebSocket endpoint for real-time event broadcasting
        mApp.ws("/ws", ws -> {
            ws.onConnect(this::handleWsConnect);
            ws.onClose(ctx -> mWsSessions.remove(ctx));
            ws.onError(ctx -> mWsSessions.remove(ctx));
            ws.onMessage(ctx -> handleWsMessage(ctx.message(), ctx::send));
        });
    }

    /**
     * Authentication middleware - validates token.
     */
    private void authenticate(Context ctx) {
        // Check Authorization header: "Bearer <token>"
        String authHeader = ctx.header("Authorization");
        String token = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            token = authHeader.substring(7);
        }

        // Fallback: check query parameter ?token=<token>
        if (token == null) {
            token = ctx.queryParam("token");
        }

        if (token == null || !mAuthToken.equals(token)) {
            throw new UnauthorizedResponse("Invalid or missing token");
        }
    }

    // ========== REST Endpoint Handlers ==========

    /**
     * GET /api/v1/info - Server information (no auth required)
     */
    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Runtime Server");
        info.put("mode", "runtime");
        info.put("version", "1.0.0");
        info.put("port", mPort);
        info.put("bindHost", mBindHost);
        info.put("tokenRequired", true);
        writeJson(ctx, info);
    }

    /**
     * POST /api/v1/runtime/load - Load project from filesystem
     * Body: { "projectPath": "/path/to/project" }
     */
    private void handleLoad(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("projectPath", "");

        if (path.isEmpty()) {
            ctx.status(400).result("Missing projectPath");
            return;
        }

        // Unload existing project if any
        if (mRuntimeProject != null && mRuntimeProject.wasExecuted()) {
            mRuntimeProject.unload();
        }

        // Load new project
        try {
            mRuntimeProject = new RunTimeProject(new File(path));
            if (mRuntimeProject.parse(path)) {
                mProjectPath = path;

                // Launch runtime (initialize plugins, etc.)
                if (mRuntimeProject.launch()) {
                    mState = RuntimeState.LOADED;

                    // Broadcast runtime state change
                    broadcastRuntimeState();

                    JSONObject response = new JSONObject();
                    response.put("status", "ok");
                    response.put("state", mState.toString().toLowerCase());
                    response.put("projectPath", mProjectPath);
                    response.put("projectName", mRuntimeProject.getProjectName());
                    writeJson(ctx, response);

                    sLogger.message("Loaded project: " + path);
                } else {
                    mRuntimeProject = null;
                    mProjectPath = null;
                    ctx.status(500).result("Failed to launch runtime");
                }
            } else {
                mRuntimeProject = null;
                mProjectPath = null;
                ctx.status(500).result("Failed to parse project");
            }
        } catch (Exception e) {
            sLogger.failure("Error loading project: " + e.getMessage());
            ctx.status(500).result("Error: " + e.getMessage());
        }
    }

    /**
     * POST /api/v1/runtime/start - Start runtime execution
     */
    private void handleStart(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }

        if (mState == RuntimeState.RUNNING) {
            ctx.status(400).result("Already running");
            return;
        }

        if (mRuntimeProject.start()) {
            mState = RuntimeState.RUNNING;
            broadcastRuntimeState();

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mState.toString().toLowerCase());
            writeJson(ctx, response);

            sLogger.message("Runtime started");
        } else {
            ctx.status(500).result("Failed to start runtime");
        }
    }

    /**
     * POST /api/v1/runtime/pause - Pause runtime execution
     */
    private void handlePause(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }

        if (mState != RuntimeState.RUNNING) {
            ctx.status(400).result("Not running");
            return;
        }

        if (mRuntimeProject.pause()) {
            mState = RuntimeState.PAUSED;
            broadcastRuntimeState();

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mState.toString().toLowerCase());
            writeJson(ctx, response);

            sLogger.message("Runtime paused");
        } else {
            ctx.status(500).result("Failed to pause runtime");
        }
    }

    /**
     * POST /api/v1/runtime/resume - Resume paused runtime execution
     */
    private void handleResume(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }

        if (mState != RuntimeState.PAUSED) {
            ctx.status(400).result("Not paused");
            return;
        }

        if (mRuntimeProject.proceed()) {
            mState = RuntimeState.RUNNING;
            broadcastRuntimeState();

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mState.toString().toLowerCase());
            writeJson(ctx, response);

            sLogger.message("Runtime resumed");
        } else {
            ctx.status(500).result("Failed to resume runtime");
        }
    }

    /**
     * POST /api/v1/runtime/stop - Stop runtime execution
     */
    private void handleStop(Context ctx) {
        if (mRuntimeProject == null) {
            ctx.status(400).result("No project loaded");
            return;
        }

        if (mState == RuntimeState.STOPPED || mState == RuntimeState.LOADED) {
            ctx.status(400).result("Not running");
            return;
        }

        if (mRuntimeProject.abort()) {
            mState = RuntimeState.LOADED;
            broadcastRuntimeState();

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", mState.toString().toLowerCase());
            writeJson(ctx, response);

            sLogger.message("Runtime stopped");
        } else {
            ctx.status(500).result("Failed to stop runtime");
        }
    }

    /**
     * POST /api/v1/runtime/unload - Unload current project
     */
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
        mState = RuntimeState.STOPPED;
        broadcastRuntimeState();

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", mState.toString().toLowerCase());
        writeJson(ctx, response);

        sLogger.message("Runtime unloaded");
    }

    /**
     * GET /api/v1/runtime/status - Get current runtime status
     */
    private void handleStatus(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("state", mState.toString().toLowerCase());

        if (mRuntimeProject != null) {
            response.put("projectPath", mProjectPath != null ? mProjectPath : "");
            response.put("projectName", mRuntimeProject.getProjectName());
            response.put("isRunning", mRuntimeProject.isRunning());
            response.put("isPaused", mRuntimeProject.isPaused());
            response.put("wasExecuted", mRuntimeProject.wasExecuted());
        } else {
            response.put("projectPath", "");
            response.put("projectName", "");
            response.put("isRunning", false);
            response.put("isPaused", false);
            response.put("wasExecuted", false);
        }

        writeJson(ctx, response);
    }

    /**
     * GET /api/v1/runtime/variables - Get runtime variables
     */
    private void handleVariables(Context ctx) {
        JSONObject response = new JSONObject();

        if (mRuntimeProject != null && mRuntimeProject.getSceneFlow() != null) {
            try {
                JSONArray vars = serializeVariables();
                response.put("variables", vars);
            } catch (Exception e) {
                sLogger.warning("Error serializing variables: " + e.getMessage());
                response.put("variables", new JSONArray());
            }
        } else {
            response.put("variables", new JSONArray());
        }

        writeJson(ctx, response);
    }

    /**
     * GET /api/v1/runtime/sceneflow - Get sceneflow structure (read-only)
     */
    private void handleSceneflow(Context ctx) {
        JSONObject response = new JSONObject();

        if (mRuntimeProject != null && mRuntimeProject.getSceneFlow() != null) {
            try {
                List<JSONObject> nodes = serializeNodes();
                List<JSONObject> edges = serializeEdges();

                response.put("nodes", new JSONArray(nodes));
                response.put("edges", new JSONArray(edges));
            } catch (Exception e) {
                sLogger.warning("Error serializing sceneflow: " + e.getMessage());
                response.put("nodes", new JSONArray());
                response.put("edges", new JSONArray());
            }
        } else {
            response.put("nodes", new JSONArray());
            response.put("edges", new JSONArray());
        }

        writeJson(ctx, response);
    }

    // ========== WebSocket Handlers ==========

    /**
     * WebSocket connection handler - authenticate and register session.
     */
    private void handleWsConnect(WsContext ctx) {
        // Check token in query parameter
        String token = ctx.queryParam("token");

        if (token == null || !mAuthToken.equals(token)) {
            ctx.send("{\"error\":\"Unauthorized\"}");
            ctx.session.close();
            return;
        }

        mWsSessions.add(ctx);

        // Send current runtime state to new client
        sendRuntimeState(ctx);

        sLogger.message("WebSocket client connected");
    }

    /**
     * WebSocket message handler - handle incoming messages.
     */
    private void handleWsMessage(String message, java.util.function.Consumer<String> sender) {
        try {
            JSONObject msg = new JSONObject(message);
            String method = msg.optString("method", "");

            // Currently, runtime server only broadcasts events
            // Clients should use REST API for control operations
            JSONObject response = new JSONObject();
            response.put("error", "Use REST API for runtime control");
            sender.accept(response.toString());
        } catch (Exception e) {
            sLogger.warning("Error handling WebSocket message: " + e.getMessage());
        }
    }

    // ========== Event Broadcasting ==========

    /**
     * Broadcasts runtime state change to all connected WebSocket clients.
     */
    private void broadcastRuntimeState() {
        JSONObject event = new JSONObject();
        event.put("event", "runtime.state");
        event.put("state", mState.toString().toLowerCase());
        event.put("timestamp", System.currentTimeMillis());

        if (mRuntimeProject != null) {
            event.put("projectPath", mProjectPath != null ? mProjectPath : "");
            event.put("projectName", mRuntimeProject.getProjectName());
        }

        broadcast(event.toString());
    }

    /**
     * Sends runtime state to a specific client.
     */
    private void sendRuntimeState(WsContext ctx) {
        JSONObject event = new JSONObject();
        event.put("event", "runtime.state");
        event.put("state", mState.toString().toLowerCase());
        event.put("timestamp", System.currentTimeMillis());

        if (mRuntimeProject != null) {
            event.put("projectPath", mProjectPath != null ? mProjectPath : "");
            event.put("projectName", mRuntimeProject.getProjectName());
        }

        ctx.send(event.toString());
    }

    /**
     * Broadcasts a message to all connected WebSocket clients.
     */
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

    // ========== Serialization Helpers ==========

    /**
     * Serializes runtime variables to JSON.
     */
    private JSONArray serializeVariables() {
        if (mRuntimeProject == null || mRuntimeProject.getSceneFlow() == null) {
            return new JSONArray();
        }

        JSONArray arr = new JSONArray();
        try {
            List<?> vars = mRuntimeProject.getVarDefInSceneFlow();
            if (vars != null) {
                for (Object v : vars) {
                    JSONObject obj = new JSONObject();
                    try {
                        de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition vd =
                            (de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition) v;
                        obj.put("name", vd.getName());
                        obj.put("type", vd.getType());
                        // TODO: Get actual runtime value from RunTimePlayer
                        obj.put("value", "");
                    } catch (Exception e) {
                        obj.put("name", "");
                        obj.put("type", "");
                        obj.put("value", "");
                    }
                    arr.put(obj);
                }
            }
        } catch (Exception e) {
            sLogger.warning("Error serializing variables: " + e.getMessage());
        }
        return arr;
    }

    /**
     * Serializes sceneflow nodes to JSON.
     */
    private List<JSONObject> serializeNodes() {
        if (mRuntimeProject == null || mRuntimeProject.getSceneFlow() == null) {
            return new ArrayList<>();
        }

        List<JSONObject> nodes = new ArrayList<>();
        collectNodes(mRuntimeProject.getSceneFlow(), nodes, true, null);
        nodes.sort(Comparator.comparing(o -> o.optString("id", "")));
        return nodes;
    }

    /**
     * Recursively collects nodes from sceneflow hierarchy.
     */
    private void collectNodes(SuperNode superNode, List<JSONObject> out, boolean isRoot, String parentId) {
        if (superNode == null) {
            return;
        }

        // Add the supernode itself
        JSONObject obj = new JSONObject();
        obj.put("id", superNode.getId());
        obj.put("name", superNode.getName());
        obj.put("type", "Super");
        obj.put("isSuper", true);
        obj.put("isRoot", isRoot);

        if (parentId != null && !parentId.isEmpty()) {
            obj.put("parentId", parentId);
        }

        JSONObject pos = new JSONObject();
        pos.put("x", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null
            ? superNode.getGraphics().getPosition().getXPos() : 0);
        pos.put("y", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null
            ? superNode.getGraphics().getPosition().getYPos() : 0);
        obj.put("position", pos);

        out.add(obj);

        // Add basic children
        if (superNode.getNodeList() != null) {
            superNode.getNodeList().stream()
                .sorted(Comparator.comparing(BasicNode::getId))
                .forEach(n -> {
                    JSONObject child = new JSONObject();
                    child.put("id", n.getId());
                    child.put("name", n.getName());
                    child.put("type", "Basic");
                    child.put("isSuper", false);
                    child.put("parentId", superNode.getId());

                    JSONObject cpos = new JSONObject();
                    cpos.put("x", n.getGraphics() != null && n.getGraphics().getPosition() != null
                        ? n.getGraphics().getPosition().getXPos() : 0);
                    cpos.put("y", n.getGraphics() != null && n.getGraphics().getPosition() != null
                        ? n.getGraphics().getPosition().getYPos() : 0);
                    child.put("position", cpos);

                    out.add(child);
                });
        }

        // Add super children recursively
        if (superNode.getSuperNodeList() != null) {
            superNode.getSuperNodeList().stream()
                .sorted(Comparator.comparing(BasicNode::getId))
                .forEach(sn -> collectNodes(sn, out, false, superNode.getId()));
        }
    }

    /**
     * Serializes sceneflow edges to JSON.
     */
    private List<JSONObject> serializeEdges() {
        if (mRuntimeProject == null || mRuntimeProject.getSceneFlow() == null) {
            return new ArrayList<>();
        }

        List<AbstractEdge> edges = new ArrayList<>();
        collectEdges(mRuntimeProject.getSceneFlow(), edges);

        return edges.stream()
            .sorted(Comparator
                .comparing(AbstractEdge::getSourceUnid)
                .thenComparing(AbstractEdge::getTargetUnid)
                .thenComparing(e -> e.getClass().getSimpleName()))
            .map(e -> {
                JSONObject obj = new JSONObject();
                String edgeId = e.getSourceUnid() + "_" + e.getTargetUnid() + "_" + e.getClass().getSimpleName();
                obj.put("id", edgeId);
                obj.put("sourceId", e.getSourceUnid());
                obj.put("targetId", e.getTargetUnid());
                obj.put("type", mapEdgeType(e));
                obj.put("label", edgeLabel(e));

                EdgeGraphics g = e.getGraphics();
                if (g != null && g.getConnection() != null) {
                    JSONArray points = new JSONArray();
                    g.getConnection().getPointList().forEach(p -> {
                        JSONObject pt = new JSONObject();
                        pt.put("x", p.getCtrlXPos());
                        pt.put("y", p.getCtrlYPos());
                        points.put(pt);
                    });
                    obj.put("points", points);
                } else {
                    obj.put("points", new JSONArray());
                }

                return obj;
            }).collect(Collectors.toList());
    }

    /**
     * Recursively collects edges from sceneflow hierarchy.
     */
    private void collectEdges(SuperNode superNode, List<AbstractEdge> out) {
        if (superNode == null) return;

        if (superNode.getNodeList() != null) {
            for (BasicNode n : superNode.getNodeList()) {
                if (n.getEdgeList() != null) {
                    out.addAll(n.getEdgeList());
                }
            }
        }

        if (superNode.getSuperNodeList() != null) {
            for (SuperNode sn : superNode.getSuperNodeList()) {
                collectEdges(sn, out);
            }
        }
    }

    /**
     * Maps edge class to type string.
     */
    private String mapEdgeType(AbstractEdge edge) {
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge) {
            return "EEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) {
            return "CEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) {
            return "PEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) {
            return "TEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) {
            return "IEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge) {
            return "FEDGE";
        }
        return "EDGE";
    }

    /**
     * Generates label text for an edge.
     */
    private String edgeLabel(AbstractEdge edge) {
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) {
            return ((de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) edge).getCondition().toString();
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) {
            return Double.toString(((de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) edge).getProbability());
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) {
            return Long.toString(((de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) edge).getTimeout());
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) {
            return ((de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) edge).getCondition().toString();
        }
        return "";
    }

    // ========== Utilities ==========

    /**
     * Generates a random authentication token.
     */
    private String generateToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    /**
     * Writes JSON response to context.
     */
    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }

    /**
     * Gets the local URL for this server.
     */
    public String getLocalUrl() {
        return "http://" + (mBindHost.equals("0.0.0.0") ? "127.0.0.1" : mBindHost) + ":" + mPort;
    }

    /**
     * Gets the authentication token.
     */
    public String getAuthToken() {
        return mAuthToken;
    }

    /**
     * Gets the current runtime state.
     */
    public RuntimeState getState() {
        return mState;
    }

    /**
     * Gets the current runtime project.
     */
    public RunTimeProject getRuntimeProject() {
        return mRuntimeProject;
    }
}
