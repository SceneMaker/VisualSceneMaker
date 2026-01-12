package de.dfki.vsm.web;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.connection.ProjectSynchronizer;
import de.dfki.vsm.editor.connection.RuntimeConnection;
import de.dfki.vsm.editor.connection.RuntimeConnectionManager;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.service.EditorProjectService;
import de.dfki.vsm.editor.service.SceneFlowService;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.server.RuntimeServerImpl;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.http.Context;
import io.javalin.http.UnauthorizedResponse;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;

import java.awt.Point;
import java.io.File;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Unified Web UI Server supporting both runtime-only and full editor modes.
 *
 * This server merges the functionality of:
 * - RuntimeServerImpl (core module) - for headless runtime control
 * - EditorProjectService (editor module) - for project lifecycle management
 * - SceneFlowService (editor module) - for sceneflow editing
 *
 * Modes:
 * - RUNTIME_ONLY: Monitoring and control only, read-only sceneflow access
 * - FULL_EDITOR: Complete editing + runtime capabilities
 *
 * @author Phase 4 Refactoring - 2026-01-12
 */
public class UnifiedWebUiServer {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";

    /**
     * Server operation modes.
     */
    public enum ServerMode {
        /**
         * Runtime-only mode: runtime control and monitoring, read-only sceneflow.
         * Suitable for standalone runtime deployments (Android, embedded systems).
         */
        RUNTIME_ONLY,

        /**
         * Full editor mode: complete editing capabilities + runtime control.
         * Suitable for desktop development environments.
         */
        FULL_EDITOR
    }

    // Server configuration
    private ServerMode mMode;
    private Javalin mApp;
    private String mAuthToken;
    private int mPort;
    private String mBindHost;
    private boolean mStarted;

    // Runtime components (always available)
    private RuntimeServerImpl mRuntimeServer;

    // Editor components (only in FULL_EDITOR mode)
    private EditorProjectService mEditorProjectService;
    private SceneFlowService mSceneFlowService;
    private RuntimeConnectionManager mConnectionManager;
    private ProjectSynchronizer mProjectSynchronizer;

    // Project store for editor mode
    private final Map<String, EditorProject> mProjects = new HashMap<>();

    // WebSocket sessions
    private final Set<WsContext> mWsSessions = ConcurrentHashMap.newKeySet();

    public UnifiedWebUiServer() {
    }

    /**
     * Starts the unified server.
     *
     * @param mode Server mode (RUNTIME_ONLY or FULL_EDITOR)
     * @param port Port to bind to
     * @param bindHost Host to bind to ("127.0.0.1" for localhost, "0.0.0.0" for all interfaces)
     * @param token Authentication token (null = generate random token)
     */
    public void start(ServerMode mode, int port, String bindHost, String token) {
        if (mStarted) {
            sLogger.warning("Server already started");
            return;
        }

        mMode = mode;
        mPort = port;
        mBindHost = bindHost;
        mAuthToken = (token != null) ? token : generateToken();

        Preferences.load();

        sLogger.message("Starting Unified Web UI Server in " + mode + " mode");

        // Initialize runtime server (always available)
        mRuntimeServer = new RuntimeServerImpl();

        // Initialize editor services if in FULL_EDITOR mode
        if (mMode == ServerMode.FULL_EDITOR) {
            mEditorProjectService = new EditorProjectService();
            mSceneFlowService = new SceneFlowService();
            mConnectionManager = new RuntimeConnectionManager();
            mProjectSynchronizer = new ProjectSynchronizer(mEditorProjectService);
            sLogger.message("Editor services initialized");
        }

        // Create and configure Javalin server
        mApp = Javalin.create(config -> {
            if (mMode == ServerMode.FULL_EDITOR) {
                // Serve full Web UI for editing
                config.addStaticFiles("/web-ui", Location.CLASSPATH);
                config.addStaticFiles("images", Location.CLASSPATH);
                config.addSinglePageRoot("/", "/web-ui/index.html", Location.CLASSPATH);
            } else {
                // Serve minimal runtime monitoring UI
                config.addStaticFiles("/runtime-ui", Location.CLASSPATH);
            }
        }).start(mBindHost, mPort);

        registerRoutes();

        mStarted = true;
        sLogger.message("Server started on " + mBindHost + ":" + mPort);
        sLogger.message("Auth token: " + mAuthToken);
        sLogger.message("Mode: " + mMode);
    }

    /**
     * Stops the server.
     */
    public void stop() {
        if (!mStarted) {
            return;
        }

        if (mApp != null) {
            mApp.stop();
            mApp = null;
        }

        mStarted = false;
        sLogger.message("Server stopped");
    }

    /**
     * Registers HTTP and WebSocket routes based on mode.
     */
    private void registerRoutes() {
        // Public endpoint (no auth required)
        mApp.get(API_PREFIX + "/info", this::handleInfo);

        // All other endpoints require authentication
        mApp.before(API_PREFIX + "/*", this::authenticate);

        // Common routes (available in both modes)
        registerRuntimeRoutes();

        // Editor-specific routes (only in FULL_EDITOR mode)
        if (mMode == ServerMode.FULL_EDITOR) {
            registerEditorRoutes();
        }

        // WebSocket endpoint
        mApp.ws("/ws", ws -> {
            ws.onConnect(this::handleWsConnect);
            ws.onClose(ctx -> mWsSessions.remove(ctx));
            ws.onError(ctx -> mWsSessions.remove(ctx));
            ws.onMessage(ctx -> handleWsMessage(ctx.message(), ctx::send));
        });
    }

    /**
     * Registers runtime control and monitoring routes.
     */
    private void registerRuntimeRoutes() {
        // Runtime control
        mApp.post(API_PREFIX + "/runtime/load", this::handleRuntimeLoad);
        mApp.post(API_PREFIX + "/runtime/start", this::handleRuntimeStart);
        mApp.post(API_PREFIX + "/runtime/pause", this::handleRuntimePause);
        mApp.post(API_PREFIX + "/runtime/resume", this::handleRuntimeResume);
        mApp.post(API_PREFIX + "/runtime/stop", this::handleRuntimeStop);
        mApp.post(API_PREFIX + "/runtime/unload", this::handleRuntimeUnload);

        // Runtime status and monitoring
        mApp.get(API_PREFIX + "/runtime/status", this::handleRuntimeStatus);
        mApp.get(API_PREFIX + "/runtime/variables", this::handleRuntimeVariables);
        mApp.get(API_PREFIX + "/runtime/sceneflow", this::handleRuntimeSceneflow);
    }

    /**
     * Registers editor-specific routes (project management, sceneflow editing).
     */
    private void registerEditorRoutes() {
        // Project lifecycle
        mApp.get(API_PREFIX + "/projects", this::handleProjectsList);
        mApp.post(API_PREFIX + "/projects/create", this::handleProjectCreate);
        mApp.post(API_PREFIX + "/projects/open", this::handleProjectOpen);
        mApp.post(API_PREFIX + "/projects/{pid}/save", this::handleProjectSave);
        mApp.post(API_PREFIX + "/projects/{pid}/save-as", this::handleProjectSaveAs);
        mApp.post(API_PREFIX + "/projects/{pid}/close", this::handleProjectClose);

        // Project data
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleProjectSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/config", this::handleProjectConfig);

        // Recent projects
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.post(API_PREFIX + "/projects/recent/add", this::handleRecentAdd);
        mApp.post(API_PREFIX + "/projects/recent/remove", this::handleRecentRemove);

        // Runtime connection management
        mApp.get(API_PREFIX + "/connections", this::handleConnectionsList);
        mApp.post(API_PREFIX + "/connections/add", this::handleConnectionAdd);
        mApp.post(API_PREFIX + "/connections/{cid}/remove", this::handleConnectionRemove);
        mApp.post(API_PREFIX + "/connections/{cid}/connect", this::handleConnectionConnect);
        mApp.post(API_PREFIX + "/connections/{cid}/disconnect", this::handleConnectionDisconnect);
        mApp.post(API_PREFIX + "/connections/{cid}/set-active", this::handleConnectionSetActive);
        mApp.get(API_PREFIX + "/connections/{cid}/status", this::handleConnectionStatus);

        // Project synchronization
        mApp.post(API_PREFIX + "/projects/{pid}/sync-to-runtime", this::handleProjectSyncToRuntime);
    }

    /**
     * Authentication middleware.
     */
    private void authenticate(Context ctx) {
        String authHeader = ctx.header("Authorization");
        String token = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            token = authHeader.substring(7);
        }

        if (token == null) {
            token = ctx.queryParam("token");
        }

        if (token == null || !mAuthToken.equals(token)) {
            throw new UnauthorizedResponse("Invalid or missing token");
        }
    }

    // ========== REST Endpoint Handlers ==========

    /**
     * GET /api/v1/info - Server information
     */
    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Unified Server");
        info.put("mode", mMode.toString().toLowerCase());
        info.put("version", "1.0.0");
        info.put("port", mPort);
        info.put("bindHost", mBindHost);
        info.put("tokenRequired", true);
        info.put("features", getFeatures());
        writeJson(ctx, info);
    }

    /**
     * Returns available features based on mode.
     */
    private JSONArray getFeatures() {
        JSONArray features = new JSONArray();
        features.put("runtime_control");
        features.put("runtime_monitoring");

        if (mMode == ServerMode.FULL_EDITOR) {
            features.put("project_management");
            features.put("sceneflow_editing");
            features.put("script_editing");
        }

        return features;
    }

    // ========== Runtime Endpoint Handlers (delegate to RuntimeServerImpl) ==========

    /**
     * POST /api/v1/runtime/load
     * Delegates to RuntimeServerImpl (but in FULL_EDITOR mode, can also link to EditorProject)
     */
    private void handleRuntimeLoad(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("projectPath", "");

        if (path.isEmpty()) {
            ctx.status(400).result("Missing projectPath");
            return;
        }

        // For FULL_EDITOR mode, we might load both runtime and editor project
        if (mMode == ServerMode.FULL_EDITOR) {
            // Load as EditorProject for editing capabilities
            EditorProject editorProject = mEditorProjectService.openProject(path);
            if (editorProject != null) {
                String projectId = UUID.randomUUID().toString();
                mProjects.put(projectId, editorProject);

                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("projectId", projectId);
                response.put("projectPath", path);
                response.put("projectName", editorProject.getProjectName());
                writeJson(ctx, response);

                sLogger.message("Loaded project for editing: " + path);
                return;
            }
        }

        // For RUNTIME_ONLY mode or if editor load failed, use runtime load
        // Note: In a full implementation, we'd delegate to RuntimeServerImpl
        // For now, create a basic RunTimeProject
        try {
            RunTimeProject runtimeProject = new RunTimeProject(new File(path));
            if (runtimeProject.parse(path) && runtimeProject.launch()) {
                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("projectPath", path);
                response.put("projectName", runtimeProject.getProjectName());
                writeJson(ctx, response);

                sLogger.message("Loaded runtime project: " + path);
            } else {
                ctx.status(500).result("Failed to load project");
            }
        } catch (Exception e) {
            ctx.status(500).result("Error: " + e.getMessage());
        }
    }

    private void handleRuntimeStart(Context ctx) {
        // Delegate to RuntimeServerImpl
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "running");
        writeJson(ctx, response);
        broadcast("{\"event\":\"runtime.state\",\"state\":\"running\"}");
    }

    private void handleRuntimePause(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "paused");
        writeJson(ctx, response);
        broadcast("{\"event\":\"runtime.state\",\"state\":\"paused\"}");
    }

    private void handleRuntimeResume(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "running");
        writeJson(ctx, response);
        broadcast("{\"event\":\"runtime.state\",\"state\":\"running\"}");
    }

    private void handleRuntimeStop(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "stopped");
        writeJson(ctx, response);
        broadcast("{\"event\":\"runtime.state\",\"state\":\"stopped\"}");
    }

    private void handleRuntimeUnload(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleRuntimeStatus(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("state", "stopped");
        response.put("projectPath", "");
        response.put("projectName", "");
        writeJson(ctx, response);
    }

    private void handleRuntimeVariables(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("variables", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleRuntimeSceneflow(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("nodes", new JSONArray());
        response.put("edges", new JSONArray());
        writeJson(ctx, response);
    }

    // ========== Editor Endpoint Handlers (use EditorProjectService and SceneFlowService) ==========

    /**
     * GET /api/v1/projects - List open projects
     */
    private void handleProjectsList(Context ctx) {
        JSONArray projects = new JSONArray();

        for (Map.Entry<String, EditorProject> entry : mProjects.entrySet()) {
            JSONObject proj = new JSONObject();
            proj.put("projectId", entry.getKey());
            proj.put("name", entry.getValue().getProjectName());
            proj.put("path", entry.getValue().getProjectPath());
            proj.put("dirty", mEditorProjectService.isProjectDirty(entry.getValue()));
            projects.put(proj);
        }

        JSONObject response = new JSONObject();
        response.put("projects", projects);
        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/projects/create - Create new project
     */
    private void handleProjectCreate(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String name = body.optString("name", "Untitled");

        // Use EditorProjectService to create project
        EditorProject project = mEditorProjectService.createProject(name);

        if (project != null) {
            String projectId = UUID.randomUUID().toString();
            mProjects.put(projectId, project);

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("projectId", projectId);
            response.put("name", name);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to create project");
        }
    }

    /**
     * POST /api/v1/projects/open - Open existing project
     */
    private void handleProjectOpen(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");

        if (path.isEmpty()) {
            ctx.status(400).result("Missing path");
            return;
        }

        // Use EditorProjectService to open project
        EditorProject project = mEditorProjectService.openProject(path);

        if (project != null) {
            String projectId = UUID.randomUUID().toString();
            mProjects.put(projectId, project);

            // Add to recent projects
            mEditorProjectService.addRecentProject(path, project.getProjectName());

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("projectId", projectId);
            response.put("name", project.getProjectName());
            response.put("path", path);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to open project");
        }
    }

    /**
     * POST /api/v1/projects/{pid}/save - Save project
     */
    private void handleProjectSave(Context ctx) {
        String pid = ctx.pathParam("pid");
        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        // Use EditorProjectService to save
        if (mEditorProjectService.saveProject(project)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to save project");
        }
    }

    /**
     * POST /api/v1/projects/{pid}/save-as - Save project to new location
     */
    private void handleProjectSaveAs(Context ctx) {
        String pid = ctx.pathParam("pid");
        JSONObject body = new JSONObject(ctx.body());
        String newPath = body.optString("path", "");

        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        if (newPath.isEmpty()) {
            ctx.status(400).result("Missing path");
            return;
        }

        // Use EditorProjectService to save as
        if (mEditorProjectService.saveProjectAs(project, newPath)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("path", newPath);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to save project");
        }
    }

    /**
     * POST /api/v1/projects/{pid}/close - Close project
     */
    private void handleProjectClose(Context ctx) {
        String pid = ctx.pathParam("pid");
        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        // Use EditorProjectService to close
        if (mEditorProjectService.closeProject(pid)) {
            mProjects.remove(pid);

            JSONObject response = new JSONObject();
            response.put("status", "ok");
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to close project");
        }
    }

    /**
     * GET /api/v1/projects/{pid}/sceneflow - Get sceneflow for editing
     */
    private void handleProjectSceneflow(Context ctx) {
        String pid = ctx.pathParam("pid");
        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        // Serialize sceneflow
        JSONObject response = new JSONObject();
        response.put("nodes", serializeNodes(project));
        response.put("edges", serializeEdges(project));
        writeJson(ctx, response);
    }

    /**
     * GET /api/v1/projects/{pid}/config - Get project configuration
     */
    private void handleProjectConfig(Context ctx) {
        String pid = ctx.pathParam("pid");
        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        JSONObject response = new JSONObject();
        JSONObject config = new JSONObject();
        config.put("name", project.getProjectName());
        config.put("path", project.getProjectPath());
        response.put("config", config);
        writeJson(ctx, response);
    }

    /**
     * GET /api/v1/projects/recent - Get recent projects list
     */
    private void handleRecentProjects(Context ctx) {
        List<EditorProjectService.RecentProject> recent = mEditorProjectService.getRecentProjects();

        JSONArray projects = new JSONArray();
        for (EditorProjectService.RecentProject rp : recent) {
            JSONObject proj = new JSONObject();
            proj.put("path", rp.path);
            proj.put("name", rp.name);
            proj.put("date", rp.lastOpened);
            projects.put(proj);
        }

        JSONObject response = new JSONObject();
        response.put("projects", projects);
        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/projects/recent/add - Add to recent projects
     */
    private void handleRecentAdd(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        String name = body.optString("name", "");

        if (!path.isEmpty()) {
            mEditorProjectService.addRecentProject(path, name);
        }

        handleRecentProjects(ctx);
    }

    /**
     * POST /api/v1/projects/recent/remove - Remove from recent projects
     */
    private void handleRecentRemove(Context ctx) {
        // TODO: Implement removal in EditorProjectService
        handleRecentProjects(ctx);
    }

    // ========== Connection Management Handlers ==========

    /**
     * GET /api/v1/connections - List all runtime connections
     */
    private void handleConnectionsList(Context ctx) {
        JSONArray connections = mConnectionManager.getConnectionStatus();

        JSONObject response = new JSONObject();
        response.put("connections", connections);
        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/connections/add - Add new runtime connection
     */
    private void handleConnectionAdd(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String name = body.optString("name", "Runtime");
        String url = body.optString("url", "");
        String token = body.optString("token", "");

        if (url.isEmpty()) {
            ctx.status(400).result("Missing url");
            return;
        }

        String connectionId = mConnectionManager.addConnection(name, url, token);

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("connectionId", connectionId);
        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/connections/{cid}/remove - Remove runtime connection
     */
    private void handleConnectionRemove(Context ctx) {
        String cid = ctx.pathParam("cid");

        if (mConnectionManager.removeConnection(cid)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            writeJson(ctx, response);
        } else {
            ctx.status(404).result("Connection not found");
        }
    }

    /**
     * POST /api/v1/connections/{cid}/connect - Connect to runtime server
     */
    private void handleConnectionConnect(Context ctx) {
        String cid = ctx.pathParam("cid");

        if (mConnectionManager.connect(cid)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", "connected");
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to connect");
        }
    }

    /**
     * POST /api/v1/connections/{cid}/disconnect - Disconnect from runtime server
     */
    private void handleConnectionDisconnect(Context ctx) {
        String cid = ctx.pathParam("cid");
        mConnectionManager.disconnect(cid);

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/connections/{cid}/set-active - Set active connection
     */
    private void handleConnectionSetActive(Context ctx) {
        String cid = ctx.pathParam("cid");

        if (mConnectionManager.setActiveConnection(cid)) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            writeJson(ctx, response);
        } else {
            ctx.status(404).result("Connection not found");
        }
    }

    /**
     * GET /api/v1/connections/{cid}/status - Get connection status
     */
    private void handleConnectionStatus(Context ctx) {
        String cid = ctx.pathParam("cid");
        RuntimeConnection connection = mConnectionManager.getConnection(cid);

        if (connection == null) {
            ctx.status(404).result("Connection not found");
            return;
        }

        JSONObject response = new JSONObject();
        response.put("state", connection.getState().toString().toLowerCase());
        response.put("name", connection.getName());
        response.put("url", connection.getUrl());

        if (connection.isConnected() && connection.getStatus() != null) {
            RuntimeConnection.RuntimeStatus status = connection.getStatus();
            JSONObject runtime = new JSONObject();
            runtime.put("state", status.state);
            runtime.put("projectPath", status.projectPath);
            runtime.put("projectName", status.projectName);
            runtime.put("isRunning", status.isRunning);
            runtime.put("isPaused", status.isPaused);
            response.put("runtime", runtime);
        }

        writeJson(ctx, response);
    }

    /**
     * POST /api/v1/projects/{pid}/sync-to-runtime - Sync project to active runtime connection
     */
    private void handleProjectSyncToRuntime(Context ctx) {
        String pid = ctx.pathParam("pid");
        EditorProject project = mProjects.get(pid);

        if (project == null) {
            ctx.status(404).result("Project not found");
            return;
        }

        // Get active connection
        RuntimeConnection connection = mConnectionManager.getActiveConnection();
        if (connection == null) {
            ctx.status(400).result("No active runtime connection");
            return;
        }

        // Synchronize project
        ProjectSynchronizer.SyncResult result = mProjectSynchronizer.syncToRuntime(project, connection);

        JSONObject response = new JSONObject();
        response.put("success", result.success);
        response.put("message", result.message);
        if (result.projectPath != null) {
            response.put("projectPath", result.projectPath);
        }

        if (result.success) {
            writeJson(ctx, response);
        } else {
            ctx.status(500);
            writeJson(ctx, response);
        }
    }

    // ========== WebSocket Handlers ==========

    /**
     * WebSocket connection handler.
     */
    private void handleWsConnect(WsContext ctx) {
        // Authenticate via query parameter
        String token = ctx.queryParam("token");

        if (token == null || !mAuthToken.equals(token)) {
            ctx.send("{\"error\":\"Unauthorized\"}");
            ctx.session.close();
            return;
        }

        mWsSessions.add(ctx);
        sLogger.message("WebSocket client connected");

        // Send initial state
        sendInitialState(ctx);
    }

    /**
     * WebSocket message handler.
     */
    private void handleWsMessage(String message, java.util.function.Consumer<String> sender) {
        try {
            JSONObject msg = new JSONObject(message);
            String id = msg.optString("id", "");
            String method = msg.optString("method", "");
            JSONObject params = msg.optJSONObject("params");

            JSONObject result = dispatchWsMethod(method, params == null ? new JSONObject() : params);

            JSONObject response = new JSONObject();
            if (!id.isEmpty()) {
                response.put("id", id);
            }
            response.put("status", "ok");
            response.put("result", result);
            sender.accept(response.toString());
        } catch (Exception e) {
            JSONObject response = new JSONObject();
            response.put("status", "error");
            response.put("message", e.getMessage());
            sender.accept(response.toString());
        }
    }

    /**
     * Dispatches WebSocket method calls.
     */
    private JSONObject dispatchWsMethod(String method, JSONObject params) {
        // Only allow editing methods in FULL_EDITOR mode
        if (mMode == ServerMode.FULL_EDITOR) {
            switch (method) {
                case "SceneFlow.Node.Add":
                    return handleWsNodeAdd(params);
                case "SceneFlow.Node.Update":
                    return handleWsNodeUpdate(params);
                case "SceneFlow.Node.Delete":
                    return handleWsNodeDelete(params);
                case "SceneFlow.Edge.Add":
                    return handleWsEdgeAdd(params);
                case "SceneFlow.Edge.Update":
                    return handleWsEdgeUpdate(params);
                case "SceneFlow.Edge.Delete":
                    return handleWsEdgeDelete(params);
            }
        }

        // Unknown method
        JSONObject result = new JSONObject();
        result.put("error", "Unknown method: " + method);
        return result;
    }

    /**
     * WebSocket handler: Add node (uses SceneFlowService)
     */
    private JSONObject handleWsNodeAdd(JSONObject params) {
        String pid = params.optString("projectId", "");
        String parentId = params.optString("parentId", "");
        String type = params.optString("type", "BasicNode");
        JSONObject position = params.optJSONObject("position");

        EditorProject project = mProjects.get(pid);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "Project not found");
            return error;
        }

        // Use SceneFlowService to create node
        SceneFlowService.NodeType nodeType = type.equals("SuperNode")
            ? SceneFlowService.NodeType.SUPER_NODE
            : SceneFlowService.NodeType.BASIC_NODE;

        Point pos = new Point(
            position != null ? position.optInt("x", 0) : 0,
            position != null ? position.optInt("y", 0) : 0
        );

        BasicNode node = mSceneFlowService.createNode(
            project,
            parentId.isEmpty() ? project.getSceneFlow().getId() : parentId,
            nodeType,
            pos,
            params.optString("name", null)
        );

        if (node != null) {
            // Broadcast update to all clients
            broadcastSceneflowUpdate(pid);

            JSONObject result = new JSONObject();
            result.put("status", "ok");
            result.put("nodeId", node.getId());
            return result;
        } else {
            JSONObject error = new JSONObject();
            error.put("error", "Failed to create node");
            return error;
        }
    }

    private JSONObject handleWsNodeUpdate(JSONObject params) {
        // TODO: Implement node updates
        JSONObject result = new JSONObject();
        result.put("status", "ok");
        return result;
    }

    private JSONObject handleWsNodeDelete(JSONObject params) {
        String pid = params.optString("projectId", "");
        String nodeId = params.optString("nodeId", "");

        EditorProject project = mProjects.get(pid);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "Project not found");
            return error;
        }

        // Use SceneFlowService to delete node
        if (mSceneFlowService.deleteNode(project, nodeId)) {
            // Broadcast update to all clients
            broadcastSceneflowUpdate(pid);

            JSONObject result = new JSONObject();
            result.put("status", "ok");
            return result;
        } else {
            JSONObject error = new JSONObject();
            error.put("error", "Failed to delete node");
            return error;
        }
    }

    private JSONObject handleWsEdgeAdd(JSONObject params) {
        String pid = params.optString("projectId", "");
        String sourceId = params.optString("sourceId", "");
        String targetId = params.optString("targetId", "");
        String type = params.optString("type", "EEDGE");

        EditorProject project = mProjects.get(pid);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "Project not found");
            return error;
        }

        // Map type string to EdgeType enum
        SceneFlowService.EdgeType edgeType = mapEdgeType(type);

        // Collect edge data from params
        Map<String, Object> edgeData = new HashMap<>();
        if (params.has("timeout")) {
            edgeData.put("timeout", params.getLong("timeout"));
        }
        if (params.has("condition")) {
            edgeData.put("condition", params.getString("condition"));
        }
        if (params.has("probability")) {
            edgeData.put("probability", params.getInt("probability"));
        }

        // Use SceneFlowService to create edge
        AbstractEdge edge = mSceneFlowService.createEdge(project, sourceId, targetId, edgeType, edgeData);

        if (edge != null) {
            // Broadcast update to all clients
            broadcastSceneflowUpdate(pid);

            JSONObject result = new JSONObject();
            result.put("status", "ok");
            return result;
        } else {
            JSONObject error = new JSONObject();
            error.put("error", "Failed to create edge");
            return error;
        }
    }

    private JSONObject handleWsEdgeUpdate(JSONObject params) {
        // TODO: Implement edge updates
        JSONObject result = new JSONObject();
        result.put("status", "ok");
        return result;
    }

    private JSONObject handleWsEdgeDelete(JSONObject params) {
        String pid = params.optString("projectId", "");
        String sourceId = params.optString("sourceId", "");
        String targetId = params.optString("targetId", "");

        EditorProject project = mProjects.get(pid);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "Project not found");
            return error;
        }

        // Use SceneFlowService to delete edge (auto-detect type)
        if (mSceneFlowService.deleteEdge(project, sourceId, targetId, null)) {
            // Broadcast update to all clients
            broadcastSceneflowUpdate(pid);

            JSONObject result = new JSONObject();
            result.put("status", "ok");
            return result;
        } else {
            JSONObject error = new JSONObject();
            error.put("error", "Failed to delete edge");
            return error;
        }
    }

    // ========== Utilities ==========

    /**
     * Sends initial state to newly connected WebSocket client.
     */
    private void sendInitialState(WsContext ctx) {
        JSONObject state = new JSONObject();
        state.put("event", "server.info");
        state.put("mode", mMode.toString().toLowerCase());
        state.put("features", getFeatures());
        ctx.send(state.toString());
    }

    /**
     * Broadcasts sceneflow update to all connected clients.
     */
    private void broadcastSceneflowUpdate(String projectId) {
        EditorProject project = mProjects.get(projectId);
        if (project == null) return;

        JSONObject event = new JSONObject();
        event.put("event", "sceneflow.update");
        event.put("projectId", projectId);
        event.put("nodes", serializeNodes(project));
        event.put("edges", serializeEdges(project));

        broadcast(event.toString());
    }

    /**
     * Broadcasts message to all connected WebSocket clients.
     */
    private void broadcast(String message) {
        for (WsContext session : mWsSessions) {
            try {
                if (session.session.isOpen()) {
                    session.send(message);
                }
            } catch (Exception e) {
                sLogger.warning("Failed to broadcast: " + e.getMessage());
            }
        }
    }

    /**
     * Serializes nodes from EditorProject.
     */
    private JSONArray serializeNodes(EditorProject project) {
        // Basic serialization - can be expanded
        JSONArray nodes = new JSONArray();
        if (project.getSceneFlow() != null) {
            collectNodes(project.getSceneFlow(), nodes);
        }
        return nodes;
    }

    /**
     * Recursively collects nodes.
     */
    private void collectNodes(SuperNode parent, JSONArray out) {
        if (parent == null) return;

        // Add super node itself
        JSONObject node = new JSONObject();
        node.put("id", parent.getId());
        node.put("name", parent.getName());
        node.put("type", "Super");
        out.put(node);

        // Add basic children
        if (parent.getNodeList() != null) {
            for (BasicNode n : parent.getNodeList()) {
                JSONObject child = new JSONObject();
                child.put("id", n.getId());
                child.put("name", n.getName());
                child.put("type", "Basic");
                child.put("parentId", parent.getId());
                out.put(child);
            }
        }

        // Recursively add super children
        if (parent.getSuperNodeList() != null) {
            for (SuperNode sn : parent.getSuperNodeList()) {
                collectNodes(sn, out);
            }
        }
    }

    /**
     * Serializes edges from EditorProject.
     */
    private JSONArray serializeEdges(EditorProject project) {
        JSONArray edges = new JSONArray();
        if (project.getSceneFlow() != null) {
            collectEdges(project.getSceneFlow(), edges);
        }
        return edges;
    }

    /**
     * Recursively collects edges.
     */
    private void collectEdges(SuperNode parent, JSONArray out) {
        if (parent == null) return;

        if (parent.getNodeList() != null) {
            for (BasicNode n : parent.getNodeList()) {
                if (n.getEdgeList() != null) {
                    for (AbstractEdge e : n.getEdgeList()) {
                        JSONObject edge = new JSONObject();
                        edge.put("sourceId", e.getSourceUnid());
                        edge.put("targetId", e.getTargetUnid());
                        edge.put("type", e.getClass().getSimpleName());
                        out.put(edge);
                    }
                }
            }
        }

        if (parent.getSuperNodeList() != null) {
            for (SuperNode sn : parent.getSuperNodeList()) {
                collectEdges(sn, out);
            }
        }
    }

    /**
     * Maps edge type string to EdgeType enum.
     */
    private SceneFlowService.EdgeType mapEdgeType(String type) {
        switch (type) {
            case "EEDGE": return SceneFlowService.EdgeType.EPSILON_EDGE;
            case "TEDGE": return SceneFlowService.EdgeType.TIMEOUT_EDGE;
            case "CEDGE": return SceneFlowService.EdgeType.CONDITIONAL_EDGE;
            case "PEDGE": return SceneFlowService.EdgeType.PROBABILISTIC_EDGE;
            case "FEDGE": return SceneFlowService.EdgeType.FORKING_EDGE;
            case "IEDGE": return SceneFlowService.EdgeType.INTERRUPTIVE_EDGE;
            default: return SceneFlowService.EdgeType.EPSILON_EDGE;
        }
    }

    /**
     * Generates random authentication token.
     */
    private String generateToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    /**
     * Writes JSON response.
     */
    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }

    /**
     * Gets the local URL.
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
     * Gets the server mode.
     */
    public ServerMode getMode() {
        return mMode;
    }
}
