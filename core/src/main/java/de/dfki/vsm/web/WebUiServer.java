package de.dfki.vsm.web;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgePoint;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeArrow;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentBoundary;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.NodeExecutedEvent;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.EdgeExecutedEvent;
import de.dfki.vsm.event.event.NodeTerminatedEvent;
import de.dfki.vsm.event.event.TimeoutEdgeStartedEvent;
import de.dfki.vsm.event.event.SceneStoppedEvent;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import java.util.List;
import java.util.ArrayList;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.core.util.Header;
import io.javalin.http.staticfiles.Location;
import io.javalin.http.Context;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;

import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.io.InputStream;

public final class WebUiServer implements EventListener {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";
    private static final int RECENT_MAX = 8;
    private static WebUiServer sInstance;
    private static final String DEMO_PROJECT_ID = "demo-project";

    private Javalin mApp;
    private boolean mAllowExternal = false;
    private final Map<String, ProjectRef> projectStore = new HashMap<>();
    private final java.util.Set<WsContext> wsSessions = ConcurrentHashMap.newKeySet();

    private WebUiServer() {
    }

    public static synchronized WebUiServer getInstance() {
        if (sInstance == null) {
            sInstance = new WebUiServer();
        }
        return sInstance;
    }

    public void setAllowExternal(boolean allow) {
        mAllowExternal = allow;
    }

    private int mPort = 8090;

    public void start() {
        start(8090, mAllowExternal);
    }

    public void start(int port, boolean allowExternal) {
        if (mApp != null) {
            return;
        }
        mPort = port;
        mAllowExternal = allowExternal;
        Preferences.load();
        mApp = Javalin.create(config -> {
            // Try to add static files if available (editor mode)
            // These may not be present in runtime-only mode
            boolean hasWebUi = getClass().getClassLoader().getResource("web-ui/index.html") != null;
            boolean hasImages = getClass().getClassLoader().getResource("images/") != null;
            if (hasWebUi) {
                config.addStaticFiles("/web-ui", Location.CLASSPATH);
                config.addSinglePageRoot("/", "/web-ui/index.html", Location.CLASSPATH);
            }
            if (hasImages) {
                config.addStaticFiles("images", Location.CLASSPATH);
            }
            // Enable CORS for cross-origin requests (Phase 8.4: remote connections)
            config.enableCorsForAllOrigins();
        }).start(allowExternal ? "0.0.0.0" : "127.0.0.1", port);
        registerRoutes();
        // Register for runtime events to broadcast to WebSocket clients
        EventDispatcher.getInstance().register(this);
        sLogger.message("Web UI server started on " + getLocalUrl());
    }

    public void stop() {
        if (mApp != null) {
            EventDispatcher.getInstance().remove(this);
            mApp.stop();
            mApp = null;
        }
    }

    /**
     * Event handler - translates domain events to UI protocol events.
     * This mirrors UiEventBridge to ensure consistent event format between
     * local (editor) and remote (runtime-server) connections.
     */
    @Override
    public void update(EventObject event) {
        if (event == null) {
            return;
        }
        System.out.println("[EVENT] Received: " + event.getClass().getSimpleName());

        // Handle VariableChangedEvent first (matches UiEventBridge order)
        if (event instanceof VariableChangedEvent) {
            VariableChangedEvent varEvent = (VariableChangedEvent) event;
            Tuple<String, String> pair = varEvent.getVarValue();
            if (pair == null || pair.getFirst() == null || pair.getFirst().isBlank()) {
                return;
            }
            String projectId = findProjectIdForEvent(event);
            JSONObject message = new JSONObject();
            message.put("type", "event");
            message.put("ts", System.currentTimeMillis());
            message.put("channel", "vars");
            message.put("event", "vars.updated");
            JSONObject payload = new JSONObject();
            if (projectId != null) {
                payload.put("projectId", projectId);
            }
            payload.put("name", pair.getFirst());
            payload.put("value", pair.getSecond() != null ? pair.getSecond() : "");
            message.put("payload", payload);
            System.out.println("[EVENT] → vars.updated: " + pair.getFirst() + " = " + pair.getSecond());
            broadcastToAll(message.toString());
            return;
        }

        String projectId = findProjectIdForEvent(event);
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        JSONObject payload = new JSONObject();
        if (projectId != null) {
            payload.put("projectId", projectId);
        }

        // Match UiEventBridge event translation exactly for consistency
        if (event instanceof NodeStartedEvent) {
            // NodeStartedEvent → runtime.nodeActive (node becomes active)
            BasicNode node = ((NodeStartedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.nodeActive");
            System.out.println("[EVENT] → runtime.nodeActive: " + node.getId());

        } else if (event instanceof NodeExecutedEvent || event instanceof NodeTerminatedEvent) {
            // Both NodeExecutedEvent and NodeTerminatedEvent → runtime.nodeStopped
            // (matches UiEventBridge behavior)
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
            System.out.println("[EVENT] → runtime.nodeStopped: " + node.getId());

        } else if (event instanceof EdgeExecutedEvent) {
            // EdgeExecutedEvent → runtime.edgeActive
            AbstractEdge edge = ((EdgeExecutedEvent) event).getEdge();
            if (edge == null) return;
            String sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            String targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            payload.put("edgeType", getEdgeTypeLowercase(edge));
            message.put("channel", "runtime");
            message.put("event", "runtime.edgeActive");
            System.out.println("[EVENT] → runtime.edgeActive: " + sourceId + " -> " + targetId);

        } else if (event instanceof TimeoutEdgeStartedEvent) {
            // TimeoutEdgeStartedEvent → runtime.timeoutProgress
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
            System.out.println("[EVENT] → runtime.timeoutProgress");

        } else if (event instanceof SceneStoppedEvent || event instanceof TerminationEvent) {
            // SceneStoppedEvent or TerminationEvent → runtime.state with status: "stopped"
            payload.put("status", "stopped");
            message.put("channel", "runtime");
            message.put("event", "runtime.state");
            System.out.println("[EVENT] → runtime.state: stopped");
            // Update runtime state in project store
            if (projectId != null) {
                ProjectRef ref = projectStore.get(projectId);
                if (ref != null) {
                    ref.runtimeState = "stopped";
                }
            }

        } else {
            // Unknown event type, skip
            System.out.println("[EVENT] Unknown, skipping: " + event.getClass().getName());
            return;
        }

        message.put("payload", payload);
        System.out.println("[EVENT] Broadcasting to " + wsSessions.size() + " clients");
        broadcastToAll(message.toString());
    }

    /**
     * Returns edge type in lowercase format matching UiEventBridge.
     * Used for runtime.edgeActive events.
     */
    private String getEdgeTypeLowercase(AbstractEdge edge) {
        if (edge instanceof EpsilonEdge) return "epsilon";
        if (edge instanceof GuargedEdge) return "conditional";
        if (edge instanceof RandomEdge) return "probabilistic";
        if (edge instanceof InterruptEdge) return "interruptive";
        if (edge instanceof TimeoutEdge) return "timeout";
        if (edge instanceof ForkingEdge) return "fork";
        return "unknown";
    }

    private String findProjectIdForEvent(EventObject event) {
        // Try to find which project this event belongs to
        for (Map.Entry<String, ProjectRef> entry : projectStore.entrySet()) {
            ProjectRef ref = entry.getValue();
            if (ref.runtimeProject != null && ref.runtimeProject.isRunning()) {
                return entry.getKey();
            }
        }
        // Return first project if only one exists
        if (projectStore.size() == 1) {
            return projectStore.keySet().iterator().next();
        }
        return null;
    }

    private void broadcastToAll(String message) {
        for (WsContext ctx : wsSessions) {
            try {
                ctx.send(message);
            } catch (Exception e) {
                sLogger.warning("Failed to send WebSocket message: " + e.getMessage());
            }
        }
    }

    public String getLocalUrl() {
        return "http://127.0.0.1:" + mPort;
    }

    private void registerRoutes() {
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/token", this::handleToken);
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.get(API_PREFIX + "/projects/samples", ctx -> handleStaticProjectList(ctx, "res/prj"));
        mApp.get(API_PREFIX + "/projects/tutorials", ctx -> handleStaticProjectList(ctx, "res/tutorials"));
        mApp.get(API_PREFIX + "/projects", this::handleProjects);
        mApp.get(API_PREFIX + "/preferences", this::handlePreferences);
        mApp.get(API_PREFIX + "/devices", this::handleDevices);
        mApp.post(API_PREFIX + "/projects/recent/remove", this::handleRecentRemove);
        mApp.post(API_PREFIX + "/projects/recent/add", this::handleRecentAdd);
        // Basic hooks for opens/saves from other clients
        mApp.post(API_PREFIX + "/projects/opened", this::handleProjectOpened);
        mApp.post(API_PREFIX + "/projects/saved", this::handleProjectSaved);

        // Project lifecycle and data endpoints (minimal placeholders).
        mApp.post(API_PREFIX + "/projects/open", this::handleProjectOpen);
        mApp.post(API_PREFIX + "/projects", this::handleProjectCreate);
        mApp.post(API_PREFIX + "/projects/{pid}/save", this::handleProjectSave);
        mApp.post(API_PREFIX + "/projects/{pid}/save-as", this::handleProjectSaveAs);
        mApp.post(API_PREFIX + "/projects/{pid}/close", this::handleProjectClose);
        mApp.get(API_PREFIX + "/projects/{pid}/config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config/keys", this::handleProjectConfigKeys);
        mApp.get(API_PREFIX + "/projects/{pid}/script", this::handleScript);
        mApp.get(API_PREFIX + "/projects/{pid}/script/scenes", this::handleScriptScenes);
        mApp.get(API_PREFIX + "/projects/{pid}/script/elements", this::handleScriptElements);
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/runtime", this::handleRuntime);
        mApp.post(API_PREFIX + "/projects/{pid}/sceneflow/navigate", this::handleSceneflowNavigate);
        mApp.post(API_PREFIX + "/projects/{pid}/script/diagnostics", this::handleScriptDiagnostics);

        // WebSocket endpoint: accepts requests and replies with JSON. Broadcasts snapshots/runtime state after mutations.
        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                System.out.println("[WS-CORE] Client connected: " + ctx.getSessionId());
                sLogger.message("[WS-CORE] Client connected: " + ctx.getSessionId());
                wsSessions.add(ctx);
            });
            ws.onClose(ctx -> {
                System.out.println("[WS-CORE] Client disconnected: " + ctx.getSessionId());
                wsSessions.remove(ctx);
            });
            ws.onError(ctx -> {
                System.out.println("[WS-CORE] WebSocket error: " + ctx.getSessionId());
                wsSessions.remove(ctx);
            });
            ws.onMessage(ctx -> {
                System.out.println("[WS-CORE] Message received from " + ctx.getSessionId() + ": " + ctx.message());
                handleWsMessage(ctx.message(), ctx::send, msg -> broadcast(ctx, msg));
            });
        });

        // Serve packaged images (e.g., vsm_logo.svg) explicitly.
        mApp.get("/images/{file}", this::handleImage);
    }

    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Web");
        info.put("port", mPort);
        info.put("tokenRequired", true);
        writeJson(ctx, info);
    }

    private void handleToken(Context ctx) {
        JSONObject token = new JSONObject();
        token.put("token", "dev-token");
        writeJson(ctx, token);
    }

    private void handleRecentProjects(Context ctx) {
        JSONArray recent = new JSONArray();
        for (int i = 0; i <= RECENT_MAX; i++) {
            String path = Preferences.getProperty("recentproject." + i + ".path");
            String name = Preferences.getProperty("recentproject." + i + ".name");
            String date = Preferences.getProperty("recentproject." + i + ".date");
            if (path == null || path.isBlank() || name == null || name.isBlank()) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("path", path);
            entry.put("name", name);
            if (date != null && !date.isBlank()) {
                entry.put("date", date);
            }
            recent.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("projects", recent);
        writeJson(ctx, response);
    }

    private void handleRecentRemove(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        if (!path.isEmpty()) {
            removeRecent(path);
        }
        handleRecentProjects(ctx);
    }

    private void handleRecentAdd(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    private void handleProjectOpened(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    private void handleProjectSaved(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    public static void addRecent(String path, String name) {
        if (path == null || name == null || path.isBlank() || name.isBlank()) {
            if (path == null || path.isBlank()) {
                return;
            }
            name = fileName(path);
        }
        // Shift existing entries down and put new at position 0
        int max = RECENT_MAX;
        // If already present, remove it first
        for (int i = 0; i <= max; i++) {
            String existing = Preferences.getProperty("recentproject." + i + ".path");
            if (path.equals(existing)) {
                for (int j = i; j < max; j++) {
                    String nextPath = Preferences.getProperty("recentproject." + (j + 1) + ".path");
                    String nextName = Preferences.getProperty("recentproject." + (j + 1) + ".name");
                    String nextDate = Preferences.getProperty("recentproject." + (j + 1) + ".date");
                    if (nextPath == null) {
                        Preferences.removeProperty("recentproject." + j + ".path");
                        Preferences.removeProperty("recentproject." + j + ".name");
                        Preferences.removeProperty("recentproject." + j + ".date");
                    } else {
                        Preferences.setProperty("recentproject." + j + ".path", nextPath);
                        Preferences.setProperty("recentproject." + j + ".name", nextName);
                        if (nextDate != null) {
                            Preferences.setProperty("recentproject." + j + ".date", nextDate);
                        } else {
                            Preferences.removeProperty("recentproject." + j + ".date");
                        }
                    }
                }
                break;
            }
        }
        // Shift down
        for (int i = max; i > 0; i--) {
            String prevPath = Preferences.getProperty("recentproject." + (i - 1) + ".path");
            String prevName = Preferences.getProperty("recentproject." + (i - 1) + ".name");
            String prevDate = Preferences.getProperty("recentproject." + (i - 1) + ".date");
            if (prevPath != null) {
                Preferences.setProperty("recentproject." + i + ".path", prevPath);
                Preferences.setProperty("recentproject." + i + ".name", prevName);
                if (prevDate != null) {
                    Preferences.setProperty("recentproject." + i + ".date", prevDate);
                }
            }
        }
        Preferences.setProperty("recentproject.0.path", path);
        Preferences.setProperty("recentproject.0.name", name);
        Preferences.setProperty("recentproject.0.date", new java.text.SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss").format(new java.util.Date()));
        Preferences.save();
    }

    public static void removeRecent(String path) {
        if (path == null) return;
        int max = RECENT_MAX;
        for (int i = 0; i <= max; i++) {
            String existing = Preferences.getProperty("recentproject." + i + ".path");
            if (path.equals(existing)) {
                for (int j = i; j < max; j++) {
                    String nextPath = Preferences.getProperty("recentproject." + (j + 1) + ".path");
                    String nextName = Preferences.getProperty("recentproject." + (j + 1) + ".name");
                    String nextDate = Preferences.getProperty("recentproject." + (j + 1) + ".date");
                    if (nextPath == null) {
                        Preferences.removeProperty("recentproject." + j + ".path");
                        Preferences.removeProperty("recentproject." + j + ".name");
                        Preferences.removeProperty("recentproject." + j + ".date");
                    } else {
                        Preferences.setProperty("recentproject." + j + ".path", nextPath);
                        Preferences.setProperty("recentproject." + j + ".name", nextName);
                        if (nextDate != null) {
                            Preferences.setProperty("recentproject." + j + ".date", nextDate);
                        } else {
                            Preferences.removeProperty("recentproject." + j + ".date");
                        }
                    }
                }
                Preferences.save();
                break;
            }
        }
    }

    private static String fileName(String path) {
        int idx = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        if (idx >= 0 && idx < path.length() - 1) {
            return path.substring(idx + 1);
        }
        return path;
    }

    private void handleStaticProjectList(Context ctx, String directory) {
        JSONArray list = new JSONArray();
        Path base = resolveResourcePath(directory);
        if (base != null && Files.exists(base) && Files.isDirectory(base)) {
            try (Stream<Path> children = Files.list(base)) {
                children
                        .filter(Files::isDirectory)
                        .sorted(Comparator.comparing(path -> path.getFileName().toString().toLowerCase()))
                        .forEach(path -> {
                            JSONObject entry = new JSONObject();
                            entry.put("name", path.getFileName().toString());
                            entry.put("path", path.toAbsolutePath().toString());
                            list.put(entry);
                        });
            } catch (Exception exc) {
                sLogger.warning("Warning: Cannot list static projects in '" + directory + "': " + exc.getMessage());
            }
        }
        JSONObject response = new JSONObject();
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handleProjects(Context ctx) {
        JSONObject response = new JSONObject();
        JSONArray list = new JSONArray();
        for (ProjectRef ref : projectStore.values()) {
            JSONObject entry = new JSONObject();
            entry.put("projectId", ref.id);
            entry.put("name", ref.name);
            entry.put("path", ref.path == null ? "" : ref.path);
            entry.put("dirty", ref.dirty);
            entry.put("pending", false);
            entry.put("runtimeState", ref.runtimeState);
            list.put(entry);
        }
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handlePreferences(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("preferences", preferencesToJson());
        writeJson(ctx, response);
    }

    private JSONObject preferencesToJson() {
        JSONObject prefs = new JSONObject();
        // Font and display preferences
        String fontSize = Preferences.getProperty("workspace_fontsize");
        if (fontSize != null && !fontSize.isBlank()) {
            prefs.put("workspace_fontsize", fontSize);
        }
        // Node dimensions
        String nodeWidth = Preferences.getProperty("node_width");
        String nodeHeight = Preferences.getProperty("node_height");
        if (nodeWidth != null) prefs.put("node_width", nodeWidth);
        if (nodeHeight != null) prefs.put("node_height", nodeHeight);
        // Grid settings
        String grid = Preferences.getProperty("grid");
        String gridX = Preferences.getProperty("grid_x");
        String gridY = Preferences.getProperty("grid_y");
        if (grid != null) prefs.put("grid", grid);
        if (gridX != null) prefs.put("grid_x", gridX);
        if (gridY != null) prefs.put("grid_y", gridY);
        // Display options
        String showNodeId = Preferences.getProperty("shownodeid");
        String showVariables = Preferences.getProperty("showvariables");
        String visualization = Preferences.getProperty("visualization");
        if (showNodeId != null) prefs.put("shownodeid", showNodeId);
        if (showVariables != null) prefs.put("showvariables", showVariables);
        if (visualization != null) prefs.put("visualization", visualization);
        return prefs;
    }

    private int getPreferenceInt(String key, int defaultValue) {
        String value = Preferences.getProperty(key);
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private void handleDevices(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("devices", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleProjectOpen(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        JSONObject response = new JSONObject();
        if (path.isBlank()) {
            ctx.status(400).result("Missing path");
            return;
        }
        String projectId = ensureProject(path, fileName(path));
        response.put("projectId", projectId);
        response.put("path", path);
        response.put("name", fileName(path));
        writeJson(ctx, response);
    }

    private void handleProjectCreate(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String name = body.optString("name", "Untitled");
        String baseDir = body.optString("baseDir", "");
        String projectId = ensureProject(baseDir, name);
        JSONObject response = new JSONObject();
        response.put("projectId", projectId);
        response.put("name", name);
        response.put("path", baseDir);
        writeJson(ctx, response);
    }

    private void handleProjectSave(Context ctx) {
        String pid = ctx.pathParam("pid");
        markClean(pid);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleProjectSaveAs(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            ProjectRef ref = projectStore.get(pid);
            if (ref != null) {
                ref.path = path;
                ref.name = fileName(path);
                ref.dirty = false;
            }
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("path", path);
        writeJson(ctx, response);
    }

    private void handleProjectClose(Context ctx) {
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            projectStore.remove(pid);
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleProjectConfig(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
            ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
            JSONObject cfgJson = new JSONObject();
            cfgJson.put("name", cfg.getProjectName());
            cfgJson.put("path", ref.path == null ? "" : ref.path);
            response.put("config", cfgJson);
        } else {
            response.put("config", new JSONObject());
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfigKeys(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("keys", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleScript(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null) {
            try {
                String script = loadFile(ref.runtimeProject.getProjectPath(), "scenescript.xml");
                response.put("script", script == null ? "" : script);
            } catch (Exception exc) {
                sLogger.warning("Warning: cannot load script for pid=" + pid + ": " + exc.getMessage());
                response.put("script", "");
            }
        } else {
            response.put("script", "");
        }
        writeJson(ctx, response);
    }

    private void handleScriptScenes(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("scenes", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleScriptElements(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("elements", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleSceneflow(Context ctx) {
        String pid = ctx.pathParam("pid");
        String superNodeIdParam = ctx.queryParam("superNodeId");
        ProjectRef ref = projectStore.get(pid);

        if (ref == null || ref.runtimeProject == null) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            empty.put("raw", "");
            writeJson(ctx, empty);
            return;
        }

        try {
            SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
            SuperNode targetSuperNode = resolveSuperNode(sceneFlow, superNodeIdParam);
            if (targetSuperNode == null) {
                targetSuperNode = sceneFlow;
            }

            JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, targetSuperNode, sceneFlow);
            writeJson(ctx, snapshot);
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot load sceneflow for pid=" + pid + ": " + exc.getMessage());
            JSONObject error = new JSONObject();
            error.put("nodes", new JSONArray());
            error.put("edges", new JSONArray());
            error.put("comments", new JSONArray());
            error.put("raw", "");
            writeJson(ctx, error);
        }
    }

    private SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
        if (superNodeId == null || superNodeId.isBlank() || "__root__".equals(superNodeId)) {
            return sceneFlow;
        }
        return findSuperNodeById(sceneFlow, superNodeId);
    }

    private SuperNode findSuperNodeById(SuperNode parent, String id) {
        if (parent == null) return null;
        if (id.equals(parent.getId())) return parent;
        for (SuperNode child : parent.getSuperNodeList()) {
            SuperNode found = findSuperNodeById(child, id);
            if (found != null) return found;
        }
        return null;
    }

    private JSONObject createSceneFlowSnapshot(RunTimeProject rtp, String projectId, SuperNode superNode, SceneFlow sceneFlow) {
        JSONObject snapshot = new JSONObject();
        snapshot.put("projectId", projectId);
        snapshot.put("superNodeId", superNode.getId() != null ? superNode.getId() : "");
        snapshot.put("revision", superNode.hashCode());

        // SuperNode info
        JSONObject superNodeJson = new JSONObject();
        superNodeJson.put("id", superNode.getId() != null ? superNode.getId() : "");
        superNodeJson.put("name", superNode.getName() != null ? superNode.getName() : "SceneFlow");
        superNodeJson.put("flavour", superNode.getFlavour() != null ? superNode.getFlavour().name() : "None");
        snapshot.put("superNode", superNodeJson);

        // Build path
        JSONArray path = new JSONArray();
        JSONArray pathNodes = new JSONArray();
        List<SuperNode> pathList = findPathToSuperNode(sceneFlow, superNode.getId());
        if (pathList == null || pathList.isEmpty()) {
            pathList = new ArrayList<>();
            pathList.add(superNode);
        }
        for (SuperNode node : pathList) {
            String nodeName = node.getName();
            if (nodeName == null || nodeName.isBlank()) {
                nodeName = "SceneFlow";
            }
            String nodeId = node.getId();
            if (nodeId == null || nodeId.isBlank()) {
                nodeId = "__root__";
            }
            path.put(nodeName);
            JSONObject pathEntry = new JSONObject();
            pathEntry.put("id", nodeId);
            pathEntry.put("name", nodeName);
            pathEntry.put("isRoot", node.getParentNode() == null);
            pathNodes.put(pathEntry);
        }
        snapshot.put("path", path);
        snapshot.put("pathNodes", pathNodes);

        // SuperNode data
        Set<String> altStartIds = collectAltStartIds(superNode);
        JSONObject superNodeData = nodeToJsonCore(superNode, superNode, altStartIds);
        superNodeData.put("isStart", superNode.getParentNode() == null ||
            (superNode.getParentNode() != null && superNode.getParentNode().getStartNodeMap().containsKey(superNode.getId())));
        superNodeData.put("isRoot", superNode.getParentNode() == null);
        snapshot.put("superNodeData", superNodeData);

        // Nodes at current level only
        JSONArray nodes = new JSONArray();
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            nodes.put(nodeToJsonCore(node, superNode, altStartIds));
        }
        snapshot.put("nodes", nodes);

        // Edges at current level only
        JSONArray edges = new JSONArray();
        int edgeIndex = 0;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                edges.put(edgeToJsonCore(edge, edgeIndex++));
            }
        }
        snapshot.put("edges", edges);

        // Comments
        JSONArray comments = new JSONArray();
        int commentIndex = 0;
        for (CommentBadge comment : superNode.getCommentList()) {
            comments.put(commentToJsonCore(comment, commentIndex++));
        }
        snapshot.put("comments", comments);

        return snapshot;
    }

    private List<SuperNode> findPathToSuperNode(SuperNode root, String targetId) {
        if (root == null) return null;
        List<SuperNode> path = new ArrayList<>();
        if (findPathRecursive(root, targetId, path)) {
            return path;
        }
        return null;
    }

    private boolean findPathRecursive(SuperNode current, String targetId, List<SuperNode> path) {
        path.add(current);
        String currentId = current.getId();
        if ((currentId != null && currentId.equals(targetId)) ||
            (currentId == null && targetId == null) ||
            ("__root__".equals(targetId) && current.getParentNode() == null)) {
            return true;
        }
        for (SuperNode child : current.getSuperNodeList()) {
            if (findPathRecursive(child, targetId, path)) {
                return true;
            }
        }
        path.remove(path.size() - 1);
        return false;
    }

    private Set<String> collectAltStartIds(SuperNode target) {
        Set<String> altStartIds = new java.util.LinkedHashSet<>();
        SuperNode parent = target.getParentNode();
        if (parent == null) {
            return altStartIds;
        }
        for (BasicNode node : parent.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (!target.getId().equals(edge.getTargetUnid())) {
                    continue;
                }
                Map<de.dfki.vsm.util.tpl.Tuple<String, BasicNode>, de.dfki.vsm.util.tpl.Tuple<String, BasicNode>> altMap = edge.getAltMap();
                if (altMap == null) {
                    continue;
                }
                for (de.dfki.vsm.util.tpl.Tuple<String, BasicNode> alt : altMap.values()) {
                    if (alt != null && alt.getFirst() != null && !alt.getFirst().isEmpty()) {
                        altStartIds.add(alt.getFirst());
                    }
                }
            }
        }
        return altStartIds;
    }

    private JSONObject nodeToJsonCore(BasicNode node, SuperNode superNode, Set<String> altStartIds) {
        JSONObject json = new JSONObject();
        json.put("id", node.getId());
        json.put("type", (node instanceof SuperNode) ? "Super" : "Basic");
        json.put("name", node.getName() != null ? node.getName() : "");
        json.put("comment", node.getComment() != null ? node.getComment() : "");
        json.put("flavour", node.getFlavour() != null ? node.getFlavour().name() : "None");
        json.put("isStart", superNode.getStartNodeMap().containsKey(node.getId()));
        json.put("isAltStart", altStartIds.contains(node.getId()));
        json.put("isHistory", node.isHistoryNode());

        int childCount = 0;
        if (node instanceof SuperNode) {
            childCount = ((SuperNode) node).getNodeAndSuperNodeList().size();
        }
        json.put("childCount", childCount);

        JSONObject graphics = new JSONObject();
        int x = 0, y = 0;
        if (node.getGraphics() != null && node.getGraphics().getPosition() != null) {
            x = node.getGraphics().getPosition().getXPos();
            y = node.getGraphics().getPosition().getYPos();
        }
        graphics.put("x", x);
        graphics.put("y", y);
        json.put("graphics", graphics);

        JSONObject size = new JSONObject();
        // Use configured node size from preferences (default 90x90)
        int nodeWidth = getPreferenceInt("node_width", 90);
        int nodeHeight = getPreferenceInt("node_height", 90);
        size.put("w", nodeWidth);
        size.put("h", nodeHeight);
        json.put("size", size);

        // Type definitions
        json.put("typeDefs", typeDefsToJsonCore(node.getTypeDefList()));
        json.put("varDefs", varDefsToJsonCore(node.getVarDefList()));
        json.put("commands", commandsToJsonCore(node.getCmdList()));

        return json;
    }

    private JSONArray typeDefsToJsonCore(List<DataTypeDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) return list;
        for (DataTypeDefinition def : defs) {
            if (def != null) {
                JSONObject json = new JSONObject();
                json.put("name", def.getName());
                json.put("flavour", def.getFlavour() != null ? def.getFlavour().name() : "");
                json.put("syntax", def.getConcreteSyntax());
                list.put(json);
            }
        }
        return list;
    }

    private JSONArray varDefsToJsonCore(List<VariableDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) return list;
        for (VariableDefinition def : defs) {
            if (def != null) {
                JSONObject json = new JSONObject();
                json.put("name", def.getName());
                json.put("type", def.getType());
                json.put("expression", def.getExp() != null ? def.getExp().getConcreteSyntax() : "");
                json.put("syntax", def.getConcreteSyntax());
                list.put(json);
            }
        }
        return list;
    }

    private JSONArray commandsToJsonCore(List<Command> commands) {
        JSONArray list = new JSONArray();
        if (commands == null) return list;
        for (Command cmd : commands) {
            if (cmd != null) {
                JSONObject json = new JSONObject();
                json.put("text", cmd.getConcreteSyntax());
                json.put("syntax", cmd.getConcreteSyntax());
                list.put(json);
            }
        }
        return list;
    }

    private JSONObject edgeToJsonCore(AbstractEdge edge, int index) {
        JSONObject json = new JSONObject();
        json.put("id", "E" + index);
        json.put("type", getEdgeType(edge));

        String sourceId = edge.getSourceUnid();
        if (sourceId == null || sourceId.isBlank()) {
            sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
        }
        String targetId = edge.getTargetUnid();
        if (targetId == null || targetId.isBlank()) {
            targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
        }
        json.put("sourceId", sourceId);
        json.put("targetId", targetId);

        // Edge graphics
        JSONObject graphics = new JSONObject();
        EdgeGraphics eg = edge.getGraphics();
        EdgeArrow arrow = eg != null ? eg.getConnection() : null;
        JSONArray points = new JSONArray();
        if (arrow != null) {
            for (EdgePoint point : arrow.getPointList()) {
                JSONObject p = new JSONObject();
                p.put("x", point.getXPos());
                p.put("y", point.getYPos());
                p.put("cx", point.getCtrlXPos());
                p.put("cy", point.getCtrlYPos());
                points.put(p);
            }
        }
        graphics.put("points", points);
        json.put("graphics", graphics);

        // Edge condition/expression
        String conditionText = "";
        if (edge instanceof GuargedEdge) {
            GuargedEdge ge = (GuargedEdge) edge;
            if (ge.getCondition() != null) {
                conditionText = ge.getCondition().getConcreteSyntax();
            }
        } else if (edge instanceof InterruptEdge) {
            InterruptEdge ie = (InterruptEdge) edge;
            if (ie.getCondition() != null) {
                conditionText = ie.getCondition().getConcreteSyntax();
            }
        }
        json.put("condition", conditionText);

        // Only set probability for RandomEdge (PEDGE) - don't set for other types
        // so frontend can distinguish and show correct label
        if (edge instanceof RandomEdge) {
            json.put("probability", ((RandomEdge) edge).getProbability());
        }

        // Timeout edge fields: timeoutMs (numeric) and timeoutExpr (expression string)
        if (edge instanceof TimeoutEdge) {
            TimeoutEdge te = (TimeoutEdge) edge;
            json.put("timeoutMs", te.getTimeout());
            json.put("timeoutExpr", te.getExpression() != null ? te.getExpression().getConcreteSyntax() : "");
        }

        return json;
    }

    private String getEdgeType(AbstractEdge edge) {
        if (edge instanceof GuargedEdge) return "CEDGE";  // Conditional/Guarded edge
        if (edge instanceof RandomEdge) return "PEDGE";   // Probabilistic edge
        if (edge instanceof InterruptEdge) return "IEDGE"; // Interrupt edge
        if (edge instanceof ForkingEdge) return "FEDGE";  // Forking edge
        if (edge instanceof TimeoutEdge) return "TEDGE";  // Timeout edge
        if (edge instanceof EpsilonEdge) return "EEDGE";  // Epsilon edge
        return "EEDGE"; // Default to epsilon
    }

    private JSONObject commentToJsonCore(CommentBadge comment, int index) {
        JSONObject json = new JSONObject();
        json.put("id", "C" + index);
        json.put("text", comment.getHTMLText() != null ? comment.getHTMLText() : "");

        // Use "rect" key to match editor's format
        JSONObject rectJson = new JSONObject();
        CommentGraphics cg = comment.getGraphics();
        CommentBoundary rect = cg != null ? cg.getRectangle() : null;
        if (rect != null) {
            rectJson.put("x", rect.getXPos());
            rectJson.put("y", rect.getYPos());
            rectJson.put("w", rect.getWidth());
            rectJson.put("h", rect.getHeight());
        } else {
            rectJson.put("x", 0);
            rectJson.put("y", 0);
            rectJson.put("w", 0);
            rectJson.put("h", 0);
        }
        json.put("rect", rectJson);

        return json;
    }

    private void handleRuntime(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        response.put("state", ref != null ? ref.runtimeState : "stopped");

        // Match editor's runtimeToJson format: globalVariables and localVariables
        if (ref != null && ref.runtimeProject != null) {
            response.put("project", ref.runtimeProject.getProjectPath());
            SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
            if (sceneFlow != null) {
                // Build type map for typeFlavor resolution
                Map<String, DataTypeDefinition> typeMap = new HashMap<>();
                for (DataTypeDefinition def : sceneFlow.getTypeDefList()) {
                    typeMap.put(def.getName(), def);
                }

                // Global variables from root sceneflow
                JSONArray globals = new JSONArray();
                for (VariableDefinition def : sceneFlow.getVarDefList()) {
                    globals.put(variableToJsonCore(def, typeMap, "global", ref.runtimeProject));
                }
                response.put("globalVariables", globals);

                // Local variables (use root sceneflow as "current" since runtime-server is headless)
                JSONArray locals = new JSONArray();
                // Note: In headless mode, we don't track current active supernode,
                // so locals would be same as globals for root - typically empty for local scope
                response.put("localVariables", locals);
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

    private void handleSceneflowNavigate(Context ctx) {
        String pid = ctx.pathParam("pid");
        JSONObject body = new JSONObject(ctx.body());
        String superNodeId = body.optString("superNodeId", "");

        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            JSONObject error = new JSONObject();
            error.put("status", "error");
            error.put("message", "Project not found");
            writeJson(ctx, error);
            return;
        }

        try {
            SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
            SuperNode targetSuperNode = resolveSuperNode(sceneFlow, superNodeId);
            if (targetSuperNode == null) {
                targetSuperNode = sceneFlow;
            }

            JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, targetSuperNode, sceneFlow);
            snapshot.put("status", "ok");
            writeJson(ctx, snapshot);
        } catch (Exception exc) {
            sLogger.warning("Navigation failed for project " + pid + ": " + exc.getMessage());
            JSONObject error = new JSONObject();
            error.put("status", "error");
            error.put("message", "Navigation failed: " + exc.getMessage());
            writeJson(ctx, error);
        }
    }

    private void handleScriptDiagnostics(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("issues", new JSONArray());
        writeJson(ctx, response);
    }

    /**
     * Convert a VariableDefinition to JSON with runtime value.
     * Matches editor's variableToJson format for consistency.
     */
    private JSONObject variableToJsonCore(VariableDefinition def, Map<String, DataTypeDefinition> typeMap, String scope, RunTimeProject project) {
        JSONObject json = new JSONObject();
        json.put("name", def.getName());
        json.put("type", def.getType());
        json.put("typeFlavor", resolveTypeFlavor(def.getType(), typeMap));
        json.put("expr", def.getExp() != null ? def.getExp().getConcreteSyntax() : "");
        json.put("scope", scope);
        // Get actual runtime value
        String value = resolveVariableValue(project, def.getName());
        if (value != null) {
            json.put("value", value);
        }
        return json;
    }

    /**
     * Resolve variable's runtime value from the interpreter environment.
     */
    private String resolveVariableValue(RunTimeProject project, String name) {
        if (project == null || name == null || name.isBlank()) {
            return null;
        }
        try {
            AbstractValue value = project.getValueOf(name);
            if (value == null) {
                return null;
            }
            return sanitizeVariableValue(value.getConcreteSyntax());
        } catch (Exception e) {
            // Variable may not be available yet in runtime environment
            return null;
        }
    }

    /**
     * Remove internal type markers from variable value display.
     */
    private String sanitizeVariableValue(String value) {
        if (value == null) {
            return null;
        }
        // Remove type markers like #s# for string, #i# for int, etc.
        return value.replaceAll("#[a-zA-Z]#", "");
    }

    /**
     * Resolve type flavor for display (Primitive, Struct, List).
     */
    private String resolveTypeFlavor(String type, Map<String, DataTypeDefinition> typeMap) {
        if (type == null) {
            return "Primitive";
        }
        DataTypeDefinition def = typeMap.get(type);
        if (def != null && def.getFlavour() != null) {
            return def.getFlavour().name();
        }
        // Built-in primitive types
        if ("Int".equalsIgnoreCase(type)
                || "Float".equalsIgnoreCase(type)
                || "Bool".equalsIgnoreCase(type)
                || "String".equalsIgnoreCase(type)) {
            return "Primitive";
        }
        return "Primitive";
    }

    private List<JSONObject> serializeNodes(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        List<JSONObject> nodes = new ArrayList<>();
        collectNodes(rtp, rtp.getSceneFlow(), nodes, true, null);
        nodes.sort(Comparator.comparing(o -> o.optString("id", "")));
        return nodes;
    }

    private void collectNodes(RunTimeProject rtp, SuperNode superNode, List<JSONObject> out, boolean isRoot, String parentId) {
        if (superNode == null) {
            return;
        }
        // Add the supernode itself (including root sceneflow so UI can show it)
        JSONObject obj = new JSONObject();
        obj.put("id", superNode.getId());
        obj.put("name", superNode.getName());
        obj.put("type", "Super");
        obj.put("isSuper", true);
        obj.put("isRoot", isRoot);
        if (parentId != null && !parentId.isBlank()) {
            obj.put("parentId", parentId);
        }

        JSONObject pos = new JSONObject();
        pos.put("x", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null ? superNode.getGraphics().getPosition().getXPos() : 0);
        pos.put("y", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null ? superNode.getGraphics().getPosition().getYPos() : 0);
        obj.put("position", pos);

        JSONObject size = new JSONObject();
        size.put("w", 140);
        size.put("h", 140);
        int childCount = (superNode.getNodeList() != null ? superNode.getNodeList().size() : 0)
                + (superNode.getSuperNodeList() != null ? superNode.getSuperNodeList().size() : 0);
        obj.put("childCount", childCount);
        obj.put("size", size);

        obj.put("isStart", isStartNode(rtp, superNode));
        obj.put("isAltStart", isAltStartNode(rtp, superNode));
        obj.put("isHistory", superNode.isHistoryNode());
        obj.put("comment", superNode.getComment() == null ? "" : superNode.getComment());
        obj.put("commands", serializeCommands(superNode.getCmdList()));
        out.add(obj);

        // Basic children
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
                        cpos.put("x", n.getGraphics() != null && n.getGraphics().getPosition() != null ? n.getGraphics().getPosition().getXPos() : 0);
                        cpos.put("y", n.getGraphics() != null && n.getGraphics().getPosition() != null ? n.getGraphics().getPosition().getYPos() : 0);
                        child.put("position", cpos);

                        JSONObject csize = new JSONObject();
                        csize.put("w", 120);
                        csize.put("h", 120);
                        child.put("size", csize);

                        child.put("isStart", isStartNode(rtp, n));
                        child.put("isAltStart", isAltStartNode(rtp, n));
                        child.put("isHistory", n.isHistoryNode());
                        child.put("comment", n.getComment() == null ? "" : n.getComment());
                        child.put("commands", serializeCommands(n.getCmdList()));
                        out.add(child);
                    });
        }
        // Super children
        if (superNode.getSuperNodeList() != null) {
            superNode.getSuperNodeList().stream()
                    .sorted(Comparator.comparing(BasicNode::getId))
                    .forEach(sn -> collectNodes(rtp, sn, out, false, superNode.getId()));
        }
    }

    private List<JSONObject> serializeEdges(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        List<AbstractEdge> edges = new ArrayList<>();
        collectEdges(rtp.getSceneFlow(), edges);
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

    private boolean isStartNode(RunTimeProject rtp, BasicNode node) {
        if (rtp == null || node == null || rtp.getSceneFlow() == null) return false;
        if (rtp.getSceneFlow().getStartNodeMap() != null) {
            return rtp.getSceneFlow().getStartNodeMap().containsKey(node.getId());
        }
        return false;
    }

    private boolean isAltStartNode(RunTimeProject rtp, BasicNode node) {
        if (rtp == null || node == null || rtp.getSceneFlow() == null) return false;
        // Alt-start nodes are stored on edges; approximate by checking alt-start maps on incoming edges
        return rtp.getSceneFlow().getEdgeList().stream().anyMatch(e -> {
            Map<?, ?> altMap = e.getCopyOfAltStartNodeMap();
            if (altMap == null) return false;
            return altMap.values().stream().anyMatch(val -> {
                if (val instanceof de.dfki.vsm.util.tpl.Tuple) {
                    Object second = ((de.dfki.vsm.util.tpl.Tuple<?, ?>) val).getSecond();
                    return second instanceof BasicNode && ((BasicNode) second).getId().equals(node.getId());
                }
                return false;
            });
        });
    }

    private List<JSONObject> serializeComments(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        ArrayList<CommentBadge> comments = rtp.getSceneFlow().getCommentList();
        if (comments == null) {
            return new ArrayList<>();
        }
        return comments.stream().map(c -> {
            JSONObject obj = new JSONObject();
            CommentGraphics cgLocal = c.getGraphics();
            String stableId = "comment";
            if (cgLocal != null && cgLocal.getRectangle() != null) {
                stableId = stableId + "_" + cgLocal.getRectangle().getXPos() + "_" + cgLocal.getRectangle().getYPos() + "_" + cgLocal.getRectangle().getWidth() + "_" + cgLocal.getRectangle().getHeight();
            } else if (c.getHTMLText() != null) {
                stableId = stableId + "_" + c.getHTMLText().hashCode();
            } else {
                stableId = stableId + "_" + UUID.randomUUID();
            }
            obj.put("id", stableId);
            obj.put("text", c.getHTMLText() == null ? "" : c.getHTMLText());
            CommentGraphics cg = c.getGraphics();
            if (cg != null && cg.getRectangle() != null) {
                JSONObject rect = new JSONObject();
                rect.put("x", cg.getRectangle().getXPos());
                rect.put("y", cg.getRectangle().getYPos());
                rect.put("w", cg.getRectangle().getWidth());
                rect.put("h", cg.getRectangle().getHeight());
                obj.put("rect", rect);
            }
            return obj;
        }).collect(Collectors.toList());
    }

    // --- Mutations (minimal, in-memory only) -------------------------------
    private void addNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        String name = params.optString("name", id);
        JSONObject pos = params.optJSONObject("position");
        JSONObject size = params.optJSONObject("size");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject node = new JSONObject();
        node.put("id", id);
        node.put("name", name);
        node.put("type", params.optString("type", "BasicNode"));
        node.put("position", pos == null ? new JSONObject().put("x", 0).put("y", 0) : pos);
        node.put("size", size == null ? new JSONObject().put("w", 120).put("h", 60) : size);
        node.put("isStart", params.optBoolean("isStart", false));
        node.put("isAltStart", params.optBoolean("isAltStart", false));
        node.put("isHistory", params.optBoolean("isHistory", false));
        node.put("comment", params.optString("comment", ""));
        node.put("commands", params.optJSONArray("commands") == null ? new JSONArray() : params.optJSONArray("commands"));
        ref.nodes.add(node);
    }

    private void updateNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.nodes.size(); i++) {
            JSONObject n = ref.nodes.get(i);
            if (id.equals(n.optString("id"))) {
                mergeJson(n, params);
                break;
            }
        }
    }

    private void deleteNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.nodes.removeIf(n -> id.equals(n.optString("id")));
        // Also remove edges connected to this node
        ref.edges.removeIf(e -> id.equals(e.optString("source")) || id.equals(e.optString("target")));
    }

    private void addEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject edge = new JSONObject();
        edge.put("id", id);
        edge.put("source", params.optString("source", ""));
        edge.put("target", params.optString("target", ""));
        edge.put("type", params.optString("type", "Edge"));
        edge.put("label", params.optString("label", ""));
        edge.put("points", params.optJSONArray("points") == null ? new JSONArray() : params.optJSONArray("points"));
        ref.edges.add(edge);
    }

    private void updateEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.edges.size(); i++) {
            JSONObject e = ref.edges.get(i);
            if (id.equals(e.optString("id"))) {
                mergeJson(e, params);
                break;
            }
        }
    }

    private void deleteEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.edges.removeIf(e -> id.equals(e.optString("id")));
    }

    private void addComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject c = new JSONObject();
        c.put("id", id);
        c.put("text", params.optString("text", ""));
        JSONObject rect = params.optJSONObject("rect");
        if (rect == null) {
            rect = new JSONObject().put("x", 0).put("y", 0).put("w", 200).put("h", 100);
        }
        c.put("rect", rect);
        ref.comments.add(c);
    }

    private void updateComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.comments.size(); i++) {
            JSONObject c = ref.comments.get(i);
            if (id.equals(c.optString("id"))) {
                mergeJson(c, params);
                break;
            }
        }
    }

    private void deleteComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.comments.removeIf(c -> id.equals(c.optString("id")));
    }

    private void setRuntimeState(String pid, String state) {
        ProjectRef ref = projectStore.get(pid);
        if (ref != null) {
            ref.runtimeState = state;
        }
    }

    // Merge params into target JSONObject (shallow), overriding only provided keys.
    private void mergeJson(JSONObject target, JSONObject updates) {
        if (target == null || updates == null) return;
        for (String key : updates.keySet()) {
            if ("projectId".equals(key)) {
                continue;
            }
            Object val = updates.get(key);
            target.put(key, val);
        }
    }

    // --- WebSocket handling -------------------------------------------------
    private void handleWsMessage(String raw, java.util.function.Consumer<String> sender, java.util.function.Consumer<String> broadcaster) {
        System.out.println("[WS-HANDLE] Starting handleWsMessage");
        try {
            System.out.println("[WS-HANDLE] Parsing message...");
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
            System.out.println("[WS-HANDLE] Dispatching method: " + method + ", id: " + id);
            JSONObject result = dispatchWs(method, params == null ? new JSONObject() : params, broadcaster);
            System.out.println("[WS-HANDLE] Dispatch returned: " + (result != null ? result.toString() : "null"));
            // Send response in the format the Web UI expects
            // Web UI expects: { type: "response", id, payload } or { type: "error", payload: { message } }
            JSONObject resp = new JSONObject();
            resp.put("type", "response");
            if (!id.isEmpty()) {
                resp.put("id", id);
            }
            resp.put("payload", result);
            // Also include status for backward compatibility
            resp.put("status", "ok");
            System.out.println("[WS-HANDLE] Sending response: " + resp.toString());
            sender.accept(resp.toString());
            System.out.println("[WS-HANDLE] Response sent successfully");
        } catch (Exception exc) {
            System.out.println("[WS-HANDLE] ERROR: " + exc.getMessage());
            exc.printStackTrace();
            JSONObject resp = new JSONObject();
            resp.put("type", "error");
            JSONObject payload = new JSONObject();
            payload.put("message", exc.getMessage());
            resp.put("payload", payload);
            resp.put("status", "error");
            sender.accept(resp.toString());
        }
    }

    private JSONObject dispatchWs(String method, JSONObject params, java.util.function.Consumer<String> broadcaster) {
        switch (method) {
            case "SceneFlow.Get":
            case "SceneFlow.Snapshot":
                return snapshotPayload(params.optString("projectId", ""));
            case "SceneFlow.Node.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addNode(params), broadcaster);
            case "SceneFlow.Node.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateNode(params), broadcaster);
            case "SceneFlow.Node.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteNode(params), broadcaster);
            case "SceneFlow.Edge.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addEdge(params), broadcaster);
            case "SceneFlow.Edge.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateEdge(params), broadcaster);
            case "SceneFlow.Edge.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteEdge(params), broadcaster);
            case "SceneFlow.Comment.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addComment(params), broadcaster);
            case "SceneFlow.Comment.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateComment(params), broadcaster);
            case "SceneFlow.Comment.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteComment(params), broadcaster);
            case "Project.Save":
            case "Project.SaveAs":
            case "Project.Close":
                JSONObject ok = new JSONObject();
                ok.put("status", "ok");
                return ok;
            case "Runtime.Play":
            case "Runtime.Start":
            case "Runtime.Pause":
            case "Runtime.Stop": {
                String pid = params.optString("projectId", "");
                System.out.println("[RUNTIME] " + method + " called for project: " + pid);
                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    System.out.println("[RUNTIME] Project not found: " + pid);
                    JSONObject err = new JSONObject();
                    err.put("error", "PROJECT_NOT_FOUND");
                    err.put("message", "Project not found: " + pid);
                    return err;
                }
                RunTimeProject rtp = ref.runtimeProject;
                boolean success = false;
                String newState = ref.runtimeState;

                if ("Runtime.Play".equals(method) || "Runtime.Start".equals(method)) {
                    System.out.println("[RUNTIME] Play/Start: isRunning=" + rtp.isRunning() + ", isPaused=" + rtp.isPaused());
                    if (rtp.isRunning()) {
                        if (rtp.isPaused()) {
                            success = rtp.proceed();
                            System.out.println("[RUNTIME] proceed() returned: " + success);
                            newState = success ? "running" : "paused";
                        } else {
                            success = true;
                            newState = "running";
                        }
                    } else {
                        boolean launched = rtp.launch();
                        System.out.println("[RUNTIME] launch() returned: " + launched);
                        if (launched) {
                            success = rtp.start();
                            System.out.println("[RUNTIME] start() returned: " + success);
                            newState = success ? "running" : "stopped";
                        } else {
                            System.out.println("[RUNTIME] launch() failed");
                        }
                    }
                } else if ("Runtime.Pause".equals(method)) {
                    if (rtp.isRunning() && !rtp.isPaused()) {
                        success = rtp.pause();
                        newState = success ? "paused" : "running";
                    } else {
                        success = true;
                        newState = rtp.isPaused() ? "paused" : (rtp.isRunning() ? "running" : "stopped");
                    }
                } else if ("Runtime.Stop".equals(method)) {
                    if (rtp.isRunning()) {
                        success = rtp.abort();
                        if (success) {
                            rtp.unload();
                        }
                        newState = "stopped";
                    } else {
                        success = true;
                        newState = "stopped";
                    }
                }

                ref.runtimeState = newState;
                sLogger.message("[RUNTIME] Final state: " + newState + ", success=" + success);
                JSONObject rt = new JSONObject();
                rt.put("state", newState);
                rt.put("projectId", pid);
                if (broadcaster != null) {
                    JSONObject evt = new JSONObject();
                    evt.put("event", "runtime.state");
                    evt.put("state", newState);
                    evt.put("projectId", pid);
                    broadcaster.accept(evt.toString());
                }
                return rt;
            }
            default:
                JSONObject unknown = new JSONObject();
                unknown.put("message", "Unhandled method: " + method);
                return unknown;
        }
    }

    private JSONObject snapshotPayload(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        JSONObject snap = new JSONObject();
        if (ref != null) {
            snap.put("nodes", new JSONArray(ref.nodes));
            snap.put("edges", new JSONArray(ref.edges));
            snap.put("comments", new JSONArray(ref.comments));
        } else {
            snap.put("nodes", new JSONArray());
            snap.put("edges", new JSONArray());
            snap.put("comments", new JSONArray());
        }
        return snap;
    }

    private JSONObject mutateAndSnapshot(String projectId, Runnable mutator, java.util.function.Consumer<String> broadcaster) {
        if (mutator != null) {
            mutator.run();
        }
        ProjectRef ref = projectStore.get(projectId);
        if (ref != null) {
            ref.dirty = true;
        }
        JSONObject snapshot = snapshotPayload(projectId);
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("snapshot", snapshot);
        if (broadcaster != null) {
            JSONObject evt = new JSONObject();
            evt.put("event", "sceneflow.snapshot");
            evt.put("projectId", projectId);
            evt.put("snapshot", snapshot);
            broadcaster.accept(evt.toString());
        }
        return resp;
    }

    private JSONArray serializeCommands(List<de.dfki.vsm.model.sceneflow.glue.command.Command> commands) {
        if (commands == null) {
            return new JSONArray();
        }
        return new JSONArray(commands.stream().map(cmd -> {
            JSONObject obj = new JSONObject();
            obj.put("cmd", cmd.toString());
            return obj;
        }).collect(Collectors.toList()));
    }

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

    private String loadFile(String baseDir, String filename) {
        try {
            Path p = Paths.get(baseDir, filename);
            if (Files.exists(p)) {
                return Files.readString(p);
            }
        } catch (Exception ignored) {
        }
        return "";
    }

    /**
     * Register an externally-loaded RunTimeProject with this server.
     * This allows projects loaded via command-line to be accessible via API.
     *
     * @param project The already-loaded RunTimeProject
     * @return The assigned project ID
     */
    public String registerProject(RunTimeProject project) {
        if (project == null) {
            return null;
        }
        String path = project.getProjectPath();
        String name = project.getProjectName();
        if (name == null || name.isBlank()) {
            name = fileName(path);
        }

        // Check if already registered by path
        for (ProjectRef ref : projectStore.values()) {
            if (ref.path != null && !ref.path.isBlank() && ref.path.equals(path)) {
                // Update the runtime project reference
                ref.runtimeProject = project;
                return ref.id;
            }
        }

        // Register as new project
        String id = UUID.randomUUID().toString();
        ProjectRef ref = new ProjectRef(id, name, path);
        ref.runtimeProject = project;
        ref.nodes = serializeNodes(project);
        ref.edges = serializeEdges(project);
        ref.comments = serializeComments(project);
        ref.runtimeState = project.isRunning() ? "running" : "stopped";
        projectStore.put(id, ref);
        sLogger.message("Registered project: " + name + " (id=" + id + ")");
        return id;
    }

    /**
     * Get the RunTimeProject for a given project ID.
     *
     * @param projectId The project ID
     * @return The RunTimeProject or null if not found
     */
    public RunTimeProject getProject(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        return ref != null ? ref.runtimeProject : null;
    }

    /**
     * Set the runtime state for a project.
     * Use this after externally starting/stopping a project (e.g., via autostart).
     *
     * @param projectId The project ID
     * @param state The runtime state ("running", "paused", "stopped")
     */
    public void setProjectRuntimeState(String projectId, String state) {
        ProjectRef ref = projectStore.get(projectId);
        if (ref != null) {
            ref.runtimeState = state;
            sLogger.message("Project " + projectId + " runtime state set to: " + state);
        }
    }

    private String ensureProject(String path, String name) {
        // Reuse existing entry by path if present.
        for (ProjectRef ref : projectStore.values()) {
            if (ref.path != null && !ref.path.isBlank() && ref.path.equals(path)) {
                return ref.id;
            }
        }

        // Try to load a real project if a path was given
        RunTimeProject rtp = null;
        if (path != null && !path.isBlank()) {
            try {
                rtp = new RunTimeProject(new java.io.File(path));
                rtp.parse(path);
                List<JSONObject> nodes = serializeNodes(rtp);
                List<JSONObject> edges = serializeEdges(rtp);
                List<JSONObject> comments = serializeComments(rtp);
                String id = UUID.randomUUID().toString();
                ProjectRef ref = new ProjectRef(id, name, path);
                ref.runtimeProject = rtp;
                ref.nodes = nodes;
                ref.edges = edges;
                ref.comments = comments;
                ref.runtimeState = "stopped";
                projectStore.put(id, ref);
                return id;
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to load project from " + path + ": " + exc.getMessage());
            }
        }

        String id = UUID.randomUUID().toString();
        ProjectRef ref = new ProjectRef(id, name, path);
        ref.runtimeProject = rtp;
        ref.runtimeState = "stopped";
        ref.nodes = new ArrayList<>();
        ref.edges = new ArrayList<>();
        ref.comments = new ArrayList<>();
        projectStore.put(id, ref);
        return id;
    }

    private void markClean(String pid) {
        if (pid == null) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref != null) {
            ref.dirty = false;
        }
    }

    private static class ProjectRef {
        final String id;
        String name;
        String path;
        boolean dirty;
        RunTimeProject runtimeProject;
        List<JSONObject> nodes = new ArrayList<>();
        List<JSONObject> edges = new ArrayList<>();
        List<JSONObject> comments = new ArrayList<>();
        String runtimeState = "stopped";

        ProjectRef(String id, String name, String path) {
            this.id = id;
            this.name = name == null ? "" : name;
            this.path = path == null ? "" : path;
            this.dirty = false;
        }
    }

    private void handleImage(Context ctx) {
        String file = ctx.pathParam("file");
        if (file == null || file.isEmpty()) {
            ctx.status(404);
            return;
        }
        String resource = "images/" + file;
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(resource)) {
            if (is == null) {
                ctx.status(404);
                return;
            }
            byte[] bytes = is.readAllBytes();
            ctx.header(Header.CACHE_CONTROL, "public, max-age=3600");
            ctx.contentType(detectMime(file));
            ctx.result(bytes);
        } catch (Exception exc) {
            ctx.status(500).result("Error loading image");
            sLogger.warning("Warning: Cannot serve image '" + resource + "': " + exc.getMessage());
        }
    }

    private String detectMime(String file) {
        String lower = file.toLowerCase();
        if (lower.endsWith(".svg")) return "image/svg+xml";
        if (lower.endsWith(".png")) return "image/png";
        if (lower.endsWith(".jpg") || lower.endsWith(".jpeg")) return "image/jpeg";
        if (lower.endsWith(".gif")) return "image/gif";
        return "application/octet-stream";
    }

    private void broadcast(WsContext origin, String message) {
        for (WsContext session : wsSessions) {
            try {
                if (session.session.isOpen() && (origin == null || session != origin)) {
                    session.send(message);
                }
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to broadcast WS message: " + exc.getMessage());
            }
        }
    }

    private Path resolveResourcePath(String directory) {
        // First try the working directory (useful during development).
        Path fsPath = Paths.get(directory);
        if (Files.exists(fsPath)) {
            return fsPath;
        }
        // Then try to resolve from the classpath (packaged in the jar).
        try {
            ClassLoader cl = getClass().getClassLoader();
            URL url = cl.getResource(directory.endsWith("/") ? directory : directory + "/");
            if (url != null) {
                URI uri = url.toURI();
                return Paths.get(uri);
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot resolve resource path '" + directory + "': " + exc.getMessage());
        }
        return null;
    }

    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }
}
