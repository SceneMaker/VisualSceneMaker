package de.dfki.vsm.web;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.acticon.ActiconAction;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.gesticon.GesticonAgent;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.gesticon.GesticonGesture;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.LLMConfig;
import de.dfki.vsm.model.project.property.ExportableProperties;
import de.dfki.vsm.model.project.property.ProjectProperty;
import de.dfki.vsm.model.project.property.value.ProjectValueProperty;
import de.dfki.vsm.model.project.property.value.ValueTYPE;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.ScriptDiagnostics;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
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
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.ListTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.MemberDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.StructTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ConstructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ParenExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.ContainsList;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryContains;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryRunTimeOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.InStateQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.PrologQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.RandomQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.TimeoutQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActionActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayDialogAction;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.StopActionActivity;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.GlueParser;
import de.dfki.vsm.runtime.project.RunTimeProject;
// PluginInterfaceRegistry no longer used - plugin interfaces now loaded from classpath via plugin-properties.json
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.NodeExecutedEvent;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.EdgeExecutedEvent;
import de.dfki.vsm.event.event.NodeTerminatedEvent;
import de.dfki.vsm.event.event.TimeoutEdgeStartedEvent;
import de.dfki.vsm.event.event.SceneExecutedEvent;
import de.dfki.vsm.event.event.SceneDoneEvent;
import de.dfki.vsm.event.event.SceneStoppedEvent;
import de.dfki.vsm.event.event.TurnExecutedEvent;
import de.dfki.vsm.event.event.TurnDoneEvent;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.EventValue;
import java.util.List;
import java.util.ArrayList;
import de.dfki.vsm.util.llm.LLMSupport;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import io.javalin.Javalin;
import io.javalin.core.util.Header;
import io.javalin.http.staticfiles.Location;
import io.javalin.http.Context;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.util.ConfigurationBuilder;

import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.Enumeration;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.lang.reflect.Modifier;
import java.util.jar.Manifest;
import java.util.jar.Attributes;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.time.Duration;
import de.dfki.vsm.model.visicon.VisiconAgent;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.model.visicon.VisiconViseme;

public final class WebUiServer implements EventListener {

    private static final Map<String, ExportablePropertyEntry> EXPORTABLE_PROPERTY_PROVIDERS = new HashMap<>();

    private static final class ExportablePropertyEntry {
        private final String providerClass;
        private final JSONObject pluginSpec;      // config.required, config.optional, config.pluginSpecific, templates
        private final JSONObject agentSpec;       // agent-level config
        private final JSONObject pluginMeta;      // plugin.id, plugin.name, plugin.description, plugin.tags
        private final JSONObject categories;      // categories.primary, categories.secondary
        private final JSONArray commands;         // commands array
        private final JSONObject variables;       // variables.writes, variables.reads

        private ExportablePropertyEntry(String providerClass, JSONObject pluginSpec, JSONObject agentSpec,
                                         JSONObject pluginMeta, JSONObject categories,
                                         JSONArray commands, JSONObject variables) {
            this.providerClass = providerClass;
            this.pluginSpec = pluginSpec;
            this.agentSpec = agentSpec;
            this.pluginMeta = pluginMeta;
            this.categories = categories;
            this.commands = commands;
            this.variables = variables;
        }

        /**
         * Converts this entry to the sceneflow-interface.json format for compatibility.
         */
        JSONObject toInterfaceJson(String className) {
            JSONObject out = new JSONObject();
            out.put("schemaVersion", "1.0");

            // Plugin metadata
            JSONObject plugin = new JSONObject();
            if (pluginMeta != null) {
                plugin.put("id", pluginMeta.optString("id", ""));
                plugin.put("name", pluginMeta.optString("name", ""));
                plugin.put("className", pluginMeta.optString("className", className));
                plugin.put("description", pluginMeta.optString("description", ""));
                plugin.put("tags", pluginMeta.optJSONArray("tags") != null ? pluginMeta.optJSONArray("tags") : new JSONArray());
            } else {
                // Derive from className
                String simpleName = className.contains(".") ? className.substring(className.lastIndexOf('.') + 1) : className;
                plugin.put("id", simpleName.toLowerCase());
                plugin.put("name", simpleName);
                plugin.put("className", className);
                plugin.put("description", "");
                plugin.put("tags", new JSONArray());
            }
            out.put("plugin", plugin);

            // Categories
            if (categories != null) {
                out.put("categories", categories);
            } else {
                JSONObject defaultCategories = new JSONObject();
                defaultCategories.put("primary", "unknown");
                defaultCategories.put("secondary", new JSONArray());
                out.put("categories", defaultCategories);
            }

            // Commands
            out.put("commands", commands != null ? commands : new JSONArray());

            // Variables
            if (variables != null) {
                out.put("writes", variables.optJSONArray("writes") != null ? variables.optJSONArray("writes") : new JSONArray());
                out.put("reads", variables.optJSONArray("reads") != null ? variables.optJSONArray("reads") : new JSONArray());
            } else {
                out.put("writes", new JSONArray());
                out.put("reads", new JSONArray());
            }

            // Agent spec
            if (agentSpec != null) {
                out.put("agentSpec", agentSpec);
            }

            // Config (from pluginSpec)
            if (pluginSpec != null) {
                JSONArray configArray = new JSONArray();
                addConfigEntries(configArray, pluginSpec.optJSONArray("required"), true);
                addConfigEntries(configArray, pluginSpec.optJSONArray("optional"), false);
                out.put("config", configArray);
            } else {
                out.put("config", new JSONArray());
            }

            return out;
        }

        private void addConfigEntries(JSONArray configArray, JSONArray source, boolean required) {
            if (source == null) return;
            for (int i = 0; i < source.length(); i++) {
                JSONObject entry = source.optJSONObject(i);
                if (entry == null) continue;
                JSONObject configEntry = new JSONObject();
                configEntry.put("key", entry.optString("name", ""));
                configEntry.put("type", entry.optString("type", "string"));
                configEntry.put("required", required);
                configEntry.put("default", entry.opt("default") != null ? String.valueOf(entry.opt("default")) : "");
                configEntry.put("description", entry.optString("description", ""));
                configArray.put(configEntry);
            }
        }
    }

    /**
     * Server operation modes.
     * RUNTIME_ONLY: Single project, runtime control, no editing commands.
     * FULL_EDITOR: Multi-project, full editing and runtime support.
     */
    public enum ServerMode {
        RUNTIME_ONLY,
        FULL_EDITOR
    }

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";
    private static final int RECENT_MAX = 8;
    private static WebUiServer sInstance;
    private static final String DEMO_PROJECT_ID = "demo-project";

    private Javalin mApp;
    private boolean mAllowExternal = false;
    private ServerMode mMode = ServerMode.FULL_EDITOR;
    private String mAuthToken;
    private final Map<String, ProjectRef> projectStore = new HashMap<>();
    private final java.util.Set<WsContext> wsSessions = ConcurrentHashMap.newKeySet();

    // Edge layout service (dock points, normalization, straightening)
    private final EdgeLayoutService mEdgeLayout = new EdgeLayoutService();

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
        loadExportablePropertyProviders();
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

    /**
     * Starts the server in a specific mode with auth token support.
     * Used by RuntimeMain for RUNTIME_ONLY mode.
     */
    public void start(int port, String bindHost, String token, ServerMode mode) {
        mMode = mode;
        mAuthToken = (token != null) ? token : generateToken();
        start(port, "0.0.0.0".equals(bindHost));
    }

    public ServerMode getMode() {
        return mMode;
    }

    public String getAuthToken() {
        return mAuthToken;
    }

    /**
     * Loads a project from the filesystem into the project store.
     * Used primarily in RUNTIME_ONLY mode for single-project loading.
     */
    public boolean loadProject(String path) {
        // Unload any existing project in the store
        for (ProjectRef ref : projectStore.values()) {
            if (ref.runtimeProject != null && ref.runtimeProject.wasExecuted()) {
                ref.runtimeProject.unload();
            }
        }
        projectStore.clear();

        try {
            RunTimeProject rtp = new RunTimeProject(new File(path));
            if (rtp.parse(path)) {
                String pid = UUID.randomUUID().toString();
                if (rtp.launch()) {
                    String name = rtp.getProjectName() != null ? rtp.getProjectName() : new File(path).getName();
                    ProjectRef ref = new ProjectRef(pid, name, path);
                    ref.runtimeProject = rtp;
                    ref.runtimeState = "stopped";
                    projectStore.put(pid, ref);
                    broadcastRuntimeState(pid, "stopped");
                    sLogger.message("Loaded project: " + path);
                    return true;
                } else {
                    sLogger.failure("Failed to launch project: " + path);
                }
            } else {
                sLogger.failure("Failed to parse project: " + path);
            }
        } catch (Exception e) {
            sLogger.failure("Error loading project: " + e.getMessage());
        }
        return false;
    }

    /**
     * Starts runtime on the first loaded project.
     * Used primarily in RUNTIME_ONLY mode.
     */
    public boolean startRuntime() {
        if (projectStore.isEmpty()) {
            return false;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject == null) {
            return false;
        }
        if (ref.runtimeProject.start()) {
            ref.runtimeState = "running";
            broadcastRuntimeState(entry.getKey(), "running");
            sLogger.message("Runtime started");
            return true;
        }
        return false;
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

        } else if (event instanceof SceneExecutedEvent) {
            SceneObject scene = ((SceneExecutedEvent) event).getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.playing");

        } else if (event instanceof SceneDoneEvent) {
            SceneObject scene = ((SceneDoneEvent) event).getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.done");

        } else if (event instanceof TurnExecutedEvent) {
            SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
            if (turn == null) return;
            payload.put("speaker", turn.getSpeaker());
            payload.put("lower", turn.getLower());
            payload.put("upper", turn.getUpper());
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.turn");

        } else if (event instanceof TurnDoneEvent) {
            SceneTurn turn = ((TurnDoneEvent) event).getTurn();
            if (turn == null) return;
            payload.put("speaker", turn.getSpeaker());
            payload.put("lower", turn.getLower());
            payload.put("upper", turn.getUpper());
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.turnDone");

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
        return SceneFlowSnapshotBuilder.getEdgeTypeLowercase(edge);
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

    private void broadcastRuntimeState(String projectId, String state) {
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        message.put("channel", "runtime");
        message.put("event", "runtime.state");
        JSONObject payload = new JSONObject();
        payload.put("state", state);
        payload.put("status", state);
        if (projectId != null) {
            payload.put("projectId", projectId);
        }
        message.put("payload", payload);
        broadcastToAll(message.toString());
    }

    public String getLocalUrl() {
        return "http://127.0.0.1:" + mPort;
    }

    private void registerRoutes() {
        // Common endpoints (available in both modes)
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/token", this::handleToken);
        mApp.get(API_PREFIX + "/projects", this::handleProjects);
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.get(API_PREFIX + "/projects/samples", ctx -> handleStaticProjectList(ctx, "res/prj"));
        mApp.get(API_PREFIX + "/projects/tutorials", ctx -> handleStaticProjectList(ctx, "res/tutorials"));
        mApp.get(API_PREFIX + "/preferences", this::handlePreferences);
        mApp.get(API_PREFIX + "/devices", this::handleDevices);
        mApp.get(API_PREFIX + "/projects/{pid}/config", this::handleEditorConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config/keys", this::handleProjectConfigKeys);
        mApp.get(API_PREFIX + "/projects/{pid}/validate/vars", this::handleProjectVariableValidation);
        mApp.get(API_PREFIX + "/projects/{pid}/plugin-interfaces", this::handlePluginInterfaces);
        mApp.get(API_PREFIX + "/projects/{pid}/script", this::handleScript);
        mApp.get(API_PREFIX + "/projects/{pid}/script/scenes", this::handleScriptScenes);
        mApp.get(API_PREFIX + "/projects/{pid}/script/elements", this::handleScriptElements);
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/runtime", this::handleRuntime);
        mApp.get(API_PREFIX + "/projects/{pid}/history/commands", this::handleCommandLog);
        mApp.post(API_PREFIX + "/projects/{pid}/sceneflow/navigate", this::handleSceneflowNavigate);

        // LLM endpoints (authoring tools)
        mApp.post(API_PREFIX + "/llm/models", this::handleLLMModels);
        mApp.post(API_PREFIX + "/llm/test", this::handleLLMTest);
        mApp.post(API_PREFIX + "/llm/generate", this::handleLLMGenerate);

        // Editor-only endpoints (not available in RUNTIME_ONLY mode)
        if (mMode == ServerMode.FULL_EDITOR) {
            mApp.post(API_PREFIX + "/projects/open", this::handleProjectOpen);
            mApp.post(API_PREFIX + "/projects", this::handleProjectCreate);
            mApp.post(API_PREFIX + "/projects/{pid}/save", this::handleProjectSave);
            mApp.post(API_PREFIX + "/projects/{pid}/save-as", this::handleProjectSaveAs);
            mApp.post(API_PREFIX + "/projects/{pid}/close", this::handleProjectClose);
            mApp.post(API_PREFIX + "/projects/recent/remove", this::handleRecentRemove);
            mApp.post(API_PREFIX + "/projects/recent/add", this::handleRecentAdd);
            mApp.post(API_PREFIX + "/projects/opened", this::handleProjectOpened);
            mApp.post(API_PREFIX + "/projects/saved", this::handleProjectSaved);
            mApp.post(API_PREFIX + "/projects/{pid}/script/diagnostics", this::handleScriptDiagnostics);
            mApp.get("/images/{file}", this::handleImage);
        }

        // Runtime-only REST endpoints (direct runtime control via REST)
        if (mMode == ServerMode.RUNTIME_ONLY) {
            mApp.post(API_PREFIX + "/runtime/load", this::handleRuntimeLoad);
            mApp.post(API_PREFIX + "/runtime/start", this::handleRuntimeStart);
            mApp.post(API_PREFIX + "/runtime/pause", this::handleRuntimePause);
            mApp.post(API_PREFIX + "/runtime/resume", this::handleRuntimeResume);
            mApp.post(API_PREFIX + "/runtime/stop", this::handleRuntimeStopRest);
            mApp.post(API_PREFIX + "/runtime/unload", this::handleRuntimeUnload);
            mApp.get(API_PREFIX + "/runtime/status", this::handleRuntimeStatus);
            mApp.get(API_PREFIX + "/runtime/variables", this::handleRuntimeVariables);
            mApp.get(API_PREFIX + "/runtime/sceneflow", this::handleRuntimeSceneflowLegacy);
        }

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
    }

    // ========== Runtime-Only REST Endpoints ==========

    private void handleRuntimeLoad(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("projectPath", "");
        if (path.isEmpty()) {
            ctx.status(400).result("Missing projectPath");
            return;
        }
        if (loadProject(path)) {
            Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
            ProjectRef ref = entry.getValue();
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", ref.runtimeState);
            response.put("projectPath", ref.path);
            response.put("projectName", ref.runtimeProject != null ? ref.runtimeProject.getProjectName() : "");
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to load project");
        }
    }

    private void handleRuntimeStart(Context ctx) {
        if (projectStore.isEmpty()) {
            ctx.status(400).result("No project loaded");
            return;
        }
        if (startRuntime()) {
            Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", entry.getValue().runtimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to start runtime");
        }
    }

    private void handleRuntimePause(Context ctx) {
        if (projectStore.isEmpty()) {
            ctx.status(400).result("No project loaded");
            return;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject != null && ref.runtimeProject.pause()) {
            ref.runtimeState = "paused";
            broadcastRuntimeState(entry.getKey(), "paused");
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", ref.runtimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to pause runtime");
        }
    }

    private void handleRuntimeResume(Context ctx) {
        if (projectStore.isEmpty()) {
            ctx.status(400).result("No project loaded");
            return;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject != null && ref.runtimeProject.proceed()) {
            ref.runtimeState = "running";
            broadcastRuntimeState(entry.getKey(), "running");
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("state", ref.runtimeState);
            writeJson(ctx, response);
        } else {
            ctx.status(500).result("Failed to resume runtime");
        }
    }

    private void handleRuntimeStopRest(Context ctx) {
        if (projectStore.isEmpty()) {
            ctx.status(400).result("No project loaded");
            return;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject != null && ref.runtimeProject.isRunning()) {
            ref.runtimeProject.abort();
        }
        ref.runtimeState = "stopped";
        broadcastRuntimeState(entry.getKey(), "stopped");
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", ref.runtimeState);
        writeJson(ctx, response);
    }

    private void handleRuntimeUnload(Context ctx) {
        if (projectStore.isEmpty()) {
            ctx.status(400).result("No project loaded");
            return;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject != null && ref.runtimeProject.wasExecuted()) {
            ref.runtimeProject.unload();
        }
        projectStore.remove(entry.getKey());
        broadcastRuntimeState(null, "stopped");
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "stopped");
        writeJson(ctx, response);
    }

    private void handleRuntimeStatus(Context ctx) {
        JSONObject response = new JSONObject();
        if (!projectStore.isEmpty()) {
            Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
            ProjectRef ref = entry.getValue();
            response.put("state", ref.runtimeState);
            response.put("projectPath", ref.path != null ? ref.path : "");
            if (ref.runtimeProject != null) {
                response.put("projectName", ref.runtimeProject.getProjectName());
                response.put("isRunning", ref.runtimeProject.isRunning());
                response.put("isPaused", ref.runtimeProject.isPaused());
            }
        } else {
            response.put("state", "stopped");
        }
        writeJson(ctx, response);
    }

    private void handleRuntimeVariables(Context ctx) {
        JSONObject response = new JSONObject();
        if (!projectStore.isEmpty()) {
            Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
            ProjectRef ref = entry.getValue();
            if (ref.runtimeProject != null && ref.runtimeProject.getSceneFlow() != null) {
                try {
                    SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                    Map<String, DataTypeDefinition> typeMap = new HashMap<>();
                    for (DataTypeDefinition def : sceneFlow.getTypeDefList()) {
                        typeMap.put(def.getName(), def);
                    }
                    JSONArray vars = new JSONArray();
                    for (VariableDefinition def : sceneFlow.getVarDefList()) {
                        vars.put(variableToJsonCore(def, typeMap, "global", ref.runtimeProject));
                    }
                    response.put("variables", vars);
                } catch (Exception e) {
                    response.put("variables", new JSONArray());
                }
            } else {
                response.put("variables", new JSONArray());
            }
        } else {
            response.put("variables", new JSONArray());
        }
        writeJson(ctx, response);
    }

    private void handleRuntimeSceneflowLegacy(Context ctx) {
        if (projectStore.isEmpty()) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            writeJson(ctx, empty);
            return;
        }
        Map.Entry<String, ProjectRef> entry = projectStore.entrySet().iterator().next();
        ProjectRef ref = entry.getValue();
        if (ref.runtimeProject == null || ref.runtimeProject.getSceneFlow() == null) {
            JSONObject empty = new JSONObject();
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            writeJson(ctx, empty);
            return;
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        JSONObject snapshot = SceneFlowSnapshotBuilder.createSnapshot(
                entry.getKey(), sceneFlow, sceneFlow, 90, 90, null);
        writeJson(ctx, snapshot);
    }

    // ========== Standard REST Endpoints ==========

    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Web");
        info.put("port", mPort);
        info.put("tokenRequired", true);
        String buildDate = "unknown";
        String revision = "unknown";
        String version = "unknown";
        String build = "unknown";
        try {
            java.io.InputStream stream = WebUiServer.class.getClassLoader().getResourceAsStream("META-INF/MANIFEST.MF");
            if (stream != null) {
                try (java.io.InputStream in = stream) {
                    java.util.jar.Manifest manifest = new java.util.jar.Manifest(in);
                    java.util.jar.Attributes attrs = manifest.getMainAttributes();
                    buildDate = attrs.getValue("Build-Date") != null ? attrs.getValue("Build-Date") : buildDate;
                    revision = attrs.getValue("Build-Revision") != null ? attrs.getValue("Build-Revision") : revision;
                    version = attrs.getValue("Last-Tag") != null ? attrs.getValue("Last-Tag") : version;
                    build = attrs.getValue("build") != null ? attrs.getValue("build") : build;
                }
            }
        } catch (Exception ignored) {
            // Leave defaults when manifest is unavailable.
        }
        String sysBuildDate = System.getProperty("vsm.buildDate");
        String sysRevision = System.getProperty("vsm.buildRevision");
        String sysVersion = System.getProperty("vsm.version");
        String sysBuild = System.getProperty("vsm.build");
        if (buildDate.equals("unknown") && sysBuildDate != null && !sysBuildDate.isBlank()) {
            buildDate = sysBuildDate;
        }
        if (revision.equals("unknown") && sysRevision != null && !sysRevision.isBlank()) {
            revision = sysRevision;
        }
        if (version.equals("unknown") && sysVersion != null && !sysVersion.isBlank()) {
            version = sysVersion;
        }
        if (build.equals("unknown") && sysBuild != null && !sysBuild.isBlank()) {
            build = sysBuild;
        }
        info.put("buildDate", buildDate);
        info.put("revision", revision);
        info.put("version", version);
        info.put("build", build);
        info.put("mode", mMode.name().toLowerCase());
        writeJson(ctx, info);
    }

    private void handleToken(Context ctx) {
        JSONObject token = new JSONObject();
        token.put("token", mAuthToken != null ? mAuthToken : "dev-token");
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
            RecentProjectStats stats = computeRecentProjectStats(path);
            if (stats != null) {
                entry.put("stats", stats.toJson());
            }
            recent.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("projects", recent);
        writeJson(ctx, response);
    }

    private static final class RecentProjectStats {
        private int superNodes;
        private int nodes;
        private int commands;
        private int scenes;
        private List<RecentSceneLanguageInfo> sceneLanguages = new ArrayList<>();
        private List<RecentPluginInfo> plugins = new ArrayList<>();

        public JSONObject toJson() {
            JSONObject json = new JSONObject();
            json.put("superNodes", superNodes);
            json.put("nodes", nodes);
            json.put("commands", commands);
            json.put("scenes", scenes);
            JSONArray sceneLanguagesArray = new JSONArray();
            for (RecentSceneLanguageInfo language : sceneLanguages) {
                sceneLanguagesArray.put(language.toJson());
            }
            json.put("sceneLanguages", sceneLanguagesArray);
            JSONArray pluginArray = new JSONArray();
            for (RecentPluginInfo plugin : plugins) {
                pluginArray.put(plugin.toJson());
            }
            json.put("plugins", pluginArray);
            return json;
        }
    }

    private static final class RecentSceneLanguageInfo {
        private String language;
        private int count;

        public JSONObject toJson() {
            JSONObject json = new JSONObject();
            json.put("language", language);
            json.put("count", count);
            return json;
        }
    }

    private static final class RecentPluginInfo {
        private String name;
        private String className;
        private boolean present;

        public JSONObject toJson() {
            JSONObject json = new JSONObject();
            json.put("name", name);
            json.put("className", className);
            json.put("present", present);
            return json;
        }
    }

    private static final class SceneFlowStats {
        private int superNodes;
        private int nodes;
        private int commands;
    }

    private RecentProjectStats computeRecentProjectStats(String path) {
        if (path == null || path.isBlank()) {
            return null;
        }
        File base = new File(path);
        if (base.isFile() && "project.xml".equalsIgnoreCase(base.getName())) {
            base = base.getParentFile();
        }
        if (base == null || !base.exists()) {
            return null;
        }
        try {
            RunTimeProject runtimeProject = new RunTimeProject(base);
            if (!runtimeProject.parseForInformation(base.getPath())) {
                return null;
            }
            RecentProjectStats stats = new RecentProjectStats();
            ProjectConfig config = runtimeProject.getProjectConfig();
            if (config != null && config.getPluginConfigList() != null) {
                Set<String> seen = new HashSet<>();
                for (PluginConfig plugin : config.getPluginConfigList()) {
                    String className = plugin.getClassName();
                    String name = plugin.getPluginName();
                    String label = (name != null && !name.isBlank()) ? name : className;
                    if (label == null || label.isBlank()) {
                        continue;
                    }
                    String key = (className == null ? "" : className) + "|" + label;
                    if (!seen.add(key)) {
                        continue;
                    }
                    RecentPluginInfo info = new RecentPluginInfo();
                    info.name = label;
                    info.className = className;
                    info.present = isPluginClassPresent(className);
                    stats.plugins.add(info);
                }
            }
            SceneFlow sceneFlow = runtimeProject.getSceneFlow();
            if (sceneFlow != null) {
                SceneFlowStats flowStats = collectSceneFlowStats(sceneFlow);
                stats.superNodes = flowStats.superNodes;
                stats.nodes = flowStats.nodes;
                stats.commands = flowStats.commands;
            }
            de.dfki.vsm.model.scenescript.SceneScript script = runtimeProject.getSceneScript();
            if (script != null) {
                stats.scenes = script.getSceneListSize();
                Map<String, Integer> perLanguage = new java.util.TreeMap<>();
                for (SceneObject scene : script.getSceneList()) {
                    if (scene == null) continue;
                    String language = scene.getLanguage();
                    String key = language == null ? "" : language.trim();
                    perLanguage.merge(key, 1, Integer::sum);
                }
                for (Map.Entry<String, Integer> entry : perLanguage.entrySet()) {
                    RecentSceneLanguageInfo languageInfo = new RecentSceneLanguageInfo();
                    languageInfo.language = entry.getKey();
                    languageInfo.count = entry.getValue();
                    stats.sceneLanguages.add(languageInfo);
                }
            }
            return stats;
        } catch (Exception e) {
            sLogger.warning("Failed to compute recent project stats for " + path + ": " + e.getMessage());
            return null;
        }
    }

    private boolean isPluginClassPresent(String className) {
        if (className == null || className.isBlank()) {
            return false;
        }
        try {
            Class.forName(className, false, getClass().getClassLoader());
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }

    private SceneFlowStats collectSceneFlowStats(SuperNode node) {
        SceneFlowStats stats = new SceneFlowStats();
        stats.commands += countCommandsForNode(node);
        for (BasicNode basicNode : node.getNodeList()) {
            stats.nodes += 1;
            stats.commands += countCommandsForNode(basicNode);
        }
        for (SuperNode superNode : node.getSuperNodeList()) {
            stats.superNodes += 1;
            SceneFlowStats childStats = collectSceneFlowStats(superNode);
            stats.superNodes += childStats.superNodes;
            stats.nodes += childStats.nodes;
            stats.commands += childStats.commands;
        }
        return stats;
    }

    private int countCommandsForNode(BasicNode node) {
        if (node == null) {
            return 0;
        }
        int count = 0;
        if (node.getCmdList() != null) {
            count += node.getCmdList().size();
        }
        if (node.getEdgeList() != null) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edge != null && edge.getCmdList() != null) {
                    count += edge.getCmdList().size();
                }
            }
        }
        return count;
    }

    private List<JSONObject> collectUndefinedVariables(SceneFlow sceneFlow) {
        if (sceneFlow == null) {
            return new ArrayList<>();
        }
        Set<String> missingKeys = new HashSet<>();
        List<JSONObject> missing = new ArrayList<>();
        collectUndefinedVariables(sceneFlow, sceneFlow, missingKeys, missing);
        return missing;
    }

    private void collectUndefinedVariables(SuperNode root, SuperNode current, Set<String> missingKeys, List<JSONObject> missing) {
        String currentId = current.getId();
        if (current.getCmdList() != null) {
            for (Command command : current.getCmdList()) {
                collectUndefinedFromCommand(root, command, missingKeys, missing, "SuperNode " + currentId);
            }
        }
        if (current.getEdgeList() != null) {
            for (AbstractEdge edge : current.getEdgeList()) {
                String edgeContext = buildEdgeContext(edge);
                if (edge.getCmdList() != null) {
                    for (Command command : edge.getCmdList()) {
                        collectUndefinedFromCommand(root, command, missingKeys, missing, edgeContext);
                    }
                }
                if (edge instanceof GuargedEdge) {
                    Expression condition = ((GuargedEdge) edge).getCondition();
                    collectUndefinedFromExpression(root, condition, missingKeys, missing, edgeContext + " condition");
                } else if (edge instanceof InterruptEdge) {
                    Expression condition = ((InterruptEdge) edge).getCondition();
                    collectUndefinedFromExpression(root, condition, missingKeys, missing, edgeContext + " condition");
                } else if (edge instanceof TimeoutEdge) {
                    Expression timeoutExpr = ((TimeoutEdge) edge).getExpression();
                    collectUndefinedFromExpression(root, timeoutExpr, missingKeys, missing, edgeContext + " timeout");
                }
            }
        }
        for (BasicNode node : current.getNodeList()) {
            String nodeId = node.getId();
            if (node.getCmdList() != null) {
                for (Command command : node.getCmdList()) {
                    collectUndefinedFromCommand(root, command, missingKeys, missing, "Node " + nodeId);
                }
            }
            if (node.getEdgeList() != null) {
                for (AbstractEdge edge : node.getEdgeList()) {
                    String edgeContext = buildEdgeContext(edge);
                    if (edge.getCmdList() != null) {
                        for (Command command : edge.getCmdList()) {
                            collectUndefinedFromCommand(root, command, missingKeys, missing, edgeContext);
                        }
                    }
                    if (edge instanceof GuargedEdge) {
                        Expression condition = ((GuargedEdge) edge).getCondition();
                        collectUndefinedFromExpression(root, condition, missingKeys, missing, edgeContext + " condition");
                    } else if (edge instanceof InterruptEdge) {
                        Expression condition = ((InterruptEdge) edge).getCondition();
                        collectUndefinedFromExpression(root, condition, missingKeys, missing, edgeContext + " condition");
                    } else if (edge instanceof TimeoutEdge) {
                        Expression timeoutExpr = ((TimeoutEdge) edge).getExpression();
                        collectUndefinedFromExpression(root, timeoutExpr, missingKeys, missing, edgeContext + " timeout");
                    }
                }
            }
        }
        for (SuperNode child : current.getSuperNodeList()) {
            collectUndefinedVariables(root, child, missingKeys, missing);
        }
    }

    private void collectUndefinedFromCommand(SuperNode root, Command command, Set<String> missingKeys, List<JSONObject> missing, String context) {
        if (command == null) {
            return;
        }
        if (command instanceof Assignment) {
            Assignment assignment = (Assignment) command;
            collectUndefinedFromExpression(root, assignment.getLeftExpression(), missingKeys, missing, context + " assignment");
            collectUndefinedFromExpression(root, assignment.getInitExpression(), missingKeys, missing, context + " assignment");
            return;
        }
        if (command instanceof PlayActionActivity) {
            PlayActionActivity invocation = (PlayActionActivity) command;
            collectUndefinedFromExpression(root, invocation.getCommand(), missingKeys, missing, context + " PlayAction");
            for (Expression exp : invocation.getArgList()) {
                collectUndefinedFromExpression(root, exp, missingKeys, missing, context + " PlayAction");
            }
            return;
        }
        if (command instanceof PlayScenesActivity) {
            PlayScenesActivity invocation = (PlayScenesActivity) command;
            collectUndefinedFromExpression(root, invocation.getArgument(), missingKeys, missing, context + " PlayScene");
            for (Expression exp : invocation.getArgList()) {
                collectUndefinedFromExpression(root, exp, missingKeys, missing, context + " PlayScene");
            }
            return;
        }
        if (command instanceof PlayDialogAction) {
            PlayDialogAction invocation = (PlayDialogAction) command;
            collectUndefinedFromExpression(root, invocation.getArg(), missingKeys, missing, context + " PlayDialogAct");
            for (Expression exp : invocation.getArgList()) {
                collectUndefinedFromExpression(root, exp, missingKeys, missing, context + " PlayDialogAct");
            }
            return;
        }
        if (command instanceof StopActionActivity) {
            StopActionActivity invocation = (StopActionActivity) command;
            collectUndefinedFromExpression(root, invocation.getCommand(), missingKeys, missing, context + " StopAction");
            for (Expression exp : invocation.getArgList()) {
                collectUndefinedFromExpression(root, exp, missingKeys, missing, context + " StopAction");
            }
            return;
        }
        if (command instanceof Expression) {
            collectUndefinedFromExpression(root, (Expression) command, missingKeys, missing, context);
        }
    }

    private void collectUndefinedFromExpression(SuperNode root, Expression exp, Set<String> missingKeys, List<JSONObject> missing, String context) {
        if (exp == null) {
            return;
        }
        Set<String> vars = new HashSet<>();
        collectVariableNames(exp, vars);
        for (String var : vars) {
            if (var == null || var.isBlank()) {
                continue;
            }
            if (findVariableDefinitionInHierarchy(root, var) == null) {
                String key = var + "|" + context;
                if (missingKeys.add(key)) {
                    JSONObject entry = new JSONObject();
                    entry.put("name", var);
                    entry.put("context", context);
                    missing.add(entry);
                }
            }
        }
    }

    private void collectVariableNames(Expression exp, Set<String> out) {
        if (exp == null) {
            return;
        }
        if (exp instanceof VariableExpression) {
            if (exp instanceof SimpleVariable) {
                out.add(((SimpleVariable) exp).getName());
            } else if (exp instanceof MemberVariable) {
                out.add(((MemberVariable) exp).getName());
            } else if (exp instanceof ArrayVariable) {
                ArrayVariable arrayVar = (ArrayVariable) exp;
                out.add(arrayVar.getName());
                collectVariableNames(arrayVar.getExpression(), out);
            }
            return;
        }
        if (exp instanceof BinaryExpression) {
            BinaryExpression bin = (BinaryExpression) exp;
            collectVariableNames(bin.getLeftExp(), out);
            collectVariableNames(bin.getRightExp(), out);
            return;
        }
        if (exp instanceof UnaryExpression) {
            collectVariableNames(((UnaryExpression) exp).getExp(), out);
            return;
        }
        if (exp instanceof ParenExpression) {
            collectVariableNames(((ParenExpression) exp).getExp(), out);
            return;
        }
        if (exp instanceof TernaryExpression) {
            TernaryExpression ternary = (TernaryExpression) exp;
            collectVariableNames(ternary.getCondition(), out);
            collectVariableNames(ternary.getThenExp(), out);
            collectVariableNames(ternary.getElseExp(), out);
            return;
        }
        if (exp instanceof ConstructExpression) {
            for (Expression arg : ((ConstructExpression) exp).getArgList()) {
                collectVariableNames(arg, out);
            }
            return;
        }
        if (exp instanceof CallingExpression) {
            for (Expression arg : ((CallingExpression) exp).getArgList()) {
                collectVariableNames(arg, out);
            }
            return;
        }
        if (exp instanceof ArrayExpression) {
            for (Expression arg : ((ArrayExpression) exp).getExpList()) {
                collectVariableNames(arg, out);
            }
            return;
        }
        if (exp instanceof StructExpression) {
            for (Assignment assignment : ((StructExpression) exp).getExpList()) {
                collectVariableNames(assignment.getLeftExpression(), out);
                collectVariableNames(assignment.getInitExpression(), out);
            }
            return;
        }
        if (exp instanceof HistoryValueOf) {
            out.add(((HistoryValueOf) exp).getVar());
            return;
        }
        if (exp instanceof HistoryRunTimeOf) {
            return;
        }
        if (exp instanceof HistoryContains) {
            return;
        }
        if (exp instanceof ContainsList) {
            collectVariableNames(((ContainsList) exp).getListExp(), out);
            collectVariableNames(((ContainsList) exp).getItemExp(), out);
            return;
        }
        if (exp instanceof InStateQuery) {
            return;
        }
        if (exp instanceof PrologQuery) {
            collectVariableNames(((PrologQuery) exp).getExpression(), out);
            return;
        }
        if (exp instanceof RandomQuery) {
            collectVariableNames(((RandomQuery) exp).getExpression(), out);
            return;
        }
        if (exp instanceof TimeoutQuery) {
            collectVariableNames(((TimeoutQuery) exp).getExpression(), out);
        }
    }

    private String buildEdgeContext(AbstractEdge edge) {
        if (edge == null) {
            return "Edge";
        }
        String source = edge.getSourceUnid();
        String target = edge.getTargetUnid();
        String type = edge.getEdgeType() != null ? edge.getEdgeType().name() : "Edge";
        return type + " " + source + "→" + target;
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

    private void handleProjectVariableValidation(Context ctx) {
        String pid = ctx.pathParam("pid").trim();
        if (pid.isEmpty()) {
            writeJson(ctx, errorResponse("BAD_REQUEST", "Missing project id"));
            return;
        }
        ProjectRef ref = projectStore.get(pid);
        SceneFlow sceneFlow = null;
        if (ref != null && ref.runtimeProject != null) {
            sceneFlow = ref.runtimeProject.getSceneFlow();
        }
        if (sceneFlow == null && ref != null && ref.path != null && !ref.path.isBlank()) {
            try {
                RunTimeProject probe = new RunTimeProject(new File(ref.path));
                if (probe.parseForInformation(ref.path)) {
                    sceneFlow = probe.getSceneFlow();
                }
            } catch (Exception e) {
                sLogger.warning("Variable validation failed to parse project: " + e.getMessage());
            }
        }
        List<JSONObject> missing = collectUndefinedVariables(sceneFlow);
        JSONObject response = new JSONObject();
        response.put("missing", new JSONArray(missing));
        writeJson(ctx, response);
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
        String xmlns = Preferences.getProperty("xmlns");
        String xmlnsXsi = Preferences.getProperty("xmlns_xsi");
        String schemaLocation = Preferences.getProperty("xsi_schemeLocation");
        if (xmlns != null) prefs.put("xmlns", xmlns);
        if (xmlnsXsi != null) prefs.put("xmlns_xsi", xmlnsXsi);
        if (schemaLocation != null) prefs.put("xsi_schemeLocation", schemaLocation);
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

    private Properties loadEditorConfig(ProjectRef ref) {
        if (ref == null) {
            Properties props = new Properties();
            ensureEditorConfigDefaults(props);
            return props;
        }
        if (ref.editorConfigLoaded && ref.editorConfig != null) {
            return ref.editorConfig;
        }
        Properties props = new Properties();
        String path = ref.path == null ? "" : ref.path.trim();
        if (!path.isBlank()) {
            java.io.File file = new java.io.File(path, "editorconfig.xml");
            if (file.exists()) {
                try (FileInputStream in = new FileInputStream(file)) {
                    props.loadFromXML(in);
                } catch (IOException exc) {
                    sLogger.warning("Warning: cannot load editorconfig.xml: " + exc.getMessage());
                }
            }
        }
        ensureEditorConfigDefaults(props);
        ref.editorConfig = props;
        ref.editorConfigLoaded = true;
        return props;
    }

    private void ensureEditorConfigDefaults(Properties props) {
        if (props == null) return;
        if (!props.containsKey("node_width")) props.setProperty("node_width", "90");
        if (!props.containsKey("node_height")) props.setProperty("node_height", "90");
        if (!props.containsKey("grid_x")) props.setProperty("grid_x", "1");
        if (!props.containsKey("grid_y")) props.setProperty("grid_y", "1");
        if (!props.containsKey("grid")) props.setProperty("grid", "true");
        if (!props.containsKey("visualization")) props.setProperty("visualization", "true");
        if (!props.containsKey("visualizationtrace")) props.setProperty("visualizationtrace", "true");
        if (!props.containsKey("shownodeid")) props.setProperty("shownodeid", "true");
        if (!props.containsKey("showvariables")) props.setProperty("showvariables", "true");
        if (!props.containsKey("workspace_fontsize")) props.setProperty("workspace_fontsize", "11");
        if (!props.containsKey("scriptfonsize")) props.setProperty("scriptfonsize", "16");
        if (!props.containsKey("scriptfonttype")) props.setProperty("scriptfonttype", "Monospaced");
        if (!props.containsKey("showsceneelements")) props.setProperty("showsceneelements", "false");
        if (!props.containsKey("defaultsupernodename")) props.setProperty("defaultsupernodename", "default");
        if (!props.containsKey("num_magnets")) props.setProperty("num_magnets", "8");
        if (!props.containsKey("autohidebottombar")) props.setProperty("autohidebottombar", "true");
    }

    private JSONObject editorConfigToJson(Properties props) {
        JSONObject json = new JSONObject();
        if (props == null) {
            return json;
        }
        for (String key : props.stringPropertyNames()) {
            json.put(key, props.getProperty(key));
        }
        return json;
    }

    private boolean saveEditorConfig(ProjectRef ref) {
        if (ref == null) return false;
        String path = ref.path == null ? "" : ref.path.trim();
        if (path.isBlank()) return false;
        Properties props = loadEditorConfig(ref);
        java.io.File file = new java.io.File(path, "editorconfig.xml");
        try {
            java.io.File parent = file.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            try (FileOutputStream out = new FileOutputStream(file)) {
                props.storeToXML(out, "VSM Editor Config");
            }
            ref.editorConfigDirty = false;
            return true;
        } catch (IOException exc) {
            sLogger.warning("Warning: cannot save editorconfig.xml: " + exc.getMessage());
            return false;
        }
    }

    private int getEditorConfigInt(ProjectRef ref, String key, int defaultValue) {
        Properties props = loadEditorConfig(ref);
        if (props == null) return defaultValue;
        String value = props.getProperty(key);
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private List<URL> collectDeviceScanUrls() {
        Set<URL> urls = new HashSet<>();
        try {
            java.security.CodeSource codeSource = WebUiServer.class.getProtectionDomain().getCodeSource();
            if (codeSource != null && codeSource.getLocation() != null) {
                URL location = codeSource.getLocation();
                File file = new File(location.toURI());
                if ((file.isFile() && file.getName().endsWith(".jar")) || file.isDirectory()) {
                    urls.add(location);
                }
            }
        } catch (Exception ignored) {
        }
        String classPath = System.getProperty("java.class.path", "");
        if (!classPath.isBlank()) {
            String[] entries = classPath.split(File.pathSeparator);
            for (String entry : entries) {
                if (entry == null || entry.isBlank()) continue;
                File file = new File(entry);
                if (!file.exists()) continue;
                if (file.isDirectory() || (file.isFile() && entry.toLowerCase().endsWith(".jar"))) {
                    try {
                        urls.add(file.toURI().toURL());
                    } catch (Exception ignored) {
                    }
                }
            }
        }
        File baseDir = new File(System.getProperty("user.dir", "."));
        File pluginsDir = new File(baseDir, "plugins");
        if (pluginsDir.isDirectory()) {
            java.util.List<File> jars = new ArrayList<>();
            File[] topLevelJars = pluginsDir.listFiles((dir, name) -> name != null && name.toLowerCase().endsWith(".jar"));
            if (topLevelJars != null) {
                jars.addAll(Arrays.asList(topLevelJars));
            }
            File[] pluginDirs = pluginsDir.listFiles(File::isDirectory);
            if (pluginDirs != null) {
                for (File pluginDir : pluginDirs) {
                    File libsDir = new File(pluginDir, "build/libs");
                    File[] builtJars = libsDir.listFiles((dir, name) -> name != null && name.toLowerCase().endsWith(".jar"));
                    if (builtJars != null) {
                        jars.addAll(Arrays.asList(builtJars));
                    }
                }
            }
            for (File jar : jars) {
                try {
                    urls.add(jar.toURI().toURL());
                } catch (Exception ignored) {
                }
            }
        }
        return new ArrayList<>(urls);
    }

    private void handleDevices(Context ctx) {
        JSONObject response = new JSONObject();
        JSONArray devices = new JSONArray();
        List<URL> scanUrls = collectDeviceScanUrls();
        if (!scanUrls.isEmpty()) {
            try {
                Reflections reflections = new Reflections(new ConfigurationBuilder()
                        .setUrls(scanUrls)
                        .addScanners(new SubTypesScanner(false))
                        .setExpandSuperTypes(false));
                Set<Class<? extends RunTimePlugin>> types = reflections.getSubTypesOf(RunTimePlugin.class);
                Set<String> seen = new HashSet<>();
                for (Class<? extends RunTimePlugin> type : types) {
                    if (type == null) continue;
                    if (Modifier.isAbstract(type.getModifiers()) || Modifier.isInterface(type.getModifiers())) {
                        continue;
                    }
                    String className = type.getCanonicalName();
                    if (className == null || className.isBlank() || !seen.add(className)) {
                        continue;
                    }
                    JSONObject entry = new JSONObject();
                    entry.put("name", type.getSimpleName());
                    entry.put("className", className);
                    devices.put(entry);
                }
            } catch (Exception exc) {
                sLogger.warning("Warning: device discovery failed: " + exc.getMessage());
            }
        }
        response.put("devices", devices);
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
        addRecent(path, fileName(path));
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
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404).result("Project not found");
            return;
        }
        String path = ref.path == null ? "" : ref.path.trim();
        if (path.isBlank()) {
            ctx.status(400).result("Save-as required: no project path");
            return;
        }
        boolean ok = ref.runtimeProject.write(new java.io.File(path));
        if (!ok) {
            ctx.status(500).result("Failed to save project");
            return;
        }
        if (ref.editorConfigLoaded && ref.editorConfigDirty) {
            if (!saveEditorConfig(ref)) {
                ctx.status(500).result("Failed to save editor config");
                return;
            }
        }
        markClean(pid);
        addRecent(path, ref.name);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleProjectSaveAs(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        String pid = ctx.pathParam("pid");
        if (pid.isEmpty()) {
            ctx.status(404).result("Project not found");
            return;
        }
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404).result("Project not found");
            return;
        }
        if (path == null || path.isBlank()) {
            ctx.status(400).result("Missing path");
            return;
        }
        ref.path = path;
        ref.name = fileName(path);
        ref.runtimeProject.setProjectPath(path);
        ref.runtimeProject.setProjectName(ref.name);
        boolean ok = ref.runtimeProject.write(new java.io.File(path));
        if (!ok) {
            ctx.status(500).result("Failed to save project");
            return;
        }
        if (ref.editorConfigLoaded && ref.editorConfigDirty) {
            if (!saveEditorConfig(ref)) {
                ctx.status(500).result("Failed to save editor config");
                return;
            }
        }
        ref.dirty = false;
        addRecent(path, ref.name);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("path", path);
        writeJson(ctx, response);
    }

    private void handleProjectClose(Context ctx) {
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            // Phase 8: Clear dock points before removing project
            ProjectRef ref = projectStore.get(pid);
            if (ref != null && ref.runtimeProject != null) {
                mEdgeLayout.clearDockPointsForProject(ref.runtimeProject.getSceneFlow());
            }
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
            response.put("config", projectConfigToJson(cfg, ref.path));
        } else {
            response.put("config", new JSONObject());
        }
        writeJson(ctx, response);
    }

    private void handlePluginInterfaces(Context ctx) {
        // Build plugin interfaces from the unified plugin-properties.json loaded at startup
        JSONObject out = new JSONObject();
        JSONArray interfaces = new JSONArray();

        for (Map.Entry<String, ExportablePropertyEntry> entry : EXPORTABLE_PROPERTY_PROVIDERS.entrySet()) {
            String className = entry.getKey();
            ExportablePropertyEntry propEntry = entry.getValue();
            JSONObject interfaceJson = propEntry.toInterfaceJson(className);
            interfaces.put(interfaceJson);
        }

        out.put("interfaces", interfaces);
        out.put("errors", new JSONArray());
        out.put("source", "classpath");
        writeJson(ctx, out);
    }

    private JSONObject projectConfigToJson(ProjectConfig cfg, String path) {
        JSONObject cfgJson = new JSONObject();
        cfgJson.put("name", cfg.getProjectName());
        cfgJson.put("path", path == null ? "" : path);
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
        JSONArray llmsJson = new JSONArray();
        Set<String> seenLLMs = new HashSet<>();
        for (LLMConfig llm : cfg.getLLMConfigList()) {
            String llmName = llm.getLLMName() == null ? "" : llm.getLLMName();
            String llmKey = llmName.trim().toLowerCase();
            if (!llmKey.isEmpty() && !seenLLMs.add(llmKey)) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("name", llm.getLLMName());
            entry.put("features", configFeaturesToJson(llm.getEntryList()));
            llmsJson.put(entry);
        }
        cfgJson.put("plugins", pluginsJson);
        cfgJson.put("agents", agentsJson);
        cfgJson.put("llms", llmsJson);
        cfgJson.put("player", playerJson);
        // Serialize LLM prompts
        JSONObject llmPromptsJson = new JSONObject();
        de.dfki.vsm.model.config.ConfigElement prompts = cfg.getLLMPrompts();
        String formatPrompt = prompts.getProperty("formatPrompt");
        llmPromptsJson.put("formatPrompt", formatPrompt != null ? formatPrompt : "");
        JSONArray actionPrompts = new JSONArray();
        for (int i = 0; ; i++) {
            String val = prompts.getProperty("actionPrompt." + i);
            if (val == null) break;
            actionPrompts.put(val);
        }
        llmPromptsJson.put("actionPrompts", actionPrompts);
        cfgJson.put("llmPrompts", llmPromptsJson);
        cfgJson.put("sceneTitleConcepts", configConceptsToJson(cfg.getSceneTitleConcepts()));
        sLogger.message("[PROJECT-CONFIG] Serialized plugins=" + pluginsJson.length()
                + " agents=" + agentsJson.length() + " llms=" + llmsJson.length());
        return cfgJson;
    }

    private void applyProjectConfigFromJson(ProjectRef ref, ProjectConfig cfg, JSONObject configJson) {
        String name = configJson.optString("name", cfg.getProjectName());
        if (ref.runtimeProject != null) {
            ref.runtimeProject.setProjectName(name);
        } else {
            cfg.setProjectName(name);
        }

        JSONArray pluginsJson = configJson.optJSONArray("plugins");
        Map<String, PluginConfig> pluginByName = new HashMap<>();
        for (PluginConfig existing : cfg.getPluginConfigList()) {
            String existingName = existing.getPluginName() == null ? "" : existing.getPluginName();
            String key = existingName.trim().toLowerCase();
            if (!key.isEmpty()) {
                pluginByName.put(key, existing);
            }
        }
        Set<String> seenPlugins = new HashSet<>();
        List<PluginConfig> nextPlugins = new ArrayList<>();
        if (pluginsJson != null) {
            for (int i = 0; i < pluginsJson.length(); i++) {
                JSONObject entry = pluginsJson.optJSONObject(i);
                if (entry == null) continue;
                String pluginName = entry.optString("name", "");
                String pluginKey = pluginName.trim().toLowerCase();
                if (!pluginKey.isEmpty() && !seenPlugins.add(pluginKey)) continue;
                PluginConfig plugin = pluginByName.get(pluginKey);
                ArrayList<ConfigFeature> featuresList = new ArrayList<>();
                JSONArray features = entry.optJSONArray("features");
                if (features != null) {
                    for (int j = 0; j < features.length(); j++) {
                        JSONObject feature = features.optJSONObject(j);
                        if (feature == null) continue;
                        String key = feature.optString("key", "");
                        String value = feature.optString("value", "");
                        if (!key.isEmpty()) {
                            featuresList.add(new ConfigFeature("Feature", key, value));
                        }
                    }
                } else if (plugin != null) {
                    featuresList = plugin.copyEntryList();
                }
                PluginConfig updated = new PluginConfig(
                        entry.optString("type", plugin != null ? plugin.getPluginType() : ""),
                        pluginName.isBlank() && plugin != null ? plugin.getPluginName() : pluginName,
                        entry.optString("className", plugin != null ? plugin.getClassName() : ""),
                        entry.optBoolean("load", plugin != null && plugin.isMarkedtoLoad()),
                        featuresList
                );
                nextPlugins.add(updated);
            }
        }
        cfg.getPluginConfigList().clear();
        cfg.getPluginConfigList().addAll(nextPlugins);

        JSONArray agentsJson = configJson.optJSONArray("agents");
        Map<String, AgentConfig> agentByName = new HashMap<>();
        for (AgentConfig existing : cfg.getAgentConfigList()) {
            String existingName = existing.getAgentName() == null ? "" : existing.getAgentName();
            String key = existingName.trim().toLowerCase();
            if (!key.isEmpty()) {
                agentByName.put(key, existing);
            }
        }
        Set<String> seenAgents = new HashSet<>();
        List<AgentConfig> nextAgents = new ArrayList<>();
        if (agentsJson != null) {
            for (int i = 0; i < agentsJson.length(); i++) {
                JSONObject entry = agentsJson.optJSONObject(i);
                if (entry == null) continue;
                String agentName = entry.optString("name", "");
                String agentKey = agentName.trim().toLowerCase();
                if (!agentKey.isEmpty() && !seenAgents.add(agentKey)) continue;
                AgentConfig agent = agentByName.get(agentKey);
                ArrayList<ConfigFeature> featuresList = new ArrayList<>();
                JSONArray features = entry.optJSONArray("features");
                if (features != null) {
                    for (int j = 0; j < features.length(); j++) {
                        JSONObject feature = features.optJSONObject(j);
                        if (feature == null) continue;
                        String key = feature.optString("key", "");
                        String value = feature.optString("value", "");
                        if (!key.isEmpty()) {
                            featuresList.add(new ConfigFeature("Feature", key, value));
                        }
                    }
                } else if (agent != null) {
                    featuresList = agent.copyEntryList();
                }
                AgentConfig updated = new AgentConfig(
                        agentName.isBlank() && agent != null ? agent.getAgentName() : agentName,
                        entry.optString("device", agent != null ? agent.getDeviceName() : ""),
                        featuresList
                );
                nextAgents.add(updated);
            }
        }
        cfg.getAgentConfigList().clear();
        cfg.getAgentConfigList().addAll(nextAgents);

        JSONArray llmsJson = configJson.optJSONArray("llms");
        List<LLMConfig> nextLLMs = new ArrayList<>();
        Set<String> seenLLMs = new HashSet<>();
        if (llmsJson != null) {
            for (int i = 0; i < llmsJson.length(); i++) {
                JSONObject entry = llmsJson.optJSONObject(i);
                if (entry == null) continue;
                String llmName = entry.optString("name", "");
                String llmKey = llmName.trim().toLowerCase();
                if (!llmKey.isEmpty() && !seenLLMs.add(llmKey)) continue;
                ArrayList<ConfigFeature> featuresList = new ArrayList<>();
                JSONArray features = entry.optJSONArray("features");
                if (features != null) {
                    for (int j = 0; j < features.length(); j++) {
                        JSONObject feature = features.optJSONObject(j);
                        if (feature == null) continue;
                        String key = feature.optString("key", "");
                        String value = feature.optString("value", "");
                        if (!key.isEmpty()) {
                            featuresList.add(new ConfigFeature("Feature", key, value));
                        }
                    }
                }
                nextLLMs.add(new LLMConfig(llmName, featuresList));
            }
        }
        cfg.getLLMConfigList().clear();
        cfg.getLLMConfigList().addAll(nextLLMs);

        JSONObject playerJson = configJson.optJSONObject("player");
        PlayerConfig player = cfg.getPlayerConfig();
        if (player != null) {
            if (playerJson != null) {
                JSONArray features = playerJson.optJSONArray("features");
                if (features != null) {
                    player.getEntryList().clear();
                    for (int j = 0; j < features.length(); j++) {
                        JSONObject feature = features.optJSONObject(j);
                        if (feature == null) continue;
                        String key = feature.optString("key", "");
                        String value = feature.optString("value", "");
                        if (!key.isEmpty()) {
                            player.getEntryList().add(new ConfigFeature("Feature", key, value));
                        }
                    }
                }
            }
        }
        // Apply LLM prompts
        JSONObject llmPromptsJson = configJson.optJSONObject("llmPrompts");
        if (llmPromptsJson != null) {
            de.dfki.vsm.model.config.ConfigElement prompts = cfg.getLLMPrompts();
            prompts.getEntryList().clear();
            String formatPrompt = llmPromptsJson.optString("formatPrompt", "");
            if (!formatPrompt.isEmpty()) {
                prompts.addProperty("formatPrompt", formatPrompt);
            }
            JSONArray actionPrompts = llmPromptsJson.optJSONArray("actionPrompts");
            if (actionPrompts != null) {
                for (int i = 0; i < actionPrompts.length(); i++) {
                    String val = actionPrompts.optString(i, "");
                    if (!val.isEmpty()) {
                        prompts.addProperty("actionPrompt." + i, val);
                    }
                }
            }
            sLogger.message("[PROJECT-CONFIG] Applied llmPrompts: formatPrompt="
                    + (formatPrompt.isEmpty() ? "(empty)" : "(set)")
                    + " actionPrompts=" + (actionPrompts != null ? actionPrompts.length() : 0));
        }
        JSONArray conceptsJson = configJson.optJSONArray("sceneTitleConcepts");
        if (conceptsJson != null) {
            de.dfki.vsm.model.config.ConfigElement concepts = cfg.getSceneTitleConcepts();
            concepts.getEntryList().clear();
            Set<String> seen = new HashSet<>();
            for (int i = 0; i < conceptsJson.length(); i++) {
                String val = conceptsJson.optString(i, "").trim();
                if (val.isEmpty()) continue;
                String key = val.toLowerCase();
                if (!seen.add(key)) continue;
                concepts.getEntryList().add(new ConfigFeature("Concept", val, val));
            }
        }
        sLogger.message("[PROJECT-CONFIG] Applied plugins=" + cfg.getPluginConfigList().size()
                + " agents=" + cfg.getAgentConfigList().size()
                + " llms=" + cfg.getLLMConfigList().size());
    }

    private JSONArray configFeaturesToJson(List<de.dfki.vsm.model.config.ConfigFeature> features) {
        JSONArray list = new JSONArray();
        if (features == null) return list;
        for (de.dfki.vsm.model.config.ConfigFeature feature : features) {
            JSONObject entry = new JSONObject();
            entry.put("key", feature.getKey() == null ? "" : feature.getKey());
            entry.put("value", feature.getValue() == null ? "" : feature.getValue());
            list.put(entry);
        }
        return list;
    }

    private JSONArray configConceptsToJson(de.dfki.vsm.model.config.ConfigElement element) {
        JSONArray list = new JSONArray();
        if (element == null || element.getEntryList() == null) return list;
        for (de.dfki.vsm.model.config.ConfigFeature feature : element.getEntryList()) {
            String value = feature.getValue();
            if (value == null || value.isBlank()) {
                value = feature.getKey();
            }
            if (value != null && !value.isBlank()) {
                list.put(value);
            }
        }
        return list;
    }

    private LLMSupport createLLMSupport(LLMConfig llmConfig) {
        String baseUrl = llmConfig.getProperty("baseUrl", "http://localhost:8234/v1/");
        String apiKey = llmConfig.getProperty("apiKey", null);
        String timeoutStr = llmConfig.getProperty("timeout", "30");
        Duration timeout = Duration.ofSeconds(Long.parseLong(timeoutStr));
        LLMSupport llm = new LLMSupport(baseUrl, apiKey, timeout);
        String model = llmConfig.getProperty("model", null);
        if (model != null && !model.isBlank()) {
            llm.setSelectedModel(model);
        }
        String tempStr = llmConfig.getProperty("temperature", null);
        if (tempStr != null && !tempStr.isBlank()) {
            llm.setDefaultTemperature(Double.parseDouble(tempStr));
        }
        return llm;
    }

    private void handleLLMModels(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String baseUrl = body.optString("baseUrl", "").trim();
        String apiKey = body.optString("apiKey", null);
        JSONObject response = new JSONObject();
        if (baseUrl.isEmpty()) {
            ctx.status(400);
            response.put("error", "baseUrl is required");
            writeJson(ctx, response);
            return;
        }
        try {
            LLMSupport llm = new LLMSupport(baseUrl, apiKey, Duration.ofSeconds(15));
            List<LLMSupport.LLMModel> models = llm.fetchAvailableModels();
            JSONArray modelsJson = new JSONArray();
            for (LLMSupport.LLMModel model : models) {
                JSONObject m = new JSONObject();
                m.put("id", model.id());
                if (model.ownedBy() != null) m.put("ownedBy", model.ownedBy());
                modelsJson.put(m);
            }
            response.put("models", modelsJson);
        } catch (Exception e) {
            ctx.status(502);
            response.put("error", "Failed to fetch models: " + e.getMessage());
        }
        writeJson(ctx, response);
    }

    private void handleLLMTest(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String baseUrl = body.optString("baseUrl", "").trim();
        String apiKey = body.optString("apiKey", null);
        JSONObject response = new JSONObject();
        if (baseUrl.isEmpty()) {
            ctx.status(400);
            response.put("error", "baseUrl is required");
            writeJson(ctx, response);
            return;
        }
        try {
            LLMSupport llm = new LLMSupport(baseUrl, apiKey, Duration.ofSeconds(15));
            List<LLMSupport.LLMModel> models = llm.fetchAvailableModels();
            response.put("ok", true);
            response.put("modelCount", models.size());
            response.put("baseUrl", llm.getBaseUri().toString());
        } catch (Exception e) {
            response.put("ok", false);
            response.put("error", e.getMessage());
        }
        writeJson(ctx, response);
    }

    private void handleLLMGenerate(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String baseUrl = body.optString("baseUrl", "").trim();
        String apiKey = body.optString("apiKey", null);
        String model = body.optString("model", "").trim();
        double temperature = body.optDouble("temperature", 0.7);
        int timeout = body.optInt("timeout", 60);
        String formatPrompt = body.optString("formatPrompt", "").trim();
        String actionPrompt = body.optString("actionPrompt", "").trim();
        JSONObject response = new JSONObject();
        if (baseUrl.isEmpty()) {
            ctx.status(400);
            response.put("error", "baseUrl is required");
            writeJson(ctx, response);
            return;
        }
        if (model.isEmpty()) {
            ctx.status(400);
            response.put("error", "model is required");
            writeJson(ctx, response);
            return;
        }
        if (actionPrompt.isEmpty()) {
            ctx.status(400);
            response.put("error", "actionPrompt is required");
            writeJson(ctx, response);
            return;
        }
        try {
            LLMSupport llm = new LLMSupport(baseUrl, apiKey, Duration.ofSeconds(timeout));
            llm.setSelectedModel(model);
            llm.setDefaultTemperature(temperature);
            LLMSupport.LLMPrompt.Builder promptBuilder = LLMSupport.LLMPrompt.builder();
            if (!formatPrompt.isEmpty()) {
                promptBuilder.addSystemMessage(formatPrompt);
            }
            promptBuilder.addUserMessage(actionPrompt);
            LLMSupport.LLMCompletion completion = llm.sendPrompt(promptBuilder.build());
            String text = completion.content();
            // Strip markdown code fences if present
            if (text != null) {
                text = text.trim();
                if (text.startsWith("```")) {
                    int firstNewline = text.indexOf('\n');
                    if (firstNewline >= 0) {
                        text = text.substring(firstNewline + 1);
                    }
                    if (text.endsWith("```")) {
                        text = text.substring(0, text.length() - 3);
                    }
                    text = text.trim();
                }
            }
            response.put("text", text != null ? text : "");
            response.put("model", completion.modelId() != null ? completion.modelId() : model);
            JSONObject usage = new JSONObject();
            usage.put("promptTokens", completion.usage().promptTokens());
            usage.put("completionTokens", completion.usage().completionTokens());
            usage.put("totalTokens", completion.usage().totalTokens());
            response.put("usage", usage);
        } catch (Exception e) {
            ctx.status(502);
            response.put("error", "LLM generation failed: " + e.getMessage());
        }
        writeJson(ctx, response);
    }

    private void handleEditorConfig(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null) {
            Properties config = loadEditorConfig(ref);
            response.put("config", editorConfigToJson(config));
        } else {
            response.put("config", new JSONObject());
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfigKeys(Context ctx) {
        String pid = ctx.pathParam("pid");
        String scope = ctx.queryParam("scope");
        if (scope == null || scope.isBlank()) {
            scope = "plugin";
        }
        String deviceName = ctx.queryParam("device");
        if (deviceName == null) {
            deviceName = "";
        }
        String className = ctx.queryParam("className");
        if (className == null) {
            className = "";
        }

        ProjectRef ref = projectStore.get(pid);
        if ((className == null || className.isBlank()) && ref != null && ref.runtimeProject != null) {
            className = resolvePluginClassName(ref.runtimeProject.getProjectConfig(), deviceName);
        }

        ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
        System.out.println("[PROJECT-CONFIG-KEYS] Looking up className='" + className + "', found=" + (entry != null));
        JSONObject spec = resolveSpecForScope(entry, scope);
        if (spec != null) {
            System.out.println("[PROJECT-CONFIG-KEYS] Using spec from plugin-properties.json");
            JSONObject response = buildSpecResponse(spec);
            response.put("supported", true);
            writeJson(ctx, response);
            return;
        }

        ExportableProperties provider = resolveExportableProperties(entry);
        boolean supported = provider != null;

        Map<ProjectProperty, ProjectValueProperty> properties = new HashMap<>();
        if (supported) {
            if ("agent".equalsIgnoreCase(scope)) {
                properties = provider.getExportableAgentProperties();
            } else {
                properties = provider.getExportableProperties();
            }
        }

        JSONArray required = new JSONArray();
        JSONArray optional = new JSONArray();
        if (properties != null) {
            for (Map.Entry<ProjectProperty, ProjectValueProperty> itemEntry : properties.entrySet()) {
                ProjectProperty property = itemEntry.getKey();
                ProjectValueProperty value = itemEntry.getValue();
                if (property == null) {
                    continue;
                }
                JSONObject item = new JSONObject();
                item.put("name", property.getName() == null ? "" : property.getName());
                if (property.getDescription() != null && !property.getDescription().isBlank()) {
                    item.put("description", property.getDescription());
                }
                item.put("required", property.isRequired());
                if (value != null) {
                    ValueTYPE type = value.getType();
                    if (type != null) {
                        item.put("type", type.name().toLowerCase());
                    }
                    Object defaultValue = value.getDefaultValue();
                    if (defaultValue != null) {
                        item.put("default", defaultValue);
                    }
                    if (value.getOptions() != null && !value.getOptions().isEmpty()) {
                        JSONArray options = new JSONArray();
                        for (String option : value.getOptions()) {
                            options.put(option);
                        }
                        item.put("options", options);
                    }
                }
                if (property.isRequired()) {
                    required.put(item);
                } else {
                    optional.put(item);
                }
            }
        }

        JSONObject response = new JSONObject();
        response.put("supported", supported);
        response.put("required", required);
        response.put("optional", optional);
        writeJson(ctx, response);
    }

    private void loadExportablePropertyProviders() {
        EXPORTABLE_PROPERTY_PROVIDERS.clear();
        String resourceName = "plugin-properties.json";
        try {
            ClassLoader cl = getClass().getClassLoader();
            Enumeration<URL> urls = cl.getResources(resourceName);
            int sourceCount = 0;
            while (urls.hasMoreElements()) {
                URL url = urls.nextElement();
                sourceCount += 1;
                try (InputStream stream = url.openStream()) {
                    String json = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
                    JSONObject root = new JSONObject(json);

                    // Check for unified format (has "plugin" at root level with "className")
                    JSONObject rootPlugin = root.optJSONObject("plugin");
                    if (rootPlugin != null && rootPlugin.has("className")) {
                        // Unified format: single plugin definition
                        String className = rootPlugin.optString("className", "").trim();
                        if (!className.isEmpty()) {
                            ExportablePropertyEntry entry = parseUnifiedPluginProperties(root, rootPlugin);
                            EXPORTABLE_PROPERTY_PROVIDERS.put(className, entry);
                        }
                    } else {
                        // Legacy format: providers map
                        JSONObject providers = root.optJSONObject("providers");
                        JSONObject source = providers != null ? providers : root;
                        for (String key : source.keySet()) {
                            if (key == null || key.isBlank()) {
                                continue;
                            }
                            Object rawValue = source.opt(key);
                            String providerClass = null;
                            JSONObject pluginSpec = null;
                            JSONObject agentSpec = null;
                            if (rawValue instanceof JSONObject) {
                                JSONObject value = (JSONObject) rawValue;
                                if (value.has("plugin") || value.has("agent")) {
                                    pluginSpec = value.optJSONObject("plugin");
                                    agentSpec = value.optJSONObject("agent");
                                } else if (value.has("required") || value.has("optional")) {
                                    pluginSpec = value;
                                }
                            } else if (rawValue != null) {
                                providerClass = String.valueOf(rawValue).trim();
                            }
                            if ((providerClass != null && !providerClass.isEmpty()) || pluginSpec != null || agentSpec != null) {
                                EXPORTABLE_PROPERTY_PROVIDERS.put(
                                        key.trim(),
                                        new ExportablePropertyEntry(providerClass, pluginSpec, agentSpec,
                                                null, null, null, null));
                            }
                        }
                    }
                }
            }
            if (sourceCount == 0) {
                System.out.println("[PROJECT-CONFIG] WARNING: no plugin-properties.json resources found on classpath.");
            } else {
                System.out.println("[PROJECT-CONFIG] Loaded exportable properties registry with "
                        + EXPORTABLE_PROPERTY_PROVIDERS.size() + " entries from " + sourceCount + " resources.");
                for (String providerKey : EXPORTABLE_PROPERTY_PROVIDERS.keySet()) {
                    System.out.println("[PROJECT-CONFIG]   - " + providerKey);
                }
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to load exportable property registry: " + exc.getMessage());
        }
    }

    /**
     * Parses unified plugin-properties.json format.
     */
    private ExportablePropertyEntry parseUnifiedPluginProperties(JSONObject root, JSONObject pluginMeta) {
        // Extract config section
        JSONObject config = root.optJSONObject("config");
        JSONObject pluginSpec = null;
        JSONObject agentSpec = null;
        if (config != null) {
            // Build pluginSpec from config.required, config.optional, config.pluginSpecific
            pluginSpec = new JSONObject();
            if (config.has("required")) {
                pluginSpec.put("required", config.optJSONArray("required"));
            }
            if (config.has("optional")) {
                pluginSpec.put("optional", config.optJSONArray("optional"));
            }
            if (config.has("pluginSpecific")) {
                pluginSpec.put("pluginSpecific", config.optJSONArray("pluginSpecific"));
            }
            // Agent config
            if (config.has("agent")) {
                agentSpec = config.optJSONObject("agent");
            }
        }

        // Templates go into pluginSpec
        JSONObject templates = root.optJSONObject("templates");
        if (templates != null && pluginSpec != null) {
            pluginSpec.put("templates", templates);
        } else if (templates != null) {
            pluginSpec = new JSONObject();
            pluginSpec.put("templates", templates);
        }

        // Categories
        JSONObject categories = root.optJSONObject("categories");

        // Commands
        JSONArray commands = root.optJSONArray("commands");

        // Variables
        JSONObject variables = root.optJSONObject("variables");

        return new ExportablePropertyEntry(null, pluginSpec, agentSpec,
                pluginMeta, categories, commands, variables);
    }

    private String resolvePluginClassName(ProjectConfig config, String deviceName) {
        if (config == null || deviceName == null || deviceName.isBlank()) {
            return "";
        }
        for (PluginConfig plugin : config.getPluginConfigList()) {
            if (plugin != null && deviceName.equals(plugin.getPluginName())) {
                return plugin.getClassName() == null ? "" : plugin.getClassName();
            }
        }
        return "";
    }

    private ExportablePropertyEntry resolveExportablePropertyEntry(String className) {
        if (className == null || className.isBlank()) {
            return null;
        }
        return EXPORTABLE_PROPERTY_PROVIDERS.get(className.trim());
    }

    private JSONObject resolveSpecForScope(ExportablePropertyEntry entry, String scope) {
        if (entry == null) {
            return null;
        }
        if ("agent".equalsIgnoreCase(scope)) {
            return entry.agentSpec;
        }
        return entry.pluginSpec;
    }

    private JSONObject buildSpecResponse(JSONObject spec) {
        JSONArray required = normalizeSpecArray(spec.optJSONArray("required"), true);
        JSONArray optional = normalizeSpecArray(spec.optJSONArray("optional"), false);
        JSONArray pluginSpecific = normalizeSpecArray(spec.optJSONArray("pluginSpecific"), false);
        JSONObject response = new JSONObject();
        response.put("required", required);
        response.put("optional", optional);
        if (pluginSpecific.length() > 0) {
            response.put("pluginSpecific", pluginSpecific);
        }
        return response;
    }

    private JSONArray normalizeSpecArray(JSONArray items, boolean required) {
        JSONArray output = new JSONArray();
        if (items == null) {
            return output;
        }
        for (int i = 0; i < items.length(); i += 1) {
            Object raw = items.get(i);
            if (!(raw instanceof JSONObject)) {
                continue;
            }
            JSONObject entry = (JSONObject) raw;
            String name = entry.optString("name", "").trim();
            if (name.isEmpty()) {
                continue;
            }
            JSONObject item = new JSONObject();
            item.put("name", name);
            item.put("required", required);
            String description = entry.optString("description", "").trim();
            if (!description.isEmpty()) {
                item.put("description", description);
            }
            String type = entry.optString("type", "").trim();
            if (!type.isEmpty()) {
                item.put("type", type);
            }
            if (entry.has("default")) {
                item.put("default", entry.get("default"));
            }
            JSONArray options = entry.optJSONArray("options");
            if (options != null && options.length() > 0) {
                item.put("options", options);
            }
            if (entry.optBoolean("readonly", false)) {
                item.put("readonly", true);
            }
            output.put(item);
        }
        return output;
    }

    private ExportableProperties resolveExportableProperties(ExportablePropertyEntry entry) {
        if (entry == null || entry.providerClass == null || entry.providerClass.isBlank()) {
            return null;
        }
        return instantiateExportableProperties(entry.providerClass);
    }

    private ExportableProperties instantiateExportableProperties(String className) {
        try {
            Class<?> klass = Class.forName(className);
            if (!ExportableProperties.class.isAssignableFrom(klass)) {
                return null;
            }
            if (Modifier.isAbstract(klass.getModifiers())) {
                return null;
            }
            return (ExportableProperties) klass.getDeclaredConstructor().newInstance();
        } catch (Exception exc) {
            return null;
        }
    }

    private void handleScript(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null) {
            try {
                ensureScriptLoaded(ref);
                response.put("text", ref.scriptText == null ? "" : ref.scriptText);
                response.put("version", ref.scriptVersion);
                response.put("parseOk", ref.scriptParseOk);
                response.put("parseErrors", diagnosticsToJson(ref.scriptParseErrors));
            } catch (Exception exc) {
                sLogger.warning("Warning: cannot load script for pid=" + pid + ": " + exc.getMessage());
                response.put("text", "");
                response.put("version", ref != null ? ref.scriptVersion : 1);
                response.put("parseOk", true);
                response.put("parseErrors", new JSONArray());
            }
        } else {
            response.put("text", "");
            response.put("version", 1);
            response.put("parseOk", true);
            response.put("parseErrors", new JSONArray());
        }
        writeJson(ctx, response);
    }

    private void handleScriptScenes(Context ctx) {
        JSONObject response = new JSONObject();
        JSONArray languages = new JSONArray();
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref != null && ref.runtimeProject != null) {
            SceneScript script = ref.runtimeProject.getSceneScript();
            Map<String, Map<String, Integer>> grouped = new java.util.TreeMap<>();
            for (SceneObject scene : script.getSceneList()) {
                if (scene == null) continue;
                String language = scene.getLanguage();
                String name = scene.getName();
                String langKey = language == null ? "" : language.trim();
                String nameKey = name == null ? "" : name.trim();
                if (nameKey.isEmpty()) {
                    continue;
                }
                grouped.computeIfAbsent(langKey, key -> new java.util.TreeMap<>())
                    .merge(nameKey, 1, Integer::sum);
            }
            for (Map.Entry<String, Map<String, Integer>> langEntry : grouped.entrySet()) {
                JSONObject langJson = new JSONObject();
                langJson.put("language", langEntry.getKey());
                JSONArray groups = new JSONArray();
                for (Map.Entry<String, Integer> groupEntry : langEntry.getValue().entrySet()) {
                    JSONObject groupJson = new JSONObject();
                    groupJson.put("name", groupEntry.getKey());
                    groupJson.put("count", groupEntry.getValue());
                    groups.put(groupJson);
                }
                langJson.put("groups", groups);
                languages.put(langJson);
            }
        }
        response.put("languages", languages);
        writeJson(ctx, response);
    }

    private void handleScriptElements(Context ctx) {
        JSONObject response = new JSONObject();
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONArray acticonJson = new JSONArray();
        JSONArray gesticonJson = new JSONArray();
        JSONArray visiconJson = new JSONArray();
        if (ref != null && ref.runtimeProject != null) {
            ActiconConfig acticon = ref.runtimeProject.getActicon();
            if (acticon != null) {
                for (ActiconAction action : acticon.getActionList()) {
                    if (action == null) continue;
                    JSONObject item = new JSONObject();
                    item.put("name", action.getActionName() == null ? "" : action.getActionName());
                    item.put("script", action.toScript());
                    acticonJson.put(item);
                }
            }

            GesticonConfig gesticon = ref.runtimeProject.getGesticon();
            if (gesticon != null) {
                for (GesticonAgent agent : gesticon.getAgentList()) {
                    if (agent == null) continue;
                    JSONObject agentJson = new JSONObject();
                    agentJson.put("agent", agent.getAgentName() == null ? "" : agent.getAgentName());
                    agentJson.put("icon", agent.getAgentIcon() == null ? "" : agent.getAgentIcon());
                    JSONArray gestures = new JSONArray();
                    for (GesticonGesture gesture : agent.getGestureList()) {
                        if (gesture == null) continue;
                        JSONObject gestureJson = new JSONObject();
                        gestureJson.put("character", gesture.getCharacter());
                        gestureJson.put("animName", gesture.getAnimName());
                        gestureJson.put("animPath", gesture.getAnimPath());
                        gestureJson.put("category", gesture.getCategory());
                        gestureJson.put("script", "");
                        gestures.put(gestureJson);
                    }
                    agentJson.put("gestures", gestures);
                    gesticonJson.put(agentJson);
                }
            }

            VisiconConfig visicon = ref.runtimeProject.getVisicon();
            if (visicon != null) {
                for (VisiconAgent agent : visicon.getAgentList()) {
                    if (agent == null) continue;
                    JSONObject agentJson = new JSONObject();
                    agentJson.put("agent", agent.getAgentName() == null ? "" : agent.getAgentName());
                    agentJson.put("icon", agent.getAgentIcon() == null ? "" : agent.getAgentIcon());
                    JSONArray visemes = new JSONArray();
                    for (VisiconViseme viseme : agent.getVisemeList()) {
                        if (viseme == null) continue;
                        JSONObject visemeJson = new JSONObject();
                        visemeJson.put("key", viseme.getKey());
                        visemeJson.put("value", viseme.getValue());
                        visemes.put(visemeJson);
                    }
                    agentJson.put("visemes", visemes);
                    visiconJson.put(agentJson);
                }
            }
        }
        response.put("acticon", acticonJson);
        response.put("gesticon", gesticonJson);
        response.put("visicon", visiconJson);
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
        return SceneFlowSnapshotBuilder.resolveSuperNode(sceneFlow, superNodeId);
    }

    private JSONObject createSceneFlowSnapshot(RunTimeProject rtp, String projectId, SuperNode superNode, SceneFlow sceneFlow) {
        ProjectRef ref = projectStore.get(projectId);
        JSONObject undoState = null;
        if (ref != null) {
            undoState = buildUndoState(ref);
        }
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        return SceneFlowSnapshotBuilder.createSnapshot(projectId, superNode, sceneFlow, nodeWidth, nodeHeight, undoState);
    }

    // Path/altStart helpers are now in SceneFlowSnapshotBuilder

    // nodeToJsonCore, typeDefsToJsonCore, varDefsToJsonCore, commandsToJsonCore
    // are now in SceneFlowSnapshotBuilder (nodeToJson, varDefsToJson, etc.)

    // edgeToJsonCore, getEdgeType, commentToJsonCore are now in SceneFlowSnapshotBuilder

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

        // Scene play history
        JSONArray historyArr = new JSONArray();
        if (ref != null && ref.runtimeProject != null) {
            for (RunTimeProject.ScenePlayRecord rec : ref.runtimeProject.getSceneHistory()) {
                JSONObject h = new JSONObject();
                h.put("timestamp", rec.timestamp);
                h.put("sceneName", rec.sceneName);
                h.put("language", rec.language);
                h.put("lower", rec.lower);
                h.put("upper", rec.upper);
                historyArr.put(h);
            }
        }
        response.put("sceneHistory", historyArr);

        writeJson(ctx, response);
    }

    private void handleCommandLog(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404);
            writeJson(ctx, new JSONObject().put("error", "PROJECT_NOT_FOUND"));
            return;
        }
        ensureCommandLogLoaded(ref);
        long since = 0;
        try {
            String raw = ctx.queryParam("since");
            if (raw != null && !raw.isBlank()) {
                since = Long.parseLong(raw.trim());
            }
        } catch (NumberFormatException ignore) {
        }
        int limit = 0;
        try {
            String raw = ctx.queryParam("limit");
            if (raw != null && !raw.isBlank()) {
                limit = Integer.parseInt(raw.trim());
            }
        } catch (NumberFormatException ignore) {
        }
        int maxLimit = Math.max(1, getEditorConfigInt(ref, "command_log_max", 5000));
        if (limit <= 0 || limit > maxLimit) {
            limit = maxLimit;
        }
        JSONArray entries = new JSONArray();
        int added = 0;
        long lastSeq = 0;
        for (CommandLogEntry entry : ref.commandLog) {
            if (entry.seq <= since) {
                lastSeq = Math.max(lastSeq, entry.seq);
                continue;
            }
            entries.put(entry.toJson());
            lastSeq = Math.max(lastSeq, entry.seq);
            added++;
            if (added >= limit) break;
        }
        JSONObject result = new JSONObject();
        result.put("projectId", pid);
        result.put("entries", entries);
        result.put("lastSeq", lastSeq);
        result.put("count", entries.length());
        writeJson(ctx, result);
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
        try {
            JSONObject body = new JSONObject(ctx.body());
            String text = body.optString("text", "");
            if (text == null || text.isBlank()) {
                response.put("parseOk", true);
                response.put("parseErrors", new JSONArray());
                writeJson(ctx, response);
                return;
            }
            ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(text);
            response.put("parseOk", result.isParseOk());
            response.put("parseErrors", diagnosticsToJson(result.getDiagnostics()));
        } catch (Exception exc) {
            response.put("parseOk", false);
            response.put("parseErrors", new JSONArray());
        }
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
            if (value.getType() == AbstractValue.Type.EVENT) {
                EventValue ev = (EventValue) value;
                return ev.getFormattedSyntax();
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
        if ("Event".equalsIgnoreCase(type)) {
            return "Event";
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
        // Gate editing commands in RUNTIME_ONLY mode
        if (mMode == ServerMode.RUNTIME_ONLY && isEditingCommand(method)) {
            return errorResponse("EDITING_NOT_SUPPORTED",
                    "Editing not supported in runtime-only mode");
        }

        switch (method) {
            case "SceneFlow.Get":
            case "SceneFlow.Snapshot":
                return snapshotPayload(params.optString("projectId", ""));
            case "SceneFlow.Node.Add":
            case "SceneFlow.Node.Create":
                return createNodeForProject(params, broadcaster);
            case "SceneFlow.Node.Update":
                return updateNodeForProject(params, broadcaster);
            case "SceneFlow.Node.Delete":
                return deleteNodeForProject(params, broadcaster);
            case "SceneFlow.Node.Move":
                return moveNodeForProject(params, broadcaster);
            case "SceneFlow.Edge.Add":
            case "SceneFlow.Edge.Create":
                return createEdgeForProject(params, broadcaster);
            case "SceneFlow.Edge.Update":
                return updateEdgeForProject(params, broadcaster);
            case "SceneFlow.Edge.Delete":
                return deleteEdgeForProject(params, broadcaster);
            case "SceneFlow.Comment.Add":
            case "SceneFlow.Comment.Create":
                return createCommentForProject(params, broadcaster);
            case "SceneFlow.Comment.Update":
                return updateCommentForProject(params, broadcaster);
            case "SceneFlow.Comment.Delete":
                return deleteCommentForProject(params, broadcaster);
            case "SceneFlow.Selection.Copy":
                return copySelectionForProject(params);
            case "SceneFlow.Selection.Paste":
                return pasteSelectionForProject(params, broadcaster);
            case "SceneFlow.Undo":
                return undoProject(params, broadcaster);
            case "SceneFlow.Redo":
                return redoProject(params, broadcaster);
            case "SceneFlow.PlayScene.Find":
                return findPlaySceneReferences(params);
            case "SceneFlow.PlayScene.FindMany":
                return findPlaySceneReferencesMany(params);
            case "SceneFlow.PlayScene.Rename":
                return renamePlaySceneReferences(params, broadcaster);
            case "Embeddings.Start":
                return startEmbeddingsService(params);
            case "Script.Update":
                return updateScriptForProject(params, broadcaster);
            case "Config.Update": {
                String pid = params.optString("projectId", "");
                JSONObject values = params.optJSONObject("values");
                if (pid.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing projectId");
                }
                if (values == null) {
                    return errorResponse("BAD_REQUEST", "Missing values");
                }
                ProjectRef ref = projectStore.get(pid);
                if (ref == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                Properties config = loadEditorConfig(ref);
                for (String key : values.keySet()) {
                    Object raw = values.get(key);
                    if (raw == null || raw == JSONObject.NULL) {
                        config.remove(key);
                    } else {
                        config.setProperty(key, String.valueOf(raw));
                    }
                }
                boolean saved = false;
                boolean pending = false;
                String path = ref.path == null ? "" : ref.path.trim();
                if (!path.isBlank()) {
                    saved = saveEditorConfig(ref);
                    if (!saved) {
                        return errorResponse("CONFIG_SAVE_FAILED", "Failed to save editor config");
                    }
                } else {
                    pending = true;
                    ref.editorConfigDirty = true;
                    ref.dirty = true;
                }
                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("config", editorConfigToJson(config));
                response.put("saved", saved);
                response.put("pending", pending);
                if (ref.runtimeProject != null) {
                    SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                    String superNodeId = params.optString("superNodeId", "");
                    SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                    JSONObject snapshot = createSceneFlowSnapshot(
                            ref.runtimeProject,
                            pid,
                            snapshotTarget != null ? snapshotTarget : sceneFlow,
                            sceneFlow
                    );
                    response.put("snapshot", snapshot);
                    if (broadcaster != null) {
                        JSONObject evt = new JSONObject();
                        evt.put("event", "sceneflow.snapshot");
                        evt.put("snapshot", snapshot);
                        broadcaster.accept(evt.toString());
                    }
                }
                return response;
            }
            case "ProjectConfig.Plugin.Create": {
                // Returns a plugin template with required keys pre-populated from plugin-properties.json
                // Also returns sceneflowVars for properties that have a sceneflowtype defined
                String className = params.optString("className", "").trim();
                String name = params.optString("name", "").trim();
                String type = params.optString("type", "device").trim();
                boolean load = params.optBoolean("load", true);

                if (className.isEmpty()) {
                    return errorResponse("BAD_REQUEST", "Missing className");
                }

                JSONObject plugin = new JSONObject();
                plugin.put("name", name);
                plugin.put("className", className);
                plugin.put("type", type);
                plugin.put("load", load);

                // Look up required keys and their defaults
                // Also collect sceneflow variable definitions
                JSONArray features = new JSONArray();
                JSONArray sceneflowVars = new JSONArray();
                ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
                if (entry != null && entry.pluginSpec != null) {
                    JSONArray required = entry.pluginSpec.optJSONArray("required");
                    if (required != null) {
                        for (int i = 0; i < required.length(); i++) {
                            JSONObject req = required.optJSONObject(i);
                            if (req != null) {
                                String key = req.optString("name", "").trim();
                                if (!key.isEmpty()) {
                                    Object defaultVal = req.opt("default");
                                    String value = defaultVal != null ? String.valueOf(defaultVal) : "";
                                    JSONObject feature = new JSONObject();
                                    feature.put("key", key);
                                    feature.put("value", value);
                                    features.put(feature);

                                    // Check if this property defines a sceneflow variable type
                                    String sceneflowType = req.optString("sceneflowtype", "").trim();
                                    if (!sceneflowType.isEmpty() && !value.isEmpty()) {
                                        JSONObject varDef = new JSONObject();
                                        varDef.put("name", value);  // The default value is the variable name
                                        varDef.put("type", sceneflowType);
                                        sceneflowVars.put(varDef);
                                    }
                                }
                            }
                        }
                    }
                    // Also add pluginSpecific items with their defaults
                    JSONArray pluginSpecific = entry.pluginSpec.optJSONArray("pluginSpecific");
                    if (pluginSpecific != null) {
                        for (int i = 0; i < pluginSpecific.length(); i++) {
                            JSONObject ps = pluginSpecific.optJSONObject(i);
                            if (ps != null) {
                                String key = ps.optString("name", "").trim();
                                if (!key.isEmpty()) {
                                    Object defaultVal = ps.opt("default");
                                    String value = defaultVal != null ? String.valueOf(defaultVal) : "";
                                    JSONObject feature = new JSONObject();
                                    feature.put("key", key);
                                    feature.put("value", value);
                                    features.put(feature);
                                }
                            }
                        }
                    }
                }
                plugin.put("features", features);

                // Check for templates section
                JSONObject templates = null;
                if (entry != null && entry.pluginSpec != null) {
                    templates = entry.pluginSpec.optJSONObject("templates");
                }

                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("plugin", plugin);
                response.put("sceneflowVars", sceneflowVars);
                if (templates != null) {
                    response.put("templates", templates);
                }
                return response;
            }
            case "Project.Templates.Install": {
                String pid = params.optString("projectId", "");
                String pluginClassName = params.optString("className", "").trim();

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (pluginClassName.isEmpty()) {
                    return errorResponse("BAD_REQUEST", "Missing className");
                }

                // Get project directory (ref.path is already the project directory, not project.xml)
                String projectPath = ref.path;
                if (projectPath == null || projectPath.isEmpty()) {
                    return errorResponse("PROJECT_PATH_UNKNOWN", "Project path not available");
                }
                File projectDir = new File(projectPath);
                if (!projectDir.isDirectory()) {
                    return errorResponse("PROJECT_DIR_INVALID", "Project directory not found");
                }

                // Find templates configuration
                ExportablePropertyEntry propEntry = resolveExportablePropertyEntry(pluginClassName);
                if (propEntry == null || propEntry.pluginSpec == null) {
                    JSONObject response = new JSONObject();
                    response.put("status", "ok");
                    response.put("createdFiles", new JSONArray());
                    response.put("message", "No templates defined for this plugin");
                    return response;
                }

                JSONObject templates = propEntry.pluginSpec.optJSONObject("templates");
                if (templates == null) {
                    JSONObject response = new JSONObject();
                    response.put("status", "ok");
                    response.put("createdFiles", new JSONArray());
                    response.put("message", "No templates defined for this plugin");
                    return response;
                }

                String resourcePath = templates.optString("resourcePath", "templates/");
                JSONArray targetDirs = templates.optJSONArray("targetDirs");
                if (targetDirs == null || targetDirs.isEmpty()) {
                    JSONObject response = new JSONObject();
                    response.put("status", "ok");
                    response.put("createdFiles", new JSONArray());
                    response.put("message", "No target directories specified");
                    return response;
                }

                // Extract templates from classpath to project directory
                JSONArray createdFiles = new JSONArray();
                JSONArray skippedFiles = new JSONArray();
                ClassLoader cl = getClass().getClassLoader();

                for (int i = 0; i < targetDirs.length(); i++) {
                    String targetDir = targetDirs.optString(i, "").trim();
                    if (targetDir.isEmpty()) continue;

                    File destDir = new File(projectDir, targetDir);
                    if (!destDir.exists()) {
                        destDir.mkdirs();
                        createdFiles.put(targetDir + "/");
                    }

                    // Try to find and copy template files
                    String templatePath = resourcePath + targetDir + "/";
                    try {
                        Enumeration<URL> resources = cl.getResources(templatePath);
                        while (resources.hasMoreElements()) {
                            URL resourceUrl = resources.nextElement();
                            copyTemplateDirectory(resourceUrl, templatePath, destDir, createdFiles, skippedFiles);
                        }
                    } catch (Exception ex) {
                        sLogger.warning("Failed to extract templates from " + templatePath + ": " + ex.getMessage());
                    }
                }

                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("createdFiles", createdFiles);
                response.put("skippedFiles", skippedFiles);
                return response;
            }
            case "ProjectConfig.Update": {
                String pid = params.optString("projectId", "");
                JSONObject configJson = params.optJSONObject("config");
                if (pid.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing projectId");
                }
                if (configJson == null) {
                    return errorResponse("BAD_REQUEST", "Missing config");
                }
                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
                if (cfg == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project config not available");
                }
                applyProjectConfigFromJson(ref, cfg, configJson);
                ref.dirty = true;
                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("config", projectConfigToJson(cfg, ref.path));
                response.put("saved", false);
                response.put("pending", true);
                if (broadcaster != null) {
                    JSONObject dirtyEvt = new JSONObject();
                    dirtyEvt.put("event", "project.dirty");
                    dirtyEvt.put("projectId", pid);
                    dirtyEvt.put("areas", new JSONArray().put("config"));
                    broadcaster.accept(dirtyEvt.toString());
                }
                return response;
            }
            case "Preferences.Update": {
                JSONObject values = params.optJSONObject("values");
                if (values == null) {
                    return errorResponse("BAD_REQUEST", "Missing values");
                }
                for (String key : values.keySet()) {
                    Object raw = values.get(key);
                    if (raw == null || raw == JSONObject.NULL) {
                        Preferences.removeProperty(key);
                    } else {
                        Preferences.setProperty(key, String.valueOf(raw));
                    }
                }
                Preferences.save();
                JSONObject prefs = preferencesToJson();
                if (broadcaster != null) {
                    JSONObject evt = new JSONObject();
                    evt.put("event", "system.preferences");
                    evt.put("preferences", prefs);
                    broadcaster.accept(evt.toString());
                }
                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("preferences", prefs);
                return response;
            }
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

            // Variable definition operations
            case "SceneFlow.Node.VarDef.Add": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject varDefJson = params.optJSONObject("varDef");
                int index = params.has("index") ? params.optInt("index", -1) : -1;

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (varDefJson == null) {
                    return errorResponse("BAD_REQUEST", "Missing varDef");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                StringBuilder error = new StringBuilder();
                VariableDefinition varDef = parseVarDef(varDefJson, dataNode, error);
                if (varDef == null) {
                    return errorResponse("VARDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid variable definition");
                }

                List<VariableDefinition> list = dataNode.getVarDefList();
                int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                list.add(insertIndex, varDef);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.VarDef.Add");
                recordCommand(ref, "SceneFlow.Node.VarDef.Add", params);
                return response;
            }

            case "SceneFlow.Node.VarDef.Update": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject varDefJson = params.optJSONObject("varDef");
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (varDefJson == null || index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing varDef or index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<VariableDefinition> list = dataNode.getVarDefList();
                if (index >= list.size()) {
                    return errorResponse("VARDEF_NOT_FOUND", "Variable definition not found at index: " + index);
                }

                StringBuilder error = new StringBuilder();
                VariableDefinition varDef = parseVarDef(varDefJson, dataNode, error);
                if (varDef == null) {
                    return errorResponse("VARDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid variable definition");
                }

                list.set(index, varDef);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.VarDef.Update");
                recordCommand(ref, "SceneFlow.Node.VarDef.Update", params);
                return response;
            }

            case "SceneFlow.Node.VarDef.Delete": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<VariableDefinition> list = dataNode.getVarDefList();
                if (index >= list.size()) {
                    return errorResponse("VARDEF_NOT_FOUND", "Variable definition not found at index: " + index);
                }

                list.remove(index);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.VarDef.Delete");
                recordCommand(ref, "SceneFlow.Node.VarDef.Delete", params);
                return response;
            }

            case "SceneFlow.Node.VarDef.Move": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int from = params.optInt("from", -1);
                int to = params.optInt("to", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (from < 0 || to < 0) {
                    return errorResponse("BAD_REQUEST", "Missing from or to index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<VariableDefinition> list = dataNode.getVarDefList();
                if (from >= list.size() || to >= list.size()) {
                    return errorResponse("VARDEF_NOT_FOUND", "Invalid index");
                }

                if (from != to) {
                    VariableDefinition entry = list.remove(from);
                    list.add(to, entry);
                }

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.VarDef.Move");
                recordCommand(ref, "SceneFlow.Node.VarDef.Move", params);
                return response;
            }

            // Type definition operations
            case "SceneFlow.Node.TypeDef.Add": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject typeDefJson = params.optJSONObject("typeDef");
                int index = params.has("index") ? params.optInt("index", -1) : -1;

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (typeDefJson == null) {
                    return errorResponse("BAD_REQUEST", "Missing typeDef");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                StringBuilder error = new StringBuilder();
                DataTypeDefinition typeDef = parseTypeDef(typeDefJson, error);
                if (typeDef == null) {
                    return errorResponse("TYPEDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid type definition");
                }

                List<DataTypeDefinition> list = dataNode.getTypeDefList();
                int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                list.add(insertIndex, typeDef);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.TypeDef.Add");
                recordCommand(ref, "SceneFlow.Node.TypeDef.Add", params);
                return response;
            }

            case "SceneFlow.Node.TypeDef.Update": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject typeDefJson = params.optJSONObject("typeDef");
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (typeDefJson == null || index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing typeDef or index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<DataTypeDefinition> list = dataNode.getTypeDefList();
                if (index >= list.size()) {
                    return errorResponse("TYPEDEF_NOT_FOUND", "Type definition not found at index: " + index);
                }

                StringBuilder error = new StringBuilder();
                DataTypeDefinition typeDef = parseTypeDef(typeDefJson, error);
                if (typeDef == null) {
                    return errorResponse("TYPEDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid type definition");
                }

                list.set(index, typeDef);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.TypeDef.Update");
                recordCommand(ref, "SceneFlow.Node.TypeDef.Update", params);
                return response;
            }

            case "SceneFlow.Node.TypeDef.Delete": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<DataTypeDefinition> list = dataNode.getTypeDefList();
                if (index >= list.size()) {
                    return errorResponse("TYPEDEF_NOT_FOUND", "Type definition not found at index: " + index);
                }

                list.remove(index);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.TypeDef.Delete");
                recordCommand(ref, "SceneFlow.Node.TypeDef.Delete", params);
                return response;
            }

            case "SceneFlow.Node.TypeDef.Move": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int from = params.optInt("from", -1);
                int to = params.optInt("to", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (from < 0 || to < 0) {
                    return errorResponse("BAD_REQUEST", "Missing from or to index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<DataTypeDefinition> list = dataNode.getTypeDefList();
                if (from >= list.size() || to >= list.size()) {
                    return errorResponse("TYPEDEF_NOT_FOUND", "Invalid index");
                }

                if (from != to) {
                    DataTypeDefinition entry = list.remove(from);
                    list.add(to, entry);
                }

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.TypeDef.Move");
                recordCommand(ref, "SceneFlow.Node.TypeDef.Move", params);
                return response;
            }

            // Command operations
            case "SceneFlow.Node.Cmd.Add": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject commandJson = params.optJSONObject("command");
                int index = params.has("index") ? params.optInt("index", -1) : -1;

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (commandJson == null) {
                    return errorResponse("BAD_REQUEST", "Missing command");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                StringBuilder error = new StringBuilder();
                Command command = parseCommandText(commandJson.optString("text", ""), error);
                if (command == null) {
                    return errorResponse("COMMAND_INVALID", error.length() > 0 ? error.toString() : "Invalid command");
                }

                List<Command> list = dataNode.getCmdList();
                int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                list.add(insertIndex, command);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Cmd.Add");
                recordCommand(ref, "SceneFlow.Node.Cmd.Add", params);
                return response;
            }

            case "SceneFlow.Node.Cmd.Update": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONObject commandJson = params.optJSONObject("command");
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (commandJson == null || index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing command or index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<Command> list = dataNode.getCmdList();
                if (index >= list.size()) {
                    return errorResponse("COMMAND_NOT_FOUND", "Command not found at index: " + index);
                }

                StringBuilder error = new StringBuilder();
                Command command = parseCommandText(commandJson.optString("text", ""), error);
                if (command == null) {
                    return errorResponse("COMMAND_INVALID", error.length() > 0 ? error.toString() : "Invalid command");
                }

                list.set(index, command);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Cmd.Update");
                recordCommand(ref, "SceneFlow.Node.Cmd.Update", params);
                return response;
            }

            case "SceneFlow.Node.Cmd.Delete": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int index = params.optInt("index", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (index < 0) {
                    return errorResponse("BAD_REQUEST", "Missing index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<Command> list = dataNode.getCmdList();
                if (index >= list.size()) {
                    return errorResponse("COMMAND_NOT_FOUND", "Command not found at index: " + index);
                }

                list.remove(index);

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Cmd.Delete");
                recordCommand(ref, "SceneFlow.Node.Cmd.Delete", params);
                return response;
            }

            case "SceneFlow.Node.Cmd.Move": {
                String pid = params.optString("projectId", "");
                String nodeId = params.optString("nodeId", "");
                String superNodeId = params.optString("superNodeId", null);
                int from = params.optInt("from", -1);
                int to = params.optInt("to", -1);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (from < 0 || to < 0) {
                    return errorResponse("BAD_REQUEST", "Missing from or to index");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                if (dataNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
                }

                List<Command> list = dataNode.getCmdList();
                if (from >= list.size() || to >= list.size()) {
                    return errorResponse("COMMAND_NOT_FOUND", "Invalid index");
                }

                if (from != to) {
                    Command entry = list.remove(from);
                    list.add(to, entry);
                }

                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Cmd.Move");
                recordCommand(ref, "SceneFlow.Node.Cmd.Move", params);
                return response;
            }

            // Edge path operations
            case "SceneFlow.Edge.Normalize":
            case "SceneFlow.Edge.Straighten": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);
                String edgeId = params.optString("edgeId", "");

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (edgeId.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing edgeId");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                AbstractEdge dataEdge = resolveEdgeById(snapshotTarget != null ? snapshotTarget : sceneFlow, edgeId);
                if (dataEdge == null) {
                    return errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
                }

                boolean isNormalize = "SceneFlow.Edge.Normalize".equals(method);
                int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
                int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
                if (!isNormalize) {
                    List<AbstractEdge> relayout = new ArrayList<>();
                    relayout.add(dataEdge);
                    mEdgeLayout.relayoutEdgesInOrder(relayout, nodeWidth, nodeHeight);
                }
                mEdgeLayout.normalizeEdge(dataEdge, nodeWidth, nodeHeight);

                JSONObject response = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                response.put("status", "ok");
                broadcastSceneFlowSnapshot(broadcaster, pid, response);
                recordHistory(ref, method);
                recordCommand(ref, method, params);
                return response;
            }

            case "SceneFlow.Edge.NormalizeAll":
            case "SceneFlow.Edge.StraightenAll": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

                boolean isNormalize = "SceneFlow.Edge.NormalizeAll".equals(method);
                int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
                int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
                if (!isNormalize) {
                    mEdgeLayout.clearDockPointsRecursive(targetNode);
                    mEdgeLayout.occupyStartSignDockPointsRecursive(targetNode);
                    List<AbstractEdge> relayout = new ArrayList<>();
                    Set<AbstractEdge> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
                    mEdgeLayout.collectEdgesRecursive(targetNode, relayout, seen);
                    mEdgeLayout.relayoutEdgesInOrder(relayout, nodeWidth, nodeHeight);
                }
                for (BasicNode node : targetNode.getNodeAndSuperNodeList()) {
                    for (AbstractEdge edge : node.getEdgeList()) {
                        mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);
                    }
                }

                JSONObject response = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                response.put("status", "ok");
                broadcastSceneFlowSnapshot(broadcaster, pid, response);
                recordHistory(ref, method);
                recordCommand(ref, method, params);
                return response;
            }

            case "SceneFlow.Edge.NormalizeGroup":
            case "SceneFlow.Edge.StraightenGroup": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONArray edgeIds = params.optJSONArray("edgeIds");

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (edgeIds == null || edgeIds.length() == 0) {
                    return errorResponse("BAD_REQUEST", "Missing edgeIds");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

                boolean isNormalize = "SceneFlow.Edge.NormalizeGroup".equals(method);
                int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
                int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
                List<AbstractEdge> groupEdges = new ArrayList<>();
                for (int i = 0; i < edgeIds.length(); i++) {
                    String edgeId = edgeIds.optString(i, "").trim();
                    if (edgeId.isEmpty()) continue;
                    AbstractEdge dataEdge = resolveEdgeById(targetNode, edgeId);
                    if (dataEdge != null) {
                        groupEdges.add(dataEdge);
                    }
                }
                if (!isNormalize && !groupEdges.isEmpty()) {
                    mEdgeLayout.relayoutEdgesInOrder(groupEdges, nodeWidth, nodeHeight);
                }
                for (AbstractEdge dataEdge : groupEdges) {
                    mEdgeLayout.normalizeEdge(dataEdge, nodeWidth, nodeHeight);
                }

                JSONObject response = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                response.put("status", "ok");
                broadcastSceneFlowSnapshot(broadcaster, pid, response);
                recordHistory(ref, method);
                recordCommand(ref, method, params);
                return response;
            }

            // Phase 8: Edge retargeting operation
            case "SceneFlow.Edge.Retarget": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);
                String edgeId = params.optString("edgeId", "");
                String targetId = params.optString("targetId", "");

                if (pid.isBlank() || edgeId.isBlank() || targetId.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing projectId, edgeId, or targetId");
                }

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);

                // Find the edge in the active supernode
                AbstractEdge dataEdge = resolveEdgeById(snapshotTarget != null ? snapshotTarget : sceneFlow, edgeId);
                if (dataEdge == null) {
                    return errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
                }

                // Find source, old target, and new target nodes
                BasicNode sourceNode = dataEdge.getSourceNode();
                BasicNode oldTargetNode = dataEdge.getTargetNode();
                BasicNode newTargetNode = resolveNodeById(snapshotTarget != null ? snapshotTarget : sceneFlow, targetId);

                if (sourceNode == null || newTargetNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Source or target node not found");
                }

                // Get node dimensions for dock point handling
                int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
                int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);

                // Release old target's dock point
                if (oldTargetNode != null) {
                    EdgeGraphics edgeGraphics = dataEdge.getGraphics();
                    if (edgeGraphics != null && edgeGraphics.getConnection() != null) {
                        List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
                        if (points != null && points.size() >= 2) {
                            EdgePoint endPt = points.get(points.size() - 1);
                            NodeGraphics oldTgtGraphics = oldTargetNode.getGraphics();
                            NodePosition oldTgtPos = oldTgtGraphics != null ? oldTgtGraphics.getPosition() : null;
                            double oldTgtX = oldTgtPos != null ? oldTgtPos.getXPos() : 0;
                            double oldTgtY = oldTgtPos != null ? oldTgtPos.getYPos() : 0;
                            int oldDockIdx = mEdgeLayout.findDockPointIndex(oldTgtX, oldTgtY, nodeWidth, nodeHeight,
                                    oldTargetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
                            if (oldDockIdx >= 0) {
                                mEdgeLayout.releaseDockPoint(oldTargetNode.getId(), oldDockIdx, false);
                            }
                        }
                    }
                }

                // Find and occupy new dock point on new target, update edge endpoint
                NodeGraphics newTgtGraphics = newTargetNode.getGraphics();
                NodePosition newTgtPos = newTgtGraphics != null ? newTgtGraphics.getPosition() : null;
                double newTgtX = newTgtPos != null ? newTgtPos.getXPos() : 0;
                double newTgtY = newTgtPos != null ? newTgtPos.getYPos() : 0;
                boolean newTgtIsSuperNode = newTargetNode instanceof SuperNode;

                // Get source position for dock point selection
                NodeGraphics srcGraphics = sourceNode.getGraphics();
                NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
                double srcX = srcPos != null ? srcPos.getXPos() : 0;
                double srcY = srcPos != null ? srcPos.getYPos() : 0;
                boolean srcIsSuperNode = sourceNode instanceof SuperNode;

                // Find best dock point pair for new configuration
                int[] dockPair;
                boolean isSelfLoop = sourceNode.getId().equals(newTargetNode.getId());
                if (isSelfLoop) {
                    dockPair = mEdgeLayout.findSelfLoopDockPointPair(sourceNode.getId(), nodeWidth, nodeHeight, srcIsSuperNode);
                } else {
                    dockPair = mEdgeLayout.findBestDockPointPair(
                        sourceNode.getId(), srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode,
                        newTargetNode.getId(), newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode
                    );
                }
                int newTgtDockIdx = dockPair[1];
                mEdgeLayout.occupyDockPoint(newTargetNode.getId(), newTgtDockIdx, false);

                // Update edge endpoint to new dock position
                double[] newTgtDock = mEdgeLayout.getDockPointPosition(newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode, newTgtDockIdx);
                EdgeGraphics edgeGraphics = dataEdge.getGraphics();
                if (edgeGraphics != null && edgeGraphics.getConnection() != null) {
                    List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
                    if (points != null && points.size() >= 2) {
                        EdgePoint endPt = points.get(points.size() - 1);
                        EdgePoint startPt = points.get(0);
                        // Update end position
                        endPt.setXPos((int) Math.round(newTgtDock[0]));
                        endPt.setYPos((int) Math.round(newTgtDock[1]));
                        // Update control point
                        if (isSelfLoop) {
                            // For self-loops, use special control points
                            double nodeCenterX = srcX + nodeWidth / 2.0;
                            double nodeCenterY = srcY + nodeHeight / 2.0;
                            double[] loopCtrl = mEdgeLayout.computeSelfLoopControlPoints(
                                startPt.getXPos(), startPt.getYPos(),
                                newTgtDock[0], newTgtDock[1],
                                nodeCenterX, nodeCenterY, nodeWidth, nodeHeight
                            );
                            startPt.setCtrlXPos((int) Math.round(loopCtrl[0]));
                            startPt.setCtrlYPos((int) Math.round(loopCtrl[1]));
                            endPt.setCtrlXPos((int) Math.round(loopCtrl[2]));
                            endPt.setCtrlYPos((int) Math.round(loopCtrl[3]));
                        } else {
                            double[] tgtCtrl = mEdgeLayout.computeInitialControlPoint(
                                startPt.getXPos(), startPt.getYPos(),
                                newTgtDock[0], newTgtDock[1], false
                            );
                            endPt.setCtrlXPos((int) Math.round(tgtCtrl[0]));
                            endPt.setCtrlYPos((int) Math.round(tgtCtrl[1]));
                        }
                    }
                }

                // Retarget the edge - remove from source's edge list
                if (dataEdge instanceof GuargedEdge) {
                    sourceNode.removeCEdge((GuargedEdge) dataEdge);
                } else if (dataEdge instanceof InterruptEdge) {
                    sourceNode.removeIEdge((InterruptEdge) dataEdge);
                } else if (dataEdge instanceof RandomEdge) {
                    sourceNode.removePEdge((RandomEdge) dataEdge);
                } else if (dataEdge instanceof ForkingEdge) {
                    sourceNode.removeFEdge((ForkingEdge) dataEdge);
                } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
                    sourceNode.removeDEdge();
                }

                // Update target references
                dataEdge.setTargetNode(newTargetNode);
                dataEdge.setTargetUnid(newTargetNode.getId());

                // Add edge back to source node's edge list with new target
                if (dataEdge instanceof GuargedEdge) {
                    sourceNode.addCEdge((GuargedEdge) dataEdge);
                } else if (dataEdge instanceof InterruptEdge) {
                    sourceNode.addIEdge((InterruptEdge) dataEdge);
                } else if (dataEdge instanceof RandomEdge) {
                    sourceNode.addPEdge((RandomEdge) dataEdge);
                } else if (dataEdge instanceof ForkingEdge) {
                    sourceNode.addFEdge((ForkingEdge) dataEdge);
                } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
                    sourceNode.setDedge(dataEdge);
                }

                JSONObject response = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                response.put("status", "ok");
                response.put("edgeId", edgeId);
                broadcastSceneFlowSnapshot(broadcaster, pid, response);
                recordHistory(ref, "SceneFlow.Edge.Retarget");
                recordCommand(ref, "SceneFlow.Edge.Retarget", params);
                return response;
            }

            // Runtime variable operations
            case "Runtime.Variable.Set": {
                String pid = params.optString("projectId", "");
                String varName = params.optString("name", "");
                String valueExpr = params.optString("value", "");
                if (valueExpr.isBlank()) {
                    valueExpr = params.optString("valueExpr", "");
                }

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (varName.isBlank() || valueExpr.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing name or value");
                }

                RunTimeProject rtp = ref.runtimeProject;
                SceneFlow sceneFlow = rtp.getSceneFlow();

                // Find the variable definition
                VariableDefinition varDef = findVariableDefinitionInHierarchy(sceneFlow, varName);
                if (varDef == null) {
                    return errorResponse("VAR_NOT_FOUND", "Variable not found: " + varName);
                }

                // Parse the expression
                Expression exp;
                try {
                    Object parsed = GlueParser.run(valueExpr.trim());
                    if (!(parsed instanceof Expression)) {
                        return errorResponse("PARSE_FAILED", "Expression could not be parsed");
                    }
                    exp = (Expression) parsed;
                } catch (Exception exc) {
                    return errorResponse("PARSE_FAILED", exc.getMessage() != null ? exc.getMessage() : "Parse failed");
                }

                // Check if expression type is supported
                if (!isSupportedRuntimeExpression(exp)) {
                    return errorResponse("UNSUPPORTED_EXPRESSION", "Expression type is not supported");
                }

                // Apply the expression
                boolean setOk = applyRuntimeExpression(rtp, varName, exp);
                if (!setOk) {
                    return errorResponse("SET_FAILED", "Failed to update variable");
                }

                JSONObject response = new JSONObject();
                response.put("status", "ok");
                response.put("projectId", pid);
                response.put("name", varName);
                String currentValue = resolveVariableValue(rtp, varName);
                if (currentValue != null) {
                    response.put("value", currentValue);
                }
                return response;
            }

            case "Runtime.Query": {
                String pid = params.optString("projectId", "");
                String query = params.optString("query", "");

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (query.isBlank()) {
                    return errorResponse("BAD_REQUEST", "Missing query");
                }

                JSONObject response = new JSONObject();
                response.put("status", "ok");
                int count = 0;
                try {
                    count = de.dfki.vsm.util.jpl.JPLEngine.query(query.trim()).size();
                } catch (Exception exc) {
                    sLogger.warning("Runtime.Query failed: " + exc.getMessage());
                }
                response.put("count", count);
                return response;
            }

            case "SceneFlow.Edge.PEdge.UpdateGroup": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);
                String sourceId = params.optString("sourceId", "");
                JSONArray updates = params.optJSONArray("updates");

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (sourceId.isBlank() || updates == null) {
                    return errorResponse("BAD_REQUEST", "Missing sourceId or updates");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

                BasicNode sourceNode = resolveNodeById(targetNode, sourceId);
                if (sourceNode == null) {
                    return errorResponse("NODE_NOT_FOUND", "Source node not found: " + sourceId);
                }

                List<RandomEdge> edges = sourceNode.getPEdgeList();
                if (edges.isEmpty()) {
                    return errorResponse("EDGE_NOT_FOUND", "No probability edges found");
                }

                // Build update map
                java.util.LinkedHashMap<RandomEdge, Integer> updateMap = new java.util.LinkedHashMap<>();
                for (int i = 0; i < updates.length(); i++) {
                    JSONObject entry = updates.optJSONObject(i);
                    if (entry == null) {
                        return errorResponse("INVALID_PAYLOAD", "Invalid edge update entry");
                    }
                    String edgeId = entry.optString("edgeId", "");
                    String targetId = entry.optString("targetId", "");
                    RandomEdge edge = resolvePEdgeForSource(targetNode, sourceNode, edgeId, targetId);
                    if (edge == null) {
                        return errorResponse("EDGE_NOT_FOUND", "Edge not found");
                    }
                    if (updateMap.containsKey(edge)) {
                        return errorResponse("DUPLICATE_EDGE", "Duplicate edge entry");
                    }
                    Object raw = entry.opt("probability");
                    int probability;
                    try {
                        probability = Integer.parseInt(String.valueOf(raw));
                    } catch (NumberFormatException ex) {
                        return errorResponse("INVALID_PROBABILITY", "Probability must be a number");
                    }
                    if (probability < 0 || probability > 100) {
                        return errorResponse("INVALID_PROBABILITY", "Probability must be between 0 and 100");
                    }
                    updateMap.put(edge, probability);
                }

                if (updateMap.size() != edges.size()) {
                    return errorResponse("EDGE_COUNT_MISMATCH", "Provide probabilities for all P-edges");
                }

                int sum = 0;
                for (int probability : updateMap.values()) {
                    sum += probability;
                }
                if (sum != 100) {
                    return errorResponse("PROBABILITY_SUM_INVALID", "Total probability must be 100%");
                }

                // Apply probabilities
                for (java.util.Map.Entry<RandomEdge, Integer> entry : updateMap.entrySet()) {
                    RandomEdge edge = entry.getKey();
                    if (edge != null) {
                        edge.setProbability(entry.getValue());
                    }
                }

                JSONObject response = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                response.put("status", "ok");
                broadcastSceneFlowSnapshot(broadcaster, pid, response);
                recordHistory(ref, "SceneFlow.Edge.PEdge.UpdateGroup");
                recordCommand(ref, "SceneFlow.Edge.PEdge.UpdateGroup", params);
                return response;
            }

            case "SceneFlow.Node.MoveGroup": {
                String pid = params.optString("projectId", "");
                String superNodeId = params.optString("superNodeId", null);
                JSONArray nodesPayload = params.optJSONArray("nodes");
                boolean snap = params.optBoolean("snap", false);

                ProjectRef ref = projectStore.get(pid);
                if (ref == null || ref.runtimeProject == null) {
                    return errorResponse("PROJECT_NOT_FOUND", "Project not found");
                }
                if (nodesPayload == null || nodesPayload.length() == 0) {
                    return errorResponse("BAD_REQUEST", "Missing nodes");
                }

                SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
                SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
                SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

                // Get node dimensions and grid settings
                int nodeW = getEditorConfigInt(ref, "node_width", 90);
                int nodeH = getEditorConfigInt(ref, "node_height", nodeW);
                // Grid scale factors (1 = one node width/height per grid cell)
                int gridScaleX = getEditorConfigInt(ref, "grid_x", 1);
                int gridScaleY = getEditorConfigInt(ref, "grid_y", gridScaleX);
                // Actual grid cell size in pixels
                int gridX = Math.max(8, nodeW * gridScaleX);
                int gridY = Math.max(8, nodeH * gridScaleY);
                // Grid origin offset (matches frontend)
                double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
                double snapOriginY = nodeH / 2.0 + nodeH / 3.0;

                // Store old positions and moved nodes
                java.util.IdentityHashMap<BasicNode, int[]> oldPositions = new java.util.IdentityHashMap<>();
                List<BasicNode> movedNodes = new ArrayList<>();

                for (int i = 0; i < nodesPayload.length(); i++) {
                    JSONObject entry = nodesPayload.optJSONObject(i);
                    if (entry == null) {
                        return errorResponse("BAD_REQUEST", "Invalid nodes entry");
                    }
                    String moveId = entry.optString("id", "");
                    double moveX = entry.has("x") ? entry.optDouble("x", Double.NaN) : Double.NaN;
                    double moveY = entry.has("y") ? entry.optDouble("y", Double.NaN) : Double.NaN;
                    if (moveId.isBlank() || Double.isNaN(moveX) || Double.isNaN(moveY)) {
                        return errorResponse("BAD_REQUEST", "Missing node id or coordinates");
                    }

                    // Use findNodeRecursive like single-node move to find nodes anywhere in hierarchy
                    BasicNode dataNode = findNodeRecursive(sceneFlow, moveId);
                    if (dataNode == null) {
                        return errorResponse("NODE_NOT_FOUND", "Node not found: " + moveId);
                    }

                    // Capture old position
                    NodeGraphics oldGraphics = dataNode.getGraphics();
                    NodePosition oldPos = oldGraphics != null ? oldGraphics.getPosition() : null;
                    int oldX = oldPos != null ? oldPos.getXPos() : 0;
                    int oldY = oldPos != null ? oldPos.getYPos() : 0;
                    oldPositions.put(dataNode, new int[] { oldX, oldY });

                    // Calculate target position
                    int targetX = Math.max(1, (int) Math.round(moveX));
                    int targetY = Math.max(1, (int) Math.round(moveY));
                    if (snap) {
                        double centerX = targetX + nodeW / 2.0;
                        double centerY = targetY + nodeH / 2.0;
                        double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
                        double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
                        targetX = (int) Math.round(snappedCenterX - nodeW / 2.0);
                        targetY = (int) Math.round(snappedCenterY - nodeH / 2.0);
                    }

                    // Update node graphics
                    NodeGraphics graphics = dataNode.getGraphics();
                    if (graphics == null) {
                        graphics = new NodeGraphics(targetX, targetY);
                        dataNode.setGraphics(graphics);
                    } else {
                        graphics.setPosition(targetX, targetY);
                    }
                    movedNodes.add(dataNode);
                }

                // Update edge endpoints
                for (BasicNode movedNode : movedNodes) {
                    int[] oldPos = oldPositions.get(movedNode);
                    updateEdgeEndpointsForMovedNode(movedNode, activeSuperNode, oldPos[0], oldPos[1]);
                }

                JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
                JSONObject response = buildSceneFlowResponse(snapshot);
                broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.MoveGroup");
                recordCommand(ref, "SceneFlow.Node.MoveGroup", params);
                return response;
            }

            default:
                JSONObject unknown = new JSONObject();
                unknown.put("message", "Unhandled method: " + method);
                return unknown;
        }
    }

    /**
     * Returns true if the given WS method is an editing command
     * (as opposed to read-only queries or runtime control).
     */
    private boolean isEditingCommand(String method) {
        if (method == null) return false;
        return method.startsWith("SceneFlow.Node.")
                || method.startsWith("SceneFlow.Edge.")
                || method.startsWith("SceneFlow.Comment.")
                || "SceneFlow.Undo".equals(method)
                || "SceneFlow.Redo".equals(method)
                || "SceneFlow.PlayScene.Rename".equals(method)
                || method.startsWith("Script.")
                || "Config.Update".equals(method)
                || "ProjectConfig.Update".equals(method)
                || "Preferences.Update".equals(method)
                || "Embeddings.Start".equals(method);
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

    private JSONObject buildSceneFlowResponse(JSONObject snapshot) {
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("snapshot", snapshot);
        return resp;
    }

    private void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
        if (broadcaster == null) {
            return;
        }
        JSONObject evt = new JSONObject();
        evt.put("event", "sceneflow.snapshot");
        evt.put("projectId", projectId);
        evt.put("snapshot", snapshot);
        broadcaster.accept(evt.toString());
    }

    /**
     * Copy template files from a classpath resource URL to a destination directory.
     * Supports both file:// and jar:// URLs.
     */
    private void copyTemplateDirectory(URL resourceUrl, String basePath, File destDir,
            JSONArray createdFiles, JSONArray skippedFiles) {
        try {
            String protocol = resourceUrl.getProtocol();
            if ("file".equals(protocol)) {
                // Resource is in filesystem (development mode)
                File sourceDir = new File(resourceUrl.toURI());
                if (sourceDir.isDirectory()) {
                    copyDirectoryContents(sourceDir, destDir, destDir.getName(), createdFiles, skippedFiles);
                }
            } else if ("jar".equals(protocol)) {
                // Resource is inside a JAR file
                String jarPath = resourceUrl.getPath();
                int jarSeparator = jarPath.indexOf("!");
                if (jarSeparator > 0) {
                    String jarFilePath = jarPath.substring(5, jarSeparator); // Remove "file:"
                    String resourcePrefix = jarPath.substring(jarSeparator + 2); // Remove "!/"
                    try (java.util.jar.JarFile jar = new java.util.jar.JarFile(jarFilePath)) {
                        Enumeration<java.util.jar.JarEntry> entries = jar.entries();
                        while (entries.hasMoreElements()) {
                            java.util.jar.JarEntry entry = entries.nextElement();
                            String entryName = entry.getName();
                            if (entryName.startsWith(resourcePrefix) && !entry.isDirectory()) {
                                String relativePath = entryName.substring(resourcePrefix.length());
                                if (relativePath.isEmpty()) continue;
                                File destFile = new File(destDir, relativePath);
                                if (destFile.exists()) {
                                    skippedFiles.put(destDir.getName() + "/" + relativePath);
                                    continue;
                                }
                                destFile.getParentFile().mkdirs();
                                try (InputStream in = jar.getInputStream(entry);
                                     java.io.FileOutputStream out = new java.io.FileOutputStream(destFile)) {
                                    byte[] buffer = new byte[4096];
                                    int bytesRead;
                                    while ((bytesRead = in.read(buffer)) != -1) {
                                        out.write(buffer, 0, bytesRead);
                                    }
                                }
                                createdFiles.put(destDir.getName() + "/" + relativePath);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            sLogger.warning("Failed to copy template directory: " + ex.getMessage());
        }
    }

    /**
     * Recursively copy contents of a source directory to a destination directory.
     */
    private void copyDirectoryContents(File sourceDir, File destDir, String pathPrefix,
            JSONArray createdFiles, JSONArray skippedFiles) {
        File[] files = sourceDir.listFiles();
        if (files == null) return;
        for (File file : files) {
            String relativePath = pathPrefix + "/" + file.getName();
            File destFile = new File(destDir, file.getName());
            if (file.isDirectory()) {
                if (!destFile.exists()) {
                    destFile.mkdirs();
                }
                copyDirectoryContents(file, destFile, relativePath, createdFiles, skippedFiles);
            } else {
                if (destFile.exists()) {
                    skippedFiles.put(relativePath);
                    continue;
                }
                try (InputStream in = new java.io.FileInputStream(file);
                     java.io.FileOutputStream out = new java.io.FileOutputStream(destFile)) {
                    byte[] buffer = new byte[4096];
                    int bytesRead;
                    while ((bytesRead = in.read(buffer)) != -1) {
                        out.write(buffer, 0, bytesRead);
                    }
                    createdFiles.put(relativePath);
                } catch (Exception ex) {
                    sLogger.warning("Failed to copy file " + file.getName() + ": " + ex.getMessage());
                }
            }
        }
    }

    private int safeRound(Double value, int fallback) {
        if (value == null || Double.isNaN(value) || Double.isInfinite(value)) {
            return fallback;
        }
        return (int) Math.round(value);
    }

    private int computeNextNodeIndex(RunTimeProject rtp) {
        return computeNextNodeIndex(rtp, false);
    }

    private int computeNextSuperNodeIndex(RunTimeProject rtp) {
        return computeNextNodeIndex(rtp, true);
    }

    private int computeNextNodeIndex(RunTimeProject rtp, boolean superNodes) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return 1;
        }
        int max = 0;
        SceneFlow sceneFlow = rtp.getSceneFlow();
        List<BasicNode> nodes = new ArrayList<>();
        collectNodes(sceneFlow, nodes);
        for (BasicNode node : nodes) {
            if (node == null) continue;
            boolean isRoot = node instanceof SuperNode && ((SuperNode) node).getParentNode() == null;
            if (isRoot) continue;
            boolean isSuper = node instanceof SuperNode;
            if (isSuper != superNodes) continue;
            Integer val = parseNodeIndex(node.getId(), superNodes);
            if (val != null && val > max) {
                max = val;
            }
        }
        return max + 1;
    }

    private Integer parseNodeIndex(String id, boolean superNode) {
        if (id == null) return null;
        String trimmed = id.trim();
        if (trimmed.isEmpty()) return null;
        String prefix = superNode ? "S" : "N";
        if (trimmed.length() > 1 && trimmed.startsWith(prefix)) {
            String rest = trimmed.substring(1);
            if (rest.matches("\\d+")) {
                try {
                    return Integer.parseInt(rest);
                } catch (NumberFormatException ignore) {
                    return null;
                }
            }
            return null;
        }
        if (trimmed.matches("\\d+")) {
            try {
                return Integer.parseInt(trimmed);
            } catch (NumberFormatException ignore) {
                return null;
            }
        }
        return null;
    }

    private String allocateNodeId(ProjectRef ref, boolean superNode, Set<String> used) {
        String prefix = superNode ? "S" : "N";
        int next = superNode ? ref.nextSuperNodeIndex : ref.nextNodeIndex;
        if (next < 1) {
            next = 1;
        }
        String candidate;
        do {
            candidate = prefix + next;
            next += 1;
        } while (used != null && used.contains(candidate));
        if (superNode) {
            ref.nextSuperNodeIndex = next;
        } else {
            ref.nextNodeIndex = next;
        }
        return candidate;
    }

    private String normalizeNodeId(String id, boolean superNode, ProjectRef ref, Set<String> used) {
        String trimmed = id == null ? "" : id.trim();
        String prefix = superNode ? "S" : "N";
        if (!trimmed.isEmpty()) {
            if (trimmed.startsWith(prefix) && trimmed.substring(1).matches("\\d+") && (used == null || !used.contains(trimmed))) {
                return trimmed;
            }
            if (trimmed.matches("\\d+")) {
                String candidate = prefix + trimmed;
                if (used == null || !used.contains(candidate)) {
                    return candidate;
                }
            }
        }
        return allocateNodeId(ref, superNode, used);
    }

    private JSONObject createNodeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> addNode(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        String nodeType = params.optString("nodeType", params.optString("type", "Basic"));
        String nodeId = params.optString("nodeId", "").trim();
        boolean isSuperNode = "Super".equalsIgnoreCase(nodeType);
        Set<String> usedIds = new java.util.HashSet<>();
        List<BasicNode> existingNodes = new ArrayList<>();
        collectNodes(sceneFlow, existingNodes);
        for (BasicNode existing : existingNodes) {
            if (existing == null) continue;
            String existingId = existing.getId();
            if (existingId != null) {
                usedIds.add(existingId);
            }
        }
        if (nodeId.isBlank()) {
            nodeId = allocateNodeId(ref, isSuperNode, usedIds);
        } else {
            nodeId = normalizeNodeId(nodeId, isSuperNode, ref, usedIds);
        }
        String name = params.optString("name", "").trim();
        if (name.isBlank()) {
            String label = isSuperNode ? "Supernode " : "Node ";
            name = label + nodeId;
        }

        BasicNode node = isSuperNode ? new SuperNode() : new BasicNode();
        node.setId(nodeId);
        node.setName(name);
        node.setGraphics(new NodeGraphics(
            safeRound(params.has("x") ? params.optDouble("x") : null, 0),
            safeRound(params.has("y") ? params.optDouble("y") : null, 0)
        ));
        node.setParentNode(activeSuperNode);
        if (node instanceof SuperNode) {
            activeSuperNode.addSuperNode((SuperNode) node);
        } else {
            activeSuperNode.addNode(node);
        }
        boolean hasStart = activeSuperNode.getStartNodeMap() != null && !activeSuperNode.getStartNodeMap().isEmpty();
        if (params.optBoolean("isStart", false) || !hasStart) {
            activeSuperNode.addStartNode(node);
        }

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        resp.put("nodeId", nodeId);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Create");
                recordCommand(ref, "SceneFlow.Node.Create", params);
                return resp;
            }

    private JSONObject updateNodeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> updateNode(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        String nodeId = params.optString("nodeId", "");
        JSONObject fields = params.optJSONObject("fields");
        if (fields == null) {
            fields = new JSONObject();
        }

        BasicNode dataNode =
            nodeId.isBlank() ? (snapshotTarget != null ? snapshotTarget : sceneFlow) : findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }
        SuperNode activeSuperNode = dataNode.getParentNode() != null ? dataNode.getParentNode() : sceneFlow;

        if (fields.has("name")) {
            String nextName = fields.optString("name", "").trim();
            dataNode.setName(nextName);
        }
        if (fields.has("comment")) {
            dataNode.setComment(fields.optString("comment", ""));
        }
        if (fields.has("isHistory")) {
            dataNode.setHistoryNodeFlag(fields.optBoolean("isHistory", false));
        }
        if (fields.has("isStart") && dataNode != activeSuperNode) {
            boolean isStart = fields.optBoolean("isStart", false);
            if (isStart) {
                activeSuperNode.addStartNode(dataNode);
            } else {
                activeSuperNode.removeStartNode(dataNode);
            }
        }

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Update");
                recordCommand(ref, "SceneFlow.Node.Update", params);
                return resp;
            }

    private JSONObject deleteNodeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> deleteNode(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        String nodeId = params.optString("nodeId", "");
        if (nodeId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing nodeId");
        }

        BasicNode dataNode = findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }
        SuperNode activeSuperNode = dataNode.getParentNode() != null ? dataNode.getParentNode() : sceneFlow;
        activeSuperNode.removeStartNode(dataNode);
        if (dataNode instanceof SuperNode) {
            activeSuperNode.removeSuperNode((SuperNode) dataNode);
        } else {
            activeSuperNode.removeNode(dataNode);
        }
        // Remove edges connected to this node within the active supernode
        for (BasicNode node : activeSuperNode.getNodeAndSuperNodeList()) {
            node.getCEdgeList().removeIf(edge -> nodeId.equals(edge.getTargetUnid()) || nodeId.equals(edge.getSourceUnid()));
            node.getIEdgeList().removeIf(edge -> nodeId.equals(edge.getTargetUnid()) || nodeId.equals(edge.getSourceUnid()));
            node.getPEdgeList().removeIf(edge -> nodeId.equals(edge.getTargetUnid()) || nodeId.equals(edge.getSourceUnid()));
            node.getFEdgeList().removeIf(edge -> nodeId.equals(edge.getTargetUnid()) || nodeId.equals(edge.getSourceUnid()));
            AbstractEdge dEdge = node.getDedge();
            if (dEdge != null && (nodeId.equals(dEdge.getTargetUnid()) || nodeId.equals(dEdge.getSourceUnid()))) {
                node.removeDEdge();
            }
        }

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Delete");
                recordCommand(ref, "SceneFlow.Node.Delete", params);
                return resp;
            }

    private JSONObject moveNodeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        String nodeId = params.optString("nodeId", "");
        if (nodeId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing nodeId");
        }
        double moveX = params.has("x") ? params.optDouble("x", Double.NaN) : Double.NaN;
        double moveY = params.has("y") ? params.optDouble("y", Double.NaN) : Double.NaN;
        if (Double.isNaN(moveX) || Double.isNaN(moveY)) {
            return errorResponse("BAD_REQUEST", "Missing coordinates");
        }
        boolean snap = params.optBoolean("snap", false);

        BasicNode dataNode = findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        int nodeW = getEditorConfigInt(ref, "node_width", 90);
        int nodeH = getEditorConfigInt(ref, "node_height", nodeW);
        // Grid scale factors (1 = one node width/height per grid cell)
        int gridScaleX = getEditorConfigInt(ref, "grid_x", 1);
        int gridScaleY = getEditorConfigInt(ref, "grid_y", gridScaleX);
        // Actual grid cell size in pixels
        int gridX = Math.max(8, nodeW * gridScaleX);
        int gridY = Math.max(8, nodeH * gridScaleY);
        // Grid origin offset (matches frontend)
        double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
        double snapOriginY = nodeH / 2.0 + nodeH / 3.0;

        int targetX = Math.max(1, (int) Math.round(moveX));
        int targetY = Math.max(1, (int) Math.round(moveY));
        if (snap) {
            double centerX = targetX + nodeW / 2.0;
            double centerY = targetY + nodeH / 2.0;
            double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
            double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
            targetX = (int) Math.round(snappedCenterX - nodeW / 2.0);
            targetY = (int) Math.round(snappedCenterY - nodeH / 2.0);
        }

        NodeGraphics graphics = dataNode.getGraphics();
        if (graphics == null) {
            graphics = new NodeGraphics();
            dataNode.setGraphics(graphics);
        }
        NodePosition oldPos = graphics.getPosition();
        int oldX = oldPos != null ? oldPos.getXPos() : 0;
        int oldY = oldPos != null ? oldPos.getYPos() : 0;
        graphics.setPosition(targetX, targetY);
        updateEdgeEndpointsForMovedNode(dataNode, snapshotTarget != null ? snapshotTarget : sceneFlow, oldX, oldY);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Node.Move");
                recordCommand(ref, "SceneFlow.Node.Move", params);
                return resp;
            }

    private JSONObject createEdgeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> addEdge(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        String sourceId = params.optString("sourceId", params.optString("source", ""));
        String targetId = params.optString("targetId", params.optString("target", ""));
        if (sourceId.isBlank() || targetId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing sourceId or targetId");
        }

        BasicNode sourceNode = resolveNodeById(activeSuperNode, sourceId);
        BasicNode targetNode = resolveNodeById(activeSuperNode, targetId);
        if (sourceNode == null || targetNode == null) {
            return errorResponse("NODE_NOT_FOUND", "Source or target node not found");
        }

        String edgeType = params.optString("edgeType", params.optString("type", "EEDGE")).trim().toUpperCase();
        String edgeConstraintError = validateEdgeCreateConstraints(sourceNode, edgeType);
        if (edgeConstraintError != null) {
            return errorResponse("EDGE_NOT_ALLOWED", edgeConstraintError);
        }
        AbstractEdge edge;
        switch (edgeType) {
            case "CEDGE":
                edge = new GuargedEdge();
                if (sourceNode.getCEdgeList() == null || sourceNode.getCEdgeList().isEmpty()) {
                    ((GuargedEdge) edge).setCondition(parseExpressionOrNull("true"));
                } else {
                    ((GuargedEdge) edge).setCondition(parseExpressionOrNull("false"));
                }
                sourceNode.addCEdge((GuargedEdge) edge);
                break;
            case "IEDGE":
                edge = new InterruptEdge();
                if (sourceNode.getIEdgeList() == null || sourceNode.getIEdgeList().isEmpty()) {
                    ((InterruptEdge) edge).setCondition(parseExpressionOrNull("true"));
                } else {
                    ((InterruptEdge) edge).setCondition(parseExpressionOrNull("false"));
                }
                sourceNode.addIEdge((InterruptEdge) edge);
                break;
            case "PEDGE": {
                RandomEdge redge = new RandomEdge();
                int probability = (sourceNode.getPEdgeList() == null || sourceNode.getPEdgeList().isEmpty()) ? 100 : 0;
                redge.setProbability(probability);
                edge = redge;
                sourceNode.addPEdge((RandomEdge) edge);
                break;
            }
            case "FEDGE":
                edge = new ForkingEdge();
                sourceNode.addFEdge((ForkingEdge) edge);
                break;
            case "TEDGE": {
                TimeoutEdge ted = new TimeoutEdge();
                try {
                    ted.setTimeout(1000);
                } catch (NumberFormatException ignore) {
                    // ignore
                }
                edge = ted;
                sourceNode.setDedge(edge);
                break;
            }
            case "EEDGE":
            default:
                edge = new EpsilonEdge();
                sourceNode.setDedge(edge);
                break;
        }

        edge.setSourceNode(sourceNode);
        edge.setTargetNode(targetNode);
        edge.setSourceUnid(sourceNode.getId());
        edge.setTargetUnid(targetNode.getId());
        edge.setGraphics(new EdgeGraphics());
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        mEdgeLayout.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Edge.Create");
                recordCommand(ref, "SceneFlow.Edge.Create", params);
                return resp;
            }

    private String validateEdgeCreateConstraints(BasicNode sourceNode, String edgeType) {
        if (sourceNode == null) {
            return "Source node not found";
        }
        String type = edgeType == null ? "" : edgeType.trim().toUpperCase();
        boolean hasC = sourceNode.getCEdgeList() != null && !sourceNode.getCEdgeList().isEmpty();
        boolean hasP = sourceNode.getPEdgeList() != null && !sourceNode.getPEdgeList().isEmpty();
        boolean hasI = sourceNode.getIEdgeList() != null && !sourceNode.getIEdgeList().isEmpty();
        boolean hasF = sourceNode.getFEdgeList() != null && !sourceNode.getFEdgeList().isEmpty();
        AbstractEdge dEdge = sourceNode.getDedge();
        boolean hasE = dEdge instanceof EpsilonEdge;
        boolean hasT = dEdge instanceof TimeoutEdge;
        boolean hasD = dEdge != null;

        if (hasP) {
            return "PEDGE".equals(type) ? null : "Only probabilistic edges are allowed on this node";
        }
        if (hasI) {
            return "IEDGE".equals(type) ? null : "Only interrupt edges are allowed on this node";
        }
        if (hasF) {
            return "FEDGE".equals(type) ? null : "Only fork edges are allowed on this node";
        }

        if (hasC) {
            if ("CEDGE".equals(type)) {
                return null;
            }
            if ("EEDGE".equals(type) || "TEDGE".equals(type)) {
                return hasD ? "Only one default/timeout edge is allowed on this node" : null;
            }
            return "Only conditional edges are allowed (plus one epsilon or timeout edge)";
        }

        if (hasD) {
            if ("CEDGE".equals(type)) {
                return null;
            }
            if (hasE) {
                return "Only conditional edges can be combined with an epsilon edge";
            }
            if (hasT) {
                return "Only conditional edges can be combined with a timeout edge";
            }
            return "Only conditional edges can be combined with the default edge";
        }

        return null;
    }

    private JSONObject updateEdgeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> updateEdge(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String edgeId = params.optString("edgeId", "");
        if (edgeId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing edgeId");
        }
        AbstractEdge edge = resolveEdgeById(activeSuperNode, edgeId);
        if (edge == null) {
            return errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }

        JSONObject fields = params.optJSONObject("fields");
        if (fields == null) {
            fields = new JSONObject();
        }

        if (fields.has("points")) {
            JSONArray points = fields.optJSONArray("points");
            if (points != null) {
                EdgeGraphics graphics = edge.getGraphics();
                if (graphics == null) {
                    graphics = new EdgeGraphics();
                    edge.setGraphics(graphics);
                }
                EdgeArrow arrow = graphics.getConnection();
                if (arrow == null) {
                    arrow = new EdgeArrow();
                    graphics.setConnection(arrow);
                }
                ArrayList<EdgePoint> pointList = new ArrayList<>();
                for (int i = 0; i < points.length(); i++) {
                    JSONObject pt = points.optJSONObject(i);
                    if (pt == null) continue;
                    int x = safeRound(pt.has("x") ? pt.optDouble("x") : null, 0);
                    int y = safeRound(pt.has("y") ? pt.optDouble("y") : null, 0);
                    int cx = safeRound(pt.has("cx") ? pt.optDouble("cx") : null, x);
                    int cy = safeRound(pt.has("cy") ? pt.optDouble("cy") : null, y);
                    pointList.add(new EdgePoint(x, cx, y, cy));
                }
                arrow.setPointList(pointList);
            }
        }

        if (fields.has("condition")) {
            String conditionText = fields.optString("condition", "").trim();
            if (edge instanceof GuargedEdge) {
                ((GuargedEdge) edge).setCondition(parseExpressionOrNull(conditionText));
            } else if (edge instanceof InterruptEdge) {
                ((InterruptEdge) edge).setCondition(parseExpressionOrNull(conditionText));
            }
        }
        if (fields.has("timeoutMs") || fields.has("timeoutExpr")) {
            if (edge instanceof TimeoutEdge) {
                TimeoutEdge te = (TimeoutEdge) edge;
                if (fields.has("timeoutMs")) {
                    try {
                        te.setTimeout(fields.optLong("timeoutMs", 0));
                        te.setExpression(null);
                    } catch (NumberFormatException ignore) {
                        // ignore invalid timeout
                    }
                } else {
                    String exprText = fields.optString("timeoutExpr", "").trim();
                    te.setExpression(parseExpressionOrNull(exprText));
                }
            }
        }
        if (fields.has("altStartMap") && (edge instanceof GuargedEdge || edge instanceof InterruptEdge)) {
            JSONArray entries = fields.optJSONArray("altStartMap");
            if (entries != null) {
                edge.getAltMap().clear();
                for (int i = 0; i < entries.length(); i++) {
                    JSONObject entry = entries.optJSONObject(i);
                    if (entry == null) continue;
                    String startId = entry.optString("startId", "").trim();
                    String altStartId = entry.optString("altStartId", "").trim();
                    if (startId.isEmpty() || altStartId.isEmpty()) continue;
                    BasicNode startNode = resolveNodeById(activeSuperNode, startId);
                    BasicNode altNode = resolveNodeById(activeSuperNode, altStartId);
                    if (startNode == null || altNode == null) continue;
                    Tuple<String, BasicNode> startTuple = new Tuple<>(startId, startNode);
                    Tuple<String, BasicNode> altTuple = new Tuple<>(altStartId, altNode);
                    edge.getAltMap().put(startTuple, altTuple);
                }
            }
        }

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Edge.Update");
                recordCommand(ref, "SceneFlow.Edge.Update", params);
                return resp;
            }

    private JSONObject deleteEdgeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> deleteEdge(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String edgeId = params.optString("edgeId", "");
        if (edgeId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing edgeId");
        }
        AbstractEdge dataEdge = resolveEdgeById(activeSuperNode, edgeId);
        if (dataEdge == null) {
            return errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }
        BasicNode sourceNode = dataEdge.getSourceNode();
        if (sourceNode != null) {
            if (dataEdge instanceof GuargedEdge) {
                sourceNode.removeCEdge((GuargedEdge) dataEdge);
            } else if (dataEdge instanceof InterruptEdge) {
                sourceNode.removeIEdge((InterruptEdge) dataEdge);
            } else if (dataEdge instanceof RandomEdge) {
                sourceNode.removePEdge((RandomEdge) dataEdge);
            } else if (dataEdge instanceof ForkingEdge) {
                sourceNode.removeFEdge((ForkingEdge) dataEdge);
            } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
                sourceNode.removeDEdge();
            }
        }
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        mEdgeLayout.releaseEdgeDockPoints(dataEdge, nodeWidth, nodeHeight);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Edge.Delete");
                recordCommand(ref, "SceneFlow.Edge.Delete", params);
                return resp;
            }

    private JSONObject createCommentForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> addComment(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        CommentBadge comment = new CommentBadge();
        comment.setParentNode(activeSuperNode);
        CommentBoundary rect = new CommentBoundary(
            safeRound(params.has("x") ? params.optDouble("x") : null, 0),
            safeRound(params.has("y") ? params.optDouble("y") : null, 0),
            200,
            120
        );
        comment.setGraphics(new CommentGraphics(rect));
        comment.setHTMLText(params.optString("text", ""));
        activeSuperNode.getCommentList().add(comment);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        String commentId = "C" + Math.max(0, activeSuperNode.getCommentList().size() - 1);
        resp.put("commentId", commentId);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Comment.Create");
                recordCommand(ref, "SceneFlow.Comment.Create", params);
                return resp;
            }

    private JSONObject updateCommentForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> updateComment(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String commentId = params.optString("commentId", "");
        if (commentId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing commentId");
        }
        CommentBadge comment = resolveCommentById(activeSuperNode, commentId);
        if (comment == null) {
            return errorResponse("COMMENT_NOT_FOUND", "Comment not found: " + commentId);
        }

        if (params.has("text")) {
            comment.setHTMLText(params.optString("text", ""));
        }
        CommentGraphics cg = comment.getGraphics();
        if (cg == null) {
            cg = new CommentGraphics();
            comment.setGraphics(cg);
        }
        CommentBoundary boundary = cg.getRectangle();
        if (boundary == null) {
            boundary = new CommentBoundary();
            cg.setRectangle(boundary);
        }
        if (params.has("x")) {
            boundary.setXPos(safeRound(params.optDouble("x"), boundary.getXPos()));
        }
        if (params.has("y")) {
            boundary.setYPos(safeRound(params.optDouble("y"), boundary.getYPos()));
        }
        if (params.has("width")) {
            boundary.setWidth(safeRound(params.optDouble("width"), boundary.getWidth()));
        }
        if (params.has("height")) {
            boundary.setHeight(safeRound(params.optDouble("height"), boundary.getHeight()));
        }
        JSONObject rect = params.optJSONObject("rect");
        if (rect != null) {
            boundary.setXPos(safeRound(rect.has("x") ? rect.optDouble("x") : null, boundary.getXPos()));
            boundary.setYPos(safeRound(rect.has("y") ? rect.optDouble("y") : null, boundary.getYPos()));
            boundary.setWidth(safeRound(rect.has("w") ? rect.optDouble("w") : null, boundary.getWidth()));
            boundary.setHeight(safeRound(rect.has("h") ? rect.optDouble("h") : null, boundary.getHeight()));
        }

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Comment.Update");
                recordCommand(ref, "SceneFlow.Comment.Update", params);
                return resp;
            }

    private JSONObject deleteCommentForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return mutateAndSnapshot(pid, () -> deleteComment(params), broadcaster);
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String commentId = params.optString("commentId", "");
        if (commentId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing commentId");
        }
        CommentBadge comment = resolveCommentById(activeSuperNode, commentId);
        if (comment == null) {
            return errorResponse("COMMENT_NOT_FOUND", "Comment not found: " + commentId);
        }
        activeSuperNode.getCommentList().remove(comment);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
                recordHistory(ref, "SceneFlow.Comment.Delete");
                recordCommand(ref, "SceneFlow.Comment.Delete", params);
                return resp;
            }

    // ===== Copy/Paste Selection Methods =====

    private JSONObject copySelectionForProject(JSONObject params) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        JSONArray nodeIdsJson = params.optJSONArray("nodeIds");
        if (nodeIdsJson == null || nodeIdsJson.isEmpty()) {
            return errorResponse("BAD_REQUEST", "Missing nodeIds");
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        Set<String> nodeIdSet = new java.util.HashSet<>();
        for (int i = 0; i < nodeIdsJson.length(); i++) {
            nodeIdSet.add(nodeIdsJson.getString(i));
        }

        // Clear existing clipboard
        ref.clipboard.clear();
        ref.clipboardEdges.clear();

        // Deep copy each selected node
        for (String nodeId : nodeIdSet) {
            BasicNode node = findNodeRecursive(sceneFlow, nodeId);
            if (node != null) {
                ref.clipboard.add(node.getCopy());
            }
        }

        // Collect edges between selected nodes
        for (BasicNode node : ref.clipboard) {
            String sourceId = node.getId();
            // Collect all edge types
            collectEdgesForClipboard(ref, node.getCEdgeList(), sourceId, "CEDGE", nodeIdSet);
            collectEdgesForClipboard(ref, node.getPEdgeList(), sourceId, "PEDGE", nodeIdSet);
            collectEdgesForClipboard(ref, node.getIEdgeList(), sourceId, "IEDGE", nodeIdSet);
            collectEdgesForClipboard(ref, node.getFEdgeList(), sourceId, "FEDGE", nodeIdSet);
            AbstractEdge dEdge = node.getDedge();
            if (dEdge != null && nodeIdSet.contains(dEdge.getTargetUnid())) {
                String edgeType = dEdge instanceof TimeoutEdge ? "TEDGE" : "EEDGE";
                long timeout = dEdge instanceof TimeoutEdge ? ((TimeoutEdge) dEdge).getTimeout() : 0;
                ref.clipboardEdges.add(new ClipboardEdge(sourceId, dEdge.getTargetUnid(),
                    edgeType, null, 0, timeout));
            }
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("copiedCount", ref.clipboard.size());
        return response;
    }

    private void collectEdgesForClipboard(ProjectRef ref, List<? extends AbstractEdge> edges,
            String sourceId, String edgeType, Set<String> nodeIdSet) {
        if (edges == null) return;
        for (AbstractEdge edge : edges) {
            String targetId = edge.getTargetUnid();
            if (nodeIdSet.contains(targetId)) {
                String condition = null;
                int probability = 0;
                if (edge instanceof GuargedEdge) {
                    Expression cond = ((GuargedEdge) edge).getCondition();
                    condition = cond != null ? cond.getFormattedSyntax() : "true";
                } else if (edge instanceof InterruptEdge) {
                    Expression cond = ((InterruptEdge) edge).getCondition();
                    condition = cond != null ? cond.getFormattedSyntax() : "true";
                } else if (edge instanceof RandomEdge) {
                    probability = ((RandomEdge) edge).getProbability();
                }
                ref.clipboardEdges.add(new ClipboardEdge(sourceId, targetId,
                    edgeType, condition, probability, 0));
            }
        }
    }

    private JSONObject pasteSelectionForProject(JSONObject params,
            java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        if (ref.clipboard.isEmpty()) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("nodeIds", new JSONArray());
            return response;
        }

        int dx = params.optInt("dx", 50);
        int dy = params.optInt("dy", 50);
        String superNodeId = params.optString("superNodeId", null);

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        // Get node dimensions and grid settings for position calculations
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        // Grid scale factors (1 = one node width/height per grid cell)
        int gridScaleX = getEditorConfigInt(ref, "grid_x", 1);
        int gridScaleY = getEditorConfigInt(ref, "grid_y", gridScaleX);
        // Actual grid cell size in pixels
        int gridX = Math.max(8, nodeWidth * gridScaleX);
        int gridY = Math.max(8, nodeHeight * gridScaleY);
        // Grid origin offset (matches frontend: nodeWidth/2 + nodeWidth/3 = nodeWidth * 5/6)
        double originX = nodeWidth / 2.0 + nodeWidth / 3.0;
        double originY = nodeHeight / 2.0 + nodeHeight / 3.0;

        // Collect all existing node IDs for uniqueness check
        Set<String> usedIds = new java.util.HashSet<>();
        List<BasicNode> existingNodes = new ArrayList<>();
        collectNodes(sceneFlow, existingNodes);
        for (BasicNode existing : existingNodes) {
            if (existing != null && existing.getId() != null) {
                usedIds.add(existing.getId());
            }
        }

        // Collect existing node positions in the active supernode for collision detection
        // Use a list of int[] pairs for distance-based collision checking
        List<int[]> occupiedPositions = new ArrayList<>();
        for (BasicNode node : activeSuperNode.getNodeAndSuperNodeList()) {
            NodeGraphics g = node.getGraphics();
            if (g != null && g.getPosition() != null) {
                int nx = g.getPosition().getXPos();
                int ny = g.getPosition().getYPos();
                // Skip invalid positions (Integer.MIN_VALUE is default unset value)
                if (nx > Integer.MIN_VALUE + 1000 && ny > Integer.MIN_VALUE + 1000) {
                    occupiedPositions.add(new int[]{nx, ny});
                }
            }
        }

        // Collision threshold - nodes closer than this are considered overlapping
        int collisionThreshold = Math.max(nodeWidth, nodeHeight);

        // Map old node IDs to new node IDs
        Map<String, String> idMapping = new java.util.HashMap<>();
        List<String> newNodeIds = new ArrayList<>();
        List<BasicNode> newNodes = new ArrayList<>();

        // Create new nodes from clipboard
        for (BasicNode clipboardNode : ref.clipboard) {
            String oldId = clipboardNode.getId();
            boolean isSuperNode = clipboardNode instanceof SuperNode;
            String newId = allocateNodeId(ref, isSuperNode, usedIds);
            usedIds.add(newId);
            idMapping.put(oldId, newId);

            // Create new node with offset position
            BasicNode newNode = isSuperNode ? new SuperNode() : new BasicNode();
            newNode.setId(newId);
            newNode.setName(clipboardNode.getName());
            newNode.setComment(clipboardNode.getComment());
            newNode.setHistoryNodeFlag(clipboardNode.isHistoryNode());

            // Copy variable definitions
            for (VariableDefinition varDef : clipboardNode.getVarDefList()) {
                newNode.addVarDef(varDef.getCopy());
            }

            // Copy type definitions
            for (DataTypeDefinition typeDef : clipboardNode.getTypeDefList()) {
                newNode.addTypeDef(typeDef.getCopy());
            }

            // Copy commands
            for (Command cmd : clipboardNode.getCmdList()) {
                newNode.addCmd(cmd.getCopy());
            }

            // Calculate initial position with offset
            NodeGraphics oldGraphics = clipboardNode.getGraphics();
            int x = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getXPos() : 0) + dx;
            int y = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getYPos() : 0) + dy;

            // Apply grid snap - snap node CENTER to grid, then convert back to position
            // 1. Convert position (top-left) to center
            double centerX = x + nodeWidth / 2.0;
            double centerY = y + nodeHeight / 2.0;
            // 2. Snap center to grid relative to origin
            double snappedCenterX = originX + Math.round((centerX - originX) / gridX) * gridX;
            double snappedCenterY = originY + Math.round((centerY - originY) / gridY) * gridY;
            // 3. Convert back to position (top-left)
            x = Math.max(1, (int) Math.round(snappedCenterX - nodeWidth / 2.0));
            y = Math.max(1, (int) Math.round(snappedCenterY - nodeHeight / 2.0));

            // Find a free position if the target position is occupied (using distance-based collision)
            // Use grid cell size as the step to maintain grid alignment
            int attempts = 0;
            while (isPositionOccupied(x, y, occupiedPositions, collisionThreshold) && attempts < 100) {
                // Move to next grid cell to find a free position
                attempts++;
                x += gridX;
                if (attempts % 5 == 0) {
                    x -= 5 * gridX;
                    y += gridY;
                }
            }

            // Mark this position as occupied for subsequent nodes
            occupiedPositions.add(new int[]{x, y});

            newNode.setGraphics(new NodeGraphics(x, y));

            // Add to parent
            newNode.setParentNode(activeSuperNode);
            if (isSuperNode) {
                activeSuperNode.addSuperNode((SuperNode) newNode);
            } else {
                activeSuperNode.addNode(newNode);
            }

            newNodeIds.add(newId);
            newNodes.add(newNode);
        }

        // Recreate edges between pasted nodes
        for (ClipboardEdge ce : ref.clipboardEdges) {
            String newSourceId = idMapping.get(ce.sourceId);
            String newTargetId = idMapping.get(ce.targetId);
            if (newSourceId == null || newTargetId == null) continue;

            BasicNode sourceNode = resolveNodeById(activeSuperNode, newSourceId);
            BasicNode targetNode = resolveNodeById(activeSuperNode, newTargetId);
            if (sourceNode == null || targetNode == null) continue;

            createEdgeFromClipboard(ref, sourceNode, targetNode, ce);
        }

        // Mark dirty and create snapshot
        ref.dirty = true;
        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        resp.put("nodeIds", new JSONArray(newNodeIds));
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        recordHistory(ref, "SceneFlow.Selection.Paste");

        return resp;
    }

    private void createEdgeFromClipboard(ProjectRef ref, BasicNode sourceNode,
            BasicNode targetNode, ClipboardEdge ce) {
        AbstractEdge edge;
        switch (ce.edgeType) {
            case "CEDGE":
                GuargedEdge cedge = new GuargedEdge();
                cedge.setCondition(parseExpressionOrNull(ce.condition != null ? ce.condition : "true"));
                sourceNode.addCEdge(cedge);
                edge = cedge;
                break;
            case "IEDGE":
                InterruptEdge iedge = new InterruptEdge();
                iedge.setCondition(parseExpressionOrNull(ce.condition != null ? ce.condition : "true"));
                sourceNode.addIEdge(iedge);
                edge = iedge;
                break;
            case "PEDGE":
                RandomEdge pedge = new RandomEdge();
                pedge.setProbability(ce.probability);
                sourceNode.addPEdge(pedge);
                edge = pedge;
                break;
            case "FEDGE":
                ForkingEdge fedge = new ForkingEdge();
                sourceNode.addFEdge(fedge);
                edge = fedge;
                break;
            case "TEDGE":
                TimeoutEdge tedge = new TimeoutEdge();
                tedge.setTimeout(ce.timeout);
                sourceNode.setDedge(tedge);
                edge = tedge;
                break;
            case "EEDGE":
            default:
                EpsilonEdge eedge = new EpsilonEdge();
                sourceNode.setDedge(eedge);
                edge = eedge;
                break;
        }

        edge.setSourceNode(sourceNode);
        edge.setTargetNode(targetNode);
        edge.setSourceUnid(sourceNode.getId());
        edge.setTargetUnid(targetNode.getId());
        edge.setGraphics(new EdgeGraphics());
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        mEdgeLayout.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);
    }

    /**
     * Check if a position is too close to any existing occupied position.
     * Uses distance-based collision detection rather than exact matching.
     */
    private boolean isPositionOccupied(int x, int y, List<int[]> occupiedPositions, int threshold) {
        for (int[] pos : occupiedPositions) {
            int dx = Math.abs(x - pos[0]);
            int dy = Math.abs(y - pos[1]);
            // Use Manhattan distance for simpler/faster check
            if (dx < threshold && dy < threshold) {
                return true;
            }
        }
        return false;
    }

    // ===== End Copy/Paste Selection Methods =====

    private JSONObject updateScriptForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        ensureScriptLoaded(ref);
        String text = params.optString("text", "");
        if (params.has("version")) {
            int clientVersion = params.optInt("version", ref.scriptVersion);
            if (clientVersion != ref.scriptVersion) {
                JSONObject mismatch = new JSONObject();
                mismatch.put("applied", false);
                mismatch.put("reason", "VERSION_MISMATCH");
                mismatch.put("version", ref.scriptVersion);
                mismatch.put("text", ref.scriptText == null ? "" : ref.scriptText);
                mismatch.put("parseOk", ref.scriptParseOk);
                mismatch.put("parseErrors", diagnosticsToJson(ref.scriptParseErrors));
                return mismatch;
            }
        }

        String previousText = ref.scriptText == null ? serializeSceneScript(ref.runtimeProject) : ref.scriptText;
        boolean ok = applyScriptText(ref.runtimeProject, text);
        if (!ok) {
            applyScriptText(ref.runtimeProject, previousText);
            ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(text);
            JSONObject failed = new JSONObject();
            failed.put("applied", false);
            failed.put("reason", "PARSE_FAILED");
            failed.put("parseOk", result.isParseOk());
            failed.put("parseErrors", diagnosticsToJson(result.getDiagnostics()));
            return failed;
        }

        ref.scriptText = text;
        ref.scriptVersion = Math.max(1, ref.scriptVersion + 1);
        ref.scriptParseOk = true;
        ref.scriptParseErrors.clear();
        ref.dirty = true;

        JSONObject resp = new JSONObject();
        resp.put("applied", true);
        resp.put("text", ref.scriptText);
        resp.put("version", ref.scriptVersion);
        resp.put("parseOk", ref.scriptParseOk);
        resp.put("parseErrors", diagnosticsToJson(ref.scriptParseErrors));
        if (broadcaster != null) {
            broadcastScriptSnapshot(broadcaster, pid, resp);
            JSONObject dirtyEvt = new JSONObject();
            dirtyEvt.put("event", "project.dirty");
            dirtyEvt.put("projectId", pid);
            dirtyEvt.put("areas", new JSONArray().put("script"));
            broadcaster.accept(dirtyEvt.toString());
        }
        recordHistory(ref, "Script.Update");
        recordCommand(ref, "Script.Update", params);
        return resp;
    }

    private JSONObject findPlaySceneReferences(JSONObject params) {
        String pid = params.optString("projectId", "");
        String sceneName = params.optString("sceneName", "").trim();
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (sceneName.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing sceneName");
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        if (sceneFlow == null) {
            return errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }
        List<JSONObject> matches = new ArrayList<>();
        collectPlaySceneReferences(sceneFlow, sceneName, matches);
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("matches", new JSONArray(matches));
        resp.put("count", matches.size());
        return resp;
    }

    private JSONObject findPlaySceneReferencesMany(JSONObject params) {
        String pid = params.optString("projectId", "");
        JSONArray namesJson = params.optJSONArray("sceneNames");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (namesJson == null || namesJson.isEmpty()) {
            return errorResponse("BAD_REQUEST", "Missing sceneNames");
        }
        Set<String> names = new HashSet<>();
        for (int i = 0; i < namesJson.length(); i++) {
            String name = namesJson.optString(i, "").trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        if (names.isEmpty()) {
            return errorResponse("BAD_REQUEST", "Missing sceneNames");
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        if (sceneFlow == null) {
            return errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }
        List<JSONObject> matches = new ArrayList<>();
        collectPlaySceneReferences(sceneFlow, names, matches);
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("matches", new JSONArray(matches));
        resp.put("count", matches.size());
        return resp;
    }

    private JSONObject renamePlaySceneReferences(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        String sceneName = params.optString("sceneName", "").trim();
        String newName = params.optString("newName", "").trim();
        String superNodeId = params.optString("superNodeId", "").trim();
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (sceneName.isBlank() || newName.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing sceneName or newName");
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        if (sceneFlow == null) {
            return errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }
        int updated = renamePlaySceneReferences(sceneFlow, sceneName, newName);
        ref.dirty = true;
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("updated", updated);

        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        if (snapshotTarget == null) {
            snapshotTarget = sceneFlow;
        }
        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        resp.put("snapshot", snapshot);
        if (broadcaster != null) {
            broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        }
        recordHistory(ref, "SceneFlow.PlayScene.Rename");
        recordCommand(ref, "SceneFlow.PlayScene.Rename", params);
        return resp;
    }

    private JSONObject undoProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        ensureHistoryLoaded(ref);
        if (ref.historyIndex <= 0) {
            JSONObject resp = new JSONObject();
            resp.put("status", "ok");
            resp.put("applied", false);
            return resp;
        }
        ref.historySuspended = true;
        ref.commandLogSuspended = true;
        try {
            ref.historyIndex = Math.max(0, ref.historyIndex - 1);
            HistoryEntry entry = ref.history.get(ref.historyIndex);
            if (!applyHistoryEntry(ref, entry)) {
                return errorResponse("UNDO_FAILED", "Failed to apply undo");
            }
        } finally {
            ref.historySuspended = false;
            ref.commandLogSuspended = false;
        }
        saveHistoryToDisk(ref);

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        SuperNode snapshotTarget = sceneFlow;
        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        JSONObject scriptSnapshot = buildScriptSnapshot(ref);
        broadcastScriptSnapshot(broadcaster, pid, scriptSnapshot);

        JSONObject resp = buildSceneFlowResponse(snapshot);
        resp.put("script", scriptSnapshot);
        resp.put("applied", true);
        if (broadcaster != null) {
            JSONObject dirtyEvt = new JSONObject();
            dirtyEvt.put("event", "project.dirty");
            dirtyEvt.put("projectId", pid);
            dirtyEvt.put("areas", new JSONArray().put("sceneflow").put("script"));
            broadcaster.accept(dirtyEvt.toString());
        }
        return resp;
    }

    private JSONObject redoProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        ensureHistoryLoaded(ref);
        if (ref.historyIndex >= ref.history.size() - 1) {
            JSONObject resp = new JSONObject();
            resp.put("status", "ok");
            resp.put("applied", false);
            return resp;
        }
        ref.historySuspended = true;
        ref.commandLogSuspended = true;
        try {
            ref.historyIndex = Math.min(ref.history.size() - 1, ref.historyIndex + 1);
            HistoryEntry entry = ref.history.get(ref.historyIndex);
            if (!applyHistoryEntry(ref, entry)) {
                return errorResponse("REDO_FAILED", "Failed to apply redo");
            }
        } finally {
            ref.historySuspended = false;
            ref.commandLogSuspended = false;
        }
        saveHistoryToDisk(ref);

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        SuperNode snapshotTarget = sceneFlow;
        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        JSONObject scriptSnapshot = buildScriptSnapshot(ref);
        broadcastScriptSnapshot(broadcaster, pid, scriptSnapshot);

        JSONObject resp = buildSceneFlowResponse(snapshot);
        resp.put("script", scriptSnapshot);
        resp.put("applied", true);
        if (broadcaster != null) {
            JSONObject dirtyEvt = new JSONObject();
            dirtyEvt.put("event", "project.dirty");
            dirtyEvt.put("projectId", pid);
            dirtyEvt.put("areas", new JSONArray().put("sceneflow").put("script"));
            broadcaster.accept(dirtyEvt.toString());
        }
        return resp;
    }

    private CommentBadge resolveCommentById(SuperNode superNode, String commentId) {
        if (superNode == null || commentId == null) {
            return null;
        }
        String normalized = commentId.trim();
        if (normalized.startsWith("C")) {
            normalized = normalized.substring(1);
        }
        int index;
        try {
            index = Integer.parseInt(normalized);
        } catch (NumberFormatException ex) {
            return null;
        }
        if (index < 0 || index >= superNode.getCommentList().size()) {
            return null;
        }
        return superNode.getCommentList().get(index);
    }

    private Expression parseExpressionOrNull(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        try {
            Object parsed = GlueParser.run(text.trim());
            if (parsed instanceof Expression) {
                return (Expression) parsed;
            }
        } catch (Exception exc) {
            return null;
        }
        return null;
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

    private void ensureScriptLoaded(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null) {
            return;
        }
        if (ref.scriptText != null) {
            if (ref.scriptVersion < 1) {
                ref.scriptVersion = 1;
            }
            return;
        }
        SceneScript scriptModel = ref.runtimeProject.getSceneScript();
        boolean alreadyLoaded = scriptModel != null && scriptModel.getSceneListSize() > 0;
        if (!alreadyLoaded) {
            String xml = "";
            if (ref.path != null && !ref.path.isBlank()) {
                xml = loadFile(ref.runtimeProject.getProjectPath(), "scenescript.xml");
            }
            if (xml != null && !xml.isBlank() && scriptModel != null) {
                scriptModel.clear();
                XMLUtilities.parseFromXMLString(scriptModel, xml, "UTF-8");
            }
        }
        String scriptText = ref.runtimeProject.getSceneScript() != null ? ref.runtimeProject.getSceneScript().getText() : "";
        ref.scriptText = scriptText == null ? "" : scriptText;
        // Reparse the generated text so mLower/mUpper character offsets match the text
        // representation (XML-stored offsets may be stale if the script was edited outside the text parser)
        if (ref.runtimeProject.getSceneScript() != null && !ref.scriptText.isBlank()) {
            ref.runtimeProject.getSceneScript().parseTXT(ref.scriptText);
        }
        ref.scriptVersion = 1;
        if (ref.scriptText == null || ref.scriptText.isBlank()) {
            ref.scriptParseOk = true;
            ref.scriptParseErrors.clear();
        } else {
            ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(ref.scriptText);
            ref.scriptParseOk = result.isParseOk();
            ref.scriptParseErrors.clear();
            ref.scriptParseErrors.addAll(result.getDiagnostics());
        }
    }

    private String serializeSceneScript(RunTimeProject project) {
        if (project == null || project.getSceneScript() == null) {
            return "";
        }
        try {
            return project.getSceneScript().getText();
        } catch (Exception exc) {
            return "";
        }
    }

    private boolean applyScriptText(RunTimeProject project, String text) {
        if (project == null || text == null) {
            return false;
        }
        SceneScript script = project.getSceneScript();
        if (script == null) {
            return false;
        }
        if (text.isBlank()) {
            script.clear();
            return true;
        }
        return script.parseTXT(text);
    }

    private void broadcastScriptSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
        if (broadcaster == null) {
            return;
        }
        JSONObject evt = new JSONObject();
        evt.put("event", "script.snapshot");
        evt.put("projectId", projectId);
        evt.put("snapshot", snapshot);
        broadcaster.accept(evt.toString());
    }

    private String serializeSceneFlowXml(SceneFlow sceneFlow) {
        if (sceneFlow == null) {
            return "";
        }
        try {
            java.io.ByteArrayOutputStream stream = new java.io.ByteArrayOutputStream();
            XMLUtilities.writeToXMLStream(sceneFlow, stream);
            return stream.toString(java.nio.charset.StandardCharsets.UTF_8);
        } catch (Exception exc) {
            return "";
        }
    }

    private boolean applySceneFlowXml(SceneFlow sceneFlow, String xml) {
        if (sceneFlow == null) {
            return false;
        }
        sceneFlow.clearContent();
        if (xml != null && !xml.isBlank()) {
            if (!XMLUtilities.parseFromXMLString(sceneFlow, xml, "UTF-8")) {
                return false;
            }
        }
        sceneFlow.establishStartNodes();
        sceneFlow.establishTargetNodes();
        sceneFlow.establishAltStartNodes();
        return true;
    }

    private JSONObject buildScriptSnapshot(ProjectRef ref) {
        ensureScriptLoaded(ref);
        ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(ref.scriptText == null ? "" : ref.scriptText);
        ref.scriptParseOk = result.isParseOk();
        ref.scriptParseErrors.clear();
        ref.scriptParseErrors.addAll(result.getDiagnostics());

        JSONObject snapshot = new JSONObject();
        snapshot.put("text", ref.scriptText == null ? "" : ref.scriptText);
        snapshot.put("version", ref.scriptVersion);
        snapshot.put("parseOk", ref.scriptParseOk);
        snapshot.put("parseErrors", diagnosticsToJson(ref.scriptParseErrors));
        if (ref != null) {
            JSONObject undoState = buildUndoState(ref);
            if (undoState != null) {
                snapshot.put("undoState", undoState);
            }
        }
        return snapshot;
    }

    private JSONObject buildUndoState(ProjectRef ref) {
        if (ref == null) {
            return null;
        }
        ensureHistoryLoaded(ref);
        int size = ref.history != null ? ref.history.size() : 0;
        int index = ref.historyIndex;
        int effectiveSize = size;
        int effectiveIndex = index;
        boolean matchesCurrent = false;
        if (size > 0 && index >= 0 && index < size && ref.runtimeProject != null) {
            String xml = serializeSceneFlowXml(ref.runtimeProject.getSceneFlow());
            ensureScriptLoaded(ref);
            String script = ref.scriptText == null ? "" : ref.scriptText;
            HistoryEntry current = ref.history.get(index);
            if (current != null) {
                matchesCurrent = current.sceneFlowXml.equals(xml) && current.scriptText.equals(script);
            }
        }
        if (size == 0) {
            effectiveIndex = -1;
            effectiveSize = 0;
        } else if (!matchesCurrent) {
            if (index < size - 1) {
                effectiveSize = size;
                effectiveIndex = index;
            } else {
                effectiveSize = size + 1;
                effectiveIndex = size;
            }
        }
        JSONObject undoState = new JSONObject();
        undoState.put("index", effectiveIndex);
        undoState.put("size", effectiveSize);
        undoState.put("canUndo", effectiveIndex > 0);
        undoState.put("canRedo", effectiveSize > 0 && effectiveIndex < effectiveSize - 1);
        return undoState;
    }

    private Path historyDir(ProjectRef ref) {
        if (ref == null || ref.path == null || ref.path.isBlank()) {
            return null;
        }
        return Paths.get(ref.path, ".history");
    }

    private Path historyFile(ProjectRef ref) {
        Path dir = historyDir(ref);
        if (dir == null) return null;
        return dir.resolve("undo.json");
    }

    private Path commandLogFile(ProjectRef ref) {
        Path dir = historyDir(ref);
        if (dir == null) return null;
        return dir.resolve("commands.jsonl");
    }

    private void ensureCommandLogLoaded(ProjectRef ref) {
        if (ref == null || ref.commandLogLoaded) {
            return;
        }
        loadCommandLogFromDisk(ref);
        ref.commandLogLoaded = true;
        if (ref.commandLog.isEmpty() && ref.runtimeProject != null) {
            recordCommandSnapshot(ref, "init");
        }
    }

    private void loadCommandLogFromDisk(ProjectRef ref) {
        Path file = commandLogFile(ref);
        if (file == null || !Files.exists(file)) {
            return;
        }
        try {
            List<String> lines = Files.readAllLines(file);
            for (String line : lines) {
                if (line == null) continue;
                String trimmed = line.trim();
                if (trimmed.isEmpty()) continue;
                JSONObject obj = new JSONObject(trimmed);
                CommandLogEntry entry = CommandLogEntry.fromJson(obj);
                if (entry != null) {
                    ref.commandLog.add(entry);
                    ref.commandSeq = Math.max(ref.commandSeq, entry.seq);
                    ref.commandCount = Math.max(ref.commandCount, entry.cmdIndex);
                }
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Failed to load command log: " + exc.getMessage());
        }
    }

    private void appendCommandLog(ProjectRef ref, CommandLogEntry entry) {
        Path file = commandLogFile(ref);
        if (file == null) return;
        try {
            Files.createDirectories(file.getParent());
            Files.writeString(
                file,
                entry.toJson().toString() + System.lineSeparator(),
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.APPEND
            );
        } catch (Exception exc) {
            sLogger.warning("Warning: Failed to append command log: " + exc.getMessage());
        }
    }

    private void rewriteCommandLog(ProjectRef ref) {
        Path file = commandLogFile(ref);
        if (file == null) return;
        try {
            Files.createDirectories(file.getParent());
            StringBuilder sb = new StringBuilder();
            for (CommandLogEntry entry : ref.commandLog) {
                sb.append(entry.toJson().toString()).append(System.lineSeparator());
            }
            Files.writeString(file, sb.toString());
        } catch (Exception exc) {
            sLogger.warning("Warning: Failed to rewrite command log: " + exc.getMessage());
        }
    }

    private void pruneCommandLog(ProjectRef ref) {
        int max = getEditorConfigInt(ref, "command_log_max", 5000);
        if (max < 1) {
            max = 1;
        }
        if (ref.commandLog.size() <= max) {
            return;
        }
        int removeCount = ref.commandLog.size() - max;
        if (removeCount > 0) {
            ref.commandLog.subList(0, removeCount).clear();
            rewriteCommandLog(ref);
        }
    }

    private void recordCommand(ProjectRef ref, String method, JSONObject params) {
        if (ref == null || ref.runtimeProject == null || ref.commandLogSuspended) {
            return;
        }
        ensureCommandLogLoaded(ref);
        ref.commandSeq += 1;
        ref.commandCount += 1;
        JSONObject payload = null;
        if (params != null) {
            payload = new JSONObject(params.toString());
        }
        CommandLogEntry entry = new CommandLogEntry(
                ref.commandSeq,
                System.currentTimeMillis(),
                "command",
                method,
                ref.commandCount,
                payload,
                "",
                ""
        );
        ref.commandLog.add(entry);
        appendCommandLog(ref, entry);
        if (ref.commandCount % 50 == 0) {
            recordCommandSnapshot(ref, "interval");
        }
        pruneCommandLog(ref);
    }

    private void recordCommandSnapshot(ProjectRef ref, String reason) {
        if (ref == null || ref.runtimeProject == null) return;
        ensureScriptLoaded(ref);
        String xml = serializeSceneFlowXml(ref.runtimeProject.getSceneFlow());
        String script = ref.scriptText == null ? "" : ref.scriptText;
        ref.commandSeq += 1;
        CommandLogEntry entry = new CommandLogEntry(
                ref.commandSeq,
                System.currentTimeMillis(),
                "snapshot",
                reason,
                ref.commandCount,
                null,
                xml,
                script
        );
        ref.commandLog.add(entry);
        appendCommandLog(ref, entry);
        pruneCommandLog(ref);
    }

    private int getUndoDepth(ProjectRef ref) {
        return getEditorConfigInt(ref, "undo_max_depth", 500);
    }

    private void ensureHistoryLoaded(ProjectRef ref) {
        if (ref == null || ref.historyLoaded) {
            return;
        }
        loadHistoryFromDisk(ref);
        ref.historyLoaded = true;
        if (ref.history.isEmpty() && ref.runtimeProject != null) {
            recordHistory(ref, "init");
        }
    }

    private void loadHistoryFromDisk(ProjectRef ref) {
        Path file = historyFile(ref);
        if (file == null || !Files.exists(file)) {
            return;
        }
        try {
            String raw = Files.readString(file);
            if (raw == null || raw.isBlank()) return;
            JSONObject obj = new JSONObject(raw);
            JSONArray arr = obj.optJSONArray("entries");
            int index = obj.optInt("index", -1);
            if (arr != null) {
                ref.history.clear();
                for (int i = 0; i < arr.length(); i++) {
                    JSONObject entry = arr.optJSONObject(i);
                    if (entry == null) continue;
                    HistoryEntry parsed = HistoryEntry.fromJson(entry);
                    if (parsed != null) {
                        ref.history.add(parsed);
                    }
                }
            }
            if (!ref.history.isEmpty()) {
                ref.historyIndex = Math.min(Math.max(index, 0), ref.history.size() - 1);
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Failed to load history: " + exc.getMessage());
        }
    }

    private void saveHistoryToDisk(ProjectRef ref) {
        Path file = historyFile(ref);
        if (file == null) return;
        try {
            Files.createDirectories(file.getParent());
            JSONObject obj = new JSONObject();
            obj.put("index", ref.historyIndex);
            JSONArray arr = new JSONArray();
            for (HistoryEntry entry : ref.history) {
                arr.put(entry.toJson());
            }
            obj.put("entries", arr);
            Files.writeString(file, obj.toString());
        } catch (Exception exc) {
            sLogger.warning("Warning: Failed to save history: " + exc.getMessage());
        }
    }

    private void recordHistory(ProjectRef ref, String reason) {
        if (ref == null || ref.runtimeProject == null || ref.historySuspended) {
            return;
        }
        ensureHistoryLoaded(ref);
        String xml = serializeSceneFlowXml(ref.runtimeProject.getSceneFlow());
        ensureScriptLoaded(ref);
        String script = ref.scriptText == null ? "" : ref.scriptText;
        HistoryEntry entry = new HistoryEntry(System.currentTimeMillis(), xml, script, reason);

        if (ref.historyIndex >= 0 && ref.historyIndex < ref.history.size()) {
            HistoryEntry current = ref.history.get(ref.historyIndex);
            if (current != null && current.sceneFlowXml.equals(entry.sceneFlowXml) && current.scriptText.equals(entry.scriptText)) {
                return;
            }
        }
        ensureCommandLogLoaded(ref);
        if (ref.historyIndex < ref.history.size() - 1) {
            ref.history.subList(ref.historyIndex + 1, ref.history.size()).clear();
        }
        ref.history.add(entry);
        ref.historyIndex = ref.history.size() - 1;

        int maxDepth = Math.max(1, getUndoDepth(ref));
        while (ref.history.size() > maxDepth) {
            ref.history.remove(0);
            ref.historyIndex = Math.max(0, ref.historyIndex - 1);
        }
        saveHistoryToDisk(ref);
    }

    private void collectPlaySceneReferences(SuperNode current, String sceneName, List<JSONObject> matches) {
        if (current == null) return;
        collectPlaySceneCommands(current, current, sceneName, "supernode", matches);
        for (BasicNode node : current.getNodeList()) {
            collectPlaySceneCommands(node, current, sceneName, "node", matches);
            for (AbstractEdge edge : node.getEdgeList()) {
                collectPlaySceneEdgeCommands(edge, node, current, sceneName, matches);
            }
        }
        for (SuperNode child : current.getSuperNodeList()) {
            collectPlaySceneReferences(child, sceneName, matches);
        }
    }

    private void collectPlaySceneReferences(SuperNode current, Set<String> sceneNames, List<JSONObject> matches) {
        if (current == null || sceneNames == null || sceneNames.isEmpty()) return;
        collectPlaySceneCommands(current, current, sceneNames, "supernode", matches);
        for (BasicNode node : current.getNodeList()) {
            collectPlaySceneCommands(node, current, sceneNames, "node", matches);
            for (AbstractEdge edge : node.getEdgeList()) {
                collectPlaySceneEdgeCommands(edge, node, current, sceneNames, matches);
            }
        }
        for (SuperNode child : current.getSuperNodeList()) {
            collectPlaySceneReferences(child, sceneNames, matches);
        }
    }

    private void collectPlaySceneCommands(BasicNode node, SuperNode owner, String sceneName, String scope, List<JSONObject> matches) {
        if (node == null) return;
        ArrayList<Command> commands = node.getCmdList();
        if (commands == null) return;
        for (int i = 0; i < commands.size(); i++) {
            Command command = commands.get(i);
            if (isPlaySceneLiteral(command, sceneName)) {
                JSONObject entry = new JSONObject();
                entry.put("scope", scope);
                entry.put("sceneName", sceneName);
                entry.put("superNodeId", owner.getId());
                entry.put("superNodeName", owner.getName());
                entry.put("nodeId", node.getId());
                entry.put("nodeName", node.getName());
                entry.put("commandIndex", i);
                entry.put("commandText", command.getConcreteSyntax());
                matches.add(entry);
            }
        }
    }

    private void collectPlaySceneCommands(BasicNode node, SuperNode owner, Set<String> sceneNames, String scope, List<JSONObject> matches) {
        if (node == null || sceneNames == null || sceneNames.isEmpty()) return;
        ArrayList<Command> commands = node.getCmdList();
        if (commands == null) return;
        for (int i = 0; i < commands.size(); i++) {
            Command command = commands.get(i);
            String literal = getPlaySceneLiteral(command);
            if (literal != null && sceneNames.contains(literal)) {
                JSONObject entry = new JSONObject();
                entry.put("scope", scope);
                entry.put("sceneName", literal);
                entry.put("superNodeId", owner.getId());
                entry.put("superNodeName", owner.getName());
                entry.put("nodeId", node.getId());
                entry.put("nodeName", node.getName());
                entry.put("commandIndex", i);
                entry.put("commandText", command.getConcreteSyntax());
                matches.add(entry);
            }
        }
    }

    private void collectPlaySceneEdgeCommands(AbstractEdge edge, BasicNode source, SuperNode owner, String sceneName, List<JSONObject> matches) {
        if (edge == null || edge.getCmdList() == null) return;
        List<Command> commands = edge.getCmdList();
        for (int i = 0; i < commands.size(); i++) {
            Command command = commands.get(i);
            if (isPlaySceneLiteral(command, sceneName)) {
                JSONObject entry = new JSONObject();
                entry.put("scope", "edge");
                entry.put("sceneName", sceneName);
                entry.put("superNodeId", owner.getId());
                entry.put("superNodeName", owner.getName());
                entry.put("nodeId", source != null ? source.getId() : "");
                entry.put("nodeName", source != null ? source.getName() : "");
                entry.put("commandIndex", i);
                entry.put("commandText", command.getConcreteSyntax());
                entry.put("edgeType", edge.getClass().getSimpleName());
                matches.add(entry);
            }
        }
    }

    private void collectPlaySceneEdgeCommands(AbstractEdge edge, BasicNode source, SuperNode owner, Set<String> sceneNames, List<JSONObject> matches) {
        if (edge == null || edge.getCmdList() == null || sceneNames == null || sceneNames.isEmpty()) return;
        List<Command> commands = edge.getCmdList();
        for (int i = 0; i < commands.size(); i++) {
            Command command = commands.get(i);
            String literal = getPlaySceneLiteral(command);
            if (literal != null && sceneNames.contains(literal)) {
                JSONObject entry = new JSONObject();
                entry.put("scope", "edge");
                entry.put("sceneName", literal);
                entry.put("superNodeId", owner.getId());
                entry.put("superNodeName", owner.getName());
                entry.put("nodeId", source != null ? source.getId() : "");
                entry.put("nodeName", source != null ? source.getName() : "");
                entry.put("commandIndex", i);
                entry.put("commandText", command.getConcreteSyntax());
                entry.put("edgeType", edge.getClass().getSimpleName());
                matches.add(entry);
            }
        }
    }

    private boolean isPlaySceneLiteral(Command command, String sceneName) {
        if (!(command instanceof PlayScenesActivity)) return false;
        PlayScenesActivity play = (PlayScenesActivity) command;
        Expression arg = play.getArgument();
        if (!(arg instanceof StringLiteral)) return false;
        StringLiteral lit = (StringLiteral) arg;
        return sceneName.equals(lit.getValue());
    }

    private String getPlaySceneLiteral(Command command) {
        if (!(command instanceof PlayScenesActivity)) return null;
        PlayScenesActivity play = (PlayScenesActivity) command;
        Expression arg = play.getArgument();
        if (!(arg instanceof StringLiteral)) return null;
        StringLiteral lit = (StringLiteral) arg;
        return lit.getValue();
    }

    private int renamePlaySceneReferences(SuperNode current, String sceneName, String newName) {
        if (current == null) return 0;
        int updated = 0;
        updated += renamePlaySceneCommands(current, sceneName, newName);
        for (BasicNode node : current.getNodeList()) {
            updated += renamePlaySceneCommands(node, sceneName, newName);
            for (AbstractEdge edge : node.getEdgeList()) {
                updated += renamePlaySceneEdgeCommands(edge, sceneName, newName);
            }
        }
        for (SuperNode child : current.getSuperNodeList()) {
            updated += renamePlaySceneReferences(child, sceneName, newName);
        }
        return updated;
    }

    private int renamePlaySceneCommands(BasicNode node, String sceneName, String newName) {
        if (node == null || node.getCmdList() == null) return 0;
        int updated = 0;
        ArrayList<Command> commands = node.getCmdList();
        for (Command command : commands) {
            if (command instanceof PlayScenesActivity) {
                PlayScenesActivity play = (PlayScenesActivity) command;
                Expression arg = play.getArgument();
                if (arg instanceof StringLiteral) {
                    StringLiteral lit = (StringLiteral) arg;
                    if (sceneName.equals(lit.getValue())) {
                        lit.setValue(newName);
                        updated += 1;
                    }
                }
            }
        }
        return updated;
    }

    private int renamePlaySceneEdgeCommands(AbstractEdge edge, String sceneName, String newName) {
        if (edge == null || edge.getCmdList() == null) return 0;
        int updated = 0;
        for (Command command : edge.getCmdList()) {
            if (command instanceof PlayScenesActivity) {
                PlayScenesActivity play = (PlayScenesActivity) command;
                Expression arg = play.getArgument();
                if (arg instanceof StringLiteral) {
                    StringLiteral lit = (StringLiteral) arg;
                    if (sceneName.equals(lit.getValue())) {
                        lit.setValue(newName);
                        updated += 1;
                    }
                }
            }
        }
        return updated;
    }

    private final Object embeddingsLock = new Object();
    private Process embeddingsProcess = null;

    private void pipeEmbeddingsOutput(Process process) {
        if (process == null) return;
        Thread t = new Thread(() -> {
            try (java.io.BufferedReader reader = new java.io.BufferedReader(
                    new java.io.InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    sLogger.message("[EMBEDDINGS] " + line);
                }
            } catch (Exception exc) {
                sLogger.warning("[EMBEDDINGS] log pipe failed: " + exc.getMessage());
            }
        }, "embeddings-log-pipe");
        t.setDaemon(true);
        t.start();
    }

    private JSONObject startEmbeddingsService(JSONObject params) {
        synchronized (embeddingsLock) {
            if (embeddingsProcess != null && embeddingsProcess.isAlive()) {
                return new JSONObject().put("status", "ok").put("started", false);
            }
            String jarPath = System.getenv("EMBEDDINGS_JAR");
            java.nio.file.Path jar = null;
            java.util.List<String> searched = new java.util.ArrayList<>();
            if (jarPath != null && !jarPath.isBlank()) {
                jar = java.nio.file.Paths.get(jarPath).toAbsolutePath().normalize();
                searched.add(jar.toString());
                if (!java.nio.file.Files.exists(jar)) {
                    jar = null;
                }
            }
            if (jar == null) {
                java.nio.file.Path start = java.nio.file.Paths.get(System.getProperty("user.dir", "."))
                        .toAbsolutePath()
                        .normalize();
                java.nio.file.Path cursor = start;
                for (int i = 0; i < 6 && cursor != null; i++) {
                    java.nio.file.Path libsDir = cursor.resolve("services/embeddings/build/libs").normalize();
                    searched.add(libsDir.toString());
                    if (java.nio.file.Files.exists(libsDir)) {
                        try (java.util.stream.Stream<java.nio.file.Path> stream = java.nio.file.Files.list(libsDir)) {
                            jar = stream
                                    .filter(path -> path.getFileName().toString().endsWith("-all.jar"))
                                    .findFirst()
                                    .orElse(null);
                        } catch (Exception ignored) {
                            jar = null;
                        }
                        if (jar != null) {
                            break;
                        }
                    }
                    java.nio.file.Path parent = cursor.getParent();
                    if (parent == null || parent.equals(cursor)) {
                        cursor = null;
                    } else {
                        cursor = parent;
                    }
                }
            }
            if (jar == null || !java.nio.file.Files.exists(jar)) {
                return errorResponse("NOT_FOUND",
                        "Embeddings jar not found. Build with :services:embeddings:shadowJar or set EMBEDDINGS_JAR. Searched: " + searched);
            }
            String port = System.getenv("EMBEDDINGS_PORT");
            if (port == null || port.isBlank()) {
                port = "4050";
            }
            try {
                ProcessBuilder pb = new ProcessBuilder("java", "-jar", jar.toString());
                pb.environment().put("EMBEDDINGS_PORT", port);
                pb.redirectErrorStream(true);
                embeddingsProcess = pb.start();
                pipeEmbeddingsOutput(embeddingsProcess);
                JSONObject resp = new JSONObject();
                resp.put("status", "ok");
                resp.put("started", true);
                resp.put("pid", embeddingsProcess.pid());
                return resp;
            } catch (Exception exc) {
                return errorResponse("START_FAILED", "Failed to start embeddings service: " + exc.getMessage());
            }
        }
    }

    private boolean applyHistoryEntry(ProjectRef ref, HistoryEntry entry) {
        if (ref == null || ref.runtimeProject == null || entry == null) {
            return false;
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        if (!applySceneFlowXml(sceneFlow, entry.sceneFlowXml)) {
            return false;
        }
        normalizeSceneFlowIds(ref);
        ref.nodes = serializeNodes(ref.runtimeProject);
        ref.edges = serializeEdges(ref.runtimeProject);
        ref.comments = serializeComments(ref.runtimeProject);
        ref.nextNodeIndex = computeNextNodeIndex(ref.runtimeProject);
        ref.nextSuperNodeIndex = computeNextSuperNodeIndex(ref.runtimeProject);
        mEdgeLayout.clearDockPointsRecursive(sceneFlow);
        initializeDockPointsForProject(ref);

        String script = entry.scriptText == null ? "" : entry.scriptText;
        applyScriptText(ref.runtimeProject, script);
        ref.scriptText = script;
        ref.scriptVersion = Math.max(1, ref.scriptVersion + 1);
        ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(ref.scriptText);
        ref.scriptParseOk = result.isParseOk();
        ref.scriptParseErrors.clear();
        ref.scriptParseErrors.addAll(result.getDiagnostics());
        ref.dirty = true;
        return true;
    }

    private JSONArray diagnosticsToJson(List<ScriptDiagnostics.Diagnostic> diagnostics) {
        JSONArray arr = new JSONArray();
        if (diagnostics == null) {
            return arr;
        }
        for (ScriptDiagnostics.Diagnostic diag : diagnostics) {
            if (diag == null) continue;
            JSONObject obj = new JSONObject();
            obj.put("from", diag.getFrom());
            obj.put("to", diag.getTo());
            obj.put("line", diag.getLine());
            obj.put("column", diag.getColumn());
            obj.put("severity", diag.getSeverity());
            obj.put("message", diag.getMessage());
            obj.put("source", diag.getSource());
            arr.put(obj);
        }
        return arr;
    }

    private boolean normalizeSceneFlowIds(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null || ref.runtimeProject.getSceneFlow() == null) {
            return false;
        }
        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        List<BasicNode> nodes = new ArrayList<>();
        collectNodes(sceneFlow, nodes);
        Set<String> used = new java.util.HashSet<>();
        boolean changed = false;
        for (BasicNode node : nodes) {
            String id = node.getId();
            boolean isRoot = node instanceof SuperNode && ((SuperNode) node).getParentNode() == null;
            if (isRoot && (id == null || id.isBlank())) {
                used.add("");
                continue;
            }
            boolean isSuper = node instanceof SuperNode && !isRoot;
            String normalized = normalizeNodeId(id, isSuper, ref, used);
            if (!normalized.equals(id)) {
                node.setId(normalized);
                id = normalized;
                changed = true;
            }
            used.add(id == null ? "" : id);
        }
        rebuildStartNodeMaps(sceneFlow);
        List<AbstractEdge> edges = new ArrayList<>();
        collectEdges(sceneFlow, edges);
        for (AbstractEdge edge : edges) {
            if (edge.getSourceNode() != null) {
                edge.setSourceUnid(edge.getSourceNode().getId());
            }
            if (edge.getTargetNode() != null) {
                edge.setTargetUnid(edge.getTargetNode().getId());
            }
        }
        return changed;
    }

    private void collectNodes(SuperNode node, List<BasicNode> out) {
        if (node == null) return;
        out.add(node);
        for (BasicNode child : node.getNodeAndSuperNodeList()) {
            if (child instanceof SuperNode) {
                collectNodes((SuperNode) child, out);
            } else {
                out.add(child);
            }
        }
    }


    private void rebuildStartNodeMaps(SuperNode node) {
        if (node == null) return;
        java.util.HashMap<String, BasicNode> next = new java.util.HashMap<>();
        for (BasicNode start : node.getStartNodeMap().values()) {
            if (start != null && start.getId() != null && !start.getId().isBlank()) {
                next.put(start.getId(), start);
            }
        }
        node.setStartNodeMap(next);
        for (SuperNode child : node.getSuperNodeList()) {
            rebuildStartNodeMaps(child);
        }
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
        ref.runtimeState = project.isRunning() ? "running" : "stopped";
        ref.nextNodeIndex = computeNextNodeIndex(project);
        ref.nextSuperNodeIndex = computeNextSuperNodeIndex(project);
        normalizeSceneFlowIds(ref);
        ref.nodes = serializeNodes(project);
        ref.edges = serializeEdges(project);
        ref.comments = serializeComments(project);
        projectStore.put(id, ref);
        ensureScriptLoaded(ref);
        // Phase 8: Initialize dock points for the registered project
        initializeDockPointsForProject(ref);
        ensureHistoryLoaded(ref);
        ensureCommandLogLoaded(ref);
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
                String id = UUID.randomUUID().toString();
                ProjectRef ref = new ProjectRef(id, name, path);
                ref.runtimeProject = rtp;
                ref.runtimeState = "stopped";
                ref.nextNodeIndex = computeNextNodeIndex(rtp);
                ref.nextSuperNodeIndex = computeNextSuperNodeIndex(rtp);
                boolean idChanged = normalizeSceneFlowIds(ref);
                ref.nodes = serializeNodes(rtp);
                ref.edges = serializeEdges(rtp);
                ref.comments = serializeComments(rtp);
                projectStore.put(id, ref);
                ensureScriptLoaded(ref);
                // Phase 8: Initialize dock points for the loaded project
                initializeDockPointsForProject(ref);
                ensureHistoryLoaded(ref);
                ensureCommandLogLoaded(ref);
                if (idChanged) {
                    rtp.write(new java.io.File(path));
                }
                return id;
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to load project from " + path + ": " + exc.getMessage());
            }
        }

        String id = UUID.randomUUID().toString();
        ProjectRef ref = new ProjectRef(id, name, path);
        rtp = new RunTimeProject();
        if (path != null && !path.isBlank()) {
            rtp.setProjectPath(path);
        }
        if (name != null && !name.isBlank()) {
            rtp.setProjectName(name);
        }
        ref.runtimeProject = rtp;
        ref.runtimeState = "stopped";
        ref.nextNodeIndex = computeNextNodeIndex(rtp);
        ref.nextSuperNodeIndex = computeNextSuperNodeIndex(rtp);
        normalizeSceneFlowIds(ref);
        ref.nodes = serializeNodes(rtp);
        ref.edges = serializeEdges(rtp);
        ref.comments = serializeComments(rtp);
        projectStore.put(id, ref);
        ensureScriptLoaded(ref);
        initializeDockPointsForProject(ref);
        ensureHistoryLoaded(ref);
        ensureCommandLogLoaded(ref);
        return id;
    }

    private void markClean(String pid) {
        if (pid == null) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref != null) {
            ref.dirty = false;
        }
    }

    // --- VarDef helper methods ---

    private JSONObject errorResponse(String code, String message) {
        JSONObject err = new JSONObject();
        err.put("error", code);
        err.put("message", message);
        return err;
    }

    private BasicNode findNodeRecursive(SuperNode parent, String nodeId) {
        if (parent == null || nodeId == null || nodeId.isBlank()) {
            return null;
        }
        if (nodeId.equals(parent.getId())) {
            return parent;
        }
        for (BasicNode node : parent.getNodeAndSuperNodeList()) {
            if (nodeId.equals(node.getId())) {
                return node;
            }
            if (node instanceof SuperNode) {
                BasicNode found = findNodeRecursive((SuperNode) node, nodeId);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private VariableDefinition parseVarDef(JSONObject source, BasicNode node, StringBuilder error) {
        if (source == null) {
            if (error != null) {
                error.append("Missing variable definition.");
            }
            return null;
        }
        String name = source.optString("name", "").trim();
        if (name.isBlank()) {
            if (error != null) {
                error.append("Variable name is required.");
            }
            return null;
        }
        String type = source.optString("type", "").trim();
        if (type.isBlank()) {
            if (error != null) {
                error.append("Variable type is required.");
            }
            return null;
        }
        // Event variables don't need an expression — they start with an empty queue
        if (type.toLowerCase().startsWith("event")) {
            return new VariableDefinition(name, type, null);
        }
        String expressionText = source.has("expression") ? source.optString("expression", "") : "";
        Expression exp = null;
        if (expressionText == null || expressionText.trim().isEmpty()) {
            exp = defaultExpressionForType(type, node);
            if (exp == null) {
                if (error != null) {
                    error.append("Expression is required for type: " + type);
                }
                return null;
            }
        } else {
            Command parsed;
            try {
                parsed = GlueParser.run(expressionText.trim());
            } catch (Exception ex) {
                if (error != null) {
                    String msg = ex.getMessage();
                    error.append(msg != null && !msg.isBlank() ? msg : "Expression parse failed.");
                }
                return null;
            }
            if (!(parsed instanceof Expression)) {
                if (error != null) {
                    error.append("Expression parse failed.");
                }
                return null;
            }
            exp = (Expression) parsed;
        }
        return new VariableDefinition(name, type, exp);
    }

    private Expression defaultExpressionForType(String type, BasicNode node) {
        if (type == null) {
            return null;
        }
        String trimmed = type.trim();
        if (trimmed.equalsIgnoreCase("Int")) {
            return new IntLiteral(0);
        }
        if (trimmed.equalsIgnoreCase("Bool")) {
            return new BoolLiteral(false);
        }
        if (trimmed.equalsIgnoreCase("Float")) {
            return new FloatLiteral(0);
        }
        if (trimmed.equalsIgnoreCase("String")) {
            return new StringLiteral("");
        }
        DataTypeDefinition def = findTypeDefInHierarchy(node, trimmed);
        if (def instanceof ListTypeDefinition) {
            return new ArrayExpression();
        }
        if (def instanceof StructTypeDefinition) {
            return new StructExpression();
        }
        return null;
    }

    private DataTypeDefinition findTypeDefInHierarchy(BasicNode node, String name) {
        if (node == null || name == null) {
            return null;
        }
        BasicNode current = node;
        while (current != null) {
            for (DataTypeDefinition def : current.getTypeDefList()) {
                if (def != null && name.equals(def.getName())) {
                    return def;
                }
            }
            current = current.getParentNode();
        }
        return null;
    }

    private DataTypeDefinition parseTypeDef(JSONObject source, StringBuilder error) {
        if (source == null) {
            if (error != null) {
                error.append("Missing type definition.");
            }
            return null;
        }
        String name = source.optString("name", "").trim();
        if (name.isBlank()) {
            if (error != null) {
                error.append("Type name is required.");
            }
            return null;
        }
        String flavourRaw = source.optString("flavour", "").trim();
        DataTypeDefinition.Flavour flavour;
        try {
            flavour = DataTypeDefinition.Flavour.valueOf(flavourRaw);
        } catch (Exception ex) {
            if (error != null) {
                error.append("Type flavour is required (List or Struct).");
            }
            return null;
        }
        if (flavour == DataTypeDefinition.Flavour.List) {
            String elementType = source.optString("elementType", "").trim();
            if (elementType.isBlank()) {
                elementType = "Int";
            }
            return new ListTypeDefinition(name, elementType);
        }
        // Struct type
        ArrayList<MemberDefinition> members = new ArrayList<>();
        JSONArray list = source.optJSONArray("members");
        if (list != null) {
            for (int i = 0; i < list.length(); i++) {
                JSONObject entry = list.optJSONObject(i);
                if (entry == null) {
                    if (error != null) {
                        error.append("Invalid struct member.");
                    }
                    return null;
                }
                String memberName = entry.optString("name", "").trim();
                String memberType = entry.optString("type", "").trim();
                if (memberName.isBlank() || memberType.isBlank()) {
                    if (error != null) {
                        error.append("Member name and type are required.");
                    }
                    return null;
                }
                members.add(new MemberDefinition(memberName, memberType));
            }
        }
        return new StructTypeDefinition(name, members);
    }

    private Command parseCommandText(String input, StringBuilder error) {
        String text = input == null ? "" : input.trim();
        if (text.isEmpty()) {
            if (error != null) {
                error.append("Command text is required.");
            }
            return null;
        }
        Command parsed;
        try {
            parsed = GlueParser.run(text);
        } catch (Exception ex) {
            if (error != null) {
                String msg = ex.getMessage();
                error.append(msg != null && !msg.isBlank() ? msg : "Command parse failed.");
            }
            return null;
        }
        if (parsed == null) {
            if (error != null) {
                error.append("Command parse failed.");
            }
            return null;
        }
        return parsed;
    }

    // --- Edge operation helper methods ---

    private AbstractEdge resolveEdgeById(SuperNode superNode, String edgeId) {
        if (superNode == null || edgeId == null) {
            return null;
        }
        String normalized = edgeId.trim();
        if (normalized.startsWith("E")) {
            normalized = normalized.substring(1);
        }
        int index;
        try {
            index = Integer.parseInt(normalized);
        } catch (NumberFormatException ex) {
            return null;
        }
        if (index < 0) {
            return null;
        }
        int edgeIndex = 0;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edgeIndex == index) {
                    return edge;
                }
                edgeIndex++;
            }
        }
        return null;
    }

    private void updateEdgeEndpointsForMovedNode(BasicNode movedNode, SuperNode parent, int oldX, int oldY) {
        if (movedNode == null || parent == null) return;

        NodeGraphics nodeGraphics = movedNode.getGraphics();
        NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
        int newX = nodePos != null ? nodePos.getXPos() : 0;
        int newY = nodePos != null ? nodePos.getYPos() : 0;

        int deltaX = newX - oldX;
        int deltaY = newY - oldY;
        if (deltaX == 0 && deltaY == 0) return;

        String nodeId = movedNode.getId();

        // Update outgoing edges (edges FROM this node) - shift start points
        for (AbstractEdge edge : movedNode.getEdgeList()) {
            EdgeGraphics edgeGraphics = edge.getGraphics();
            if (edgeGraphics == null) continue;
            EdgeArrow arrow = edgeGraphics.getConnection();
            if (arrow == null) continue;
            List<EdgePoint> points = arrow.getPointList();
            if (points == null || points.isEmpty()) continue;

            EdgePoint startPt = points.get(0);
            startPt.setXPos(startPt.getXPos() + deltaX);
            startPt.setYPos(startPt.getYPos() + deltaY);
            startPt.setCtrlXPos(startPt.getCtrlXPos() + deltaX);
            startPt.setCtrlYPos(startPt.getCtrlYPos() + deltaY);
        }

        // Update incoming edges (edges TO this node) - shift end points
        for (BasicNode otherNode : parent.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : otherNode.getEdgeList()) {
                String targetId = edge.getTargetUnid();
                if (!nodeId.equals(targetId)) continue;

                EdgeGraphics edgeGraphics = edge.getGraphics();
                if (edgeGraphics == null) continue;
                EdgeArrow arrow = edgeGraphics.getConnection();
                if (arrow == null) continue;
                List<EdgePoint> points = arrow.getPointList();
                if (points == null || points.size() < 2) continue;

                EdgePoint endPt = points.get(points.size() - 1);
                endPt.setXPos(endPt.getXPos() + deltaX);
                endPt.setYPos(endPt.getYPos() + deltaY);
                endPt.setCtrlXPos(endPt.getCtrlXPos() + deltaX);
                endPt.setCtrlYPos(endPt.getCtrlYPos() + deltaY);
            }
        }
    }


    // Convenience: initialize dock points using ProjectRef config
    private void initializeDockPointsForProject(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null) return;
        int nodeWidth = getEditorConfigInt(ref, "node_width", 90);
        int nodeHeight = getEditorConfigInt(ref, "node_height", nodeWidth);
        mEdgeLayout.initializeDockPointsForProject(ref.runtimeProject, nodeWidth, nodeHeight);
    }

    // --- PEdge helper methods ---

    private BasicNode resolveNodeById(SuperNode superNode, String nodeId) {
        if (superNode == null || nodeId == null) {
            return null;
        }
        if (nodeId.equals(superNode.getId())) {
            return superNode;
        }
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            if (nodeId.equals(node.getId())) {
                return node;
            }
        }
        return null;
    }

    private RandomEdge resolvePEdgeForSource(SuperNode superNode, BasicNode sourceNode, String edgeId, String targetId) {
        if (sourceNode == null) {
            return null;
        }
        // Try to resolve by edge ID first
        if (edgeId != null && !edgeId.isBlank()) {
            AbstractEdge resolved = resolveEdgeById(superNode, edgeId);
            if (resolved instanceof RandomEdge) {
                RandomEdge pedge = (RandomEdge) resolved;
                BasicNode edgeSource = pedge.getSourceNode();
                if (edgeSource != null && edgeSource.equals(sourceNode)) {
                    return pedge;
                }
            }
        }
        // Fall back to target ID matching
        if (targetId != null && !targetId.isBlank()) {
            for (RandomEdge edge : sourceNode.getPEdgeList()) {
                if (edge == null) {
                    continue;
                }
                String target = edge.getTargetUnid();
                if ((target == null || target.isBlank()) && edge.getTargetNode() != null) {
                    target = edge.getTargetNode().getId();
                }
                if (targetId.equals(target)) {
                    return edge;
                }
            }
        }
        return null;
    }

    // --- Runtime variable helper methods ---

    private VariableDefinition findVariableDefinitionInHierarchy(SuperNode node, String name) {
        if (node == null || name == null || name.isBlank()) {
            return null;
        }
        // Search in this node's variables
        for (VariableDefinition def : node.getVarDefList()) {
            if (name.equals(def.getName())) {
                return def;
            }
        }
        // Search recursively in child supernodes
        for (BasicNode child : node.getNodeAndSuperNodeList()) {
            if (child instanceof SuperNode) {
                VariableDefinition found = findVariableDefinitionInHierarchy((SuperNode) child, name);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private boolean isSupportedRuntimeExpression(Expression exp) {
        if (exp instanceof BoolLiteral
                || exp instanceof IntLiteral
                || exp instanceof FloatLiteral
                || exp instanceof StringLiteral) {
            return true;
        }
        if (exp instanceof UnaryExpression) {
            Expression inner = ((UnaryExpression) exp).getExp();
            return inner instanceof IntLiteral || inner instanceof FloatLiteral;
        }
        return false;
    }

    private boolean applyRuntimeExpression(RunTimeProject project, String name, Expression exp) {
        if (exp == null || project == null || name == null || name.isBlank()) {
            return false;
        }
        if (exp instanceof BoolLiteral) {
            return project.setVariable(name, ((BoolLiteral) exp).getValue());
        }
        if (exp instanceof IntLiteral) {
            return project.setVariable(name, ((IntLiteral) exp).getValue());
        }
        if (exp instanceof FloatLiteral) {
            return project.setVariable(name, ((FloatLiteral) exp).getValue());
        }
        if (exp instanceof StringLiteral) {
            return project.setVariable(name, ((StringLiteral) exp).getValue());
        }
        if (exp instanceof UnaryExpression) {
            Expression inner = ((UnaryExpression) exp).getExp();
            if (inner instanceof IntLiteral) {
                return project.setVariable(name, -1 * ((IntLiteral) inner).getValue());
            }
            if (inner instanceof FloatLiteral) {
                return project.setVariable(name, -1.0f * ((FloatLiteral) inner).getValue());
            }
        }
        return false;
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
        int nextNodeIndex = 1;
        int nextSuperNodeIndex = 1;
        String scriptText;
        int scriptVersion = 1;
        boolean scriptParseOk = true;
        List<ScriptDiagnostics.Diagnostic> scriptParseErrors = new ArrayList<>();
        Properties editorConfig;
        boolean editorConfigLoaded = false;
        boolean editorConfigDirty = false;
        List<HistoryEntry> history = new ArrayList<>();
        int historyIndex = -1;
        boolean historyLoaded = false;
        boolean historySuspended = false;
        List<CommandLogEntry> commandLog = new ArrayList<>();
        long commandSeq = 0;
        int commandCount = 0;
        boolean commandLogLoaded = false;
        boolean commandLogSuspended = false;
        // Clipboard for copy/paste operations
        List<BasicNode> clipboard = new ArrayList<>();
        List<ClipboardEdge> clipboardEdges = new ArrayList<>();

        ProjectRef(String id, String name, String path) {
            this.id = id;
            this.name = name == null ? "" : name;
            this.path = path == null ? "" : path;
            this.dirty = false;
            this.scriptText = null;
            this.scriptVersion = 1;
            this.scriptParseOk = true;
        }
    }

    // Helper class for storing edge data in clipboard
    private static class ClipboardEdge {
        final String sourceId;
        final String targetId;
        final String edgeType;  // "EEDGE", "CEDGE", "PEDGE", "IEDGE", "FEDGE", "TEDGE"
        final String condition; // For CEDGE, IEDGE
        final int probability;  // For PEDGE
        final long timeout;     // For TEDGE

        ClipboardEdge(String sourceId, String targetId, String edgeType,
                      String condition, int probability, long timeout) {
            this.sourceId = sourceId;
            this.targetId = targetId;
            this.edgeType = edgeType;
            this.condition = condition;
            this.probability = probability;
            this.timeout = timeout;
        }
    }

    private static class HistoryEntry {
        final long timestamp;
        final String sceneFlowXml;
        final String scriptText;
        final String reason;

        HistoryEntry(long timestamp, String sceneFlowXml, String scriptText, String reason) {
            this.timestamp = timestamp;
            this.sceneFlowXml = sceneFlowXml == null ? "" : sceneFlowXml;
            this.scriptText = scriptText == null ? "" : scriptText;
            this.reason = reason == null ? "" : reason;
        }

        JSONObject toJson() {
            JSONObject obj = new JSONObject();
            obj.put("ts", timestamp);
            obj.put("sceneFlowXml", sceneFlowXml);
            obj.put("scriptText", scriptText);
            obj.put("reason", reason);
            return obj;
        }

        static HistoryEntry fromJson(JSONObject obj) {
            if (obj == null) return null;
            long ts = obj.optLong("ts", System.currentTimeMillis());
            String xml = obj.optString("sceneFlowXml", "");
            String script = obj.optString("scriptText", "");
            String reason = obj.optString("reason", "");
            return new HistoryEntry(ts, xml, script, reason);
        }
    }

    private static class CommandLogEntry {
        final long seq;
        final long timestamp;
        final String kind;
        final String method;
        final int cmdIndex;
        final JSONObject payload;
        final String sceneFlowXml;
        final String scriptText;

        CommandLogEntry(long seq, long timestamp, String kind, String method, int cmdIndex,
                        JSONObject payload, String sceneFlowXml, String scriptText) {
            this.seq = seq;
            this.timestamp = timestamp;
            this.kind = kind == null ? "" : kind;
            this.method = method == null ? "" : method;
            this.cmdIndex = cmdIndex;
            this.payload = payload;
            this.sceneFlowXml = sceneFlowXml == null ? "" : sceneFlowXml;
            this.scriptText = scriptText == null ? "" : scriptText;
        }

        JSONObject toJson() {
            JSONObject obj = new JSONObject();
            obj.put("seq", seq);
            obj.put("ts", timestamp);
            obj.put("kind", kind);
            obj.put("method", method);
            obj.put("cmdIndex", cmdIndex);
            if (payload != null) {
                obj.put("payload", payload);
            }
            if (!sceneFlowXml.isBlank()) {
                obj.put("sceneFlowXml", sceneFlowXml);
            }
            if (!scriptText.isBlank()) {
                obj.put("scriptText", scriptText);
            }
            return obj;
        }

        static CommandLogEntry fromJson(JSONObject obj) {
            if (obj == null) return null;
            long seq = obj.optLong("seq", 0);
            long ts = obj.optLong("ts", System.currentTimeMillis());
            String kind = obj.optString("kind", "");
            String method = obj.optString("method", "");
            int cmdIndex = obj.optInt("cmdIndex", 0);
            JSONObject payload = obj.optJSONObject("payload");
            String xml = obj.optString("sceneFlowXml", "");
            String script = obj.optString("scriptText", "");
            return new CommandLogEntry(seq, ts, kind, method, cmdIndex, payload, xml, script);
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


    private String generateToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }
}
