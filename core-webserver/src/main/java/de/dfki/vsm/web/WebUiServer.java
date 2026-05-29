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
import de.dfki.vsm.model.sceneflow.chart.AliasNode;
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
import de.dfki.vsm.runtime.logic.LogicEngines;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.EventValue;
import de.dfki.vsm.runtime.gateway.RuntimeGateway;
import de.dfki.vsm.runtime.gateway.RuntimeGateways;
import de.dfki.vsm.runtime.api.RuntimeCommandEndpoint;
import de.dfki.vsm.runtime.api.RuntimeWsProtocol;
import java.util.List;
import java.util.ArrayList;
import de.dfki.vsm.util.llm.LLMSupport;
import de.dfki.vsm.util.llm.HttpTransport;
import de.dfki.vsm.util.llm.JdkHttpTransport;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import io.javalin.Javalin;
import io.javalin.http.Header;
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
import java.lang.reflect.Field;
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

public final class WebUiServer implements EventListener, RuntimeCommandEndpoint {

    private static final Map<String, ExportablePropertyEntry> EXPORTABLE_PROPERTY_PROVIDERS = new HashMap<>();
    private static final Set<String> RESERVED_META_VARIABLES = Set.of("__vsm_mode");

    private static final class ExportablePropertyEntry {
        private final String providerClass;
        private final JSONObject pluginSpec;      // config.required, config.optional, config.pluginSpecific, templates
        private final JSONObject agentSpec;       // agent-level config
        private final JSONObject pluginMeta;      // plugin.id, plugin.name, plugin.description, plugin.tags
        private final JSONObject categories;      // categories.primary, categories.secondary
        private final JSONArray commands;         // commands array
        private final JSONObject variables;       // variables.writes, variables.reads
        final String specVersion;                 // from specVersion field in plugin-properties.json

        private ExportablePropertyEntry(String providerClass, JSONObject pluginSpec, JSONObject agentSpec,
                                         JSONObject pluginMeta, JSONObject categories,
                                         JSONArray commands, JSONObject variables, String specVersion) {
            this.providerClass = providerClass;
            this.pluginSpec = pluginSpec;
            this.agentSpec = agentSpec;
            this.pluginMeta = pluginMeta;
            this.categories = categories;
            this.commands = commands;
            this.variables = variables;
            this.specVersion = specVersion != null ? specVersion : "";
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
                plugin.put("androidCompatible", pluginMeta.optBoolean("androidCompatible", false));
            } else {
                // Derive from className
                String simpleName = className.contains(".") ? className.substring(className.lastIndexOf('.') + 1) : className;
                plugin.put("id", simpleName.toLowerCase());
                plugin.put("name", simpleName);
                plugin.put("className", className);
                plugin.put("description", "");
                plugin.put("tags", new JSONArray());
                plugin.put("androidCompatible", false);
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

            // Spec version (for plugin update detection)
            if (!specVersion.isEmpty()) {
                out.put("specVersion", specVersion);
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
    private static final int RECENT_MAX = 19;
    private static final int SEMANTIC_DOC_VERSION = 2;
    private static final String SEMANTIC_SCHEMA_ID = "vsm.semantic.annotations";
    private static final String SEMANTIC_BASIC_PROVIDER = "ud";
    private static final String SEMANTIC_UD_URL_DEFAULT = "http://127.0.0.1:4061/analyze";
    private static final int SEMANTIC_UD_TIMEOUT_MS = 6000;
    private static final double RUNTIME_VIZ_EVENT_RATE_PER_SEC = 1500.0d;
    private static final double RUNTIME_VIZ_EVENT_BURST = 3000.0d;
    private static final int RUNTIME_VIZ_EVENT_RATE_MIN = 100;
    private static final int RUNTIME_VIZ_EVENT_RATE_MAX = 20000;
    private static final int RUNTIME_VIZ_EVENT_BURST_MIN = 200;
    private static final int RUNTIME_VIZ_EVENT_BURST_MAX = 40000;
    private static final long RUNTIME_VIZ_DROP_LOG_INTERVAL_MS = 2000L;
    private static WebUiServer sInstance;
    private static final String DEMO_PROJECT_ID = "demo-project";

    private Javalin mApp;
    private boolean mAllowExternal = false;
    private ServerMode mMode = ServerMode.FULL_EDITOR;
    private String mAuthToken;
    private final Map<String, ProjectRef> projectStore = new HashMap<>();
    private final Map<String, WsCommandHandler> wsCommandRegistry = new HashMap<>();
    private final java.util.Set<WsContext> wsSessions = ConcurrentHashMap.newKeySet();
    /** Maps WS session-id → project-id for clients that joined via Session.Subscribe. */
    private final ConcurrentHashMap<String, String> wsProjectSubscriptions = new ConcurrentHashMap<>();
    /** Tracks runtime lifecycle and exclusive hardware-resource arbitration across all sessions. */
    private final RuntimeOrchestrator mRuntimeOrchestrator = new RuntimeOrchestrator();
    /** Named-user token registry; the legacy shared token is registered on server start. */
    private final SessionGate mSessionGate = new SessionGate();
    private final ConcurrentHashMap<String, RuntimeVizRateLimiter> runtimeVizRateLimiters = new ConcurrentHashMap<>();
    private final RuntimeGateway runtimeGateway;
    private final RuntimeCommandService runtimeCommandService = new RuntimeCommandService();
    private final NodeVarDefCommandService nodeVarDefCommandService = new NodeVarDefCommandService();
    private final NodeTypeDefCommandService nodeTypeDefCommandService = new NodeTypeDefCommandService();
    private final NodeCmdCommandService nodeCmdCommandService = new NodeCmdCommandService();
    private final CommentCommandService commentCommandService = new CommentCommandService();
    private final SelectionCommandService selectionCommandService = new SelectionCommandService();
    private final UndoRedoCommandService undoRedoCommandService = new UndoRedoCommandService();
    private final PlaySceneCommandService playSceneCommandService = new PlaySceneCommandService();
    private final ScriptCommandService scriptCommandService = new ScriptCommandService();
    private final ConfigCommandService configCommandService = new ConfigCommandService();
    private final PluginCreateCommandService pluginCreateCommandService = new PluginCreateCommandService();
    private final ProjectTemplatesInstallCommandService projectTemplatesInstallCommandService = new ProjectTemplatesInstallCommandService();
    private final ProjectConfigUpdateCommandService projectConfigUpdateCommandService = new ProjectConfigUpdateCommandService();
    private final PreferencesCommandService preferencesCommandService = new PreferencesCommandService();
    private final EdgeCrudCommandService edgeCrudCommandService = new EdgeCrudCommandService();
    private final EdgeLayoutCommandService edgeLayoutCommandService = new EdgeLayoutCommandService();
    private final EdgeRetargetCommandService edgeRetargetCommandService = new EdgeRetargetCommandService();
    private final EdgeProbabilityCommandService edgeProbabilityCommandService = new EdgeProbabilityCommandService();
    private final NodeMoveGroupCommandService nodeMoveGroupCommandService = new NodeMoveGroupCommandService();

    private static final class RuntimeVizRateLimiter {
        private double tokens = RUNTIME_VIZ_EVENT_BURST;
        private long lastRefillNanos = 0L;
        private long droppedSinceLastLog = 0L;
        private long lastLogTs = 0L;

        private synchronized boolean allow(final long nowNanos, final double ratePerSec, final double burst) {
            if (lastRefillNanos <= 0L) {
                lastRefillNanos = nowNanos;
            }
            long elapsedNanos = Math.max(0L, nowNanos - lastRefillNanos);
            tokens = Math.min(tokens, Math.max(1.0d, burst));
            double refill = (elapsedNanos / 1_000_000_000.0d) * Math.max(1.0d, ratePerSec);
            if (refill > 0.0d) {
                tokens = Math.min(Math.max(1.0d, burst), tokens + refill);
                lastRefillNanos = nowNanos;
            }
            if (tokens >= 1.0d) {
                tokens -= 1.0d;
                return true;
            }
            droppedSinceLastLog++;
            return false;
        }

        private synchronized long consumeDroppedForLog(final long nowMs) {
            if (droppedSinceLastLog <= 0L) {
                return 0L;
            }
            if (lastLogTs > 0L && (nowMs - lastLogTs) < RUNTIME_VIZ_DROP_LOG_INTERVAL_MS) {
                return 0L;
            }
            long dropped = droppedSinceLastLog;
            droppedSinceLastLog = 0L;
            lastLogTs = nowMs;
            return dropped;
        }
    }
    private final RuntimeCommandService.Context runtimeCommandContext;
    private final NodeVarDefCommandService.Context nodeVarDefCommandContext = new NodeVarDefCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public VariableDefinition parseVarDef(JSONObject source, BasicNode node, StringBuilder error) {
            return WebUiServer.this.parseVarDef(source, node, error);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public int renameVariableReferences(SuperNode root, String oldName, String newName) {
            return WebUiServer.this.renameVariableReferences(root, oldName, newName);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final NodeTypeDefCommandService.Context nodeTypeDefCommandContext = new NodeTypeDefCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public DataTypeDefinition parseTypeDef(JSONObject source, StringBuilder error) {
            return WebUiServer.this.parseTypeDef(source, error);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final NodeCmdCommandService.Context nodeCmdCommandContext = new NodeCmdCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public Command parseCommandText(String text, StringBuilder error) {
            return WebUiServer.this.parseCommandText(text, error);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final EdgeLayoutCommandService.Context edgeLayoutCommandContext = new EdgeLayoutCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public AbstractEdge resolveEdgeById(SuperNode root, String edgeId) {
            return WebUiServer.this.resolveEdgeById(root, edgeId);
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.getEditorConfigInt(ref, key, fallback) : fallback;
        }

        @Override
        public void relayoutEdgesInOrder(List<AbstractEdge> edges, int nodeWidth, int nodeHeight) {
            mEdgeLayout.relayoutEdgesInOrder(edges, nodeWidth, nodeHeight);
        }

        @Override
        public void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);
        }

        @Override
        public void clearDockPointsRecursive(SuperNode root) {
            mEdgeLayout.clearDockPointsRecursive(root);
        }

        @Override
        public void occupyStartSignDockPointsRecursive(SuperNode root) {
            mEdgeLayout.occupyStartSignDockPointsRecursive(root);
        }

        @Override
        public void collectEdgesRecursive(SuperNode root, List<AbstractEdge> edges, Set<AbstractEdge> seen) {
            mEdgeLayout.collectEdgesRecursive(root, edges, seen);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final EdgeRetargetCommandService.Context edgeRetargetCommandContext = new EdgeRetargetCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public AbstractEdge resolveEdgeById(SuperNode root, String edgeId) {
            return WebUiServer.this.resolveEdgeById(root, edgeId);
        }

        @Override
        public BasicNode resolveNodeById(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.getEditorConfigInt(ref, key, fallback) : fallback;
        }

        @Override
        public int findDockPointIndex(double nodeX, double nodeY, int nodeWidth, int nodeHeight, boolean isSuperNode, double pointX, double pointY) {
            return mEdgeLayout.findDockPointIndex(nodeX, nodeY, nodeWidth, nodeHeight, isSuperNode, pointX, pointY);
        }

        @Override
        public void releaseDockPoint(String nodeId, int dockIndex, boolean isSource) {
            mEdgeLayout.releaseDockPoint(nodeId, dockIndex, isSource);
        }

        @Override
        public int[] findSelfLoopDockPointPair(String nodeId, int nodeWidth, int nodeHeight, boolean isSuperNode) {
            return mEdgeLayout.findSelfLoopDockPointPair(nodeId, nodeWidth, nodeHeight, isSuperNode);
        }

        @Override
        public int[] findBestDockPointPair(String sourceNodeId, double srcX, double srcY, int srcWidth, int srcHeight, boolean srcIsSuperNode, String targetNodeId, double tgtX, double tgtY, int tgtWidth, int tgtHeight, boolean tgtIsSuperNode) {
            return mEdgeLayout.findBestDockPointPair(sourceNodeId, srcX, srcY, srcWidth, srcHeight, srcIsSuperNode,
                    targetNodeId, tgtX, tgtY, tgtWidth, tgtHeight, tgtIsSuperNode);
        }

        @Override
        public void occupyDockPoint(String nodeId, int dockIndex, boolean isSource) {
            mEdgeLayout.occupyDockPoint(nodeId, dockIndex, isSource);
        }

        @Override
        public double[] getDockPointPosition(double nodeX, double nodeY, int nodeWidth, int nodeHeight, boolean isSuperNode, int dockIndex) {
            return mEdgeLayout.getDockPointPosition(nodeX, nodeY, nodeWidth, nodeHeight, isSuperNode, dockIndex);
        }

        @Override
        public double[] computeSelfLoopControlPoints(double startX, double startY, double endX, double endY, double nodeCenterX, double nodeCenterY, int nodeWidth, int nodeHeight) {
            return mEdgeLayout.computeSelfLoopControlPoints(startX, startY, endX, endY, nodeCenterX, nodeCenterY, nodeWidth, nodeHeight);
        }

        @Override
        public double[] computeInitialControlPoint(double startX, double startY, double endX, double endY, boolean isStart) {
            return mEdgeLayout.computeInitialControlPoint(startX, startY, endX, endY, isStart);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final EdgeProbabilityCommandService.Context edgeProbabilityCommandContext = new EdgeProbabilityCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public BasicNode resolveNodeById(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public RandomEdge resolvePEdgeForSource(SuperNode root, BasicNode sourceNode, String edgeId, String targetId) {
            return WebUiServer.this.resolvePEdgeForSource(root, sourceNode, edgeId, targetId);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final NodeMoveGroupCommandService.Context nodeMoveGroupCommandContext = new NodeMoveGroupCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.getEditorConfigInt(ref, key, fallback) : fallback;
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public void updateEdgeEndpointsForMovedNode(BasicNode movedNode, SuperNode activeSuperNode, int oldX, int oldY) {
            WebUiServer.this.updateEdgeEndpointsForMovedNode(movedNode, activeSuperNode, oldX, oldY);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final CommentCommandService.Context commentCommandContext = new CommentCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject mutateAndSnapshotLegacy(String projectId, String operation, JSONObject params, java.util.function.Consumer<String> broadcaster) {
            switch (operation) {
                case "add":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.addComment(params), broadcaster);
                case "update":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.updateComment(params), broadcaster);
                case "delete":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.deleteComment(params), broadcaster);
                default:
                    return WebUiServer.this.errorResponse("BAD_REQUEST", "Unsupported legacy comment operation: " + operation);
            }
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final SelectionCommandService.Context selectionCommandContext = new SelectionCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.getEditorConfigInt(ref, key, fallback) : fallback;
        }

        @Override
        public void collectNodes(SuperNode node, List<BasicNode> out) {
            WebUiServer.this.collectNodes(node, out);
        }

        @Override
        public String allocateNodeId(String projectId, boolean superNode, Set<String> used) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref == null) {
                return superNode ? "S1" : "N1";
            }
            return WebUiServer.this.allocateNodeId(ref, superNode, used);
        }

        @Override
        public BasicNode resolveNodeById(SuperNode root, String nodeId) {
            return WebUiServer.this.findNodeRecursive(root, nodeId);
        }

        @Override
        public Expression parseExpressionOrNull(String text) {
            return WebUiServer.this.parseExpressionOrNull(text);
        }

        @Override
        public void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }

        @Override
        public void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void markDirty(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.dirty = true;
            }
        }

        @Override
        public List<BasicNode> clipboardNodes(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.clipboard : new ArrayList<>();
        }

        @Override
        public List<SelectionCommandService.ClipboardEdgeData> clipboardEdges(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.clipboardEdges : new ArrayList<>();
        }

        @Override
        public Set<String> clipboardStartNodeIds(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.clipboardStartNodeIds : new HashSet<>();
        }
    };
    private final UndoRedoCommandService.Context undoRedoCommandContext = new UndoRedoCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public void ensureHistoryLoaded(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.ensureHistoryLoaded(ref);
            }
        }

        @Override
        public int historyIndex(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.historyIndex : -1;
        }

        @Override
        public int historySize(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.history.size() : 0;
        }

        @Override
        public void setHistoryIndex(String projectId, int index) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.historyIndex = index;
            }
        }

        @Override
        public void setHistorySuspended(String projectId, boolean value) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.historySuspended = value;
            }
        }

        @Override
        public void setCommandLogSuspended(String projectId, boolean value) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.commandLogSuspended = value;
            }
        }

        @Override
        public boolean applyHistoryEntryAtIndex(String projectId, int index) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref == null || index < 0 || index >= ref.history.size()) {
                return false;
            }
            return WebUiServer.this.applyHistoryEntry(ref, ref.history.get(index));
        }

        @Override
        public void saveHistoryToDisk(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.saveHistoryToDisk(ref);
            }
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SceneFlow snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public JSONObject buildScriptSnapshot(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.buildScriptSnapshot(ref) : new JSONObject();
        }

        @Override
        public void broadcastScriptSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastScriptSnapshot(broadcaster, projectId, snapshot);
        }
    };
    private final PlaySceneCommandService.Context playSceneCommandContext = new PlaySceneCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public void collectPlaySceneReferences(SuperNode root, String sceneName, List<JSONObject> matches) {
            WebUiServer.this.collectPlaySceneReferences(root, sceneName, matches);
        }

        @Override
        public void collectPlaySceneReferences(SuperNode root, Set<String> sceneNames, List<JSONObject> matches) {
            WebUiServer.this.collectPlaySceneReferences(root, sceneNames, matches);
        }

        @Override
        public int renamePlaySceneReferences(SuperNode root, String sceneName, String newName) {
            return WebUiServer.this.renamePlaySceneReferences(root, sceneName, newName);
        }

        @Override
        public void markDirty(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.dirty = true;
            }
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };
    private final ScriptCommandService.Context scriptCommandContext = new ScriptCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public void ensureScriptLoaded(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.ensureScriptLoaded(ref);
            }
        }

        @Override
        public int scriptVersion(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.scriptVersion : 1;
        }

        @Override
        public String scriptText(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? (ref.scriptText == null ? "" : ref.scriptText) : "";
        }

        @Override
        public boolean scriptParseOk(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null && ref.scriptParseOk;
        }

        @Override
        public JSONArray scriptParseErrors(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.diagnosticsToJson(ref.scriptParseErrors) : new JSONArray();
        }

        @Override
        public String serializeSceneScript(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.serializeSceneScript(ref.runtimeProject) : "";
        }

        @Override
        public boolean applyScriptText(String projectId, String text) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null && WebUiServer.this.applyScriptText(ref.runtimeProject, text);
        }

        @Override
        public void setScriptText(String projectId, String text) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.scriptText = text;
            }
        }

        @Override
        public void setScriptVersion(String projectId, int version) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.scriptVersion = version;
            }
        }

        @Override
        public void setScriptParseOk(String projectId, boolean value) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.scriptParseOk = value;
            }
        }

        @Override
        public void clearScriptParseErrors(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.scriptParseErrors.clear();
            }
        }

        @Override
        public void markDirty(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.dirty = true;
            }
        }

        @Override
        public void broadcastScriptSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastScriptSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }

        @Override
        public JSONArray diagnosticsToJson(List<ScriptDiagnostics.Diagnostic> diagnostics) {
            return WebUiServer.this.diagnosticsToJson(diagnostics);
        }
    };
    private final ConfigCommandService.Context configCommandContext = new ConfigCommandService.Context() {
        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public boolean projectExists(String projectId) {
            return projectStore.containsKey(projectId);
        }

        @Override
        public Properties loadEditorConfig(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.loadEditorConfig(ref) : new Properties();
        }

        @Override
        public boolean saveEditorConfig(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null && WebUiServer.this.saveEditorConfig(ref);
        }

        @Override
        public String projectPath(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.path : "";
        }

        @Override
        public void setEditorConfigDirty(String projectId, boolean value) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.editorConfigDirty = value;
            }
        }

        @Override
        public void setProjectDirty(String projectId, boolean value) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.dirty = value;
            }
        }

        @Override
        public JSONObject editorConfigToJson(Properties props) {
            return WebUiServer.this.editorConfigToJson(props);
        }

        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }
    };
    private final PluginCreateCommandService.Context pluginCreateCommandContext = new PluginCreateCommandService.Context() {
        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public JSONObject pluginSpec(String className) {
            ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
            return entry != null ? entry.pluginSpec : null;
        }

        @Override
        public JSONObject variablesSpec(String className) {
            ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
            return entry != null ? entry.variables : null;
        }

        @Override
        public String specVersion(String className) {
            ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
            return entry != null ? entry.specVersion : "";
        }
    };
    private final ProjectTemplatesInstallCommandService.Context projectTemplatesInstallCommandContext = new ProjectTemplatesInstallCommandService.Context() {
        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public boolean hasRuntimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null && ref.runtimeProject != null;
        }

        @Override
        public String projectPath(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.path : "";
        }

        @Override
        public JSONObject pluginSpec(String className) {
            ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
            return entry != null ? entry.pluginSpec : null;
        }

        @Override
        public ClassLoader classLoader() {
            return WebUiServer.this.getClass().getClassLoader();
        }

        @Override
        public void copyTemplateDirectory(URL resourceUrl, String basePath, File destDir, JSONArray createdFiles, JSONArray skippedFiles) {
            WebUiServer.this.copyTemplateDirectory(resourceUrl, basePath, destDir, createdFiles, skippedFiles);
        }

        @Override
        public void warn(String message) {
            sLogger.warning(message);
        }
    };
    private final ProjectConfigUpdateCommandService.Context projectConfigUpdateCommandContext = new ProjectConfigUpdateCommandService.Context() {
        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public boolean hasRuntimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null && ref.runtimeProject != null;
        }

        @Override
        public ProjectConfig projectConfig(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return (ref != null && ref.runtimeProject != null) ? ref.runtimeProject.getProjectConfig() : null;
        }

        @Override
        public void applyProjectConfigFromJson(String projectId, ProjectConfig config, JSONObject configJson) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.applyProjectConfigFromJson(ref, config, configJson);
            }
        }

        @Override
        public void markDirty(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                ref.dirty = true;
            }
        }

        @Override
        public JSONObject projectConfigToJson(ProjectConfig config, String projectPath) {
            return WebUiServer.this.projectConfigToJson(config, projectPath);
        }

        @Override
        public String projectPath(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.path : "";
        }
    };
    private final PreferencesCommandService.Context preferencesCommandContext = new PreferencesCommandService.Context() {
        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public void removePreference(String key) {
            Preferences.removeProperty(key);
        }

        @Override
        public void setPreference(String key, String value) {
            Preferences.setProperty(key, value);
        }

        @Override
        public void savePreferences() {
            Preferences.save();
        }

        @Override
        public JSONObject preferencesToJson() {
            return WebUiServer.this.preferencesToJson();
        }
    };
    private final EdgeCrudCommandService.Context edgeCrudCommandContext = new EdgeCrudCommandService.Context() {
        @Override
        public RunTimeProject runtimeProject(String projectId) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? ref.runtimeProject : null;
        }

        @Override
        public JSONObject mutateAndSnapshotLegacy(String projectId, String operation, JSONObject params, java.util.function.Consumer<String> broadcaster) {
            switch (operation) {
                case "add":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.addEdge(params), broadcaster);
                case "update":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.updateEdge(params), broadcaster);
                case "delete":
                    return WebUiServer.this.mutateAndSnapshot(projectId, () -> WebUiServer.this.deleteEdge(params), broadcaster);
                default:
                    return WebUiServer.this.errorResponse("BAD_REQUEST", "Unsupported legacy edge operation: " + operation);
            }
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return WebUiServer.this.errorResponse(code, message);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            return WebUiServer.this.resolveSuperNode(sceneFlow, superNodeId);
        }

        @Override
        public BasicNode resolveNodeById(SuperNode root, String nodeId) {
            return WebUiServer.this.resolveNodeById(root, nodeId);
        }

        @Override
        public AbstractEdge resolveEdgeById(SuperNode root, String edgeId) {
            return WebUiServer.this.resolveEdgeById(root, edgeId);
        }

        @Override
        public Expression parseExpressionOrNull(String text) {
            return WebUiServer.this.parseExpressionOrNull(text);
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            ProjectRef ref = projectStore.get(projectId);
            return ref != null ? WebUiServer.this.getEditorConfigInt(ref, key, fallback) : fallback;
        }

        @Override
        public void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }

        @Override
        public void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.normalizeEdge(edge, nodeWidth, nodeHeight);
        }

        @Override
        public void releaseEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
            mEdgeLayout.releaseEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return WebUiServer.this.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return WebUiServer.this.buildSceneFlowResponse(snapshot);
        }

        @Override
        public void broadcastSceneFlowSnapshot(java.util.function.Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
            WebUiServer.this.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        }

        @Override
        public void recordHistory(String projectId, String action) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordHistory(ref, action);
            }
        }

        @Override
        public void recordCommand(String projectId, String action, JSONObject params) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) {
                WebUiServer.this.recordCommand(ref, action, params);
            }
        }
    };

    // Edge layout service (dock points, normalization, straightening)
    private final EdgeLayoutService mEdgeLayout = new EdgeLayoutService();

    @FunctionalInterface
    private interface WsCommandHandler {
        JSONObject handle(String method, JSONObject params, java.util.function.Consumer<String> broadcaster);
    }

    private WebUiServer() {
        runtimeGateway = RuntimeGateways.createDefault(this::dispatchCommand, this::snapshot);
        runtimeCommandContext = new WebUiRuntimeCommandContext(
                this::loadProject,
                this::firstLoadedProjectId,
                this::runtimeProjectForId,
                this::runtimeStateForId,
                this::setRuntimeStateForId,
                this::projectPathForId,
                this::projectNameForId,
                this::removeProjectById,
                this::errorResponse,
                this::addRuntimeCapabilities,
                sLogger::message,
                this::handleRuntimeVariableSetCommand,
                this::handleRuntimeQueryCommand
        );
        initializeWsCommandRegistry();
    }

    private void registerWsCommands(final WsCommandHandler handler, final String... methods) {
        for (String method : methods) {
            wsCommandRegistry.put(method, handler);
        }
    }

    private void initializeWsCommandRegistry() {
        registerSceneFlowWsCommands();
        registerProjectWsCommands();
        registerRuntimeWsCommands();
    }

    private void registerSceneFlowWsCommands() {
        registerWsCommands((method, params, broadcaster) -> runtimeGateway.snapshot(params.optString("projectId", "")),
                "SceneFlow.Get", "SceneFlow.Snapshot");
        registerWsCommands((method, params, broadcaster) -> createNodeForProject(params, broadcaster),
                "SceneFlow.Node.Add", "SceneFlow.Node.Create");
        registerWsCommands((method, params, broadcaster) -> createAliasNodeForProject(params, broadcaster),
                "SceneFlow.Node.CreateAlias");
        registerWsCommands((method, params, broadcaster) -> updateNodeForProject(params, broadcaster),
                "SceneFlow.Node.Update");
        registerWsCommands((method, params, broadcaster) -> deleteNodeForProject(params, broadcaster),
                "SceneFlow.Node.Delete");
        registerWsCommands((method, params, broadcaster) -> moveNodeForProject(params, broadcaster),
                "SceneFlow.Node.Move");
        registerWsCommands((method, params, broadcaster) -> edgeCrudCommandService.dispatch(method, params, broadcaster, edgeCrudCommandContext),
                "SceneFlow.Edge.Add", "SceneFlow.Edge.Create", "SceneFlow.Edge.Update", "SceneFlow.Edge.Delete");
        registerWsCommands((method, params, broadcaster) -> commentCommandService.dispatch(method, params, broadcaster, commentCommandContext),
                "SceneFlow.Comment.Add", "SceneFlow.Comment.Create", "SceneFlow.Comment.Update", "SceneFlow.Comment.Delete");
        registerWsCommands((method, params, broadcaster) -> selectionCommandService.dispatch(method, params, broadcaster, selectionCommandContext),
                "SceneFlow.Selection.Copy", "SceneFlow.Selection.Paste");
        registerWsCommands((method, params, broadcaster) -> undoRedoCommandService.dispatch(method, params, broadcaster, undoRedoCommandContext),
                "SceneFlow.Undo", "SceneFlow.Redo");
        registerWsCommands((method, params, broadcaster) -> playSceneCommandService.dispatch(method, params, broadcaster, playSceneCommandContext),
                "SceneFlow.PlayScene.Find", "SceneFlow.PlayScene.FindMany", "SceneFlow.PlayScene.Rename");
        registerWsCommands((method, params, broadcaster) -> nodeVarDefCommandService.dispatch(method, params, broadcaster, nodeVarDefCommandContext),
                "SceneFlow.Node.VarDef.Add", "SceneFlow.Node.VarDef.Update", "SceneFlow.Node.VarDef.Delete", "SceneFlow.Node.VarDef.Move");
        registerWsCommands((method, params, broadcaster) -> nodeTypeDefCommandService.dispatch(method, params, broadcaster, nodeTypeDefCommandContext),
                "SceneFlow.Node.TypeDef.Add", "SceneFlow.Node.TypeDef.Update", "SceneFlow.Node.TypeDef.Delete", "SceneFlow.Node.TypeDef.Move");
        registerWsCommands((method, params, broadcaster) -> nodeCmdCommandService.dispatch(method, params, broadcaster, nodeCmdCommandContext),
                "SceneFlow.Node.Cmd.Add", "SceneFlow.Node.Cmd.Update", "SceneFlow.Node.Cmd.Delete", "SceneFlow.Node.Cmd.Move");
        registerWsCommands((method, params, broadcaster) -> edgeLayoutCommandService.dispatch(method, params, broadcaster, edgeLayoutCommandContext),
                "SceneFlow.Edge.Normalize", "SceneFlow.Edge.Straighten", "SceneFlow.Edge.NormalizeAll", "SceneFlow.Edge.StraightenAll",
                "SceneFlow.Edge.NormalizeGroup", "SceneFlow.Edge.StraightenGroup");
        registerWsCommands((method, params, broadcaster) -> edgeRetargetCommandService.dispatch(params, broadcaster, edgeRetargetCommandContext),
                "SceneFlow.Edge.Retarget");
        registerWsCommands((method, params, broadcaster) -> edgeProbabilityCommandService.dispatch(params, broadcaster, edgeProbabilityCommandContext),
                "SceneFlow.Edge.PEdge.UpdateGroup");
        registerWsCommands((method, params, broadcaster) -> nodeMoveGroupCommandService.dispatch(params, broadcaster, nodeMoveGroupCommandContext),
                "SceneFlow.Node.MoveGroup");
    }

    private void registerProjectWsCommands() {
        registerWsCommands((method, params, broadcaster) -> startEmbeddingsService(params),
                "Embeddings.Start");
        registerWsCommands((method, params, broadcaster) -> scriptCommandService.dispatch(params, broadcaster, scriptCommandContext),
                "Script.Update");
        registerWsCommands((method, params, broadcaster) -> {
            if (broadcaster != null) {
                JSONObject evt = new JSONObject();
                evt.put("type", "event");
                evt.put("event", "script.live");
                evt.put("projectId", params.optString("projectId", ""));
                evt.put("text", params.optString("text", ""));
                broadcaster.accept(evt.toString());
            }
            return new JSONObject().put("status", "ok");
        }, "Script.Live");
        registerWsCommands((method, params, broadcaster) -> configCommandService.dispatch(params, broadcaster, configCommandContext),
                "Config.Update");
        registerWsCommands((method, params, broadcaster) -> pluginCreateCommandService.dispatch(params, pluginCreateCommandContext),
                "ProjectConfig.Plugin.Create");
        registerWsCommands((method, params, broadcaster) -> handlePluginGetUpdate(params),
                "ProjectConfig.Plugin.GetUpdate");
        registerWsCommands((method, params, broadcaster) -> projectTemplatesInstallCommandService.dispatch(params, projectTemplatesInstallCommandContext),
                "Project.Templates.Install");
        registerWsCommands((method, params, broadcaster) -> projectConfigUpdateCommandService.dispatch(params, broadcaster, projectConfigUpdateCommandContext),
                "ProjectConfig.Update");
        registerWsCommands((method, params, broadcaster) -> preferencesCommandService.dispatch(params, broadcaster, preferencesCommandContext),
                "Preferences.Update");
        registerWsCommands((method, params, broadcaster) -> new JSONObject().put("status", "ok"),
                "Project.Save", "Project.SaveAs", "Project.Close");
    }

    private void registerRuntimeWsCommands() {
        registerWsCommands((method, params, broadcaster) -> runtimeCommandService.dispatchRuntimeCommand(method, params, broadcaster, runtimeCommandContext),
                "Runtime.Load", "Runtime.Play", "Runtime.Start", "Runtime.Resume", "Runtime.Pause", "Runtime.Stop",
                "Runtime.Unload", "Runtime.Variable.Set", "Runtime.Query");
    }

    public static synchronized WebUiServer getInstance() {
        if (sInstance == null) {
            sInstance = new WebUiServer();
        }
        return sInstance;
    }

    public RuntimeGateway getRuntimeGateway() {
        return runtimeGateway;
    }

    @Override
    public JSONObject dispatchCommand(String method, JSONObject params, java.util.function.Consumer<String> broadcaster) {
        return dispatchWs(method, params, broadcaster);
    }

    @Override
    public JSONObject snapshot(String projectId) {
        return snapshotPayload(projectId);
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
        final boolean hasWebUi = getClass().getClassLoader().getResource("web-ui/index.html") != null;
        final boolean hasImages = getClass().getClassLoader().getResource("images/") != null;
        mApp = Javalin.create(config -> {
            // Try to add static files if available (editor mode)
            // These may not be present in runtime-only mode
            if (hasWebUi) {
                config.staticFiles.add(staticFiles -> {
                    staticFiles.hostedPath = "/web-ui";
                    staticFiles.directory = "web-ui";
                    staticFiles.location = Location.CLASSPATH;
                });
                config.spaRoot.addFile("/web-ui", "/web-ui/index.html", Location.CLASSPATH);
            }
            if (hasImages) {
                config.staticFiles.add(staticFiles -> {
                    staticFiles.hostedPath = "/images";
                    staticFiles.directory = "images";
                    staticFiles.location = Location.CLASSPATH;
                });
            }
            // Enable CORS for cross-origin requests (Phase 8.4: remote connections)
            config.bundledPlugins.enableCors(cors -> cors.addRule(it -> it.anyHost()));
        }).start(allowExternal ? "0.0.0.0" : "127.0.0.1", port);
        if (hasWebUi) {
            mApp.get("/", ctx -> ctx.redirect("/web-ui/"));
        }
        registerRoutes();
        // EventDispatcher registration now happens per-project when projects are added to projectStore.
        // See onProjectRegistered() / onProjectRemoved().
        sLogger.message("Web UI server started on " + getLocalUrl());
    }

    /**
     * Starts the server in a specific mode with auth token support.
     * Used by RuntimeMain for RUNTIME_ONLY mode.
     */
    public void start(int port, String bindHost, String token, ServerMode mode) {
        mMode = mode;
        mAuthToken = (token != null) ? token : generateToken();
        // Register the shared server token in the SessionGate so it can be used
        // for per-user token provisioning once Phase F auth is fully enforced.
        mSessionGate.clear();
        mSessionGate.provisionWithToken(mAuthToken, "admin", "Admin",
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_RUNTIME_ADMIN, SessionGate.ROLE_VIEWER));
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
        final String normalizedPath = normalizeProjectPath(path);
        // Unload any existing project in the store
        for (ProjectRef ref : projectStore.values()) {
            if (ref.runtimeProject != null && ref.runtimeProject.wasExecuted()) {
                ref.runtimeProject.unload();
            }
        }
        projectStore.clear();

        try {
            RunTimeProject rtp = new RunTimeProject();
            if (rtp.parse(normalizedPath)) {
                String pid = UUID.randomUUID().toString();
                if (rtp.launch()) {
                    String name = rtp.getProjectName() != null ? rtp.getProjectName() : new File(normalizedPath).getName();
                    ProjectRef ref = new ProjectRef(pid, name, normalizedPath);
                    ref.runtimeProject = rtp;
                    ref.runtimeState = "stopped";
                    projectStore.put(pid, ref);
                    registerProjectDispatcher(pid, ref);
                    broadcastRuntimeState(pid, "stopped");
                    sLogger.message("Loaded project: " + normalizedPath);
                    return true;
                } else {
                    sLogger.failure("Failed to launch project: " + normalizedPath);
                }
            } else {
                sLogger.failure("Failed to parse project: " + normalizedPath);
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
            // Deregister from all project dispatchers.
            for (ProjectRef ref : projectStore.values()) {
                unregisterProjectDispatcher(ref);
            }
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
            broadcastToAll(message.toString());
            return;
        }

        String projectId = findProjectIdForEvent(event);
        if (isRuntimeVisualizationEvent(event) && shouldDropRuntimeVisualizationEvent(projectId, event)) {
            return;
        }
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
                payload.put("ancestorIds", buildAncestorIds(node.getParentNode()));
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.nodeActive");

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
                payload.put("ancestorIds", buildAncestorIds(node.getParentNode()));
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.nodeStopped");

        } else if (event instanceof EdgeExecutedEvent) {
            // EdgeExecutedEvent → runtime.edgeActive
            AbstractEdge edge = ((EdgeExecutedEvent) event).getEdge();
            if (edge == null) return;
            String sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            String targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            if (edge.getSourceNode() != null && edge.getSourceNode().getParentNode() != null) {
                payload.put("sourceParentId", edge.getSourceNode().getParentNode().getId());
                payload.put("sourceAncestorIds", buildAncestorIds(edge.getSourceNode().getParentNode()));
            }
            if (edge.getTargetNode() != null && edge.getTargetNode().getParentNode() != null) {
                payload.put("targetParentId", edge.getTargetNode().getParentNode().getId());
            }
            payload.put("edgeType", getEdgeTypeLowercase(edge));
            message.put("channel", "runtime");
            message.put("event", "runtime.edgeActive");

        } else if (event instanceof TimeoutEdgeStartedEvent) {
            // TimeoutEdgeStartedEvent → runtime.timeoutProgress
            TimeoutEdgeStartedEvent te = (TimeoutEdgeStartedEvent) event;
            TimeoutEdge edge = te.getEdge();
            if (edge == null) return;
            String sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            String targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            if (edge.getSourceNode() != null && edge.getSourceNode().getParentNode() != null) {
                payload.put("sourceParentId", edge.getSourceNode().getParentNode().getId());
                payload.put("sourceAncestorIds", buildAncestorIds(edge.getSourceNode().getParentNode()));
            }
            if (edge.getTargetNode() != null && edge.getTargetNode().getParentNode() != null) {
                payload.put("targetParentId", edge.getTargetNode().getParentNode().getId());
            }
            payload.put("edgeType", "timeout");
            payload.put("timeoutMs", te.getTimeoutMs());
            payload.put("startedAt", te.getStartedAt());
            payload.put("elapsedMs", 0L);
            payload.put("ratio", 0.0);
            message.put("channel", "runtime");
            message.put("event", "runtime.timeoutProgress");

        } else if (event instanceof SceneExecutedEvent) {
            SceneExecutedEvent sceneEvent = (SceneExecutedEvent) event;
            SceneObject scene = sceneEvent.getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            if (!sceneEvent.getNodeId().isBlank()) {
                payload.put("nodeId", sceneEvent.getNodeId());
            }
            if (!sceneEvent.getParentId().isBlank()) {
                payload.put("parentId", sceneEvent.getParentId());
            }
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.playing");

        } else if (event instanceof SceneDoneEvent) {
            SceneDoneEvent sceneEvent = (SceneDoneEvent) event;
            SceneObject scene = sceneEvent.getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            if (!sceneEvent.getNodeId().isBlank()) {
                payload.put("nodeId", sceneEvent.getNodeId());
            }
            if (!sceneEvent.getParentId().isBlank()) {
                payload.put("parentId", sceneEvent.getParentId());
            }
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
            if (projectId != null && !projectId.isBlank()) {
                runtimeVizRateLimiters.remove(projectId);
            } else {
                runtimeVizRateLimiters.remove("__global__");
            }
            // Update runtime state in project store
            if (projectId != null) {
                ProjectRef ref = projectStore.get(projectId);
                if (ref != null) {
                    ref.runtimeState = "stopped";
                }
            }

        } else {
            // Unknown event type, skip
            return;
        }

        message.put("payload", payload);
        broadcastToAll(message.toString());
    }

    private boolean isRuntimeVisualizationEvent(final EventObject event) {
        return event instanceof NodeStartedEvent
                || event instanceof NodeExecutedEvent
                || event instanceof NodeTerminatedEvent
                || event instanceof EdgeExecutedEvent
                || event instanceof TimeoutEdgeStartedEvent;
    }

    private boolean shouldDropRuntimeVisualizationEvent(final String projectId, final EventObject event) {
        // Never drop node lifecycle events, otherwise UI activity counters can
        // desynchronize (nodeActive without matching nodeStopped) and nodes may
        // appear permanently active.
        if (event instanceof NodeStartedEvent
                || event instanceof NodeExecutedEvent
                || event instanceof NodeTerminatedEvent) {
            return false;
        }
        final String limiterKey = (projectId == null || projectId.isBlank()) ? "__global__" : projectId;
        final RuntimeVizRateLimiter limiter = runtimeVizRateLimiters.computeIfAbsent(limiterKey, ignored -> new RuntimeVizRateLimiter());
        final double ratePerSec = getRuntimeVizRatePerSec(projectId);
        final double burst = getRuntimeVizBurst(projectId);
        final long nowNanos = System.nanoTime();
        if (limiter.allow(nowNanos, ratePerSec, burst)) {
            return false;
        }
        final long droppedToLog = limiter.consumeDroppedForLog(System.currentTimeMillis());
        if (droppedToLog > 0L) {
            sLogger.warning("[RuntimeViz] Dropping " + droppedToLog
                    + " high-frequency visualization events for project "
                    + limiterKey + " to keep server responsive (latest event="
                    + event.getClass().getSimpleName() + ", rate="
                    + Math.round(ratePerSec) + "/s, burst=" + Math.round(burst) + ").");
        }
        return true;
    }

    private double getRuntimeVizRatePerSec(final String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return RUNTIME_VIZ_EVENT_RATE_PER_SEC;
        }
        final ProjectRef ref = projectStore.get(projectId);
        if (ref == null) {
            return RUNTIME_VIZ_EVENT_RATE_PER_SEC;
        }
        int configured = readRuntimeVizIntFromProjectConfig(ref, "runtimeVizRate", -1);
        if (configured <= 0) {
            configured = getEditorConfigInt(ref, "runtime_viz_rate", (int) Math.round(RUNTIME_VIZ_EVENT_RATE_PER_SEC));
        }
        final int clamped = Math.max(RUNTIME_VIZ_EVENT_RATE_MIN, Math.min(RUNTIME_VIZ_EVENT_RATE_MAX, configured));
        return clamped;
    }

    private double getRuntimeVizBurst(final String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return RUNTIME_VIZ_EVENT_BURST;
        }
        final ProjectRef ref = projectStore.get(projectId);
        if (ref == null) {
            return RUNTIME_VIZ_EVENT_BURST;
        }
        int configured = readRuntimeVizIntFromProjectConfig(ref, "runtimeVizBurst", -1);
        if (configured <= 0) {
            configured = getEditorConfigInt(ref, "runtime_viz_burst", (int) Math.round(RUNTIME_VIZ_EVENT_BURST));
        }
        final int clamped = Math.max(RUNTIME_VIZ_EVENT_BURST_MIN, Math.min(RUNTIME_VIZ_EVENT_BURST_MAX, configured));
        return clamped;
    }

    private int readRuntimeVizIntFromProjectConfig(final ProjectRef ref, final String key, final int fallback) {
        if (ref == null || ref.runtimeProject == null || ref.runtimeProject.getProjectConfig() == null) {
            return fallback;
        }
        de.dfki.vsm.model.config.ConfigElement services = ref.runtimeProject.getProjectConfig().getSemanticServices();
        if (services == null) {
            return fallback;
        }
        String value = services.getProperty(key);
        if (value == null || value.isBlank()) {
            return fallback;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException ignored) {
            return fallback;
        }
    }

    private Integer parseRuntimeVizConfigValue(final String raw, final int min, final int max) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        try {
            int parsed = Integer.parseInt(raw.trim());
            return Math.max(min, Math.min(max, parsed));
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    /**
     * Returns edge type in lowercase format matching UiEventBridge.
     * Used for runtime.edgeActive events.
     */
    private String getEdgeTypeLowercase(AbstractEdge edge) {
        return SceneFlowSnapshotBuilder.getEdgeTypeLowercase(edge);
    }

    /**
     * Builds the full ancestor ID chain starting from the given node, walking up to the root.
     * Used in runtime.nodeActive/nodeStopped/edgeActive events so the UI can resolve
     * deeply nested nodes to their visible ancestor at any view level.
     */
    private JSONArray buildAncestorIds(SuperNode start) {
        JSONArray arr = new JSONArray();
        SuperNode current = start;
        while (current != null) {
            String id = current.getId();
            arr.put(id != null ? id : "__root__");
            current = current.getParentNode();
        }
        return arr;
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

    private String firstLoadedProjectId() {
        if (projectStore.isEmpty()) {
            return null;
        }
        return projectStore.keySet().iterator().next();
    }

    private RunTimeProject runtimeProjectForId(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        return ref != null ? ref.runtimeProject : null;
    }

    private String runtimeStateForId(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        return ref != null ? ref.runtimeState : null;
    }

    private void setRuntimeStateForId(String projectId, String state) {
        ProjectRef ref = projectStore.get(projectId);
        if (ref != null) {
            ref.runtimeState = state;
        }
        syncOrchestratorState(projectId, state);
    }

    /** Maps a string runtime state to {@link RuntimeOrchestrator.RuntimeState} and updates the orchestrator. */
    private void syncOrchestratorState(String projectId, String state) {
        if (projectId == null || !mRuntimeOrchestrator.contains(projectId)) return;
        RuntimeOrchestrator.RuntimeState s;
        if ("running".equals(state))      s = RuntimeOrchestrator.RuntimeState.RUNNING;
        else if ("paused".equals(state))  s = RuntimeOrchestrator.RuntimeState.PAUSED;
        else                              s = RuntimeOrchestrator.RuntimeState.STOPPED;
        mRuntimeOrchestrator.setState(projectId, s);
    }

    private String projectPathForId(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        return ref != null && ref.path != null ? ref.path : "";
    }

    private String projectNameForId(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        if (ref == null || ref.runtimeProject == null || ref.runtimeProject.getProjectName() == null) {
            return "";
        }
        return ref.runtimeProject.getProjectName();
    }

    private void removeProjectById(String projectId) {
        mRuntimeOrchestrator.unregister(projectId);
        ProjectRef ref = projectStore.remove(projectId);
        unregisterProjectDispatcher(ref);
    }

    private JSONObject handleRuntimeVariableSetCommand(String projectId, String name, String valueExpr) {
        ProjectRef ref = projectStore.get(projectId);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        RunTimeProject runtimeProject = ref.runtimeProject;
        SceneFlow sceneFlow = runtimeProject.getSceneFlow();
        VariableDefinition varDef = findVariableDefinitionInHierarchy(sceneFlow, name);
        if (varDef == null) {
            return errorResponse("VAR_NOT_FOUND", "Variable not found: " + name);
        }

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

        if (!isSupportedRuntimeExpression(exp)) {
            return errorResponse("UNSUPPORTED_EXPRESSION", "Expression type is not supported");
        }

        boolean setOk = applyRuntimeExpression(runtimeProject, name, exp);
        if (!setOk) {
            return errorResponse("SET_FAILED", "Failed to update variable");
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("projectId", projectId);
        response.put("name", name);
        String currentValue = resolveVariableValue(runtimeProject, name);
        if (currentValue != null) {
            response.put("value", currentValue);
        }
        addRuntimeCapabilities(response);
        return response;
    }

    private JSONObject handleRuntimeQueryCommand(String projectId, String query) {
        ProjectRef ref = projectStore.get(projectId);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (!isLogicEnabled()) {
            return errorResponse("UNSUPPORTED_FEATURE", "Logic engine is disabled on this platform");
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        int count = 0;
        try {
            count = LogicEngines.get().query(query.trim()).size();
        } catch (Exception exc) {
            sLogger.warning("Runtime.Query failed: " + exc.getMessage());
        }
        response.put("count", count);
        addRuntimeCapabilities(response);
        return response;
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
        syncOrchestratorState(projectId, state);
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

    // -------------------------------------------------------------------------
    // Per-project dispatcher registration / event routing (Component 2)
    // -------------------------------------------------------------------------

    /**
     * Creates a per-project {@link EventListener} forwarder, stores it in the
     * project's {@link CollaborationSession} and registers it on the project's
     * {@link EventDispatcher}.  Must be called after the project is put into
     * {@code projectStore}.
     */
    private void registerProjectDispatcher(String pid, ProjectRef ref) {
        mRuntimeOrchestrator.register(pid);
        if (ref.runtimeProject == null) return;
        EventListener forwarder = event -> handleProjectEvent(pid, event);
        ref.collaborationSession.setEventForwarder(forwarder);
        ref.runtimeProject.getEventDispatcher().register(forwarder);
    }

    /** Removes the stored per-project forwarder from the project's dispatcher. */
    private void unregisterProjectDispatcher(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null) return;
        EventListener forwarder = ref.collaborationSession.getEventForwarder();
        if (forwarder != null) {
            ref.runtimeProject.getEventDispatcher().remove(forwarder);
        }
    }

    /**
     * Sends {@code message} to the project's collaboration-session subscribers
     * when at least one is present; falls back to {@link #broadcastToAll} so
     * that legacy (unsubscribed) clients continue to receive events.
     */
    private void broadcastToProjectOrAll(String projectId, String message) {
        if (projectId != null) {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null && ref.collaborationSession.subscriberCount() > 0) {
                ref.collaborationSession.broadcast(message);
                return;
            }
        }
        broadcastToAll(message);
    }

    /**
     * Per-project event handler.  Identical logic to {@link #update(EventObject)}
     * but the {@code projectId} is known precisely (injected by the per-project
     * forwarder lambda), so no heuristic lookup is needed.
     */
    private void handleProjectEvent(String projectId, EventObject event) {
        if (event == null) return;

        if (event instanceof VariableChangedEvent) {
            VariableChangedEvent varEvent = (VariableChangedEvent) event;
            Tuple<String, String> pair = varEvent.getVarValue();
            if (pair == null || pair.getFirst() == null || pair.getFirst().isBlank()) return;
            JSONObject message = new JSONObject();
            message.put("type", "event");
            message.put("ts", System.currentTimeMillis());
            message.put("channel", "vars");
            message.put("event", "vars.updated");
            JSONObject payload = new JSONObject();
            payload.put("projectId", projectId);
            payload.put("name", pair.getFirst());
            payload.put("value", pair.getSecond() != null ? pair.getSecond() : "");
            message.put("payload", payload);
            broadcastToProjectOrAll(projectId, message.toString());
            return;
        }

        if (isRuntimeVisualizationEvent(event) && shouldDropRuntimeVisualizationEvent(projectId, event)) return;

        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        JSONObject payload = new JSONObject();
        payload.put("projectId", projectId);

        if (event instanceof NodeStartedEvent) {
            BasicNode node = ((NodeStartedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
                payload.put("ancestorIds", buildAncestorIds(node.getParentNode()));
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
                payload.put("ancestorIds", buildAncestorIds(node.getParentNode()));
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
            if (edge.getSourceNode() != null && edge.getSourceNode().getParentNode() != null) {
                payload.put("sourceParentId", edge.getSourceNode().getParentNode().getId());
                payload.put("sourceAncestorIds", buildAncestorIds(edge.getSourceNode().getParentNode()));
            }
            if (edge.getTargetNode() != null && edge.getTargetNode().getParentNode() != null) {
                payload.put("targetParentId", edge.getTargetNode().getParentNode().getId());
            }
            payload.put("edgeType", getEdgeTypeLowercase(edge));
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
            if (edge.getSourceNode() != null && edge.getSourceNode().getParentNode() != null) {
                payload.put("sourceParentId", edge.getSourceNode().getParentNode().getId());
                payload.put("sourceAncestorIds", buildAncestorIds(edge.getSourceNode().getParentNode()));
            }
            if (edge.getTargetNode() != null && edge.getTargetNode().getParentNode() != null) {
                payload.put("targetParentId", edge.getTargetNode().getParentNode().getId());
            }
            payload.put("edgeType", "timeout");
            payload.put("timeoutMs", te.getTimeoutMs());
            payload.put("startedAt", te.getStartedAt());
            payload.put("elapsedMs", 0L);
            payload.put("ratio", 0.0);
            message.put("channel", "runtime");
            message.put("event", "runtime.timeoutProgress");

        } else if (event instanceof SceneExecutedEvent) {
            SceneExecutedEvent sceneEvent = (SceneExecutedEvent) event;
            SceneObject scene = sceneEvent.getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            if (!sceneEvent.getNodeId().isBlank()) payload.put("nodeId", sceneEvent.getNodeId());
            if (!sceneEvent.getParentId().isBlank()) payload.put("parentId", sceneEvent.getParentId());
            message.put("channel", "runtime");
            message.put("event", "runtime.scene.playing");

        } else if (event instanceof SceneDoneEvent) {
            SceneDoneEvent sceneEvent = (SceneDoneEvent) event;
            SceneObject scene = sceneEvent.getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            if (!sceneEvent.getNodeId().isBlank()) payload.put("nodeId", sceneEvent.getNodeId());
            if (!sceneEvent.getParentId().isBlank()) payload.put("parentId", sceneEvent.getParentId());
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
            payload.put("status", "stopped");
            message.put("channel", "runtime");
            message.put("event", "runtime.state");
            runtimeVizRateLimiters.remove(projectId);
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null) ref.runtimeState = "stopped";

        } else {
            return;
        }

        message.put("payload", payload);
        broadcastToProjectOrAll(projectId, message.toString());
    }

    // -------------------------------------------------------------------------

    public String getLocalUrl() {
        return "http://127.0.0.1:" + mPort;
    }

    private void registerRoutes() {
        // Common endpoints (available in both modes)
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/transport", this::handleTransport);
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
        mApp.get(API_PREFIX + "/projects/{pid}/plugins/dashboard", this::handlePluginDashboard);
        mApp.post(API_PREFIX + "/projects/{pid}/plugins/{name}/health", this::handlePluginHealth);
        mApp.post(API_PREFIX + "/projects/{pid}/plugins/{name}/params", this::handlePluginParams);
        mApp.get(API_PREFIX + "/projects/{pid}/script", this::handleScript);
        mApp.get(API_PREFIX + "/projects/{pid}/script/scenes", this::handleScriptScenes);
        mApp.get(API_PREFIX + "/projects/{pid}/script/elements", this::handleScriptElements);
        mApp.get(API_PREFIX + "/projects/{pid}/semantic", this::handleSemanticGet);
        mApp.put(API_PREFIX + "/projects/{pid}/semantic", this::handleSemanticPut);
        mApp.get(API_PREFIX + "/projects/{pid}/ui-prefs", this::handleUiPrefsGet);
        mApp.put(API_PREFIX + "/projects/{pid}/ui-prefs", this::handleUiPrefsPut);
        mApp.get(API_PREFIX + "/projects/{pid}/variables", this::handleVariables);
        mApp.get(API_PREFIX + "/projects/{pid}/screens", this::handleScreensGet);
        mApp.put(API_PREFIX + "/projects/{pid}/screens", this::handleScreensPut);
        mApp.get(API_PREFIX + "/projects/{pid}/character-config", this::handleCharacterConfigGet);
        mApp.put(API_PREFIX + "/projects/{pid}/character-config", this::handleCharacterConfigPut);
        mApp.get(API_PREFIX + "/projects/{pid}/assets/{filename}", this::handleAssetsGet);
        mApp.post(API_PREFIX + "/projects/{pid}/semantic/syntax", this::handleSemanticSyntaxAnalyze);
        mApp.post(API_PREFIX + "/projects/{pid}/semantic/analyze", this::handleSemanticAnalyze);
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/export", this::handleProjectExport);
        mApp.get(API_PREFIX + "/projects/{pid}/runtime", this::handleRuntime);
        mApp.get(API_PREFIX + "/projects/{pid}/subscribers", this::handleProjectSubscribers);
        mApp.get(API_PREFIX + "/projects/{pid}/operations", this::handleProjectOperations);
        mApp.get(API_PREFIX + "/projects/{pid}/presence", this::handleProjectPresence);
        mApp.get(API_PREFIX + "/projects/{pid}/history/commands", this::handleCommandLog);
        mApp.post(API_PREFIX + "/projects/{pid}/sceneflow/navigate", this::handleSceneflowNavigate);

        // Sessions endpoints (Phase E: session-oriented view on active projects)
        mApp.get(API_PREFIX + "/sessions", this::handleSessions);
        mApp.get(API_PREFIX + "/sessions/{id}/presence", this::handleSessionPresence);
        mApp.get(API_PREFIX + "/sessions/{id}/operations", this::handleSessionOperations);

        // Admin token management (Phase F: named-user provisioning)
        mApp.get(API_PREFIX + "/admin/tokens", this::handleAdminListTokens);
        mApp.post(API_PREFIX + "/admin/tokens", this::handleAdminProvisionToken);

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
            if (isRuntimeRestMutationsEnabled()) {
                mApp.post(API_PREFIX + "/runtime/load", this::handleRuntimeLoad);
                mApp.post(API_PREFIX + "/runtime/start", this::handleRuntimeStart);
                mApp.post(API_PREFIX + "/runtime/pause", this::handleRuntimePause);
                mApp.post(API_PREFIX + "/runtime/resume", this::handleRuntimeResume);
                mApp.post(API_PREFIX + "/runtime/stop", this::handleRuntimeStopRest);
                mApp.post(API_PREFIX + "/runtime/unload", this::handleRuntimeUnload);
            } else {
                mApp.post(API_PREFIX + "/runtime/load", this::handleRuntimeMutationDeprecatedUnavailable);
                mApp.post(API_PREFIX + "/runtime/start", this::handleRuntimeMutationDeprecatedUnavailable);
                mApp.post(API_PREFIX + "/runtime/pause", this::handleRuntimeMutationDeprecatedUnavailable);
                mApp.post(API_PREFIX + "/runtime/resume", this::handleRuntimeMutationDeprecatedUnavailable);
                mApp.post(API_PREFIX + "/runtime/stop", this::handleRuntimeMutationDeprecatedUnavailable);
                mApp.post(API_PREFIX + "/runtime/unload", this::handleRuntimeMutationDeprecatedUnavailable);
            }
            mApp.get(API_PREFIX + "/runtime/status", this::handleRuntimeStatus);
            mApp.get(API_PREFIX + "/runtime/variables", this::handleRuntimeVariables);
            mApp.get(API_PREFIX + "/runtime/sceneflow", this::handleRuntimeSceneflowLegacy);
        }

        // WebSocket endpoint: accepts requests and replies with JSON. Broadcasts snapshots/runtime state after mutations.
        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                ctx.session.setIdleTimeout(java.time.Duration.ofMinutes(10));
                sLogger.message("WS client connected: " + ctx.sessionId());
                wsSessions.add(ctx);
            });
            ws.onClose(ctx -> {
                wsSessions.remove(ctx);
                cleanupWsSubscription(ctx);
            });
            ws.onError(ctx -> {
                sLogger.warning("WS client error: " + ctx.sessionId());
                wsSessions.remove(ctx);
                cleanupWsSubscription(ctx);
            });
            ws.onMessage(ctx -> {
                // Intercept presence/session commands before regular dispatch
                String raw = ctx.message();
                if (raw.contains("\"Session.Subscribe\"")) {
                    handleSessionSubscribe(ctx, raw);
                } else if (raw.contains("\"Presence.Update\"")) {
                    handlePresenceUpdate(ctx, raw);
                } else {
                    handleWsMessage(ctx, raw, ctx::send, msg -> broadcast(ctx, msg));
                }
            });
        });
    }

    // ========== Runtime-Only REST Endpoints ==========

    private void handleRuntimeLoad(Context ctx) {
        JSONObject body;
        try {
            body = new JSONObject(ctx.body());
        } catch (Exception exc) {
            writeRuntimeMutationResponse(ctx, errorResponse("BAD_REQUEST", "Invalid JSON body"));
            return;
        }
        dispatchRuntimeMutation(ctx, "Runtime.Load", body);
    }

    private void handleRuntimeStart(Context ctx) {
        dispatchRuntimeMutation(ctx, "Runtime.Start", runtimeCommandParamsWithProjectId());
    }

    private void handleRuntimePause(Context ctx) {
        dispatchRuntimeMutation(ctx, "Runtime.Pause", runtimeCommandParamsWithProjectId());
    }

    private void handleRuntimeResume(Context ctx) {
        dispatchRuntimeMutation(ctx, "Runtime.Resume", runtimeCommandParamsWithProjectId());
    }

    private void handleRuntimeStopRest(Context ctx) {
        dispatchRuntimeMutation(ctx, "Runtime.Stop", runtimeCommandParamsWithProjectId());
    }

    private void handleRuntimeUnload(Context ctx) {
        dispatchRuntimeMutation(ctx, "Runtime.Unload", runtimeCommandParamsWithProjectId());
    }

    private void handleRuntimeMutationDeprecatedUnavailable(Context ctx) {
        markRuntimeMutationDeprecated(ctx);
        JSONObject response = errorResponse("ENDPOINT_DEPRECATED",
                "Runtime REST mutation endpoints are disabled. Use WebSocket /ws commands.");
        response.put("status", "error");
        response.put("preferredTransport", "ws");
        ctx.status(410);
        writeJson(ctx, response);
    }

    private void dispatchRuntimeMutation(Context ctx, String method, JSONObject params) {
        markRuntimeMutationDeprecated(ctx);
        JSONObject result = runtimeGateway.dispatch(method, params, this::broadcastToAll);
        writeRuntimeMutationResponse(ctx, result);
    }

    private JSONObject runtimeCommandParamsWithProjectId() {
        JSONObject params = new JSONObject();
        String pid = firstLoadedProjectId();
        if (pid != null && !pid.isBlank()) {
            params.put("projectId", pid);
        }
        return params;
    }

    private void markRuntimeMutationDeprecated(Context ctx) {
        ctx.header("Warning", "299 - \"Deprecated runtime REST mutation endpoint; use WebSocket /ws commands\"");
        ctx.header("Deprecation", "true");
        ctx.header("X-VSM-Preferred-Transport", "ws");
    }

    private void writeRuntimeMutationResponse(Context ctx, JSONObject payload) {
        JSONObject response = normalizeRuntimeMutationPayload(payload);
        String status = response.optString("status", "ok");
        if ("error".equalsIgnoreCase(status)) {
            String code = response.optString("error", "");
            if ("BAD_REQUEST".equals(code)) {
                ctx.status(400);
            } else if ("PROJECT_NOT_FOUND".equals(code)) {
                ctx.status(404);
            } else if ("UNSUPPORTED_FEATURE".equals(code)) {
                ctx.status(501);
            } else {
                ctx.status(500);
            }
        }
        writeJson(ctx, response);
    }

    private JSONObject normalizeRuntimeMutationPayload(JSONObject payload) {
        if (payload == null) {
            JSONObject err = errorResponse("INTERNAL_ERROR", "No response from runtime command");
            err.put("status", "error");
            return err;
        }
        JSONObject normalized = new JSONObject(payload.toString());
        if (normalized.has("error") && normalized.opt("error") instanceof JSONObject) {
            JSONObject errorObj = normalized.optJSONObject("error");
            String code = errorObj != null ? errorObj.optString("code", "ERROR") : "ERROR";
            String message = errorObj != null ? errorObj.optString("message", "") : "";
            normalized.remove("error");
            normalized.put("status", "error");
            normalized.put("error", code);
            if (!message.isBlank()) {
                normalized.put("message", message);
            }
            addRuntimeCapabilities(normalized);
            return normalized;
        }
        if (normalized.has("error")) {
            normalized.put("status", "error");
            addRuntimeCapabilities(normalized);
            return normalized;
        }
        if (!normalized.has("status")) {
            normalized.put("status", "ok");
        }
        addRuntimeCapabilities(normalized);
        return normalized;
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
        addRuntimeCapabilities(response);
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
        info.put("allowExternal", mAllowExternal);
        // Expose the server's real hostname and LAN IP so clients can build
        // share links that work from other machines (not just localhost).
        try {
            java.net.InetAddress localHost = java.net.InetAddress.getLocalHost();
            info.put("hostname", localHost.getHostName());
            info.put("hostAddress", localHost.getHostAddress());
        } catch (Exception ignored) { /* leave absent */ }
        // Prefer a non-loopback LAN address when available
        try {
            java.util.Enumeration<java.net.NetworkInterface> ifaces =
                    java.net.NetworkInterface.getNetworkInterfaces();
            outer:
            while (ifaces.hasMoreElements()) {
                java.net.NetworkInterface iface = ifaces.nextElement();
                if (!iface.isUp() || iface.isLoopback() || iface.isVirtual()) continue;
                java.util.Enumeration<java.net.InetAddress> addrs = iface.getInetAddresses();
                while (addrs.hasMoreElements()) {
                    java.net.InetAddress addr = addrs.nextElement();
                    if (addr instanceof java.net.Inet4Address && !addr.isLoopbackAddress()) {
                        info.put("lanAddress", addr.getHostAddress());
                        info.put("lanHostname", addr.getCanonicalHostName());
                        break outer;
                    }
                }
            }
        } catch (Exception ignored) { /* leave absent */ }
        addRuntimeCapabilities(info);
        writeJson(ctx, info);
    }

    private void handleTransport(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("preferred", "ws");
        response.put("commandTransport", "ws");
        response.put("eventTransport", "ws");
        response.put("bootstrapTransport", "http");
        response.put("runtimeRestMutationsEnabled", isRuntimeRestMutationsEnabled());
        response.put("wsPath", "/ws");
        response.put("apiPrefix", API_PREFIX);

        JSONArray bootstrapEndpoints = new JSONArray();
        bootstrapEndpoints.put(API_PREFIX + "/info");
        bootstrapEndpoints.put(API_PREFIX + "/token");
        bootstrapEndpoints.put(API_PREFIX + "/projects");
        bootstrapEndpoints.put(API_PREFIX + "/projects/{pid}/sceneflow");
        bootstrapEndpoints.put(API_PREFIX + "/runtime/status");
        response.put("bootstrapEndpoints", bootstrapEndpoints);

        writeJson(ctx, response);
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
                entry.put("androidProject", stats.androidProject);
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
        private boolean androidProject;
        private List<RecentSceneLanguageInfo> sceneLanguages = new ArrayList<>();
        private List<RecentPluginInfo> plugins = new ArrayList<>();

        public JSONObject toJson() {
            JSONObject json = new JSONObject();
            json.put("superNodes", superNodes);
            json.put("nodes", nodes);
            json.put("commands", commands);
            json.put("scenes", scenes);
            json.put("androidProject", androidProject);
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
            if (config != null) {
                stats.androidProject = config.isAndroidProject();
            }
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
            if (RESERVED_META_VARIABLES.contains(var)) {
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
                // Left side is a field/parameter name, not a variable reference — skip it
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
                            // Merge optional tutorial.json metadata (name, description, level, duration, tags)
                            Path meta = path.resolve("tutorial.json");
                            if (Files.exists(meta)) {
                                try {
                                    String raw = Files.readString(meta);
                                    JSONObject m = new JSONObject(raw);
                                    for (String key : m.keySet()) {
                                        entry.put(key, m.get(key));
                                    }
                                } catch (Exception ignored) {}
                            }
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
            boolean androidProject = false;
            if (ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
                androidProject = ref.runtimeProject.getProjectConfig().isAndroidProject();
            }
            entry.put("androidProject", androidProject);
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
        String normalizedPath = normalizeProjectPath(path);
        JSONObject response = new JSONObject();
        if (path.isBlank()) {
            ctx.status(400).result("Missing path");
            return;
        }
        String projectId = ensureProject(normalizedPath, fileName(normalizedPath), false);
        if (projectId == null || projectId.isBlank()) {
            ctx.status(400).result("Failed to open project: " + normalizedPath);
            return;
        }
        addRecent(normalizedPath, fileName(normalizedPath));
        response.put("projectId", projectId);
        response.put("path", normalizedPath);
        response.put("name", fileName(normalizedPath));
        writeJson(ctx, response);
    }

    private void handleProjectCreate(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String name = body.optString("name", "Untitled");
        String baseDir = body.optString("baseDir", "");
        String projectPath = resolveProjectDirectory(baseDir, name);
        String projectId = ensureProject(projectPath, name, true);
        JSONObject response = new JSONObject();
        response.put("projectId", projectId);
        response.put("name", name);
        response.put("path", projectPath);
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
        String requestedName = body.optString("name", "").trim();
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
        String projectName = requestedName;
        if (projectName == null || projectName.isBlank()) {
            projectName = ref.runtimeProject.getProjectName();
        }
        if (projectName == null || projectName.isBlank()) {
            projectName = ref.name;
        }
        String projectPath = resolveProjectDirectory(path, projectName);
        String normalizedCurrentPath = normalizeProjectPath(ref.path);
        String normalizedTargetPath = normalizeProjectPath(projectPath);
        if (!normalizedTargetPath.isBlank()) {
            Path targetDir = Paths.get(normalizedTargetPath);
            boolean sameTarget = !normalizedCurrentPath.isBlank()
                    && normalizedCurrentPath.equals(normalizedTargetPath);
            if (Files.exists(targetDir) && !sameTarget) {
                ctx.status(409).result("A project named \"" + projectName + "\" already exists in the selected directory.");
                return;
            }
        }
        ref.path = projectPath;
        ref.name = fileName(projectPath);
        ref.runtimeProject.setProjectPath(projectPath);
        ref.runtimeProject.setProjectName(ref.name);
        boolean ok = ref.runtimeProject.write(new java.io.File(projectPath));
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
        addRecent(projectPath, ref.name);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("path", projectPath);
        response.put("name", ref.name);
        writeJson(ctx, response);
    }

    private void handleProjectExport(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404).result("Project not found");
            return;
        }
        SceneFlow sf = ref.runtimeProject.getSceneFlow();
        String xml = sf != null ? serializeSceneFlowXml(sf) : "";
        String filename = (ref.name != null && !ref.name.isBlank() ? ref.name : "sceneflow")
                .replaceAll("[^\\w\\-.]", "_") + ".xml";
        ctx.header("Content-Disposition", "attachment; filename=\"" + filename + "\"");
        ctx.contentType("application/xml");
        ctx.result(xml);
    }

    private void handleProjectClose(Context ctx) {
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            ProjectRef ref = projectStore.get(pid);
            if (ref != null && ref.runtimeProject != null) {
                mEdgeLayout.clearDockPointsForProject(ref.runtimeProject.getSceneFlow());
            }
            unregisterProjectDispatcher(ref);
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

    private void handlePluginDashboard(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONArray plugins = new JSONArray();
        if (ref != null && ref.runtimeProject != null) {
            ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
            if (cfg != null) {
                Set<String> seen = new HashSet<>();
                boolean runtimeActive = ref.runtimeProject.isRunning()
                        || ref.runtimeProject.isPaused();
                for (PluginConfig plugin : cfg.getPluginConfigList()) {
                    String name = plugin.getPluginName() == null ? "" : plugin.getPluginName();
                    String key = name.trim().toLowerCase();
                    if (!key.isEmpty() && !seen.add(key)) continue;
                    JSONObject entry = new JSONObject();
                    entry.put("instanceName", name);
                    entry.put("className", plugin.getClassName() == null ? "" : plugin.getClassName());
                    entry.put("load", plugin.isMarkedtoLoad());
                    entry.put("type", plugin.getPluginType() == null ? "" : plugin.getPluginType());
                    entry.put("features", configFeaturesToJson(plugin.getEntryList()));
                    // Compute meta first so we can use serviceModel to set status correctly.
                    String className = plugin.getClassName();
                    ExportablePropertyEntry propEntry = className != null ? resolveExportablePropertyEntry(className) : null;
                    JSONObject meta;
                    String serviceModel;
                    if (propEntry != null) {
                        meta = propEntry.toInterfaceJson(className);
                        // Explicit declaration in plugin-properties.json wins over heuristic.
                        String declared = meta.optString("serviceModel", null);
                        serviceModel = (declared != null && !declared.isBlank())
                                ? declared : deriveServiceModel(meta);
                        meta.put("serviceModel", serviceModel);
                    } else {
                        meta = new JSONObject();
                        serviceModel = deriveServiceModelFromFeatures(configFeaturesToJson(plugin.getEntryList()));
                        meta.put("serviceModel", serviceModel);
                    }
                    entry.put("meta", meta);
                    // Runtime status:
                    //   "ok"       – self-contained plugin loaded (loaded = working by definition)
                    //   "loaded"   – service plugin loaded in JVM but remote service not yet verified
                    //   "not_loaded" – plugin not present in active runtime (or runtime not started)
                    if (runtimeActive && ref.runtimeProject.getPlugin(name) != null) {
                        entry.put("status", "service".equals(serviceModel) ? "loaded" : "ok");
                    } else {
                        entry.put("status", "not_loaded");
                    }
                    plugins.put(entry);
                }
            }
        }
        JSONObject response = new JSONObject();
        response.put("plugins", plugins);
        writeJson(ctx, response);
    }

    /** Heuristic: classify a plugin as "service" or "self-contained" based on its config schema. */
    private String deriveServiceModel(JSONObject interfaceJson) {
        JSONArray config = interfaceJson.optJSONArray("config");
        if (config != null) {
            for (int i = 0; i < config.length(); i++) {
                JSONObject item = config.optJSONObject(i);
                if (item == null) continue;
                String k = item.optString("key", "").toLowerCase();
                if (k.contains("url") || k.contains("host") || k.contains("port") || k.startsWith("ws")) {
                    return "service";
                }
            }
        }
        // Also check tags (nested under "plugin" in toInterfaceJson output)
        JSONObject pluginMeta = interfaceJson.optJSONObject("plugin");
        JSONArray tags = pluginMeta != null ? pluginMeta.optJSONArray("tags") : null;
        if (tags != null) {
            for (int i = 0; i < tags.length(); i++) {
                String tag = tags.optString(i, "").toLowerCase();
                if (tag.equals("asr") || tag.equals("tts") || tag.equals("llm") || tag.equals("dialog")
                        || tag.equals("logging") || tag.equals("web") || tag.equals("socket")) {
                    return "service";
                }
            }
        }
        return "self-contained";
    }

    /** Fallback heuristic when no static metadata is available: check feature keys. */
    private String deriveServiceModelFromFeatures(JSONArray features) {
        if (features == null) return "self-contained";
        for (int i = 0; i < features.length(); i++) {
            JSONObject f = features.optJSONObject(i);
            if (f == null) continue;
            String k = f.optString("key", "").toLowerCase();
            if (k.contains("url") || k.contains("host") || k.contains("port") || k.startsWith("ws")) {
                return "service";
            }
        }
        return "self-contained";
    }

    private void handlePluginHealth(Context ctx) {
        String pid = ctx.pathParam("pid");
        String instanceName = ctx.pathParam("name");
        ProjectRef ref = projectStore.get(pid);
        JSONObject health = new JSONObject();
        health.put("checkedAt", System.currentTimeMillis());
        if (ref == null || ref.runtimeProject == null) {
            health.put("status", "not_loaded");
            health.put("message", "Project not found");
        } else {
            RunTimePlugin plugin = ref.runtimeProject.getPlugin(instanceName);
            if (plugin instanceof de.dfki.vsm.runtime.plugin.PluginHealthCheckable checkable) {
                de.dfki.vsm.runtime.plugin.PluginHealthCheckable.HealthStatus hs = checkable.healthCheck();
                health.put("status", hs.healthy() ? "ok" : "error");
                health.put("message", hs.message() == null ? "" : hs.message());
            } else if (plugin != null) {
                // Plugin is loaded. For service plugins also probe TCP so we can report latency;
                // for self-contained plugins presence alone is sufficient.
                JSONObject tcpResult = tcpHealthCheck(pid, instanceName, ref);
                if (tcpResult.has("latency") || tcpResult.has("endpoint")) {
                    // TCP probe succeeded or found host/port — use its richer result
                    health = tcpResult;
                    if (!"error".equals(tcpResult.optString("status"))) {
                        health.put("status", "ok");
                    }
                } else {
                    health.put("status", "ok");
                    health.put("message", "Plugin loaded");
                }
            } else {
                // Plugin not loaded; try TCP reachability for service plugins
                health = tcpHealthCheck(pid, instanceName, ref);
            }
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("health", health);
        writeJson(ctx, response);
    }

    private JSONObject tcpHealthCheck(String pid, String instanceName, ProjectRef ref) {
        JSONObject health = new JSONObject();
        health.put("checkedAt", System.currentTimeMillis());
        PluginConfig cfg = ref.runtimeProject.getProjectConfig().getPluginConfig(instanceName);
        if (cfg == null) {
            health.put("status", "not_loaded");
            health.put("message", "Plugin not configured");
            return health;
        }
        // Find host/port or URL in features
        String host = null;
        int port = -1;
        for (de.dfki.vsm.model.config.ConfigFeature f : cfg.getEntryList()) {
            String k = f.getKey() == null ? "" : f.getKey().toLowerCase();
            String v = f.getValue() == null ? "" : f.getValue().trim();
            if (v.isEmpty()) continue;
            if (k.contains("url") || k.startsWith("ws")) {
                try {
                    java.net.URI uri = java.net.URI.create(v);
                    host = uri.getHost();
                    port = uri.getPort();
                    if (port < 0) port = uri.getScheme().startsWith("https") || uri.getScheme().equals("wss") ? 443 : 80;
                    break;
                } catch (Exception ignored) {}
            } else if (k.contains("host")) {
                host = v;
            } else if (k.contains("port") && host != null) {
                try { port = Integer.parseInt(v); } catch (NumberFormatException ignored) {}
            }
        }
        if (host != null && port > 0) {
            health.put("endpoint", host + ":" + port);
            long start = System.currentTimeMillis();
            try (java.net.Socket s = new java.net.Socket()) {
                s.connect(new java.net.InetSocketAddress(host, port), 1500);
                health.put("status", "ok");
                health.put("message", "Reachable at " + host + ":" + port);
                health.put("latency", System.currentTimeMillis() - start);
            } catch (Exception e) {
                health.put("status", "error");
                health.put("message", "Cannot reach " + host + ":" + port);
            }
        } else {
            health.put("status", "not_loaded");
            health.put("message", "Runtime not started");
        }
        return health;
    }

    /**
     * WS command: ProjectConfig.Plugin.GetUpdate
     * <p>
     * Params: { projectId, className, [knownVarNames: [string]] }
     * <p>
     * Returns: { specVersion, newVars: [{name, type}], upToDate: bool }
     * <p>
     * Computes the set of SceneFlow variables the current plugin spec would create, then
     * subtracts the variables already present in the project's SceneFlow (or the caller's
     * knownVarNames list if provided). The result drives the PluginDashboard "Update
     * available" badge and the apply-update workflow.
     */
    private JSONObject handlePluginGetUpdate(final JSONObject params) {
        String projectId = params.optString("projectId", "").trim();
        String className = params.optString("className", "").trim();
        if (projectId.isEmpty() || className.isEmpty()) {
            return errorResponse("BAD_REQUEST", "Missing projectId or className");
        }

        // Compute what variables the current spec would create (dry-run, no side effects)
        JSONObject createResult = pluginCreateCommandService.dispatch(
                new JSONObject()
                        .put("className", className)
                        .put("name", "_update_check")
                        .put("type", "device"),
                pluginCreateCommandContext);
        if (!"ok".equals(createResult.optString("status"))) {
            return createResult;
        }
        JSONArray specVars = createResult.optJSONArray("sceneflowVars");
        if (specVars == null) specVars = new JSONArray();

        // Build set of existing variable names from sceneflow OR caller-supplied list
        Set<String> existing = new HashSet<>();
        JSONArray knownVarNames = params.optJSONArray("knownVarNames");
        if (knownVarNames != null) {
            for (int i = 0; i < knownVarNames.length(); i++) {
                String n = knownVarNames.optString(i, "").trim();
                if (!n.isEmpty()) existing.add(n);
            }
        } else {
            ProjectRef ref = projectStore.get(projectId);
            if (ref != null && ref.runtimeProject != null) {
                SceneFlow sf = ref.runtimeProject.getSceneFlow();
                if (sf != null) {
                    for (VariableDefinition def : sf.getVarDefList()) {
                        if (def.getName() != null) existing.add(def.getName());
                    }
                }
            }
        }

        // Find spec vars missing from the project
        JSONArray newVars = new JSONArray();
        for (int i = 0; i < specVars.length(); i++) {
            JSONObject v = specVars.optJSONObject(i);
            if (v == null) continue;
            String varName = v.optString("name", "").trim();
            if (!varName.isEmpty() && !existing.contains(varName)) {
                newVars.put(v);
            }
        }

        ExportablePropertyEntry entry = resolveExportablePropertyEntry(className);
        String specVersion = entry != null ? entry.specVersion : "";

        JSONObject result = new JSONObject();
        result.put("specVersion", specVersion);
        result.put("newVars", newVars);
        result.put("upToDate", newVars.length() == 0);
        result.put("status", "ok");
        return result;
    }

    private void handlePluginParams(Context ctx) {
        if (mMode == ServerMode.RUNTIME_ONLY) {
            writeJson(ctx, errorResponse("FORBIDDEN", "Editing not allowed in runtime-only mode"));
            return;
        }
        String pid = ctx.pathParam("pid");
        String instanceName = ctx.pathParam("name");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            writeJson(ctx, errorResponse("NOT_FOUND", "Project not found"));
            return;
        }
        JSONObject body;
        try {
            body = new JSONObject(ctx.body());
        } catch (Exception e) {
            writeJson(ctx, errorResponse("BAD_REQUEST", "Invalid JSON body"));
            return;
        }
        JSONArray newFeatures = body.optJSONArray("features");
        if (newFeatures == null) {
            writeJson(ctx, errorResponse("BAD_REQUEST", "Missing features array"));
            return;
        }
        ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
        PluginConfig target = cfg.getPluginConfig(instanceName);
        if (target == null) {
            writeJson(ctx, errorResponse("NOT_FOUND", "Plugin not found: " + instanceName));
            return;
        }
        target.getEntryList().clear();
        for (int i = 0; i < newFeatures.length(); i++) {
            JSONObject f = newFeatures.optJSONObject(i);
            if (f == null) continue;
            String k = f.optString("key", "").trim();
            String v = f.optString("value", "");
            if (!k.isEmpty()) {
                target.getEntryList().add(new ConfigFeature("Feature", k, v));
            }
        }
        ref.dirty = true;
        // Broadcast updated config so all clients refresh
        JSONObject evt = new JSONObject();
        evt.put("type", "event");
        evt.put("event", "project.config");
        evt.put("projectId", pid);
        evt.put("config", projectConfigToJson(cfg, ref.path));
        broadcastToAll(evt.toString());
        writeJson(ctx, new JSONObject().put("status", "ok"));
    }

    private JSONObject projectConfigToJson(ProjectConfig cfg, String path) {
        JSONObject cfgJson = new JSONObject();
        cfgJson.put("name", cfg.getProjectName());
        cfgJson.put("path", path == null ? "" : path);
        cfgJson.put("androidProject", cfg.isAndroidProject());
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
        JSONObject llmSelectionsJson = new JSONObject();
        de.dfki.vsm.model.config.ConfigElement llmSelections = cfg.getLLMSelections();
        String generateSelection = llmSelections.getProperty("generate");
        String semanticSelection = llmSelections.getProperty("semantic");
        llmSelectionsJson.put("generate", generateSelection != null ? generateSelection : "");
        llmSelectionsJson.put("semantic", semanticSelection != null ? semanticSelection : "");
        cfgJson.put("llmSelections", llmSelectionsJson);
        JSONObject semanticServicesJson = new JSONObject();
        de.dfki.vsm.model.config.ConfigElement semanticServices = cfg.getSemanticServices();
        String basicProvider = semanticServices.getProperty("basicProvider");
        String udUrl = semanticServices.getProperty("udUrl");
        String udTimeoutMs = semanticServices.getProperty("udTimeoutMs");
        String analyzeSyntax = semanticServices.getProperty("analyzeSyntax");
        String analyzeSvo = semanticServices.getProperty("analyzeSvo");
        String analyzeDaTr = semanticServices.getProperty("analyzeDaTr");
        String daTrLlm = semanticServices.getProperty("daTrLlm");
        String semanticSystemPrompt = semanticServices.getProperty("systemPrompt");
        String semanticPromptTemplate = semanticServices.getProperty("promptTemplate");
        String runtimeVizRate = semanticServices.getProperty("runtimeVizRate");
        String runtimeVizBurst = semanticServices.getProperty("runtimeVizBurst");
        semanticServicesJson.put("basicProvider", basicProvider != null ? basicProvider : "");
        semanticServicesJson.put("udUrl", udUrl != null ? udUrl : "");
        semanticServicesJson.put("udTimeoutMs", udTimeoutMs != null ? udTimeoutMs : "");
        semanticServicesJson.put("analyzeSyntax", analyzeSyntax != null ? analyzeSyntax : "");
        semanticServicesJson.put("analyzeSvo", analyzeSvo != null ? analyzeSvo : "");
        semanticServicesJson.put("analyzeDaTr", analyzeDaTr != null ? analyzeDaTr : "");
        semanticServicesJson.put("daTrLlm", daTrLlm != null ? daTrLlm : "");
        semanticServicesJson.put("systemPrompt", semanticSystemPrompt != null ? semanticSystemPrompt : "");
        semanticServicesJson.put("promptTemplate", semanticPromptTemplate != null ? semanticPromptTemplate : "");
        semanticServicesJson.put("runtimeVizRate", runtimeVizRate != null ? runtimeVizRate : "");
        semanticServicesJson.put("runtimeVizBurst", runtimeVizBurst != null ? runtimeVizBurst : "");
        cfgJson.put("semanticServices", semanticServicesJson);
        cfgJson.put("sceneTitleConcepts", configConceptsToJson(cfg.getSceneTitleConcepts()));
        sLogger.message("[PROJECT-CONFIG] Serialized plugins=" + pluginsJson.length()
                + " agents=" + agentsJson.length() + " llms=" + llmsJson.length());
        return cfgJson;
    }

    private void applyProjectConfigFromJson(ProjectRef ref, ProjectConfig cfg, JSONObject configJson) {
        String name = configJson.optString("name", cfg.getProjectName());
        boolean androidProject = configJson.has("androidProject")
                ? configJson.optBoolean("androidProject", cfg.isAndroidProject())
                : cfg.isAndroidProject();
        if (ref.runtimeProject != null) {
            ref.runtimeProject.setProjectName(name);
        } else {
            cfg.setProjectName(name);
        }
        cfg.setAndroidProject(androidProject);

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
        JSONObject llmSelectionsJson = configJson.optJSONObject("llmSelections");
        if (llmSelectionsJson != null) {
            de.dfki.vsm.model.config.ConfigElement selections = cfg.getLLMSelections();
            selections.getEntryList().clear();
            String generateSelection = llmSelectionsJson.optString("generate", "");
            String semanticSelection = llmSelectionsJson.optString("semantic", "");
            if (!generateSelection.isEmpty()) {
                selections.addProperty("generate", generateSelection);
            }
            if (!semanticSelection.isEmpty()) {
                selections.addProperty("semantic", semanticSelection);
            }
        }
        JSONObject semanticServicesJson = configJson.optJSONObject("semanticServices");
        if (semanticServicesJson != null) {
            de.dfki.vsm.model.config.ConfigElement services = cfg.getSemanticServices();
            services.getEntryList().clear();
            String basicProvider = semanticServicesJson.optString("basicProvider", "").trim();
            String udUrl = semanticServicesJson.optString("udUrl", "").trim();
            String udTimeoutMs = semanticServicesJson.optString("udTimeoutMs", "").trim();
            String analyzeSyntax = semanticServicesJson.optString("analyzeSyntax", "").trim();
            String analyzeSvo = semanticServicesJson.optString("analyzeSvo", "").trim();
            String analyzeDaTr = semanticServicesJson.optString("analyzeDaTr", "").trim();
            String daTrLlm = semanticServicesJson.optString("daTrLlm", "").trim();
            String semanticSystemPrompt = semanticServicesJson.optString("systemPrompt", "");
            String semanticPromptTemplate = semanticServicesJson.optString("promptTemplate", "");
            String runtimeVizRate = semanticServicesJson.optString("runtimeVizRate", "").trim();
            String runtimeVizBurst = semanticServicesJson.optString("runtimeVizBurst", "").trim();
            if (!basicProvider.isEmpty()) {
                services.addProperty("basicProvider", basicProvider);
            }
            if (!udUrl.isEmpty()) {
                services.addProperty("udUrl", udUrl);
            }
            if (!udTimeoutMs.isEmpty()) {
                services.addProperty("udTimeoutMs", udTimeoutMs);
            }
            if (!analyzeSyntax.isEmpty()) {
                services.addProperty("analyzeSyntax", analyzeSyntax);
            } else if (!analyzeSvo.isEmpty()) {
                services.addProperty("analyzeSyntax", analyzeSvo);
            }
            // Keep legacy key for compatibility with older clients.
            if (!analyzeSvo.isEmpty()) {
                services.addProperty("analyzeSvo", analyzeSvo);
            } else if (!analyzeSyntax.isEmpty()) {
                services.addProperty("analyzeSvo", analyzeSyntax);
            }
            if (!analyzeDaTr.isEmpty()) {
                services.addProperty("analyzeDaTr", analyzeDaTr);
            }
            if (!daTrLlm.isEmpty()) {
                services.addProperty("daTrLlm", daTrLlm);
            }
            if (!semanticSystemPrompt.isEmpty()) {
                services.addProperty("systemPrompt", semanticSystemPrompt);
            }
            if (!semanticPromptTemplate.isEmpty()) {
                services.addProperty("promptTemplate", semanticPromptTemplate);
            }
            Integer runtimeVizRateInt = parseRuntimeVizConfigValue(runtimeVizRate, RUNTIME_VIZ_EVENT_RATE_MIN, RUNTIME_VIZ_EVENT_RATE_MAX);
            if (runtimeVizRateInt != null) {
                services.addProperty("runtimeVizRate", String.valueOf(runtimeVizRateInt));
            }
            Integer runtimeVizBurstInt = parseRuntimeVizConfigValue(runtimeVizBurst, RUNTIME_VIZ_EVENT_BURST_MIN, RUNTIME_VIZ_EVENT_BURST_MAX);
            if (runtimeVizBurstInt != null) {
                services.addProperty("runtimeVizBurst", String.valueOf(runtimeVizBurstInt));
            }
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
        LLMSupport llm = new LLMSupport(new JdkHttpTransport(), baseUrl, apiKey, timeout);
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
            LLMSupport llm = new LLMSupport(new JdkHttpTransport(), baseUrl, apiKey, Duration.ofSeconds(15));
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
            LLMSupport llm = new LLMSupport(new JdkHttpTransport(), baseUrl, apiKey, Duration.ofSeconds(15));
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
            LLMSupport llm = new LLMSupport(new JdkHttpTransport(), baseUrl, apiKey, Duration.ofSeconds(timeout));
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
        JSONObject spec = resolveSpecForScope(entry, scope);
        if (spec != null) {
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
                                                null, null, null, null, null));
                            }
                        }
                    }
                }
            }
            if (sourceCount == 0) {
                sLogger.warning("No plugin-properties.json resources found on classpath.");
            } else {
                sLogger.message("Loaded exportable properties registry with "
                        + EXPORTABLE_PROPERTY_PROVIDERS.size() + " entries from " + sourceCount + " resources.");
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

        // Spec version for plugin update detection
        String specVersion = root.optString("specVersion", "");

        return new ExportablePropertyEntry(null, pluginSpec, agentSpec,
                pluginMeta, categories, commands, variables, specVersion);
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

    private void handleSemanticGet(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        ensureScriptLoaded(ref);
        writeJson(ctx, loadSemanticDocument(ref));
    }

    private void handleSemanticPut(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        ensureScriptLoaded(ref);
        try {
            JSONObject body = new JSONObject(ctx.body());
            JSONObject semantic = body.optJSONObject("semantic");
            if (semantic == null) {
                semantic = body;
            }
            JSONObject normalized = normalizeSemanticDocument(ref, semantic);
            if (!saveSemanticDocument(ref, normalized)) {
                ctx.status(500);
                writeJson(ctx, errorResponse("SEMANTIC_SAVE_FAILED", "Failed to save semantic annotations"));
                return;
            }
            writeJson(ctx, normalized);
        } catch (Exception exc) {
            ctx.status(400);
            writeJson(ctx, errorResponse("SEMANTIC_INVALID", "Invalid semantic payload: " + exc.getMessage()));
        }
    }

    private void handleUiPrefsGet(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        writeJson(ctx, loadUiPrefs(ref));
    }

    private void handleUiPrefsPut(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        try {
            JSONObject body = new JSONObject(ctx.body());
            JSONObject prefs = body.optJSONObject("uiPrefs");
            if (prefs == null) {
                prefs = body;
            }
            if (!saveUiPrefs(ref, prefs)) {
                ctx.status(500);
                writeJson(ctx, errorResponse("UIPREFS_SAVE_FAILED", "Failed to save UI preferences"));
                return;
            }
            writeJson(ctx, prefs);
        } catch (Exception exc) {
            ctx.status(400);
            writeJson(ctx, errorResponse("UIPREFS_INVALID", "Invalid UI preferences payload: " + exc.getMessage()));
        }
    }

    private Path uiPrefsPath(ProjectRef ref) {
        if (ref == null || ref.path == null || ref.path.isBlank()) {
            return null;
        }
        return Paths.get(ref.path, "ui-prefs.json");
    }

    private JSONObject loadUiPrefs(ProjectRef ref) {
        Path path = uiPrefsPath(ref);
        if (path == null || !Files.exists(path)) {
            return new JSONObject();
        }
        try {
            String raw = Files.readString(path, StandardCharsets.UTF_8);
            return new JSONObject(raw);
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot load ui-prefs.json: " + exc.getMessage());
            return new JSONObject();
        }
    }

    private boolean saveUiPrefs(ProjectRef ref, JSONObject prefs) {
        Path path = uiPrefsPath(ref);
        if (path == null) {
            return false;
        }
        try {
            Files.createDirectories(path.getParent());
            Files.writeString(
                    path,
                    prefs.toString(2),
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return true;
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot save ui-prefs.json: " + exc.getMessage());
            return false;
        }
    }

    // ── Variables ─────────────────────────────────────────────────────────────

    private void handleVariables(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        JSONArray vars = new JSONArray();
        try {
            for (VariableDefinition def : ref.runtimeProject.getSceneFlow().getVarDefList()) {
                JSONObject v = new JSONObject();
                v.put("name", def.getName());
                v.put("type", def.getType());
                vars.put(v);
            }
        } catch (Exception e) {
            sLogger.warning("handleVariables: " + e.getMessage());
        }
        JSONObject response = new JSONObject();
        response.put("variables", vars);
        writeJson(ctx, response);
    }

    // ── Screens (screens.json) ────────────────────────────────────────────────

    private Path screensPath(ProjectRef ref) {
        if (ref == null || ref.path == null || ref.path.isBlank()) return null;
        return Paths.get(ref.path, "screens.json");
    }

    private void handleScreensGet(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        Path path = screensPath(ref);
        if (path == null || !Files.exists(path)) {
            writeJson(ctx, new JSONObject());
            return;
        }
        try {
            String raw = Files.readString(path, StandardCharsets.UTF_8);
            writeJson(ctx, new JSONObject(raw));
        } catch (Exception e) {
            ctx.status(500);
            writeJson(ctx, errorResponse("SCREENS_READ_FAILED", e.getMessage()));
        }
    }

    private void handleScreensPut(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        Path path = screensPath(ref);
        if (path == null) {
            ctx.status(409);
            writeJson(ctx, errorResponse("SCREENS_SAVE_FAILED",
                    "Project has not been saved to disk yet. Use File \u2192 Save As to choose a location, then try again."));
            return;
        }
        try {
            JSONObject body = new JSONObject(ctx.body());
            Files.createDirectories(path.getParent());
            Files.writeString(path, body.toString(2), StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
            writeJson(ctx, body);
        } catch (Exception e) {
            ctx.status(400);
            writeJson(ctx, errorResponse("SCREENS_SAVE_FAILED", e.getMessage()));
        }
    }

    // ── Character config (character-config.json) ──────────────────────────────

    private Path characterConfigPath(ProjectRef ref) {
        if (ref == null || ref.path == null || ref.path.isBlank()) return null;
        return Paths.get(ref.path, "character-config.json");
    }

    private void handleCharacterConfigGet(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        Path path = characterConfigPath(ref);
        if (path != null && Files.exists(path)) {
            try {
                writeJson(ctx, new JSONObject(Files.readString(path, StandardCharsets.UTF_8)));
            } catch (Exception e) {
                ctx.status(500);
                writeJson(ctx, errorResponse("CHARACTER_CONFIG_READ_FAILED", e.getMessage()));
            }
            return;
        }
        // No file — synthesise from charamel-ws plugin config if present.
        JSONObject synthesised = synthesiseCharacterConfig(ref.runtimeProject);
        writeJson(ctx, synthesised != null ? synthesised : new JSONObject());
    }

    /** Builds {"url":"<character_url>?server=ws://localhost:<ws_port>/ws"} from the
     *  charamel-ws plugin config, or returns null if no such plugin is configured. */
    private JSONObject synthesiseCharacterConfig(de.dfki.vsm.runtime.project.RunTimeProject project) {
        final String CHARAMEL_CLASS   = "charamelWs";
        final String DEFAULT_CHAR_URL = "https://vuppetmaster.de/dev/ubidenz/";
        for (de.dfki.vsm.model.project.PluginConfig pc :
                project.getProjectConfig().getPluginConfigList()) {
            if (!pc.getClassName().contains(CHARAMEL_CLASS)) continue;
            String baseUrl = pc.getProperty("character_url");
            if (baseUrl == null || baseUrl.isBlank()) baseUrl = DEFAULT_CHAR_URL;
            if (!baseUrl.endsWith("/")) baseUrl += "/";
            String wsPort = pc.getProperty("ws_port");
            if (wsPort == null || wsPort.isBlank()) wsPort = "3030";
            JSONObject cfg = new JSONObject();
            cfg.put("url", baseUrl + "?server=ws://localhost:" + wsPort + "/ws");
            return cfg;
        }
        return null;
    }

    private void handleCharacterConfigPut(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        Path path = characterConfigPath(ref);
        if (path == null) {
            ctx.status(409);
            writeJson(ctx, errorResponse("CHARACTER_CONFIG_SAVE_FAILED",
                    "Project has not been saved to disk yet. Use File → Save As to choose a location, then try again."));
            return;
        }
        try {
            JSONObject body = new JSONObject(ctx.body());
            Files.createDirectories(path.getParent());
            Files.writeString(path, body.toString(2), StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
            writeJson(ctx, body);
        } catch (Exception e) {
            ctx.status(400);
            writeJson(ctx, errorResponse("CHARACTER_CONFIG_SAVE_FAILED", e.getMessage()));
        }
    }

    private void handleAssetsGet(Context ctx) {
        String pid      = ctx.pathParam("pid");
        String filename = ctx.pathParam("filename");
        ProjectRef ref  = projectStore.get(pid);
        if (ref == null || ref.path == null || ref.path.isBlank()) { ctx.status(404); return; }
        Path assetsDir = Paths.get(ref.path, "screens-assets");
        Path assetPath = assetsDir.resolve(filename).normalize();
        if (!assetPath.startsWith(assetsDir)) { ctx.status(403); return; }
        if (!Files.exists(assetPath) || !Files.isRegularFile(assetPath)) { ctx.status(404); return; }
        try {
            ctx.result(Files.newInputStream(assetPath)).contentType(resolveMediaType(filename));
        } catch (IOException e) {
            ctx.status(500);
        }
    }

    private static String resolveMediaType(String filename) {
        String f = filename.toLowerCase();
        if (f.endsWith(".jpg") || f.endsWith(".jpeg")) return "image/jpeg";
        if (f.endsWith(".png"))  return "image/png";
        if (f.endsWith(".gif"))  return "image/gif";
        if (f.endsWith(".webp")) return "image/webp";
        if (f.endsWith(".svg"))  return "image/svg+xml";
        if (f.endsWith(".mp4"))  return "video/mp4";
        if (f.endsWith(".webm")) return "video/webm";
        if (f.endsWith(".ogv"))  return "video/ogg";
        if (f.endsWith(".mp3"))  return "audio/mpeg";
        if (f.endsWith(".wav"))  return "audio/wav";
        if (f.endsWith(".oga") || f.endsWith(".ogg")) return "audio/ogg";
        return "application/octet-stream";
    }

    private void handleSemanticSyntaxAnalyze(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        ensureScriptLoaded(ref);
        try {
            JSONObject body = new JSONObject(ctx.body());
            String text = body.optString("text", ref.scriptText == null ? "" : ref.scriptText);
            if (text == null) text = "";
            String language = body.optString("language", "de");
            Integer line = body.has("line") ? Integer.valueOf(body.optInt("line", 1)) : null;
            String speaker = body.optString("speaker", "");
            Integer baseOffset = body.has("baseOffset") ? Integer.valueOf(body.optInt("baseOffset", 0)) : null;
            boolean persist = body.optBoolean("persist", false);
            boolean debug = body.optBoolean("debug", false);

            JSONObject syntaxLayers = new JSONObject()
                    .put("basic", true)
                    .put("dialogueAct", false)
                    .put("themeRheme", false);
            JSONObject syntax = analyzeSemanticWithUd(ref, text, syntaxLayers, language, line, speaker, baseOffset, debug);
            if (syntax == null) {
                syntax = analyzeSemanticHeuristic(ref, text, syntaxLayers);
            }
            if (!persist) {
                writeJson(ctx, syntax);
            } else if (saveSemanticDocument(ref, syntax)) {
                writeJson(ctx, syntax);
            } else {
                ctx.status(500);
                writeJson(ctx, errorResponse("SEMANTIC_SAVE_FAILED", "Failed to save semantic syntax analysis"));
            }
        } catch (Exception exc) {
            ctx.status(500);
            writeJson(ctx, errorResponse("SEMANTIC_SYNTAX_FAILED", "Semantic syntax analysis failed: " + exc.getMessage()));
        }
    }

    private void handleSemanticAnalyze(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        ensureScriptLoaded(ref);
        try {
            JSONObject body = new JSONObject(ctx.body());
            String text = body.optString("text", ref.scriptText == null ? "" : ref.scriptText);
            if (text == null) text = "";
            JSONObject layers = body.optJSONObject("layers");
            boolean useLlm = body.optBoolean("useLlm", true);
            boolean persist = body.optBoolean("persist", true);
            int llmIndex = body.optInt("llmIndex", 0);
            String systemPrompt = body.optString("systemPrompt", "");
            String prompt = body.optString("prompt", "");
            String language = body.optString("language", "de");
            String basicProvider = body.optString("basicProvider",
                    semanticBasicProvider(ref));
            boolean includeBasic = layers == null || layers.optBoolean("basic", true);
            boolean includeDa = layers == null || layers.optBoolean("dialogueAct", true);
            boolean includeTr = layers == null || layers.optBoolean("themeRheme", true);

            JSONObject udLayers = new JSONObject()
                    .put("basic", includeBasic)
                    .put("dialogueAct", false)
                    .put("themeRheme", false);
            JSONObject llmLayers = new JSONObject()
                    .put("basic", includeBasic && "llm".equalsIgnoreCase(basicProvider))
                    .put("dialogueAct", includeDa)
                    .put("themeRheme", includeTr);

            JSONObject udDoc = null;
            if (includeBasic && !"llm".equalsIgnoreCase(basicProvider)) {
                udDoc = analyzeSemanticWithUd(ref, text, udLayers, language, null, null, null, false);
            }

            JSONObject llmDoc = null;
            if (useLlm && (includeDa || includeTr || "llm".equalsIgnoreCase(basicProvider))) {
                llmDoc = analyzeSemanticWithLlm(ref, text, llmLayers, llmIndex, systemPrompt, prompt);
            }

            JSONObject semantic = null;
            if (llmDoc != null && udDoc != null) {
                semantic = mergeSemanticDocuments(ref, llmDoc, udDoc,
                        includeBasic, false, false);
                setDocumentLayerSource(semantic, "basic", "ud");
            } else if (llmDoc != null) {
                semantic = llmDoc;
            } else if (udDoc != null) {
                semantic = udDoc;
            }

            if (semantic != null && includeBasic && !hasAnyBasicAnnotations(semantic)) {
                JSONObject basicFallback = null;
                if (useLlm) {
                    JSONObject llmBasicOnly = new JSONObject()
                            .put("basic", true)
                            .put("dialogueAct", false)
                            .put("themeRheme", false);
                    basicFallback = analyzeSemanticWithLlm(ref, text, llmBasicOnly, llmIndex, systemPrompt, prompt);
                }
                if (basicFallback == null) {
                    JSONObject heuristicBasicOnly = new JSONObject()
                            .put("basic", true)
                            .put("dialogueAct", false)
                            .put("themeRheme", false);
                    basicFallback = analyzeSemanticHeuristic(ref, text, heuristicBasicOnly);
                }
                if (basicFallback != null) {
                    semantic = mergeSemanticDocuments(ref, semantic, basicFallback, true, false, false);
                }
            }

            if (semantic == null) {
                semantic = analyzeSemanticHeuristic(ref, text, layers);
            }
            if (!persist) {
                writeJson(ctx, semantic);
            } else if (saveSemanticDocument(ref, semantic)) {
                writeJson(ctx, semantic);
            } else {
                ctx.status(500);
                writeJson(ctx, errorResponse("SEMANTIC_SAVE_FAILED", "Failed to save semantic analysis"));
            }
        } catch (Exception exc) {
            ctx.status(500);
            writeJson(ctx, errorResponse("SEMANTIC_ANALYSIS_FAILED", "Semantic analysis failed: " + exc.getMessage()));
        }
    }

    private JSONObject defaultSemanticDocument(ProjectRef ref) {
        String now = java.time.Instant.now().toString();
        JSONObject doc = new JSONObject();
        doc.put("version", SEMANTIC_DOC_VERSION);
        doc.put("schema", new JSONObject()
                .put("id", SEMANTIC_SCHEMA_ID)
                .put("version", SEMANTIC_DOC_VERSION));
        doc.put("scriptHash", sha256(ref != null ? ref.scriptText : ""));
        doc.put("generatedAt", now);
        doc.put("updatedAt", now);
        doc.put("provenance", new JSONObject()
                .put("source", "editor-web-ui")
                .put("service", "")
                .put("model", "")
                .put("analyzedAt", now)
                .put("layers", new JSONObject()
                        .put("basic", "unknown")
                        .put("dialogueAct", "unknown")
                        .put("themeRheme", "unknown")));
        doc.put("annotations", new JSONArray());
        return doc;
    }

    private Path semanticDocumentPath(ProjectRef ref) {
        if (ref == null || ref.path == null || ref.path.isBlank()) {
            return null;
        }
        return Paths.get(ref.path, "semantic-annotations.json");
    }

    private JSONObject loadSemanticDocument(ProjectRef ref) {
        JSONObject fallback = defaultSemanticDocument(ref);
        Path path = semanticDocumentPath(ref);
        if (path == null || !Files.exists(path)) {
            return fallback;
        }
        try {
            String raw = Files.readString(path, StandardCharsets.UTF_8);
            JSONObject parsed = new JSONObject(raw);
            return normalizeSemanticDocument(ref, parsed);
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot load semantic annotations: " + exc.getMessage());
            return fallback;
        }
    }

    private JSONObject normalizeSemanticDocument(ProjectRef ref, JSONObject source) {
        JSONObject fallback = defaultSemanticDocument(ref);
        String now = java.time.Instant.now().toString();
        JSONObject out = new JSONObject();
        out.put("version", source == null ? SEMANTIC_DOC_VERSION : source.optInt("version", SEMANTIC_DOC_VERSION));
        JSONObject schema = source == null ? null : source.optJSONObject("schema");
        out.put("schema", schema == null
                ? fallback.getJSONObject("schema")
                : new JSONObject(schema.toString())
                        .put("id", schema.optString("id", SEMANTIC_SCHEMA_ID))
                        .put("version", schema.optInt("version", SEMANTIC_DOC_VERSION)));
        out.put("scriptHash", sha256(ref != null ? ref.scriptText : ""));
        out.put("generatedAt", source == null ? fallback.optString("generatedAt", now)
                : source.optString("generatedAt", now));
        out.put("updatedAt", now);
        JSONObject provenance = source == null ? null : source.optJSONObject("provenance");
        out.put("provenance", provenance == null
                ? fallback.getJSONObject("provenance")
                : new JSONObject(provenance.toString()));
        JSONArray anns = source == null ? null : source.optJSONArray("annotations");
        JSONArray normalizedAnnotations = new JSONArray();
        if (anns != null) {
            for (int i = 0; i < anns.length(); i++) {
                JSONObject ann = anns.optJSONObject(i);
                if (ann == null) {
                    continue;
                }
                JSONObject normalized = new JSONObject(ann.toString());
                if (!normalized.has("provenance")) {
                    JSONObject annProv = new JSONObject().put("layers", new JSONObject());
                    if (normalized.has("basic")) {
                        annProv.getJSONObject("layers").put("basic", "unknown");
                    }
                    if (normalized.has("dialogueAct")) {
                        annProv.getJSONObject("layers").put("dialogueAct", "unknown");
                    }
                    if (normalized.has("themeRheme")) {
                        annProv.getJSONObject("layers").put("themeRheme", "unknown");
                    }
                    annProv.put("analyzedAt", out.optString("updatedAt", now));
                    normalized.put("provenance", annProv);
                }
                normalizedAnnotations.put(normalized);
            }
        }
        out.put("annotations", normalizedAnnotations);
        return out;
    }

    private boolean hasAnyBasicAnnotations(JSONObject doc) {
        if (doc == null) return false;
        JSONArray anns = doc.optJSONArray("annotations");
        if (anns == null) return false;
        for (int i = 0; i < anns.length(); i++) {
            JSONObject ann = anns.optJSONObject(i);
            if (ann != null && ann.has("basic")) {
                JSONObject basic = ann.optJSONObject("basic");
                if (basic != null && basic.length() > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    private JSONObject mergeSemanticDocuments(ProjectRef ref, JSONObject baseDoc, JSONObject overlayDoc,
                                              boolean overlayBasic, boolean overlayDa, boolean overlayTr) {
        JSONObject base = normalizeSemanticDocument(ref, baseDoc);
        JSONObject overlay = normalizeSemanticDocument(ref, overlayDoc);
        JSONArray baseAnns = base.optJSONArray("annotations");
        JSONArray overlayAnns = overlay.optJSONArray("annotations");
        int max = Math.max(baseAnns == null ? 0 : baseAnns.length(), overlayAnns == null ? 0 : overlayAnns.length());
        JSONArray merged = new JSONArray();
        for (int i = 0; i < max; i++) {
            JSONObject srcBase = baseAnns == null ? null : baseAnns.optJSONObject(i);
            JSONObject srcOverlay = overlayAnns == null ? null : overlayAnns.optJSONObject(i);
            JSONObject out = srcBase == null ? new JSONObject() : new JSONObject(srcBase.toString());
            if (srcOverlay != null) {
                if (!out.has("id") && srcOverlay.has("id")) out.put("id", srcOverlay.get("id"));
                if (!out.has("line") && srcOverlay.has("line")) out.put("line", srcOverlay.get("line"));
                if (!out.has("speaker") && srcOverlay.has("speaker")) out.put("speaker", srcOverlay.get("speaker"));
                if (!out.has("text") && srcOverlay.has("text")) out.put("text", srcOverlay.get("text"));
                if (overlayBasic && srcOverlay.has("basic")) {
                    out.put("basic", srcOverlay.opt("basic"));
                    setAnnotationLayerSource(out, "basic", "ud");
                }
                if (overlayDa && srcOverlay.has("dialogueAct")) {
                    out.put("dialogueAct", srcOverlay.opt("dialogueAct"));
                    setAnnotationLayerSource(out, "dialogueAct", "llm");
                }
                if (overlayTr && srcOverlay.has("themeRheme")) {
                    out.put("themeRheme", srcOverlay.opt("themeRheme"));
                    setAnnotationLayerSource(out, "themeRheme", "llm");
                }
            }
            merged.put(out);
        }
        base.put("annotations", merged);
        return normalizeSemanticDocument(ref, base);
    }

    private void setDocumentLayerSource(JSONObject doc, String layer, String value) {
        if (doc == null || layer == null || layer.isBlank() || value == null || value.isBlank()) return;
        JSONObject prov = doc.optJSONObject("provenance");
        if (prov == null) {
            prov = new JSONObject();
            doc.put("provenance", prov);
        }
        JSONObject layers = prov.optJSONObject("layers");
        if (layers == null) {
            layers = new JSONObject();
            prov.put("layers", layers);
        }
        layers.put(layer, value);
        prov.put("analyzedAt", java.time.Instant.now().toString());
    }

    private void setAnnotationLayerSource(JSONObject annotation, String layer, String value) {
        if (annotation == null || layer == null || layer.isBlank() || value == null || value.isBlank()) return;
        JSONObject prov = annotation.optJSONObject("provenance");
        if (prov == null) {
            prov = new JSONObject();
            annotation.put("provenance", prov);
        }
        JSONObject layers = prov.optJSONObject("layers");
        if (layers == null) {
            layers = new JSONObject();
            prov.put("layers", layers);
        }
        layers.put(layer, value);
        prov.put("analyzedAt", java.time.Instant.now().toString());
    }

    private JSONObject analyzeSemanticWithUd(ProjectRef ref, String text, JSONObject layers, String language,
                                             Integer line, String speaker, Integer baseOffset, boolean debug) {
        if (ref == null || text == null || text.isBlank()) {
            return null;
        }
        if (layers != null && !layers.optBoolean("basic", true)) {
            return null;
        }
        try {
            String udUrl = semanticUdUrl(ref);
            int timeoutMs = semanticUdTimeoutMs(ref);
            JSONObject payload = new JSONObject();
            payload.put("text", text);
            payload.put("language", language == null || language.isBlank() ? "de" : language);
            if (line != null) {
                payload.put("line", line.intValue());
            }
            if (speaker != null && !speaker.isBlank()) {
                payload.put("speaker", speaker);
            }
            if (baseOffset != null) {
                payload.put("baseOffset", baseOffset.intValue());
            }
            if (debug) {
                payload.put("debug", true);
            }
            HttpTransport transport = new JdkHttpTransport();
            HttpTransport.HttpResponseData resp = transport.postJson(
                    URI.create(udUrl),
                    payload.toString(),
                    Map.of("Content-Type", "application/json"),
                    Duration.ofMillis(timeoutMs)
            );
            if (resp.statusCode() < 200 || resp.statusCode() >= 300) {
                sLogger.warning("Semantic UD fallback: HTTP " + resp.statusCode() + " from " + udUrl);
                return null;
            }
            String content = resp.body() == null ? "" : resp.body().trim();
            if (content.isEmpty() || content.charAt(0) != '{') {
                return null;
            }
            JSONObject doc = normalizeSemanticDocument(ref, new JSONObject(content));
            setDocumentLayerSource(doc, "basic", "ud");
            JSONArray anns = doc.optJSONArray("annotations");
            if (anns != null) {
                for (int i = 0; i < anns.length(); i++) {
                    JSONObject ann = anns.optJSONObject(i);
                    if (ann != null) {
                        setAnnotationLayerSource(ann, "basic", "ud");
                    }
                }
            }
            return doc;
        } catch (Exception exc) {
            sLogger.warning("Semantic UD fallback to non-UD provider: " + exc.getMessage());
            return null;
        }
    }

    private String semanticBasicProvider(ProjectRef ref) {
        String fromProject = null;
        if (ref != null && ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
            de.dfki.vsm.model.config.ConfigElement services = ref.runtimeProject.getProjectConfig().getSemanticServices();
            if (services != null) {
                fromProject = services.getProperty("basicProvider");
            }
        }
        if (fromProject != null && !fromProject.isBlank()) {
            return fromProject.trim();
        }
        return System.getProperty("semantic.basic.provider", SEMANTIC_BASIC_PROVIDER);
    }

    private String semanticUdUrl(ProjectRef ref) {
        String fromProject = null;
        if (ref != null && ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
            de.dfki.vsm.model.config.ConfigElement services = ref.runtimeProject.getProjectConfig().getSemanticServices();
            if (services != null) {
                fromProject = services.getProperty("udUrl");
            }
        }
        if (fromProject != null && !fromProject.isBlank()) {
            return fromProject.trim();
        }
        return System.getProperty("semantic.ud.url", SEMANTIC_UD_URL_DEFAULT);
    }

    private int semanticUdTimeoutMs(ProjectRef ref) {
        String fromProject = null;
        if (ref != null && ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
            de.dfki.vsm.model.config.ConfigElement services = ref.runtimeProject.getProjectConfig().getSemanticServices();
            if (services != null) {
                fromProject = services.getProperty("udTimeoutMs");
            }
        }
        if (fromProject != null && !fromProject.isBlank()) {
            try {
                int value = Integer.parseInt(fromProject.trim());
                if (value > 0) {
                    return value;
                }
            } catch (NumberFormatException ignored) {
                // Fall through to system/default value.
            }
        }
        return Integer.getInteger("semantic.ud.timeout.ms", SEMANTIC_UD_TIMEOUT_MS);
    }

    private boolean saveSemanticDocument(ProjectRef ref, JSONObject semantic) {
        Path path = semanticDocumentPath(ref);
        if (path == null) {
            return false;
        }
        try {
            Files.createDirectories(path.getParent());
            Files.writeString(
                    path,
                    semantic.toString(2),
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return true;
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot save semantic annotations: " + exc.getMessage());
            return false;
        }
    }

    private JSONObject analyzeSemanticWithLlm(ProjectRef ref, String text, JSONObject layers, int llmIndex, String systemPrompt, String customPrompt) {
        if (ref == null || ref.runtimeProject == null || text == null || text.isBlank()) {
            return null;
        }
        ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
        if (cfg == null || cfg.getLLMConfigList() == null || cfg.getLLMConfigList().isEmpty()) {
            return null;
        }
        int selectedIndex = Math.max(0, llmIndex);
        if (selectedIndex >= cfg.getLLMConfigList().size()) {
            selectedIndex = 0;
        }
        LLMConfig llmConfig = cfg.getLLMConfigList().get(selectedIndex);
        try {
            LLMSupport llm = createLLMSupport(llmConfig);
            if (llm.getSelectedModel() == null) {
                return null;
            }
            String layerText = "basic:true, dialogueAct:true, themeRheme:true";
            if (layers != null) {
                layerText = "basic:" + layers.optBoolean("basic", true)
                        + ", dialogueAct:" + layers.optBoolean("dialogueAct", true)
                        + ", themeRheme:" + layers.optBoolean("themeRheme", true);
            }
            boolean wantBasic = layers == null || layers.optBoolean("basic", true);
            boolean wantDa = layers == null || layers.optBoolean("dialogueAct", true);
            boolean wantTr = layers == null || layers.optBoolean("themeRheme", true);
            StringBuilder fields = new StringBuilder("id, line (1-based), speaker, text");
            if (wantBasic) {
                fields.append(", basic.subject/basic.verb/basic.object each with text/from/to absolute char offsets");
            }
            if (wantDa) {
                fields.append(", dialogueAct.label/dialogueAct.confidence");
            }
            if (wantTr) {
                fields.append(", themeRheme.theme/themeRheme.rheme/themeRheme.confidence");
            }
            String prompt = """
                    Analyze the scene script and return JSON only (no markdown).
                    Output object fields: version (number), annotations (array).
                    Each annotation: %s.
                    Layers: %s
                    Script:
                    %s
                    """.formatted(fields.toString(), layerText, text);
            if (customPrompt != null && !customPrompt.isBlank()) {
                prompt = customPrompt
                        .replace("{{layers}}", layerText)
                        .replace("{{script}}", text);
            }
            if (systemPrompt != null && !systemPrompt.isBlank()) {
                prompt = "System instruction:\n" + systemPrompt.trim() + "\n\nUser instruction:\n" + prompt;
            }
            LLMSupport.LLMCompletion completion = llm.sendPrompt(prompt);
            String content = completion.content() == null ? "" : completion.content().trim();
            if (content.startsWith("```")) {
                int nl = content.indexOf('\n');
                if (nl >= 0) {
                    content = content.substring(nl + 1);
                }
                if (content.endsWith("```")) {
                    content = content.substring(0, content.length() - 3);
                }
                content = content.trim();
            }
            if (content.isEmpty() || content.charAt(0) != '{') {
                return null;
            }
            JSONObject doc = normalizeSemanticDocument(ref, new JSONObject(content));
            if (layers == null || layers.optBoolean("basic", true)) {
                setDocumentLayerSource(doc, "basic", "llm");
            }
            if (layers == null || layers.optBoolean("dialogueAct", true)) {
                setDocumentLayerSource(doc, "dialogueAct", "llm");
            }
            if (layers == null || layers.optBoolean("themeRheme", true)) {
                setDocumentLayerSource(doc, "themeRheme", "llm");
            }
            JSONArray anns = doc.optJSONArray("annotations");
            if (anns != null) {
                for (int i = 0; i < anns.length(); i++) {
                    JSONObject ann = anns.optJSONObject(i);
                    if (ann == null) continue;
                    if (layers == null || layers.optBoolean("basic", true)) {
                        setAnnotationLayerSource(ann, "basic", "llm");
                    }
                    if (layers == null || layers.optBoolean("dialogueAct", true)) {
                        setAnnotationLayerSource(ann, "dialogueAct", "llm");
                    }
                    if (layers == null || layers.optBoolean("themeRheme", true)) {
                        setAnnotationLayerSource(ann, "themeRheme", "llm");
                    }
                }
            }
            return doc;
        } catch (Exception exc) {
            sLogger.warning("Semantic LLM analysis fallback to heuristic: " + exc.getMessage());
            return null;
        }
    }

    private JSONObject analyzeSemanticHeuristic(ProjectRef ref, String text, JSONObject layers) {
        JSONObject doc = defaultSemanticDocument(ref);
        JSONArray annotations = new JSONArray();
        if (text == null) {
            text = "";
        }
        boolean includeBasic = layers == null || layers.optBoolean("basic", true);
        boolean includeDa = layers == null || layers.optBoolean("dialogueAct", true);
        boolean includeTr = layers == null || layers.optBoolean("themeRheme", true);
        String[] lines = text.split("\n", -1);
        int cursor = 0;
        java.util.regex.Pattern tokenPattern = java.util.regex.Pattern.compile("[\\p{L}\\$][\\p{L}\\p{N}_'\\-$]*");
        for (int i = 0; i < lines.length; i += 1) {
            String line = lines[i];
            int colonIndex = line.indexOf(':');
            String speaker = "";
            String utteranceRaw;
            if (colonIndex > 0 && colonIndex < line.length() - 1) {
                speaker = line.substring(0, colonIndex).trim();
                utteranceRaw = line.substring(colonIndex + 1);
            } else {
                utteranceRaw = line;
            }
            String utterance = utteranceRaw.trim();
            if (utterance.isEmpty()) {
                cursor += line.length() + 1;
                continue;
            }
            int lead = utteranceRaw.indexOf(utterance);
            int utteranceStart = colonIndex > 0
                    ? cursor + colonIndex + 1 + Math.max(0, lead)
                    : cursor + Math.max(0, lead);

            JSONArray tokens = new JSONArray();
            java.util.regex.Matcher matcher = tokenPattern.matcher(utterance);
            while (matcher.find()) {
                JSONObject tok = new JSONObject();
                tok.put("text", matcher.group());
                tok.put("from", utteranceStart + matcher.start());
                tok.put("to", utteranceStart + matcher.end());
                tokens.put(tok);
            }

            JSONObject ann = new JSONObject();
            ann.put("id", "ann-" + UUID.randomUUID());
            ann.put("line", i + 1);
            ann.put("speaker", speaker);
            ann.put("text", utterance);

            if (includeBasic) {
                JSONObject basic = new JSONObject();
                if (tokens.length() > 0) basic.put("subject", tokens.getJSONObject(0));
                if (tokens.length() > 1) basic.put("verb", tokens.getJSONObject(1));
                if (tokens.length() > 2) {
                    int objStart = tokens.getJSONObject(2).optInt("from", utteranceStart);
                    int objFrom = Math.max(utteranceStart, objStart);
                    int objTo = utteranceStart + utterance.length();
                    String objText = utterance.substring(Math.min(utterance.length(), Math.max(0, objFrom - utteranceStart)));
                    JSONObject obj = new JSONObject();
                    obj.put("text", objText);
                    obj.put("from", objFrom);
                    obj.put("to", objTo);
                    basic.put("object", obj);
                }
                ann.put("basic", basic);
            }

            if (includeDa) {
                JSONObject da = new JSONObject();
                da.put("label", detectDialogueActLabel(utterance));
                da.put("confidence", 0.55);
                ann.put("dialogueAct", da);
            }

            if (includeTr) {
                JSONObject tr = new JSONObject();
                String theme = tokens.length() > 0 ? tokens.getJSONObject(0).optString("text", "") : "";
                tr.put("theme", theme);
                tr.put("rheme", theme.isEmpty() ? utterance : utterance.replaceFirst("^" + java.util.regex.Pattern.quote(theme) + "\\s*", ""));
                tr.put("confidence", 0.5);
                ann.put("themeRheme", tr);
            }

            JSONObject meta = new JSONObject();
            meta.put("source", "heuristic");
            meta.put("generatedAt", java.time.Instant.now().toString());
            ann.put("meta", meta);
            annotations.put(ann);
            cursor += line.length() + 1;
        }
        doc.put("annotations", annotations);
        if (includeBasic) {
            setDocumentLayerSource(doc, "basic", "heuristic");
        }
        if (includeDa) {
            setDocumentLayerSource(doc, "dialogueAct", "heuristic");
        }
        if (includeTr) {
            setDocumentLayerSource(doc, "themeRheme", "heuristic");
        }
        return doc;
    }

    private String detectDialogueActLabel(String utterance) {
        String lower = utterance == null ? "" : utterance.toLowerCase();
        if (utterance != null && utterance.endsWith("?")) return "question";
        if (lower.startsWith("please ") || lower.startsWith("can you") || lower.startsWith("could you")) return "request";
        if (lower.startsWith("hi ") || lower.startsWith("hello")) return "greeting";
        if (lower.startsWith("thanks") || lower.startsWith("thank you")) return "thank";
        return "inform";
    }

    private String sha256(String input) {
        try {
            java.security.MessageDigest digest = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest((input == null ? "" : input).getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder("sha256:");
            for (byte b : bytes) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception exc) {
            return "sha256:";
        }
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

    private void handleProjectSubscribers(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        JSONObject response = new JSONObject();
        response.put("projectId", pid);
        response.put("subscriberCount", ref.collaborationSession.subscriberCount());
        JSONArray sessionIds = new JSONArray();
        for (WsContext sub : ref.collaborationSession.getSubscribers()) {
            sessionIds.put(sub.sessionId());
        }
        response.put("sessionIds", sessionIds);
        writeJson(ctx, response);
    }

    /**
     * {@code GET /api/v1/projects/{pid}/operations?since={seq}}
     *
     * <p>Returns all operations committed after {@code since} (exclusive).
     * Omitting the {@code since} query parameter (or passing {@code -1}) returns
     * the full in-memory log.  Used by late-joining clients to catch up on
     * operations they missed while disconnected.</p>
     */
    private void handleProjectOperations(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("PROJECT_NOT_FOUND", "Project not found"));
            return;
        }
        long since = -1L;
        String sinceParam = ctx.queryParam("since");
        if (sinceParam != null && !sinceParam.isBlank()) {
            try {
                since = Long.parseLong(sinceParam.trim());
            } catch (NumberFormatException ignored) {
                ctx.status(400);
                writeJson(ctx, errorResponse("BAD_REQUEST", "'since' must be a long integer"));
                return;
            }
        }
        OperationLog opLog = ref.collaborationSession.getOperationLog();
        List<SceneFlowOperation> ops = opLog.since(since);
        JSONArray arr = new JSONArray();
        for (SceneFlowOperation op : ops) {
            arr.put(op.toJson());
        }
        JSONObject response = new JSONObject();
        response.put("projectId", pid);
        response.put("currentSeq", opLog.currentSeq());
        response.put("operations", arr);
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
                    obj.put("commands", serializeCommands(e.getCmdList()));
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
        // Collect edges owned by the SuperNode itself (e.g. IEdges on SuperNodes)
        if (superNode.getEdgeList() != null) {
            out.addAll(superNode.getEdgeList());
        }
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

    /**
     * Handles the {@code Session.Subscribe} command: registers the client as a
     * subscriber for a specific project so that project-scoped events are
     * routed only to that client (and other subscribers of the same project).
     */
    private void handleSessionSubscribe(WsContext ctx, String raw) {
        String requestId = null;
        try {
            JSONObject req = new JSONObject(raw);
            requestId = req.optString("id", null);
            // sendCommand() puts data in "payload"; direct callers may use "params"
            JSONObject params = req.optJSONObject("params");
            if (params == null) params = req.optJSONObject("payload");
            String projectId = params != null ? params.optString("projectId", null) : null;
            if (projectId == null || projectId.isBlank()) {
                sendWsError(ctx, requestId, "projectId is required");
                return;
            }
            ProjectRef ref = projectStore.get(projectId);
            if (ref == null) {
                sendWsError(ctx, requestId, "Project not found: " + projectId);
                return;
            }
            // Remove from any previous project subscription
            cleanupWsSubscription(ctx);
            // Persistent client token (stored in browser localStorage, survives page reloads)
            String clientToken = params != null ? params.optString("clientToken", null) : null;
            // First subscriber with a non-blank token becomes the owner; subsequent
            // subscribers with the same token reclaim ownership after a reconnect.
            if (clientToken != null && !clientToken.isBlank()) {
                if (ref.ownerClientToken == null) {
                    ref.ownerClientToken = clientToken;
                }
            }
            ref.collaborationSession.subscribe(ctx);
            wsProjectSubscriptions.put(ctx.sessionId(), projectId);

            // Presence: register user, broadcast presence.joined to others
            String displayName = params != null ? params.optString("displayName", null) : null;
            String userId = ctx.sessionId();
            UserPresence presence = ref.collaborationSession.getPresenceManager().join(userId, displayName);
            broadcastPresenceEvent(projectId, "presence.joined", presence, ref, ctx);

            boolean isOwner = clientToken != null && !clientToken.isBlank()
                    && clientToken.equals(ref.ownerClientToken);
            JSONObject result = new JSONObject();
            result.put("projectId", projectId);
            result.put("myUserId", userId);
            result.put("isOwner", isOwner);
            result.put("subscriberCount", ref.collaborationSession.subscriberCount());
            result.put("presence", presenceListJson(ref));
            ctx.send(RuntimeWsProtocol.successResponse(requestId, result).toString());
            sLogger.message("WS client " + ctx.sessionId() + " subscribed to project " + projectId);
        } catch (Exception exc) {
            sLogger.warning("Session.Subscribe failed: " + exc.getMessage());
            JSONObject err = new JSONObject();
            if (requestId != null) err.put("id", requestId);
            err.put("status", "error");
            err.put("error", exc.getMessage());
            try { ctx.send(err.toString()); } catch (Exception ignored) {}
        }
    }

    /** Removes a WS client from the project it subscribed to (if any). */
    private void cleanupWsSubscription(WsContext ctx) {
        String previousProjectId = wsProjectSubscriptions.remove(ctx.sessionId());
        if (previousProjectId != null) {
            ProjectRef ref = projectStore.get(previousProjectId);
            if (ref != null) {
                // Presence: remove user, broadcast presence.left before unsubscribing
                UserPresence left = ref.collaborationSession.getPresenceManager()
                        .leave(ctx.sessionId());
                if (left != null) {
                    broadcastPresenceEvent(previousProjectId, "presence.left", left, ref, ctx);
                }
                ref.collaborationSession.unsubscribe(ctx);
            }
        }
    }

    /**
     * Handles {@code Presence.Update}: updates the sender's awareness state and
     * broadcasts {@code presence.update} to the other subscribers of the same project.
     */
    private void handlePresenceUpdate(WsContext ctx, String raw) {
        String requestId = null;
        try {
            JSONObject req = new JSONObject(raw);
            requestId = req.optString("id", null);
            // sendCommand() puts data in "payload"; direct callers may use "params"
            JSONObject params = req.optJSONObject("params");
            if (params == null) params = req.optJSONObject("payload");
            String projectId = params != null ? params.optString("projectId", null) : null;
            if (projectId == null || projectId.isBlank()) {
                sendWsError(ctx, requestId, "projectId is required");
                return;
            }
            ProjectRef ref = projectStore.get(projectId);
            if (ref == null) {
                sendWsError(ctx, requestId, "Project not found: " + projectId);
                return;
            }
            String userId = ctx.sessionId();
            String activeNodeId = params.optString("activeNodeId", null);
            if ("".equals(activeNodeId)) activeNodeId = null;
            JSONObject viewport = params.optJSONObject("viewport");

            UserPresence updated = ref.collaborationSession.getPresenceManager()
                    .update(userId, activeNodeId, viewport);
            if (updated == null) {
                // User not yet present — auto-join so Presence.Update works without prior Subscribe
                updated = ref.collaborationSession.getPresenceManager().join(userId, null);
                updated = ref.collaborationSession.getPresenceManager()
                        .update(userId, activeNodeId, viewport);
            }
            // Broadcast to everyone else in the session
            broadcastPresenceEvent(projectId, "presence.update", updated, ref, ctx);

            ctx.send(RuntimeWsProtocol.successResponse(requestId, new JSONObject()).toString());
        } catch (Exception exc) {
            sLogger.warning("Presence.Update failed: " + exc.getMessage());
            sendWsError(ctx, requestId, exc.getMessage());
        }
    }

    /**
     * Broadcasts a presence event ({@code presence.joined}, {@code presence.update},
     * {@code presence.left}) to all project subscribers <em>except</em> the originating
     * client.
     */
    private void broadcastPresenceEvent(String projectId, String eventName,
                                         UserPresence presence, ProjectRef ref,
                                         WsContext origin) {
        JSONObject event = new JSONObject();
        event.put("type", "event");
        event.put("ts", System.currentTimeMillis());
        event.put("channel", "presence");
        event.put("event", eventName);
        JSONObject payload = presence.toJson();
        payload.put("projectId", projectId);
        event.put("payload", payload);
        String msg = event.toString();
        // Use broadcastExcept so the sender doesn't receive its own echo.
        ref.collaborationSession.broadcastExcept(origin, msg);
    }

    private void handleProjectPresence(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404).result("Project not found: " + pid);
            return;
        }
        JSONObject resp = new JSONObject();
        resp.put("projectId", pid);
        resp.put("presence", presenceListJson(ref));
        writeJson(ctx, resp);
    }

    /** Returns a JSON array of all current presence records for a project. */
    private JSONArray presenceListJson(ProjectRef ref) {
        JSONArray arr = new JSONArray();
        for (UserPresence p : ref.collaborationSession.getPresenceManager().getAll()) {
            arr.put(p.toJson());
        }
        return arr;
    }

    // -------------------------------------------------------------------------
    // Phase E: /api/v1/sessions endpoints
    // -------------------------------------------------------------------------

    /**
     * {@code GET /api/v1/sessions}
     *
     * <p>Lists all active project sessions with their runtime state, presence count,
     * subscriber count, and latest operation sequence number.  Backed by the
     * existing {@code projectStore} so it stays consistent with
     * {@code GET /api/v1/projects}.</p>
     */
    private void handleSessions(Context ctx) {
        JSONArray sessions = new JSONArray();
        for (Map.Entry<String, ProjectRef> entry : projectStore.entrySet()) {
            String pid = entry.getKey();
            ProjectRef ref = entry.getValue();
            JSONObject s = new JSONObject();
            s.put("projectId", pid);
            s.put("name", ref.name != null ? ref.name : "");
            s.put("runtimeState", ref.runtimeState);
            s.put("subscriberCount", ref.collaborationSession.subscriberCount());
            s.put("presenceCount", ref.collaborationSession.getPresenceManager().size());
            s.put("operationSeq", ref.collaborationSession.getOperationLog().currentSeq());
            RuntimeOrchestrator.RuntimeState orch = mRuntimeOrchestrator.getState(pid);
            s.put("orchestratorState", orch != null ? orch.name() : "IDLE");
            sessions.put(s);
        }
        JSONObject response = new JSONObject();
        response.put("sessions", sessions);
        response.put("count", sessions.length());
        writeJson(ctx, response);
    }

    /**
     * {@code GET /api/v1/sessions/{id}/presence}
     *
     * <p>Returns current presence records for the session identified by project-id.
     * Delegates to the same data as {@code GET /api/v1/projects/{pid}/presence}.</p>
     */
    private void handleSessionPresence(Context ctx) {
        String pid = ctx.pathParam("id");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("SESSION_NOT_FOUND", "No session found for: " + pid));
            return;
        }
        JSONObject resp = new JSONObject();
        resp.put("projectId", pid);
        resp.put("presence", presenceListJson(ref));
        writeJson(ctx, resp);
    }

    /**
     * {@code GET /api/v1/sessions/{id}/operations?since={seq}}
     *
     * <p>Catch-up operation log query for the session.  Delegates to the same
     * data as {@code GET /api/v1/projects/{pid}/operations}.</p>
     */
    private void handleSessionOperations(Context ctx) {
        String pid = ctx.pathParam("id");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) {
            ctx.status(404);
            writeJson(ctx, errorResponse("SESSION_NOT_FOUND", "No session found for: " + pid));
            return;
        }
        long since = -1L;
        String sinceParam = ctx.queryParam("since");
        if (sinceParam != null && !sinceParam.isBlank()) {
            try {
                since = Long.parseLong(sinceParam.trim());
            } catch (NumberFormatException ignored) {
                ctx.status(400);
                writeJson(ctx, errorResponse("BAD_REQUEST", "'since' must be a long integer"));
                return;
            }
        }
        OperationLog opLog = ref.collaborationSession.getOperationLog();
        List<SceneFlowOperation> ops = opLog.since(since);
        JSONArray arr = new JSONArray();
        for (SceneFlowOperation op : ops) {
            arr.put(op.toJson());
        }
        JSONObject response = new JSONObject();
        response.put("projectId", pid);
        response.put("currentSeq", opLog.currentSeq());
        response.put("operations", arr);
        writeJson(ctx, response);
    }

    // -------------------------------------------------------------------------
    // Phase F: /api/v1/admin/tokens endpoints
    // -------------------------------------------------------------------------

    /**
     * {@code GET /api/v1/admin/tokens}
     *
     * <p>Lists all provisioned named-user tokens (bearer token strings are
     * omitted for security; only metadata is returned).  Requires the shared
     * server token to be included in the request {@code ?token=} parameter or
     * {@code Authorization: Bearer} header for future enforcement.</p>
     */
    private void handleAdminListTokens(Context ctx) {
        JSONArray tokens = new JSONArray();
        for (UserToken ut : mSessionGate.listTokens()) {
            tokens.put(ut.toJson());
        }
        JSONObject response = new JSONObject();
        response.put("tokens", tokens);
        response.put("count", tokens.length());
        writeJson(ctx, response);
    }

    /**
     * {@code POST /api/v1/admin/tokens}
     *
     * <p>Provisions a new named-user token.  Request body:
     * <pre>{ "userId": "alice", "displayName": "Alice", "roles": ["editor"] }</pre>
     * Response includes the bearer token string (shown once; store securely).
     * </p>
     */
    private void handleAdminProvisionToken(Context ctx) {
        JSONObject body;
        try {
            body = new JSONObject(ctx.body());
        } catch (Exception e) {
            ctx.status(400);
            writeJson(ctx, errorResponse("BAD_REQUEST", "Invalid JSON body"));
            return;
        }
        String userId = body.optString("userId", "").trim();
        if (userId.isEmpty()) {
            ctx.status(400);
            writeJson(ctx, errorResponse("BAD_REQUEST", "userId is required"));
            return;
        }
        String displayName = body.optString("displayName", null);
        Set<String> roles = new HashSet<>();
        org.json.JSONArray rolesArr = body.optJSONArray("roles");
        if (rolesArr != null) {
            for (int i = 0; i < rolesArr.length(); i++) {
                roles.add(rolesArr.optString(i, ""));
            }
        }
        if (roles.isEmpty()) {
            roles.add(SessionGate.ROLE_VIEWER);
        }
        try {
            UserToken ut = mSessionGate.provision(userId, displayName, roles);
            ctx.status(201);
            writeJson(ctx, ut.toJsonWithToken());
        } catch (Exception e) {
            ctx.status(500);
            writeJson(ctx, errorResponse("PROVISION_FAILED", e.getMessage()));
        }
    }

    private void sendWsError(WsContext ctx, String requestId, String message) {
        JSONObject err = RuntimeWsProtocol.errorResponse(message != null ? message : "Unknown error");
        if (requestId != null) err.put("id", requestId);
        try { ctx.send(err.toString()); } catch (Exception ignored) {}
    }

    private void handleWsMessage(WsContext ctx, String raw,
                                 java.util.function.Consumer<String> sender,
                                 java.util.function.Consumer<String> broadcaster) {
        try {
            // Parse envelope once so we can read top-level fields (e.g. basedOnSeq)
            // as well as the nested params without double-parsing.
            JSONObject envelope = new JSONObject(raw);
            String id = envelope.optString("id", "");
            String method = envelope.optString("method", "");
            if (method.isEmpty()) method = envelope.optString("name", "");
            long basedOnSeq = envelope.optLong("basedOnSeq", -1L);

            JSONObject params = envelope.optJSONObject("params");
            if (params == null) params = envelope.optJSONObject("payload");
            if (params == null) params = new JSONObject();

            // ---- OperationLog: conflict check before dispatch ----
            String projectId = params.optString("projectId", "");
            ProjectRef ref = !projectId.isEmpty() ? projectStore.get(projectId) : null;
            if (ref != null && isEditingCommand(method) && mMode != ServerMode.RUNTIME_ONLY) {
                OperationLog opLog = ref.collaborationSession.getOperationLog();
                OperationLog.AppendResult check = opLog.checkConflict(method, params, basedOnSeq);
                if (!check.isAccepted()) {
                    JSONObject conflict = new JSONObject();
                    if (!id.isEmpty()) conflict.put("id", id);
                    conflict.put("status", "conflict");
                    conflict.put("currentSeq", opLog.currentSeq());
                    conflict.put("resolution", "rejected");
                    conflict.put("reason", check.rejectionReason);
                    sender.accept(conflict.toString());
                    return;
                }
            }

            // ---- Regular dispatch ----
            JSONObject result = runtimeGateway.dispatch(method, params, broadcaster);

            // ---- OperationLog: commit + broadcast after successful dispatch ----
            if (ref != null && isEditingCommand(method) && mMode != ServerMode.RUNTIME_ONLY) {
                String userId = ctx != null ? ctx.sessionId() : "";
                OperationLog.AppendResult appended =
                        ref.collaborationSession.getOperationLog().append(method, params, basedOnSeq, userId);
                broadcastOperationApplied(projectId, appended.seq, userId, method, params,
                        appended.resolution, ref);
            }

            JSONObject resp = RuntimeWsProtocol.successResponse(id, result);
            sender.accept(resp.toString());
        } catch (Exception exc) {
            sLogger.failure("WS message handling failed: " + exc.getMessage());
            JSONObject resp = RuntimeWsProtocol.errorResponse(exc.getMessage());
            sender.accept(resp.toString());
        }
    }

    /**
     * Broadcasts an {@code operation.applied} event to all subscribers of the project
     * (or to all clients if none are subscribed), so that collaborators can replay
     * the operation and advance their local sequence number.
     */
    private void broadcastOperationApplied(String projectId, long seq, String userId,
                                            String method, JSONObject params,
                                            OperationLog.Resolution resolution,
                                            ProjectRef ref) {
        JSONObject event = new JSONObject();
        event.put("type", "event");
        event.put("ts", System.currentTimeMillis());
        event.put("channel", "operations");
        event.put("event", "operation.applied");
        JSONObject payload = new JSONObject();
        payload.put("projectId", projectId);
        payload.put("seq", seq);
        payload.put("userId", userId);
        payload.put("method", method);
        if (params != null) payload.put("params", params);
        payload.put("resolution", resolution.name().toLowerCase());
        event.put("payload", payload);
        broadcastToProjectOrAll(projectId, event.toString());
    }

    private JSONObject dispatchWs(String method, JSONObject params, java.util.function.Consumer<String> broadcaster) {
        // Gate editing commands in RUNTIME_ONLY mode
        if (mMode == ServerMode.RUNTIME_ONLY && isEditingCommand(method)) {
            return errorResponse("EDITING_NOT_SUPPORTED",
                    "Editing not supported in runtime-only mode");
        }
        JSONObject safeParams = params != null ? params : new JSONObject();
        WsCommandHandler handler = wsCommandRegistry.get(method);
        if (handler != null) {
            return handler.handle(method, safeParams, broadcaster);
        }
        JSONObject unknown = new JSONObject();
        unknown.put("message", "Unhandled method: " + method);
        return unknown;
    }

    /**
     * Returns true if the given WS method is an editing command
     * (as opposed to read-only queries or runtime control).
     */
    private boolean isEditingCommand(String method) {
        if (method == null) return false;
        if ("Script.Live".equals(method)) return false; // live draft — not a committed edit
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
            evt.put("type", "event");
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
        evt.put("type", "event");
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
                    String jarFilePath = Paths.get(new URI(jarPath.substring(0, jarSeparator))).toString();
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

        if (node instanceof SuperNode) {
            SuperNode createdSuperNode = (SuperNode) node;
            BasicNode historyNode = createdSuperNode.getHistoryNode();
            if (historyNode == null) {
                String historyNodeId = allocateNodeId(ref, false, usedIds);
                usedIds.add(historyNodeId);
                historyNode = new BasicNode();
                historyNode.setId(historyNodeId);
                historyNode.setName("History");
                historyNode.setHistoryNodeFlag(true);
                historyNode.setGraphics(new NodeGraphics(15, 15));
                historyNode.setParentNode(createdSuperNode);
                createdSuperNode.addNode(historyNode);
                createdSuperNode.setHistoryNode(historyNode);
            }
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

    private JSONObject createAliasNodeForProject(JSONObject params, java.util.function.Consumer<String> broadcaster) {
        String pid = params.optString("projectId", "");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null || ref.runtimeProject == null) {
            return errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        SceneFlow sceneFlow = ref.runtimeProject.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        String refId = params.optString("refId", "").trim();

        if (refId.isBlank()) {
            return errorResponse("BAD_REQUEST", "Missing refId");
        }

        // Validate: refId must be a direct child of root SceneFlow, not an AliasNode
        SuperNode canonical = null;
        for (SuperNode sn : sceneFlow.getSuperNodeList()) {
            if (!(sn instanceof AliasNode) && refId.equals(sn.getId())) {
                canonical = sn;
                break;
            }
        }
        if (canonical == null) {
            return errorResponse("BAD_REQUEST",
                    "refId must be a top-level (root-level) SuperNode: " + refId);
        }

        SuperNode snapshotTarget = resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        // Allocate a unique ID
        Set<String> usedIds = new java.util.HashSet<>();
        List<BasicNode> existingNodes = new ArrayList<>();
        collectNodes(sceneFlow, existingNodes);
        for (BasicNode existing : existingNodes) {
            if (existing != null && existing.getId() != null) usedIds.add(existing.getId());
        }
        String aliasId = allocateNodeId(ref, true, usedIds);

        String name = params.optString("name", "").trim();
        if (name.isBlank()) {
            name = canonical.getName() != null ? canonical.getName() : "Node";
        }

        int xPos = safeRound(params.has("x") ? params.optDouble("x") : null,
                canonical.getGraphics() != null ? canonical.getGraphics().getPosition().getXPos() + 140 : 140);
        int yPos = safeRound(params.has("y") ? params.optDouble("y") : null,
                canonical.getGraphics() != null ? canonical.getGraphics().getPosition().getYPos() + 50 : 50);

        AliasNode alias = new AliasNode();
        alias.setId(aliasId);
        alias.setName(name);
        alias.setRefId(refId);
        alias.setGraphics(new NodeGraphics(xPos, yPos));
        alias.setParentNode(activeSuperNode);
        alias.resolve(sceneFlow.buildTopLevelSuperNodeIndex());
        activeSuperNode.addSuperNode(alias);

        JSONObject snapshot = createSceneFlowSnapshot(ref.runtimeProject, pid, snapshotTarget, sceneFlow);
        JSONObject resp = buildSceneFlowResponse(snapshot);
        resp.put("nodeId", aliasId);
        broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        recordHistory(ref, "SceneFlow.Node.CreateAlias");
        recordCommand(ref, "SceneFlow.Node.CreateAlias", params);
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
            boolean nextHistory = fields.optBoolean("isHistory", false);
            SuperNode parent = dataNode.getParentNode();
            boolean isParentHistoryNode = parent != null && parent.getHistoryNode() == dataNode;
            if (isParentHistoryNode && !nextHistory) {
                return errorResponse("BAD_REQUEST", "Cannot remove history flag from a supernode history node.");
            }
            dataNode.setHistoryNodeFlag(nextHistory);
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
        if (dataNode.isHistoryNode()) {
            return errorResponse("BAD_REQUEST", "History nodes cannot be deleted.");
        }
        // Guard: prevent deletion of a canonical SuperNode that still has aliases
        if (dataNode instanceof SuperNode && !(dataNode instanceof AliasNode)) {
            List<JSONObject> aliases = SceneFlowSnapshotBuilder.collectAliasesOf(sceneFlow, nodeId);
            if (!aliases.isEmpty()) {
                return errorResponse("ALIAS_CONFLICT",
                        "Cannot delete \"" + dataNode.getName() + "\": it is referenced by "
                        + aliases.size() + " alias(es). Remove all aliases first.");
            }
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
        evt.put("type", "event");
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

    private int renameVariableReferences(SuperNode current, String oldName, String newName) {
        if (current == null || oldName == null || oldName.isBlank() || newName == null || newName.isBlank() || oldName.equals(newName)) {
            return 0;
        }
        int updated = 0;
        updated += renameVariableReferencesInVarDefs(current.getVarDefList(), oldName, newName);
        updated += renameVariableReferencesInCommands(current.getCmdList(), oldName, newName);
        if (current.getEdgeList() != null) {
            for (AbstractEdge edge : current.getEdgeList()) {
                updated += renameVariableReferencesInEdge(edge, oldName, newName);
            }
        }
        for (BasicNode node : current.getNodeList()) {
            updated += renameVariableReferencesInVarDefs(node.getVarDefList(), oldName, newName);
            updated += renameVariableReferencesInCommands(node.getCmdList(), oldName, newName);
            if (node.getEdgeList() != null) {
                for (AbstractEdge edge : node.getEdgeList()) {
                    updated += renameVariableReferencesInEdge(edge, oldName, newName);
                }
            }
        }
        for (SuperNode child : current.getSuperNodeList()) {
            updated += renameVariableReferences(child, oldName, newName);
        }
        return updated;
    }

    private int renameVariableReferencesInVarDefs(List<VariableDefinition> varDefs, String oldName, String newName) {
        if (varDefs == null || varDefs.isEmpty()) {
            return 0;
        }
        int updated = 0;
        for (VariableDefinition varDef : varDefs) {
            if (varDef != null) {
                updated += renameVariableReferencesInExpression(varDef.getExp(), oldName, newName);
            }
        }
        return updated;
    }

    private int renameVariableReferencesInCommands(List<Command> commands, String oldName, String newName) {
        if (commands == null || commands.isEmpty()) {
            return 0;
        }
        int updated = 0;
        for (Command command : commands) {
            updated += renameVariableReferencesInCommand(command, oldName, newName);
        }
        return updated;
    }

    private int renameVariableReferencesInEdge(AbstractEdge edge, String oldName, String newName) {
        if (edge == null) {
            return 0;
        }
        int updated = renameVariableReferencesInCommands(edge.getCmdList(), oldName, newName);
        if (edge instanceof GuargedEdge) {
            updated += renameVariableReferencesInExpression(((GuargedEdge) edge).getCondition(), oldName, newName);
        } else if (edge instanceof InterruptEdge) {
            updated += renameVariableReferencesInExpression(((InterruptEdge) edge).getCondition(), oldName, newName);
        } else if (edge instanceof TimeoutEdge) {
            updated += renameVariableReferencesInExpression(((TimeoutEdge) edge).getExpression(), oldName, newName);
        }
        return updated;
    }

    private int renameVariableReferencesInCommand(Command command, String oldName, String newName) {
        if (command == null) {
            return 0;
        }
        if (command instanceof Assignment) {
            Assignment assignment = (Assignment) command;
            int updated = renameVariableReferencesInExpression(assignment.getLeftExpression(), oldName, newName);
            updated += renameVariableReferencesInExpression(assignment.getInitExpression(), oldName, newName);
            return updated;
        }
        if (command instanceof PlayActionActivity) {
            PlayActionActivity invocation = (PlayActionActivity) command;
            int updated = renameVariableReferencesInExpression(invocation.getCommand(), oldName, newName);
            for (Expression exp : invocation.getArgList()) {
                updated += renameVariableReferencesInExpression(exp, oldName, newName);
            }
            return updated;
        }
        if (command instanceof PlayScenesActivity) {
            PlayScenesActivity invocation = (PlayScenesActivity) command;
            int updated = renameVariableReferencesInExpression(invocation.getArgument(), oldName, newName);
            for (Expression exp : invocation.getArgList()) {
                updated += renameVariableReferencesInExpression(exp, oldName, newName);
            }
            return updated;
        }
        if (command instanceof PlayDialogAction) {
            PlayDialogAction invocation = (PlayDialogAction) command;
            int updated = renameVariableReferencesInExpression(invocation.getArg(), oldName, newName);
            for (Expression exp : invocation.getArgList()) {
                updated += renameVariableReferencesInExpression(exp, oldName, newName);
            }
            return updated;
        }
        if (command instanceof StopActionActivity) {
            StopActionActivity invocation = (StopActionActivity) command;
            int updated = renameVariableReferencesInExpression(invocation.getCommand(), oldName, newName);
            for (Expression exp : invocation.getArgList()) {
                updated += renameVariableReferencesInExpression(exp, oldName, newName);
            }
            return updated;
        }
        if (command instanceof Expression) {
            return renameVariableReferencesInExpression((Expression) command, oldName, newName);
        }
        return 0;
    }

    private int renameVariableReferencesInExpression(Expression exp, String oldName, String newName) {
        if (exp == null) {
            return 0;
        }
        if (exp instanceof SimpleVariable) {
            return renameVariableNameField(exp, "mName", ((SimpleVariable) exp).getName(), oldName, newName);
        }
        if (exp instanceof MemberVariable) {
            return renameVariableNameField(exp, "mName", ((MemberVariable) exp).getName(), oldName, newName);
        }
        if (exp instanceof ArrayVariable) {
            ArrayVariable arrayVar = (ArrayVariable) exp;
            int updated = renameVariableNameField(exp, "mName", arrayVar.getName(), oldName, newName);
            updated += renameVariableReferencesInExpression(arrayVar.getExpression(), oldName, newName);
            return updated;
        }
        if (exp instanceof BinaryExpression) {
            BinaryExpression binary = (BinaryExpression) exp;
            int updated = renameVariableReferencesInExpression(binary.getLeftExp(), oldName, newName);
            updated += renameVariableReferencesInExpression(binary.getRightExp(), oldName, newName);
            return updated;
        }
        if (exp instanceof UnaryExpression) {
            return renameVariableReferencesInExpression(((UnaryExpression) exp).getExp(), oldName, newName);
        }
        if (exp instanceof ParenExpression) {
            return renameVariableReferencesInExpression(((ParenExpression) exp).getExp(), oldName, newName);
        }
        if (exp instanceof TernaryExpression) {
            TernaryExpression ternary = (TernaryExpression) exp;
            int updated = renameVariableReferencesInExpression(ternary.getCondition(), oldName, newName);
            updated += renameVariableReferencesInExpression(ternary.getThenExp(), oldName, newName);
            updated += renameVariableReferencesInExpression(ternary.getElseExp(), oldName, newName);
            return updated;
        }
        if (exp instanceof ConstructExpression) {
            int updated = 0;
            for (Expression arg : ((ConstructExpression) exp).getArgList()) {
                updated += renameVariableReferencesInExpression(arg, oldName, newName);
            }
            return updated;
        }
        if (exp instanceof CallingExpression) {
            int updated = 0;
            for (Expression arg : ((CallingExpression) exp).getArgList()) {
                updated += renameVariableReferencesInExpression(arg, oldName, newName);
            }
            return updated;
        }
        if (exp instanceof ArrayExpression) {
            int updated = 0;
            for (Expression arg : ((ArrayExpression) exp).getExpList()) {
                updated += renameVariableReferencesInExpression(arg, oldName, newName);
            }
            return updated;
        }
        if (exp instanceof StructExpression) {
            int updated = 0;
            for (Assignment assignment : ((StructExpression) exp).getExpList()) {
                updated += renameVariableReferencesInExpression(assignment.getLeftExpression(), oldName, newName);
                updated += renameVariableReferencesInExpression(assignment.getInitExpression(), oldName, newName);
            }
            return updated;
        }
        if (exp instanceof HistoryValueOf) {
            return renameVariableNameField(exp, "mVariable", ((HistoryValueOf) exp).getVar(), oldName, newName);
        }
        if (exp instanceof ContainsList) {
            ContainsList containsList = (ContainsList) exp;
            int updated = renameVariableReferencesInExpression(containsList.getListExp(), oldName, newName);
            updated += renameVariableReferencesInExpression(containsList.getItemExp(), oldName, newName);
            return updated;
        }
        if (exp instanceof PrologQuery) {
            return renameVariableReferencesInExpression(((PrologQuery) exp).getExpression(), oldName, newName);
        }
        if (exp instanceof RandomQuery) {
            return renameVariableReferencesInExpression(((RandomQuery) exp).getExpression(), oldName, newName);
        }
        if (exp instanceof TimeoutQuery) {
            return renameVariableReferencesInExpression(((TimeoutQuery) exp).getExpression(), oldName, newName);
        }
        return 0;
    }

    private int renameVariableNameField(Object target, String fieldName, String currentValue, String oldName, String newName) {
        if (target == null || currentValue == null || !oldName.equals(currentValue)) {
            return 0;
        }
        return setStringField(target, fieldName, newName) ? 1 : 0;
    }

    private boolean setStringField(Object target, String fieldName, String value) {
        if (target == null || fieldName == null || fieldName.isBlank()) {
            return false;
        }
        Class<?> clazz = target.getClass();
        while (clazz != null && clazz != Object.class) {
            try {
                Field field = clazz.getDeclaredField(fieldName);
                field.setAccessible(true);
                field.set(target, value);
                return true;
            } catch (NoSuchFieldException ignored) {
                clazz = clazz.getSuperclass();
            } catch (IllegalAccessException ignored) {
                return false;
            }
        }
        return false;
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
        initializeProjectRefCaches(ref);
        projectStore.put(id, ref);
        registerProjectDispatcher(id, ref);
        initializeProjectRefPersistenceState(ref);
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

    private String ensureProject(String path, String name, boolean allowEmptyFallback) {
        final String normalizedPath = normalizeProjectPath(path);
        // Reuse existing entry by path if present.
        for (ProjectRef ref : projectStore.values()) {
            if (ref.path != null && !ref.path.isBlank() && ref.path.equals(normalizedPath)) {
                return ref.id;
            }
        }

        // Try to load a real project if a path was given
        RunTimeProject rtp = null;
        if (normalizedPath != null && !normalizedPath.isBlank()) {
            try {
                rtp = new RunTimeProject();
                if (!rtp.parse(normalizedPath)) {
                    sLogger.warning("Warning: failed to parse project from " + normalizedPath);
                    if (!allowEmptyFallback) {
                        return null;
                    }
                    rtp = null;
                }
                if (rtp != null) {
                String id = UUID.randomUUID().toString();
                ProjectRef ref = new ProjectRef(id, name, normalizedPath);
                ref.runtimeProject = rtp;
                ref.runtimeState = "stopped";
                boolean idChanged = initializeProjectRefCaches(ref);
                projectStore.put(id, ref);
                registerProjectDispatcher(id, ref);
                initializeProjectRefPersistenceState(ref);
                if (idChanged) {
                    rtp.write(new java.io.File(normalizedPath));
                }
                    return id;
                }
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to load project from " + normalizedPath + ": " + exc.getMessage());
                if (!allowEmptyFallback) {
                    return null;
                }
            }
        }

        if (!allowEmptyFallback) {
            return null;
        }

        String id = UUID.randomUUID().toString();
        ProjectRef ref = new ProjectRef(id, name, normalizedPath);
        rtp = new RunTimeProject();
        if (normalizedPath != null && !normalizedPath.isBlank()) {
            rtp.setProjectPath(normalizedPath);
        }
        if (name != null && !name.isBlank()) {
            rtp.setProjectName(name);
        }
        ref.runtimeProject = rtp;
        ref.runtimeState = "stopped";
        initializeProjectRefCaches(ref);
        projectStore.put(id, ref);
        registerProjectDispatcher(id, ref);
        initializeProjectRefPersistenceState(ref);
        return id;
    }

    private boolean initializeProjectRefCaches(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null) {
            return false;
        }
        boolean idChanged = false;
        try {
            ref.nextNodeIndex = computeNextNodeIndex(ref.runtimeProject);
            ref.nextSuperNodeIndex = computeNextSuperNodeIndex(ref.runtimeProject);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to compute node indices: " + exc.getMessage());
        }
        try {
            idChanged = normalizeSceneFlowIds(ref);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to normalize scene flow ids: " + exc.getMessage());
        }
        try {
            ref.nodes = serializeNodes(ref.runtimeProject);
        } catch (Exception exc) {
            ref.nodes = new ArrayList<>();
            sLogger.warning("Warning: failed to serialize nodes: " + exc.getMessage());
        }
        try {
            ref.edges = serializeEdges(ref.runtimeProject);
        } catch (Exception exc) {
            ref.edges = new ArrayList<>();
            sLogger.warning("Warning: failed to serialize edges: " + exc.getMessage());
        }
        try {
            ref.comments = serializeComments(ref.runtimeProject);
        } catch (Exception exc) {
            ref.comments = new ArrayList<>();
            sLogger.warning("Warning: failed to serialize comments: " + exc.getMessage());
        }
        try {
            initializeDockPointsForProject(ref);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to initialize dock points: " + exc.getMessage());
        }
        return idChanged;
    }

    private void initializeProjectRefPersistenceState(ProjectRef ref) {
        if (ref == null) {
            return;
        }
        try {
            ensureScriptLoaded(ref);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to load script state: " + exc.getMessage());
        }
        try {
            ensureHistoryLoaded(ref);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to load history: " + exc.getMessage());
        }
        try {
            ensureCommandLogLoaded(ref);
        } catch (Exception exc) {
            sLogger.warning("Warning: failed to load command log: " + exc.getMessage());
        }
    }

    private static String normalizeProjectPath(String path) {
        if (path == null) {
            return "";
        }
        String trimmed = path.trim();
        if (trimmed.isEmpty()) {
            return "";
        }
        File base = new File(trimmed);
        if (base.isFile() && "project.xml".equalsIgnoreCase(base.getName())) {
            File parent = base.getParentFile();
            if (parent != null) {
                return parent.getPath();
            }
        }
        if ("project.xml".equalsIgnoreCase(base.getName())) {
            File parent = base.getParentFile();
            if (parent != null) {
                return parent.getPath();
            }
        }
        return trimmed;
    }

    static String resolveProjectDirectory(String path, String projectName) {
        String normalizedPath = normalizeProjectPath(path);
        if (normalizedPath.isBlank()) {
            return "";
        }
        String normalizedName = projectName == null ? "" : projectName.trim();
        if (normalizedName.isBlank()) {
            return normalizedPath;
        }
        Path basePath = Paths.get(normalizedPath).normalize();
        Path fileName = basePath.getFileName();
        if (fileName != null && normalizedName.equals(fileName.toString())) {
            return basePath.toString();
        }
        return basePath.resolve(normalizedName).toString();
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
        addRuntimeCapabilities(err);
        return err;
    }

    private boolean isLogicEnabled() {
        return LogicEngines.get().isEnabled();
    }

    private String runtimePlatform() {
        String forced = System.getProperty("vsm.platform", "").trim().toLowerCase();
        if (!forced.isBlank()) {
            return forced;
        }
        String vmName = System.getProperty("java.vm.name", "").toLowerCase();
        String runtimeName = System.getProperty("java.runtime.name", "").toLowerCase();
        if (vmName.contains("dalvik") || runtimeName.contains("android")) {
            return "android";
        }
        return "desktop";
    }

    private void addRuntimeCapabilities(JSONObject target) {
        if (target == null) {
            return;
        }
        target.put("logicEnabled", isLogicEnabled());
        target.put("platform", runtimePlatform());
        target.put("preferredTransport", "ws");
        target.put("commandTransport", "ws");
        target.put("eventTransport", "ws");
        target.put("bootstrapTransport", "http");
        target.put("runtimeRestMutationsEnabled", isRuntimeRestMutationsEnabled());
        target.put("wsPath", "/ws");
        target.put("apiPrefix", API_PREFIX);
    }

    private boolean isRuntimeRestMutationsEnabled() {
        String value = System.getProperty("vsm.runtime.rest.mutations.enabled", "false");
        return "true".equalsIgnoreCase(value) || "1".equals(value) || "yes".equalsIgnoreCase(value);
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
        List<SelectionCommandService.ClipboardEdgeData> clipboardEdges = new ArrayList<>();
        Set<String> clipboardStartNodeIds = new HashSet<>();
        // Per-project collaboration session (subscriber routing + event forwarder)
        final CollaborationSession collaborationSession;
        /**
         * Persistent client token of the session owner.
         * Set on first {@code Session.Subscribe}; survives owner reconnects because the
         * token is stored in the browser's localStorage and re-sent on every subscribe.
         */
        String ownerClientToken = null;

        ProjectRef(String id, String name, String path) {
            this.id = id;
            this.name = name == null ? "" : name;
            this.path = path == null ? "" : path;
            this.dirty = false;
            this.scriptText = null;
            this.scriptVersion = 1;
            this.scriptParseOk = true;
            this.collaborationSession = new CollaborationSession(id);
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

    // Cache: classpath dir → extracted filesystem path (populated once per process).
    private final java.util.concurrent.ConcurrentHashMap<String, Path> mExtractedDirs =
            new java.util.concurrent.ConcurrentHashMap<>();

    private Path resolveResourcePath(String directory) {
        // 1. Relative to CWD (e.g. when running packaged with resources alongside).
        Path fsPath = Paths.get(directory);
        if (Files.exists(fsPath)) {
            return fsPath;
        }
        // 2. Development fallback: resources may live in a sibling module (e.g. editor).
        //    Try common source/build locations relative to the project root (CWD).
        for (String prefix : new String[]{
                "editor/build/resources/main",
                "editor/src/main/resources"}) {
            Path devPath = Paths.get(prefix).resolve(directory);
            if (Files.exists(devPath)) {
                return devPath;
            }
        }
        // 3. Classpath lookup (packaged JAR or Gradle run with all modules on classpath).
        try {
            ClassLoader cl = getClass().getClassLoader();
            URL url = cl.getResource(directory.endsWith("/") ? directory : directory + "/");
            if (url != null) {
                if ("file".equals(url.getProtocol())) {
                    return Paths.get(url.toURI());
                }
                if ("jar".equals(url.getProtocol())) {
                    // Inside a fat jar — Paths.get(jar: URI) throws FileSystemNotFoundException.
                    // Extract the directory to a temp location so real filesystem paths are returned.
                    return mExtractedDirs.computeIfAbsent(directory, d -> extractClasspathDirToTemp(d, url));
                }
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot resolve resource path '" + directory + "': " + exc.getMessage());
        }
        return null;
    }

    /** Extract a classpath directory from a JAR to a temp directory and return the temp path. */
    private Path extractClasspathDirToTemp(String directory, URL jarUrl) {
        try {
            Path dest = Files.createTempDirectory("vsm-" + directory.replace("/", "-").replace(" ", "_"));
            String jarUrlStr = jarUrl.getPath();   // file:/path%20with%20space/some.jar!/res/tutorials/
            int bang = jarUrlStr.indexOf("!/");
            if (bang < 0) return null;
            // Decode URL-encoded characters (e.g. %20 for spaces in "VSM Web.app")
            String jarFilePath = Paths.get(new URI(jarUrlStr.substring(0, bang))).toString();
            String prefix      = jarUrlStr.substring(bang + 2); // strip "!/"
            try (java.util.jar.JarFile jar = new java.util.jar.JarFile(jarFilePath)) {
                java.util.Enumeration<java.util.jar.JarEntry> entries = jar.entries();
                while (entries.hasMoreElements()) {
                    java.util.jar.JarEntry entry = entries.nextElement();
                    String name = entry.getName();
                    if (!name.startsWith(prefix) || name.equals(prefix)) continue;
                    String relative = name.substring(prefix.length());
                    Path target = dest.resolve(relative);
                    if (entry.isDirectory()) {
                        Files.createDirectories(target);
                    } else {
                        Files.createDirectories(target.getParent());
                        try (InputStream in = jar.getInputStream(entry)) {
                            Files.copy(in, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                        }
                    }
                }
            }
            sLogger.message("[vsm] extracted classpath dir '" + directory + "' to " + dest);
            return dest;
        } catch (Exception e) {
            sLogger.warning("Warning: Cannot extract classpath dir '" + directory + "': " + e.getMessage());
            return null;
        }
    }


    private String generateToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }
}
