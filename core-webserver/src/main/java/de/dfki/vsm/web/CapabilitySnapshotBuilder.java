package de.dfki.vsm.web;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.plugin.CommandParam;
import de.dfki.vsm.model.plugin.PluginCommand;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneParam;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneSection;
import de.dfki.vsm.model.scenescript.ScriptEntity;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.SceneWord;
import de.dfki.vsm.model.scenescript.UttrElement;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Builds the capability snapshot that describes what a project offers: its plugins and agents, the
 * scenes its script declares, and the shape of its flow.
 *
 * <p>Consumed by the SceneFlow IR pipeline to decide what a generated flow may refer to, and served
 * over HTTP so an authoring assistant can ask the same question of a live project.
 *
 * <p>The contract is {@code doc/capability-snapshot.schema.json}. Adding a field here means adding it
 * there too: the schema sets {@code additionalProperties:false} throughout, so an undeclared field
 * makes every snapshot invalid.
 */
public final class CapabilitySnapshotBuilder {

    public static final String SNAPSHOT_VERSION = "1.2";

    /** Edge types the model can express, reported so a generator knows what it may emit. */
    private static final List<String> ALLOWED_EDGE_TYPES =
            List.of("EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE");

    private CapabilitySnapshotBuilder() {
    }

    /**
     * Builds a snapshot of a project that is already loaded.
     *
     * @param source free-text origin, a directory for a build-time snapshot or a project id for a
     *               live one, recorded so a consumer can tell where a snapshot came from
     */
    public static JSONObject build(final RunTimeProject project, final String source) {
        return build(project, source, null);
    }

    /**
     * @param projectDirectory where screens.json lives, or null when unavailable. Screens are stored
     *                         beside the project rather than in the model, so without this the
     *                         snapshot simply reports none.
     */
    public static JSONObject build(
            final RunTimeProject project, final String source, final Path projectDirectory) {
        if (project == null) {
            throw new IllegalArgumentException("Cannot build a capability snapshot without a project");
        }
        return new JSONObject()
                .put("snapshotVersion", SNAPSHOT_VERSION)
                .put("generatedAt", OffsetDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX")))
                .put("source", source == null ? "" : source)
                .put("project", buildProject(project))
                .put("script", buildScript(project.getSceneScript()))
                .put("screens", buildScreens(projectDirectory))
                .put("flow", buildFlow(project.getSceneFlow()));
    }

    /**
     * Loads a project from disk and snapshots it.
     *
     * <p>A snapshot is a read of what a project offers, so this must leave the directory untouched.
     * Two things would otherwise change it. {@code parse} ends with {@code loadRunTimePlugins()} and
     * would start real devices merely to describe a project, so {@code parseForInformation} is used
     * instead. And a project.xml carrying no uuid normally has a generated one written back on load,
     * which would dirty every project a caller reads, so that is switched off here.
     */
    public static JSONObject buildFromDirectory(final Path projectDirectory) {
        if (projectDirectory == null || !Files.isDirectory(projectDirectory)) {
            throw new IllegalArgumentException("Not a project directory: " + projectDirectory);
        }
        final RunTimeProject project = new RunTimeProject();
        project.setPersistGeneratedUUID(false);
        try {
            if (!project.parseForInformation(projectDirectory.toAbsolutePath().toString())) {
                throw new IllegalStateException("Cannot read project for information: " + projectDirectory);
            }
            return build(project, projectDirectory.toAbsolutePath().toString(), projectDirectory);
        } finally {
            // Every RunTimeProject brings its own event dispatcher, whose timer thread is not a
            // daemon. A project opened only to be read is never stopped by anything else, so
            // without this a command-line snapshot writes its output and then hangs forever with
            // nothing left to do.
            project.getEventDispatcher().abort();
        }
    }

    private static JSONObject buildProject(final RunTimeProject project) {
        final ProjectConfig config = project.getProjectConfig();
        final JSONArray plugins = new JSONArray();
        final JSONArray agents = new JSONArray();

        if (config != null) {
            for (PluginConfig plugin : config.getPluginConfigList()) {
                final String className = nullToEmpty(plugin.getClassName());
                plugins.put(new JSONObject()
                        .put("name", nullToEmpty(plugin.getPluginName()))
                        .put("className", className)
                        .put("type", nullToEmpty(plugin.getPluginType()))
                        .put("load", plugin.isMarkedtoLoad())
                        .put("commands", declaredCommands(className))
                        .put("writesVariables", declaredVariables(className, "writes"))
                        .put("readsVariables", declaredVariables(className, "reads")));
            }
            for (AgentConfig agent : config.getAgentConfigList()) {
                final JSONArray features = new JSONArray();
                for (ConfigFeature feature : agent.getEntryList()) {
                    features.put(new JSONObject()
                            .put("key", nullToEmpty(feature.getKey()))
                            .put("value", nullToEmpty(feature.getValue())));
                }
                agents.put(new JSONObject()
                        .put("name", nullToEmpty(agent.getAgentName()))
                        .put("device", nullToEmpty(agent.getDeviceName()))
                        .put("features", features));
            }
        }

        return new JSONObject()
                .put("name", config == null ? "" : nullToEmpty(config.getProjectName()))
                .put("androidProject", config != null && config.isAndroidProject())
                .put("plugins", plugins)
                .put("agents", agents);
    }

    /**
     * The commands a plugin declares, so a consumer knows what it may ask an agent to do.
     *
     * <p>An agent reaches its commands through {@code agent.device -> plugin.name -> plugin.commands}.
     * They are reported once per plugin rather than repeated on every agent, because two agents on
     * the same plugin offer exactly the same commands.
     *
     * <p>Trimmed to what a caller needs in order to build a call: prose descriptions and worked
     * examples are left in plugin-properties.json, since a project using htmlgui-ws declares 21
     * commands and carrying their full text would dominate the snapshot.
     */
    private static JSONArray declaredCommands(final String className) {
        final JSONArray out = new JSONArray();
        for (PluginCommand command : WebUiServer.pluginCommandsForClassName(className)) {
            final JSONArray params = new JSONArray();
            for (CommandParam param : command.getParams()) {
                final JSONObject entry = new JSONObject()
                        .put("name", nullToEmpty(param.getName()))
                        .put("type", nullToEmpty(param.getType()))
                        .put("required", param.isRequired());
                if (param.getEnum() != null && !param.getEnum().isEmpty()) {
                    entry.put("enum", new JSONArray(param.getEnum()));
                }
                params.put(entry);
            }
            out.put(new JSONObject()
                    .put("name", nullToEmpty(command.getName()))
                    .put("type", nullToEmpty(command.getType()))
                    .put("summary", nullToEmpty(command.getSummary()))
                    .put("params", params));
        }
        return out;
    }

    /** The variables a plugin declares it writes or reads, from its plugin-properties.json. */
    private static JSONArray declaredVariables(final String className, final String direction) {
        final JSONArray out = new JSONArray();
        final JSONObject variables = WebUiServer.pluginVariablesForClassName(className);
        final JSONArray declared = variables == null ? null : variables.optJSONArray(direction);
        for (int i = 0; declared != null && i < declared.length(); i++) {
            final JSONObject entry = declared.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            final String name = trimmed(entry.optString("var", entry.optString("name", "")));
            if (name.isEmpty()) {
                continue;
            }
            out.put(new JSONObject()
                    .put("name", name)
                    .put("type", nullToEmpty(entry.optString("type", ""))));
        }
        return out;
    }

    /**
     * The screens a project defines and the variables each one is wired to.
     *
     * <p>A screen reads a variable through {@code bindVar}, {@code dataVar} or {@code srcVar}, and
     * writes one through {@code sendsVar}, which is how a control such as a button or slider hands a
     * value back to the flow. The direction is the point: it tells a consumer whether a variable has
     * to be set before the screen is shown, or will be set by the person using it.
     *
     * <p>Screens live in screens.json beside the project rather than in the model, so this returns an
     * empty inventory when no directory is available.
     */
    private static JSONObject buildScreens(final Path projectDirectory) {
        final JSONArray screens = new JSONArray();
        final JSONObject out = new JSONObject().put("screens", screens);
        if (projectDirectory == null) {
            return out;
        }
        final Path screensFile = projectDirectory.resolve("screens.json");
        if (!Files.isRegularFile(screensFile)) {
            return out;
        }
        try {
            final JSONObject root = new JSONObject(Files.readString(screensFile));
            final JSONObject defined = root.optJSONObject("screens");
            if (defined != null) {
                for (String name : sortedNames(defined)) {
                    final Set<String> reads = new LinkedHashSet<>();
                    final Set<String> writes = new LinkedHashSet<>();
                    collectBindings(defined.opt(name), reads, writes);
                    screens.put(new JSONObject()
                            .put("name", name)
                            .put("readsVariables", new JSONArray(sortedList(reads)))
                            .put("writesVariables", new JSONArray(sortedList(writes))));
                }
            }
            // The character frame can take its source from a variable too, which is a project-level
            // binding rather than one belonging to any single screen.
            final JSONObject character = root.optJSONObject("character");
            final String characterSrcVar = character == null ? "" : trimmed(character.optString("srcVar", ""));
            if (!characterSrcVar.isEmpty()) {
                out.put("characterSrcVariable", characterSrcVar);
            }
        } catch (Exception malformed) {
            // A screens.json a consumer cannot parse is reported as no screens rather than failing
            // the whole snapshot, which is still useful without them.
            return new JSONObject().put("screens", new JSONArray());
        }
        return out;
    }

    private static void collectBindings(
            final Object node, final Set<String> reads, final Set<String> writes) {
        if (node instanceof JSONObject object) {
            for (String key : object.keySet()) {
                final Object value = object.opt(key);
                if (value instanceof String text && !text.isBlank()) {
                    switch (key) {
                        case "bindVar", "dataVar", "srcVar" -> reads.add(text.trim());
                        case "sendsVar" -> writes.add(text.trim());
                        default -> { }
                    }
                }
                collectBindings(value, reads, writes);
            }
        } else if (node instanceof JSONArray array) {
            for (int i = 0; i < array.length(); i++) {
                collectBindings(array.opt(i), reads, writes);
            }
        }
    }

    private static List<String> sortedNames(final JSONObject object) {
        final List<String> names = new ArrayList<>(object.keySet());
        names.sort(String::compareTo);
        return names;
    }

    private static List<String> sortedList(final Set<String> values) {
        final List<String> out = new ArrayList<>(values);
        out.sort(String::compareTo);
        return out;
    }

    /**
     * Scenes are content rather than graph structure, so they sit beside the flow rather than inside
     * it. The name is the scene <em>group</em> name, which is what PlayScene takes: language variants
     * of one name are one entry.
     */
    private static JSONObject buildScript(final SceneScript script) {
        final Map<String, SceneGroupFacts> groups = new LinkedHashMap<>();
        final JSONArray sections = new JSONArray();

        if (script != null) {
            // Section headings structure the script document and are never spoken. They live only in
            // the entity list, alongside the scenes rather than inside them.
            for (ScriptEntity entity : script.getEntityList()) {
                if (entity instanceof SceneSection section) {
                    final String heading = trimmed(section.getText());
                    if (!heading.isEmpty()) {
                        sections.put(heading);
                    }
                }
            }
            for (SceneObject scene : script.getSceneList()) {
                final String name = trimmed(scene.getName());
                if (name.isEmpty()) {
                    continue;
                }
                final SceneGroupFacts facts = groups.computeIfAbsent(name, key -> new SceneGroupFacts());
                final String language = trimmed(scene.getLanguage());
                if (!language.isEmpty()) {
                    facts.languages.add(language);
                }
                collectTurns(scene, facts);
            }
        }

        final JSONArray scenes = new JSONArray();
        groups.entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .forEach(entry -> scenes.put(entry.getValue().toJson(entry.getKey())));

        return new JSONObject().put("scenes", scenes).put("sections", sections);
    }

    private static void collectTurns(final SceneObject scene, final SceneGroupFacts facts) {
        for (SceneTurn turn : scene.getTurnList()) {
            facts.turnCount++;
            final String speaker = trimmed(turn.getSpeaker());
            if (!speaker.isEmpty()) {
                facts.speakers.add(speaker);
            }
            for (SceneUttr utterance : turn.getUttrList()) {
                for (UttrElement element : utterance.getWordList()) {
                    if (element instanceof SceneWord) {
                        facts.wordCount++;
                    } else if (element instanceof SceneParam param) {
                        // A scene with parameters cannot be played without arguments.
                        final String parameter = trimmed(param.getName());
                        if (!parameter.isEmpty()) {
                            facts.parameters.add(parameter);
                        }
                    } else if (element instanceof ActionObject action) {
                        final String command = trimmed(action.getName());
                        if (!command.isEmpty()) {
                            facts.inlineCommands.add(command);
                        }
                        // An empty actor addresses the turn's own speaker rather than another agent.
                        final String actor = trimmed(action.getActor());
                        if (!actor.isEmpty()) {
                            facts.referencedAgents.add(actor);
                        }
                    }
                }
            }
        }
    }

    private static JSONObject buildFlow(final SceneFlow flow) {
        final JSONArray variables = new JSONArray();
        final JSONArray nodes = new JSONArray();
        final JSONArray edges = new JSONArray();
        final JSONArray startNodeIds = new JSONArray();
        final String rootId = flow == null ? "SceneFlow" : blankToDefault(flow.getId(), "SceneFlow");

        if (flow != null) {
            for (VariableDefinition definition : flow.getVarDefList()) {
                variables.put(new JSONObject()
                        .put("name", nullToEmpty(definition.getName()))
                        .put("type", nullToEmpty(definition.getType()))
                        .put("scope", "global")
                        .put("ownerNodeId", rootId));
            }
            for (String startNodeId : sortedKeys(flow.getStartNodeMap())) {
                startNodeIds.put(startNodeId);
            }
            collectNodes(flow, rootId, nodes, edges);
        }

        return new JSONObject()
                .put("rootId", rootId)
                .put("startNodeIds", startNodeIds)
                .put("variables", variables)
                .put("allowedEdgeTypes", new JSONArray(ALLOWED_EDGE_TYPES))
                .put("nodes", nodes)
                .put("edges", edges);
    }

    private static void collectNodes(
            final SuperNode container,
            final String parentId,
            final JSONArray nodes,
            final JSONArray edges) {
        for (BasicNode node : container.getNodeList()) {
            nodes.put(new JSONObject()
                    .put("id", nullToEmpty(node.getId()))
                    .put("name", nullToEmpty(node.getName()))
                    .put("parentSuperNodeId", parentId)
                    .put("isSuperNode", false)
                    .put("isHistoryNode", node.isHistoryNode())
                    .put("commandCount", node.getCmdList().size()));
            collectEdges(node, edges);
        }
        for (SuperNode superNode : container.getSuperNodeList()) {
            final JSONArray superStartNodeIds = new JSONArray();
            for (String startNodeId : sortedKeys(superNode.getStartNodeMap())) {
                superStartNodeIds.put(startNodeId);
            }
            nodes.put(new JSONObject()
                    .put("id", nullToEmpty(superNode.getId()))
                    .put("name", nullToEmpty(superNode.getName()))
                    .put("parentSuperNodeId", parentId)
                    .put("isSuperNode", true)
                    .put("isHistoryNode", false)
                    .put("startNodeIds", superStartNodeIds)
                    .put("commandCount", superNode.getCmdList().size()));
            collectEdges(superNode, edges);
            collectNodes(superNode, nullToEmpty(superNode.getId()), nodes, edges);
        }
    }

    private static void collectEdges(final BasicNode owner, final JSONArray edges) {
        for (AbstractEdge edge : owner.getEdgeList()) {
            final JSONObject entry = new JSONObject()
                    .put("type", edgeType(edge))
                    .put("sourceNodeId", nullToEmpty(owner.getId()))
                    .put("targetNodeId", nullToEmpty(edge.getTargetUnid()));
            if (edge instanceof TimeoutEdge timeout) {
                entry.put("timeoutMs", timeout.getTimeout());
            } else if (edge instanceof RandomEdge random) {
                entry.put("probability", random.getProbability());
            } else if (edge instanceof GuargedEdge guarded) {
                putCondition(entry, guarded.getCondition());
            } else if (edge instanceof InterruptEdge interrupt) {
                putCondition(entry, interrupt.getCondition());
            }
            edges.put(entry);
        }
    }

    private static void putCondition(final JSONObject entry, final Expression condition) {
        if (condition == null) {
            return;
        }
        final String text = trimmed(condition.getConcreteSyntax());
        if (!text.isEmpty()) {
            entry.put("conditionText", text);
        }
    }

    private static String edgeType(final AbstractEdge edge) {
        if (edge instanceof EpsilonEdge) {
            return "EEDGE";
        }
        if (edge instanceof GuargedEdge) {
            return "CEDGE";
        }
        if (edge instanceof RandomEdge) {
            return "PEDGE";
        }
        if (edge instanceof TimeoutEdge) {
            return "TEDGE";
        }
        if (edge instanceof ForkingEdge) {
            return "FEDGE";
        }
        if (edge instanceof InterruptEdge) {
            return "IEDGE";
        }
        return "EEDGE";
    }

    private static List<String> sortedKeys(final Map<String, BasicNode> startNodeMap) {
        final List<String> ids = new ArrayList<>();
        if (startNodeMap != null) {
            ids.addAll(startNodeMap.keySet());
        }
        // The model holds start nodes in a hash map, so without this the order would vary between
        // runs of the same project and every regenerated snapshot would show a spurious diff.
        ids.sort(String::compareTo);
        return ids;
    }

    private static String nullToEmpty(final String value) {
        return value == null ? "" : value;
    }

    private static String trimmed(final String value) {
        return value == null ? "" : value.trim();
    }

    private static String blankToDefault(final String value, final String fallback) {
        final String trimmed = trimmed(value);
        return trimmed.isEmpty() ? fallback : trimmed;
    }

    /** Facts accumulated across the language variants that share one scene group name. */
    private static final class SceneGroupFacts {
        private final Set<String> languages = new LinkedHashSet<>();
        private final Set<String> speakers = new LinkedHashSet<>();
        private final Set<String> parameters = new LinkedHashSet<>();
        private final Set<String> referencedAgents = new LinkedHashSet<>();
        private final Set<String> inlineCommands = new LinkedHashSet<>();
        private int turnCount;
        private int wordCount;

        private JSONObject toJson(final String name) {
            return new JSONObject()
                    .put("name", name)
                    .put("languages", sorted(languages))
                    .put("speakers", sorted(speakers))
                    .put("turnCount", turnCount)
                    .put("wordCount", wordCount)
                    .put("parameters", sorted(parameters))
                    .put("referencedAgents", sorted(referencedAgents))
                    .put("inlineCommands", sorted(inlineCommands));
        }

        private static JSONArray sorted(final Set<String> values) {
            final List<String> out = new ArrayList<>(values);
            out.sort(String::compareTo);
            return new JSONArray(out);
        }
    }
}
