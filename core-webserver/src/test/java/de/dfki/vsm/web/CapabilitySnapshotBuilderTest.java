package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the snapshot against the committed fixture, which was produced by the Groovy generator this
 * class replaces. Everything must agree except the two fields that are deliberately different.
 */
class CapabilitySnapshotBuilderTest {

    private static final Path REPO_ROOT = repoRoot();
    private static final Path DESIGN_PATTERNS = REPO_ROOT.resolve("doc/DesignPatterns");
    private static final Path FIXTURE = REPO_ROOT.resolve("doc/capability-snapshot.designpatterns.json");

    /** Tests run with the module directory as CWD, the fixture lives at the repository root. */
    private static Path repoRoot() {
        Path candidate = Path.of("").toAbsolutePath();
        for (int i = 0; i < 4 && candidate != null; i++) {
            if (Files.isDirectory(candidate.resolve("doc/DesignPatterns"))) {
                return candidate;
            }
            candidate = candidate.getParent();
        }
        return Path.of("").toAbsolutePath();
    }

    @Test
    void matchesTheCommittedFixtureExceptForFieldsThatMustDiffer() throws Exception {
        JSONObject expected = new JSONObject(Files.readString(FIXTURE));
        JSONObject actual = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);

        assertEquals(expected.getString("snapshotVersion"), actual.getString("snapshotVersion"));
        // The command inventory is read from plugin jars on the classpath. This module does not
        // depend on the plugins, so it is compared separately in the root module where they are
        // present; here the rest of the project section has to match exactly.
        assertEquals(withoutClasspathDependentFields(expected.getJSONObject("project")).toString(),
                withoutClasspathDependentFields(actual.getJSONObject("project")).toString(),
                "project section");
        assertEquals(expected.getJSONObject("script").toString(),
                actual.getJSONObject("script").toString(), "script section");

        JSONObject expectedFlow = expected.getJSONObject("flow");
        JSONObject actualFlow = actual.getJSONObject("flow");
        assertEquals(expectedFlow.getString("rootId"), actualFlow.getString("rootId"));
        assertEquals(sortedStrings(expectedFlow.getJSONArray("startNodeIds")),
                sortedStrings(actualFlow.getJSONArray("startNodeIds")), "start nodes");
        assertEquals(expectedFlow.getJSONArray("variables").toString(),
                actualFlow.getJSONArray("variables").toString(), "variables");
        assertEquals(expectedFlow.getJSONArray("allowedEdgeTypes").toString(),
                actualFlow.getJSONArray("allowedEdgeTypes").toString(), "allowed edge types");
        assertEquals(expectedFlow.getJSONArray("nodes").length(),
                actualFlow.getJSONArray("nodes").length(), "node count");
        assertEquals(expectedFlow.getJSONArray("edges").length(),
                actualFlow.getJSONArray("edges").length(), "edge count");
    }

    /** Node identity and structure must survive the move, whatever order the model yields them in. */
    @Test
    void reportsTheSameNodesAndEdgesAsTheFixture() throws Exception {
        JSONObject expected = new JSONObject(Files.readString(FIXTURE));
        JSONObject actual = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);

        assertEquals(nodeKeys(expected), nodeKeys(actual), "node id, parent and supernode flag");
        assertEquals(edgeKeys(expected), edgeKeys(actual), "edge type, source, target and condition");
    }

    /**
     * Counts the commands a node actually carries.
     *
     * <p>The Groovy generator this replaced counted {@code <Command>} elements, which the XML never
     * contains: the real children are PlayAction, PlayScene and Assignment. Every snapshot it ever
     * produced therefore reported zero commands for every node, so a consumer asking "does this node
     * do anything" always got no. Reading the model fixes it.
     */
    @Test
    void countsTheCommandsANodeActuallyCarries() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);

        assertEquals(2, commandCountOf(snapshot, "N26"), "N26 has a PlayAction and an Assignment");
        assertEquals(1, commandCountOf(snapshot, "N41"), "N41 has a PlayAction");
        assertEquals(2, commandCountOf(snapshot, "N42"), "N42 has a PlayAction and an Assignment");
        assertEquals(0, commandCountOf(snapshot, "N1"), "N1 carries no commands");
    }

    private int commandCountOf(final JSONObject snapshot, final String nodeId) {
        JSONArray nodes = snapshot.getJSONObject("flow").getJSONArray("nodes");
        for (int i = 0; i < nodes.length(); i++) {
            if (nodeId.equals(nodes.getJSONObject(i).getString("id"))) {
                return nodes.getJSONObject(i).getInt("commandCount");
            }
        }
        throw new AssertionError("No such node in the snapshot: " + nodeId);
    }

    @Test
    void scenesCarryTheirSpeakersAndParameters() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONArray scenes = snapshot.getJSONObject("script").getJSONArray("scenes");

        assertEquals(1, scenes.length(), "DesignPatterns declares one scene");
        JSONObject welcome = scenes.getJSONObject(0);
        assertEquals("Welcome", welcome.getString("name"));
        assertEquals("[\"Anne\"]", welcome.getJSONArray("speakers").toString());
        assertEquals("[\"de\"]", welcome.getJSONArray("languages").toString());
    }

    /** Start nodes live in a hash map, so without sorting a regenerated snapshot shows false diffs. */
    @Test
    void isStableAcrossRepeatedBuilds() throws Exception {
        JSONObject first = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONObject second = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);

        assertEquals(first.getJSONObject("flow").toString(), second.getJSONObject("flow").toString());
        assertEquals(first.getJSONObject("script").toString(), second.getJSONObject("script").toString());
    }

    /**
     * Reading a project must leave it byte for byte as it was.
     *
     * <p>A project.xml carrying no uuid normally has a generated one written back on load, so
     * snapshotting a directory used to modify it. Anyone running this over a corpus of other
     * people's projects would have left a modified file in every one of them.
     */
    @Test
    void readingAProjectDoesNotModifyItOnDisk() throws Exception {
        Path source = REPO_ROOT.resolve("doc/IntakeInterview");
        Path copy = Files.createTempDirectory("snapshot-readonly");
        try (var entries = Files.list(source)) {
            for (Path entry : entries.filter(Files::isRegularFile).toList()) {
                Files.copy(entry, copy.resolve(entry.getFileName()));
            }
        }
        // The fixture projects already carry a uuid, so remove it to recreate the case that writes.
        Path projectXml = copy.resolve("project.xml");
        Files.writeString(projectXml,
                Files.readString(projectXml).replaceAll(" uuid=\"[^\"]*\"", ""));
        String before = Files.readString(projectXml);

        CapabilitySnapshotBuilder.buildFromDirectory(copy);

        assertEquals(before, Files.readString(projectXml),
                "Describing a project must not write a uuid back into it");
    }

    /**
     * Describing a project must not start its devices. parse() ends with loadRunTimePlugins(), which
     * is why the builder uses parseForInformation() instead.
     */
    @Test
    void readingAProjectDoesNotLaunchItsPlugins() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONArray plugins = snapshot.getJSONObject("project").getJSONArray("plugins");

        assertFalse(plugins.isEmpty(), "DesignPatterns declares a timer plugin");
        assertTrue(plugins.getJSONObject(0).getBoolean("load"),
                "The plugin is declared as loadable, and describing it must still not load it");
    }

    private List<String> nodeKeys(final JSONObject snapshot) {
        List<String> keys = new ArrayList<>();
        JSONArray nodes = snapshot.getJSONObject("flow").getJSONArray("nodes");
        for (int i = 0; i < nodes.length(); i++) {
            JSONObject node = nodes.getJSONObject(i);
            keys.add(node.getString("id") + "|" + node.getString("parentSuperNodeId")
                    + "|" + node.getBoolean("isSuperNode") + "|" + node.getBoolean("isHistoryNode"));
        }
        keys.sort(String::compareTo);
        return keys;
    }

    private List<String> edgeKeys(final JSONObject snapshot) {
        List<String> keys = new ArrayList<>();
        JSONArray edges = snapshot.getJSONObject("flow").getJSONArray("edges");
        for (int i = 0; i < edges.length(); i++) {
            JSONObject edge = edges.getJSONObject(i);
            keys.add(edge.getString("type") + "|" + edge.getString("sourceNodeId")
                    + "|" + edge.getString("targetNodeId")
                    + "|" + edge.optString("conditionText", "")
                    + "|" + edge.optLong("timeoutMs", -1)
                    + "|" + edge.optInt("probability", -1));
        }
        keys.sort(String::compareTo);
        return keys;
    }

    private List<String> sortedStrings(final JSONArray array) {
        List<String> out = new ArrayList<>();
        for (int i = 0; i < array.length(); i++) {
            out.add(array.getString(i));
        }
        out.sort(String::compareTo);
        return out;
    }

    // ---- plugin command inventory and screen bindings ----

    /**
     * Every plugin carries the three inventory fields, whether or not this runtime can fill them.
     *
     * <p>Their contents come from plugin-properties.json on the classpath, which this module does not
     * depend on, so a snapshot built here reports no commands. That is the same thing a deployment
     * without plugin jars would report, and it is why the contents are asserted in the root module
     * instead, in CapabilitySnapshotCommandInventoryTest.
     */
    @Test
    void everyPluginCarriesTheInventoryFields() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONObject timer = pluginNamed(snapshot, "TimerExecutor");

        assertNotNull(timer, "DesignPatterns declares the timer plugin");
        for (String field : new String[] {"commands", "writesVariables", "readsVariables"}) {
            assertNotNull(timer.optJSONArray(field), field + " must always be present");
        }
    }

    /** The agent to plugin join has to actually resolve, or the inventory is unusable. */
    @Test
    void everyAgentResolvesToAPluginThatDeclaresCommands() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONArray agents = snapshot.getJSONObject("project").getJSONArray("agents");

        assertFalse(agents.isEmpty());
        for (int i = 0; i < agents.length(); i++) {
            String device = agents.getJSONObject(i).getString("device");
            assertNotNull(pluginNamed(snapshot, device),
                    "Agent " + agents.getJSONObject(i).getString("name")
                            + " names device " + device + ", which no plugin provides");
        }
    }

    /**
     * The direction is the point: a variable a screen reads has to hold a value before the screen is
     * shown, while one it writes is set by the person using it.
     */
    @Test
    void screensReportWhichVariablesTheyReadAndWrite() throws Exception {
        Path example = REPO_ROOT.resolve("plugins/charamel-embed/ExampleProject");
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(example);
        JSONArray screens = snapshot.getJSONObject("screens").getJSONArray("screens");

        assertFalse(screens.isEmpty(), "ExampleProject defines screens");
        JSONObject start = screens.getJSONObject(0);

        // Sliders bind a variable for display; buttons send one back to the flow.
        assertTrue(start.getJSONArray("readsVariables").toList().contains("emo_intensity"),
                "A slider bound to emo_intensity reads it");
        assertTrue(start.getJSONArray("writesVariables").toList().contains("emo_type"),
                "A button sending emo_type writes it");
        assertFalse(start.getJSONArray("readsVariables").toList().contains("emo_type"),
                "A variable a control sends must not be reported as one the screen reads");
    }

    @Test
    void theCharacterFrameSourceVariableIsReportedSeparately() throws Exception {
        Path example = REPO_ROOT.resolve("plugins/charamel-embed/ExampleProject");
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(example);

        assertEquals("character_url",
                snapshot.getJSONObject("screens").getString("characterSrcVariable"),
                "The character frame binding belongs to the project, not to a single screen");
    }

    /** A project without screens.json is normal and must not fail the snapshot. */
    @Test
    void aProjectWithoutScreensReportsNone() throws Exception {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        assertTrue(snapshot.getJSONObject("screens").getJSONArray("screens").isEmpty());
    }

    /** A snapshot without its screens is still worth having, so a broken file must not fail it. */
    @Test
    void anUnreadableScreensFileDoesNotFailTheSnapshot() throws Exception {
        Path copy = Files.createTempDirectory("snapshot-bad-screens");
        try (var entries = Files.list(DESIGN_PATTERNS)) {
            for (Path entry : entries.filter(Files::isRegularFile).toList()) {
                Files.copy(entry, copy.resolve(entry.getFileName()));
            }
        }
        Files.writeString(copy.resolve("screens.json"), "{ this is not json");

        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(copy);
        assertTrue(snapshot.getJSONObject("screens").getJSONArray("screens").isEmpty());
        assertFalse(snapshot.getJSONObject("flow").getJSONArray("nodes").isEmpty(),
                "The rest of the snapshot must still be there");
    }

    private JSONObject withoutClasspathDependentFields(final JSONObject project) {
        JSONObject copy = new JSONObject(project.toString());
        JSONArray plugins = copy.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            JSONObject plugin = plugins.getJSONObject(i);
            plugin.remove("commands");
            plugin.remove("writesVariables");
            plugin.remove("readsVariables");
        }
        return copy;
    }

    private JSONObject pluginNamed(final JSONObject snapshot, final String name) {
        JSONArray plugins = snapshot.getJSONObject("project").getJSONArray("plugins");
        for (int i = 0; i < plugins.length(); i++) {
            if (name.equals(plugins.getJSONObject(i).getString("name"))) {
                return plugins.getJSONObject(i);
            }
        }
        return null;
    }
}
