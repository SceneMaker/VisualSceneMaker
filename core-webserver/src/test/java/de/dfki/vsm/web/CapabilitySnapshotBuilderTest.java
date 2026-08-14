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
        assertEquals(expected.getJSONObject("project").toString(),
                actual.getJSONObject("project").toString(), "project section");
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
}
