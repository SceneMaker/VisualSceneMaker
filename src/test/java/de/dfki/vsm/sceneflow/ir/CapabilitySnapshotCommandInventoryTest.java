package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.web.CapabilitySnapshotBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the part of the capability snapshot that only works when plugin jars are on the classpath.
 *
 * <p>Lives in the root module rather than beside the builder because the command inventory is read
 * from the plugin-properties.json each plugin ships, and core-webserver does not depend on the
 * plugins. A snapshot built without them reports no commands, which is correct for a deployment
 * without plugin jars and useless for testing that the inventory works.
 */
class CapabilitySnapshotCommandInventoryTest {

    private static final Path DESIGN_PATTERNS = Path.of("doc/DesignPatterns");
    private static final Path CHARAMEL_EXAMPLE = Path.of("plugins/charamel-embed/ExampleProject");

    @Test
    void pluginsCarryTheCommandsTheyDeclare() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(DESIGN_PATTERNS);
        JSONArray commands = pluginNamed(snapshot, "TimerExecutor").getJSONArray("commands");

        assertFalse(commands.isEmpty(), "The timer plugin declares commands");
        JSONObject init = commandNamed(commands, "init");
        assertNotNull(init, "Expected the timer's init command, got: " + names(commands));
        assertFalse(init.getJSONArray("params").isEmpty(), "init takes an id parameter");
    }

    /**
     * An agent reaches its commands through agent.device to plugin.name to plugin.commands. If that
     * join does not resolve, the inventory cannot be used to answer "what can this agent do".
     */
    @Test
    void everyAgentResolvesToAPluginCarryingCommands() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);
        JSONArray agents = snapshot.getJSONObject("project").getJSONArray("agents");

        assertFalse(agents.isEmpty());
        for (int i = 0; i < agents.length(); i++) {
            String agentName = agents.getJSONObject(i).getString("name");
            String device = agents.getJSONObject(i).getString("device");
            JSONObject plugin = pluginNamed(snapshot, device);

            assertNotNull(plugin, "Agent " + agentName + " names device " + device
                    + ", which no plugin provides");
            assertFalse(plugin.getJSONArray("commands").isEmpty(),
                    "Agent " + agentName + " resolves to " + device + ", which declares no commands");
        }
    }

    /** Two agents on one plugin offer the same commands, which is why they are not repeated. */
    @Test
    void commandsAreReportedOncePerPluginRatherThanPerAgent() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);
        JSONArray agents = snapshot.getJSONObject("project").getJSONArray("agents");

        for (int i = 0; i < agents.length(); i++) {
            assertFalse(agents.getJSONObject(i).has("commands"),
                    "An agent must not carry its own copy of the plugin's commands");
        }
        assertEquals(pluginNamed(snapshot, "CharamelEmbedXenia").getJSONArray("commands").length(),
                pluginNamed(snapshot, "CharamelEmbedBob").getJSONArray("commands").length(),
                "Both agents run the same plugin class, so both offer the same commands");
    }

    @Test
    void pluginsReportTheVariablesTheyWrite() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);
        JSONArray writes = pluginNamed(snapshot, "webpage").getJSONArray("writesVariables");

        assertFalse(writes.isEmpty(), "htmlgui-ws declares variables it writes");
        boolean hasConversationLog = false;
        for (int i = 0; i < writes.length(); i++) {
            hasConversationLog |= "conversation_log".equals(writes.getJSONObject(i).getString("name"));
        }
        assertTrue(hasConversationLog, "Expected conversation_log among the declared writes");
    }

    /**
     * Two characters on one plugin class must be tellable apart by the variables they set.
     *
     * <p>Both instances declare the same config keys, so the declared name alone says nothing about
     * which character it belongs to. Only the variable each instance is bound to can appear in a
     * condition, so without the binding "wait until Xenia is ready" cannot be written at all.
     */
    @Test
    void eachPluginInstanceReportsTheVariablesItIsBoundTo() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);

        assertEquals("avatar_ready",
                writtenVariable(snapshot, "CharamelEmbedXenia", "characterReady"));
        assertEquals("bob_ready",
                writtenVariable(snapshot, "CharamelEmbedBob", "characterReady"));
        assertEquals("avatar_connected",
                writtenVariable(snapshot, "CharamelEmbedXenia", "sceneflowVar"));
    }

    /**
     * Connecting and being ready are different moments, and an author has to be able to tell which
     * is which. charamel-embed sets one when the page connects and the other only once the model is
     * loaded and audio is unlocked; speaking in between fails silently.
     */
    @Test
    void declaredVariablesCarryWhatThePluginMeansByThem() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);
        JSONArray writes = pluginNamed(snapshot, "CharamelEmbedXenia").getJSONArray("writesVariables");

        String connected = null;
        String ready = null;
        for (int i = 0; i < writes.length(); i++) {
            JSONObject entry = writes.getJSONObject(i);
            if ("sceneflowVar".equals(entry.getString("name"))) {
                connected = entry.optString("description", "");
            }
            if ("characterReady".equals(entry.getString("name"))) {
                ready = entry.optString("description", "");
            }
        }
        assertNotNull(connected);
        assertNotNull(ready);
        assertFalse(connected.isBlank(), "A connection variable has to say what connecting means");
        assertFalse(ready.equals(connected), "Connected and ready must not read as the same thing");
    }

    /**
     * A project that sets no feature still gets the plugin's declared default at runtime.
     *
     * <p>doc/IntakeInterview binds no connection variable and its flow nevertheless waits on
     * gui_connected, which is htmlgui-ws's default for sceneflowStateVar. Reporting that as unbound
     * would hide the variable the flow already depends on.
     */
    @Test
    void anUnsetBindingFallsBackToThePluginsDeclaredDefault() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(Path.of("doc/IntakeInterview"));

        assertEquals("gui_connected", writtenVariable(snapshot, "HtmlGuiWs", "sceneflowStateVar"));
    }

    /**
     * htmlgui-ws has to declare the agent features it reads, or an agent added to it stays mute.
     *
     * <p>A spoken line is appended to the variable named by the agent's own {@code var} feature, and
     * an agent with none appends nowhere: the scene plays, the flow moves on, and nothing appears.
     * The declaration is what lets both the add-agent dialog and the Flow Assistant fill it in, so an
     * author never has to know that the feature exists.
     */
    @Test
    void theWebInterfaceDeclaresTheAgentFeaturesItReads() {
        JSONObject spec = de.dfki.vsm.web.WebUiServer.agentSpecForClassName(
                "de.dfki.vsm.xtension.responsiveweb.HtmlGuiWsExecutor");
        assertNotNull(spec, "htmlgui-ws has to declare an agent spec");
        JSONArray fixed = spec.optJSONArray("fixed");
        assertNotNull(fixed);

        JSONObject var = null;
        for (int i = 0; i < fixed.length(); i++) {
            if ("var".equals(fixed.getJSONObject(i).optString("name"))) {
                var = fixed.getJSONObject(i);
            }
        }
        assertNotNull(var, "Expected a declared \"var\" feature, got: " + fixed);
        assertEquals("conversation_log", var.optString("default"),
                "The default has to be the variable the chat screen's feed reads");
    }

    /** A command is only usable if its parameters come with it. */
    @Test
    void commandParametersCarryTypeAndWhetherTheyAreRequired() {
        JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(CHARAMEL_EXAMPLE);
        JSONArray commands = pluginNamed(snapshot, "webpage").getJSONArray("commands");

        JSONObject withParams = null;
        for (int i = 0; i < commands.length() && withParams == null; i++) {
            if (!commands.getJSONObject(i).getJSONArray("params").isEmpty()) {
                withParams = commands.getJSONObject(i);
            }
        }
        assertNotNull(withParams, "htmlgui-ws has commands taking parameters");

        JSONObject param = withParams.getJSONArray("params").getJSONObject(0);
        assertTrue(param.has("name") && param.has("type") && param.has("required"),
                "A parameter needs a name, a type and whether it is required: " + param);
    }

    /** The flow variable a plugin instance's declared write is wired to. */
    private String writtenVariable(
            final JSONObject snapshot, final String pluginName, final String declaredName) {
        JSONObject plugin = pluginNamed(snapshot, pluginName);
        assertNotNull(plugin, "No plugin named " + pluginName);
        JSONArray writes = plugin.getJSONArray("writesVariables");
        for (int i = 0; i < writes.length(); i++) {
            if (declaredName.equals(writes.getJSONObject(i).getString("name"))) {
                return writes.getJSONObject(i).optString("boundTo", null);
            }
        }
        return null;
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

    private JSONObject commandNamed(final JSONArray commands, final String name) {
        for (int i = 0; i < commands.length(); i++) {
            if (name.equals(commands.getJSONObject(i).getString("name"))) {
                return commands.getJSONObject(i);
            }
        }
        return null;
    }

    private String names(final JSONArray commands) {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < commands.length(); i++) {
            out.append(i == 0 ? "" : ", ").append(commands.getJSONObject(i).optString("name"));
        }
        return out.toString();
    }
}
