package de.dfki.vsm.model.plugin;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PluginCommandTest {

    private static JSONObject readPluginProperties(String relativePath) throws IOException {
        String text = Files.readString(Path.of(relativePath));
        return new JSONObject(text);
    }

    @Test
    void parsesCommandsWithRichParamMetadata() throws IOException {
        JSONObject root = readPluginProperties("../plugins/htmlgui-ws/src/main/resources/plugin-properties.json");
        List<PluginCommand> commands = PluginCommand.fromJsonArray(root.optJSONArray("commands"));
        assertFalse(commands.isEmpty());

        PluginCommand appendMessage = commands.stream()
                .filter(c -> "appendMessage".equals(c.getName()))
                .findFirst()
                .orElseThrow();
        assertEquals("action", appendMessage.getType());
        assertEquals(3, appendMessage.getParams().size());

        CommandParam role = appendMessage.getParams().stream()
                .filter(p -> "role".equals(p.getName()))
                .findFirst()
                .orElseThrow();
        assertEquals("string", role.getType());
        assertFalse(role.isRequired());
        assertEquals("agent", role.getDefault());
    }

    @Test
    void parsesCommandsWithEmptyParams() throws IOException {
        JSONObject root = readPluginProperties("../plugins/charamel-ws/src/main/resources/plugin-properties.json");
        List<PluginCommand> commands = PluginCommand.fromJsonArray(root.optJSONArray("commands"));
        assertFalse(commands.isEmpty());

        PluginCommand stop = commands.stream()
                .filter(c -> "stop".equals(c.getName()))
                .findFirst()
                .orElseThrow();
        assertTrue(stop.getParams().isEmpty());
    }

    @Test
    void toleratesMissingOrNullCommandsArray() {
        assertTrue(PluginCommand.fromJsonArray(null).isEmpty());
    }
}
