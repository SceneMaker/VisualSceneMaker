package de.dfki.vsm.web;

import de.dfki.vsm.model.project.PluginConfig;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class PortPoolManagerTest {

    private Path dir;

    @BeforeEach
    void setUp() throws IOException {
        dir = Files.createTempDirectory("vsm-port-pool-test");
    }

    @AfterEach
    void tearDown() throws IOException {
        try (var walk = Files.walk(dir)) {
            walk.sorted(Comparator.reverseOrder()).forEach(p -> {
                try {
                    Files.delete(p);
                } catch (IOException ignored) {
                    // best-effort cleanup
                }
            });
        }
    }

    private PluginConfig htmlguiWsLike() {
        PluginConfig pc = new PluginConfig("device", "webpage", "de.dfki.vsm.xtension.responsiveweb.HtmlGuiWsExecutor");
        pc.addProperty("wss_port", "4040");
        pc.addProperty("ws_port", "4041");
        pc.addProperty("html_port", "8080");
        pc.addProperty("sceneflowStateVar", "gui_connected"); // not a port — must survive untouched
        return pc;
    }

    private PluginConfig charamelEmbedLike(String name) {
        PluginConfig pc = new PluginConfig("device", name, "de.dfki.vsm.xtension.charamelEmbed.CharamelEmbedExecutor");
        pc.addProperty("port", "3040");
        return pc;
    }

    @Test
    void allocatesOnePortPerPortKeyAcrossAllConfigs() {
        PortPoolManager pool = new PortPoolManager(20000, 10, dir.resolve("registry.json"));
        List<PluginConfig> configs = List.of(htmlguiWsLike(), charamelEmbedLike("CharamelEmbedXenia"));

        pool.ensureAllocated("project-1", configs);

        // 3 htmlgui-ws ports + 1 charamel-embed port = 4 allocated, none colliding, none reused
        assertEquals(6, pool.freeCount()); // 10 - 4
        java.util.Set<String> assignedValues = new java.util.HashSet<>();
        for (PluginConfig pc : configs) {
            for (var f : pc.getEntryList()) {
                if (f.getKey().toLowerCase().endsWith("port")) {
                    assertTrue(assignedValues.add(f.getValue()), "port reused: " + f.getValue());
                    int port = Integer.parseInt(f.getValue());
                    assertTrue(port >= 20000 && port < 20010);
                }
            }
        }
        assertEquals("gui_connected", configs.get(0).getProperty("sceneflowStateVar")); // untouched
    }

    @Test
    void secondEnsureAllocatedForSameOwnerIsANoOp() {
        PortPoolManager pool = new PortPoolManager(20000, 10, dir.resolve("registry.json"));
        PluginConfig config = charamelEmbedLike("CharamelEmbedXenia");

        pool.ensureAllocated("project-1", List.of(config));
        String firstPort = config.getProperty("port");
        int freeAfterFirst = pool.freeCount();

        // Same owner, called again (e.g. a second Runtime.Start) — must not allocate again,
        // since charamel-embed's transport never rebinds to a new port anyway (see class docs).
        pool.ensureAllocated("project-1", List.of(config));

        assertEquals(firstPort, config.getProperty("port"));
        assertEquals(freeAfterFirst, pool.freeCount());
    }

    @Test
    void twoDifferentOwnersGetNonOverlappingPorts() {
        PortPoolManager pool = new PortPoolManager(20000, 10, dir.resolve("registry.json"));
        PluginConfig configA = charamelEmbedLike("CharamelEmbedXenia");
        PluginConfig configB = charamelEmbedLike("CharamelEmbedXenia"); // same plugin name, different project

        pool.ensureAllocated("project-A", List.of(configA));
        pool.ensureAllocated("project-B", List.of(configB));

        assertNotEquals(configA.getProperty("port"), configB.getProperty("port"));
        assertEquals(8, pool.freeCount());
    }

    @Test
    void releaseReturnsPortsForReuse() {
        PortPoolManager pool = new PortPoolManager(20000, 2, dir.resolve("registry.json"));
        PluginConfig config = charamelEmbedLike("CharamelEmbedXenia");
        pool.ensureAllocated("project-1", List.of(config));
        assertEquals(1, pool.freeCount());

        pool.release("project-1");
        assertEquals(2, pool.freeCount());
        assertFalse(pool.isAllocated("project-1"));
    }

    @Test
    void releaseOnUnknownOwnerIsANoOp() {
        PortPoolManager pool = new PortPoolManager(20000, 5, dir.resolve("registry.json"));
        assertDoesNotThrow(() -> pool.release("never-allocated"));
        assertEquals(5, pool.freeCount());
    }

    @Test
    void exhaustionThrowsExplicitErrorRatherThanQueueing() {
        // Decision 16: explicit error, not silent queueing.
        PortPoolManager pool = new PortPoolManager(20000, 2, dir.resolve("registry.json"));
        pool.ensureAllocated("project-A", List.of(charamelEmbedLike("CharamelEmbedXenia"))); // takes 1
        pool.ensureAllocated("project-B", List.of(charamelEmbedLike("CharamelEmbedXenia"))); // takes the last 1

        PluginConfig needsTwo = htmlguiWsLike(); // needs 3 ports, only 0 free
        assertThrows(PortPoolManager.PortPoolExhaustedException.class,
                () -> pool.ensureAllocated("project-C", List.of(needsTwo)));
        // A failed allocation must not have partially consumed the pool or recorded project-C.
        assertFalse(pool.isAllocated("project-C"));
    }

    @Test
    void configWithNoPortPropertiesAllocatesNothingButIsRememberedAsChecked() {
        PortPoolManager pool = new PortPoolManager(20000, 5, dir.resolve("registry.json"));
        PluginConfig noPorts = new PluginConfig("device", "Timer", "de.dfki.vsm.xtension.timer.TimerExecutor");
        noPorts.addProperty("_specVersion", "1.0");

        pool.ensureAllocated("project-1", List.of(noPorts));

        assertEquals(5, pool.freeCount());
        assertTrue(pool.isAllocated("project-1")); // remembered so it's not rescanned every launch
    }

    @Test
    void writesRegistryFileReflectingCurrentAllocations() throws IOException {
        Path registry = dir.resolve("registry.json");
        PortPoolManager pool = new PortPoolManager(20000, 10, registry);
        pool.ensureAllocated("project-1", List.of(charamelEmbedLike("CharamelEmbedXenia")));

        assertTrue(Files.exists(registry));
        JSONObject root = new JSONObject(Files.readString(registry, StandardCharsets.UTF_8));
        assertTrue(root.has("project-1"));
        JSONObject details = root.getJSONObject("project-1");
        assertTrue(details.has("CharamelEmbedXenia.port"));

        pool.release("project-1");
        JSONObject afterRelease = new JSONObject(Files.readString(registry, StandardCharsets.UTF_8));
        assertFalse(afterRelease.has("project-1"));
    }
}
