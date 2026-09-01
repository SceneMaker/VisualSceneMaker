package de.dfki.vsm.web;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Exercises AlmaAgentSyncService against the plugin's own bundled default project XML. */
class AlmaAgentSyncServiceTest {

    private static final Path REPO_ROOT = repoRoot();
    private static final Path DEFAULT_PROJECT_XML =
            REPO_ROOT.resolve("plugins/alma/src/main/resources/templates/alma/default-project.xml");

    private static Path repoRoot() {
        Path candidate = Path.of("").toAbsolutePath();
        for (int i = 0; i < 4 && candidate != null; i++) {
            if (Files.isDirectory(candidate.resolve("plugins/alma"))) {
                return candidate;
            }
            candidate = candidate.getParent();
        }
        throw new IllegalStateException("Could not locate repository root from " + Path.of("").toAbsolutePath());
    }

    private ProjectConfig projectWithAlmaDevice(String projectRelPath) {
        ProjectConfig cfg = new ProjectConfig();
        ArrayList<de.dfki.vsm.model.config.ConfigFeature> features = new ArrayList<>();
        features.add(new de.dfki.vsm.model.config.ConfigFeature("Feature", "project", projectRelPath));
        PluginConfig plugin = new PluginConfig("device", "alma", "de.dfki.vsm.xtension.alma.ALMAExecutor", true, features);
        cfg.getPluginConfigList().add(plugin);
        return cfg;
    }

    @Test
    void syncAddsOneAgentPerCharacter() {
        ProjectConfig cfg = projectWithAlmaDevice("plugins/alma/src/main/resources/templates/alma/default-project.xml");

        AlmaAgentSyncService.sync(cfg, REPO_ROOT);

        List<String> agentNames = cfg.getAgentConfigList().stream().map(AgentConfig::getAgentName).toList();
        assertTrue(agentNames.contains("Anne_alma"));
        assertTrue(agentNames.contains("Bruno_alma"));
        assertTrue(agentNames.contains("Clementine_alma"));
        assertEquals(3, agentNames.size());
        assertEquals("alma", cfg.getAgentConfig("Anne_alma").getDeviceName());
    }

    @Test
    void syncIsIdempotent() {
        ProjectConfig cfg = projectWithAlmaDevice("plugins/alma/src/main/resources/templates/alma/default-project.xml");

        AlmaAgentSyncService.sync(cfg, REPO_ROOT);
        AlmaAgentSyncService.sync(cfg, REPO_ROOT);

        assertEquals(3, cfg.getAgentConfigList().size());
    }

    @Test
    void syncVariablesDeclaresFiveVarsPerCharacter() {
        ProjectConfig cfg = projectWithAlmaDevice("plugins/alma/src/main/resources/templates/alma/default-project.xml");
        SceneFlow sceneFlow = new SceneFlow();

        boolean changed = AlmaAgentSyncService.syncVariables(cfg, sceneFlow, REPO_ROOT);

        assertTrue(changed);
        List<String> varNames = sceneFlow.getVarDefList().stream().map(VariableDefinition::getName).toList();
        assertEquals(15, varNames.size());
        assertTrue(varNames.contains("Anne_alma_dominantemotion"));
        assertTrue(varNames.contains("Anne_alma_dominantemotionintensity"));
        assertTrue(varNames.contains("Anne_alma_mood"));
        assertTrue(varNames.contains("Anne_alma_moodtendency"));
        assertTrue(varNames.contains("Anne_alma_emotions"));

        boolean changedAgain = AlmaAgentSyncService.syncVariables(cfg, sceneFlow, REPO_ROOT);
        assertFalse(changedAgain);
        assertEquals(15, sceneFlow.getVarDefList().size());
    }

    @Test
    void syncIgnoresNonAlmaPluginsAndMissingProjectFile() {
        ProjectConfig cfg = new ProjectConfig();
        cfg.getPluginConfigList().add(new PluginConfig("device", "console", "de.dfki.vsm.xtension.console.ConsoleExecutor", true));

        AlmaAgentSyncService.sync(cfg, REPO_ROOT);
        assertTrue(cfg.getAgentConfigList().isEmpty());

        ProjectConfig missingFile = projectWithAlmaDevice("nowhere/does-not-exist.xml");
        AlmaAgentSyncService.sync(missingFile, REPO_ROOT);
        assertTrue(missingFile.getAgentConfigList().isEmpty());
    }
}
