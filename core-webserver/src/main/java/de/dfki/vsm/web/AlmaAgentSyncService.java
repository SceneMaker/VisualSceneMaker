package de.dfki.vsm.web;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

/**
 * Auto-registers a "&lt;CharacterAffect&gt;_alma" VSM agent, and a matching set of per-character
 * "&lt;CharacterAffect&gt;_alma_&lt;field&gt;" SceneFlow variables, for every character defined in
 * an ALMA device's configured project XML — so an author gets usable agents/variables without
 * hand-typing name/device pairs or fixed global variable names that would otherwise collide across
 * characters. Runs whenever a project's config is saved (add-device dialog, project settings, Flow
 * Assistant device-add) rather than at Play time: CapabilitySnapshotBuilder never launches plugins
 * to describe a project, so anything derived only inside ALMAExecutor.launch() would stay invisible
 * to the editor/Flow Assistant until the project had been run once.
 *
 * <p>Additive and idempotent only: an agent/variable name already present is left untouched, and a
 * character removed from the XML does not delete its previously-synced agent or variables.
 */
final class AlmaAgentSyncService {

    private static final String ALMA_CLASS_NAME = "de.dfki.vsm.xtension.alma.ALMAExecutor";
    private static final String AGENT_SUFFIX = "_alma";

    /** Matches ALMAExecutor's onAffectInfo/onEmotionVector variable-name suffixes. */
    private static final String[] VARIABLE_SUFFIXES = {
            "dominantemotion", "dominantemotionintensity", "mood", "moodtendency", "emotions"
    };

    private static final LOGConsoleLogger sLogger = LOGConsoleLogger.getInstance();

    private AlmaAgentSyncService() {
    }

    static void sync(final ProjectConfig cfg, final Path projectDir) {
        forEachAlmaCharacter(cfg, projectDir, (deviceName, character) ->
                addAgentIfMissing(cfg, character + AGENT_SUFFIX, deviceName));
    }

    /** @return true if at least one variable was newly declared, so the caller can decide to broadcast. */
    static boolean syncVariables(final ProjectConfig cfg, final SceneFlow sceneFlow, final Path projectDir) {
        if (sceneFlow == null) {
            return false;
        }
        boolean[] changed = {false};
        forEachAlmaCharacter(cfg, projectDir, (deviceName, character) -> {
            for (String suffix : VARIABLE_SUFFIXES) {
                if (addVariableIfMissing(sceneFlow, character + AGENT_SUFFIX + "_" + suffix)) {
                    changed[0] = true;
                }
            }
        });
        return changed[0];
    }

    private interface CharacterAction {
        void accept(String deviceName, String character);
    }

    private static void forEachAlmaCharacter(final ProjectConfig cfg, final Path projectDir, final CharacterAction action) {
        if (cfg == null || projectDir == null) {
            return;
        }
        for (PluginConfig plugin : cfg.getPluginConfigList()) {
            if (!ALMA_CLASS_NAME.equals(plugin.getClassName())) {
                continue;
            }
            String projectRel = plugin.getProperty("project");
            if (projectRel == null || projectRel.isBlank()) {
                continue;
            }
            File file = projectDir.resolve(projectRel).toFile();
            if (!file.isFile()) {
                continue;
            }
            String deviceName = plugin.getPluginName();
            for (String character : readCharacterNames(file)) {
                action.accept(deviceName, character);
            }
        }
    }

    private static void addAgentIfMissing(final ProjectConfig cfg, final String agentName, final String deviceName) {
        for (AgentConfig existing : cfg.getAgentConfigList()) {
            if (agentName.equalsIgnoreCase(existing.getAgentName())) {
                return;
            }
        }
        cfg.getAgentConfigList().add(new AgentConfig(agentName, deviceName));
        sLogger.message("[alma] auto-registered agent " + agentName + " -> " + deviceName);
    }

    private static boolean addVariableIfMissing(final SceneFlow sceneFlow, final String varName) {
        for (VariableDefinition existing : sceneFlow.getVarDefList()) {
            if (varName.equals(existing.getName())) {
                return false;
            }
        }
        sceneFlow.getVarDefList().add(new VariableDefinition(varName, "String", new StringLiteral("")));
        sLogger.message("[alma] auto-declared variable " + varName);
        return true;
    }

    private static Set<String> readCharacterNames(final File file) {
        Set<String> names = new HashSet<>();
        try {
            Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(file);
            NodeList list = doc.getElementsByTagName("CharacterAffect");
            for (int i = 0; i < list.getLength(); i++) {
                Element element = (Element) list.item(i);
                String name = element.getAttribute("name").trim();
                if (!name.isEmpty()) {
                    names.add(name);
                }
            }
        } catch (Exception ex) {
            sLogger.warning("[alma] could not read character names from " + file + ": " + ex.getMessage());
        }
        return names;
    }
}
