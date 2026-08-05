package de.dfki.vsm.web;

import de.dfki.vsm.model.behavior.BehaviorTag;
import de.dfki.vsm.model.behavior.BehaviorTaxonomy;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Extracts the behavior-command placement corpus: one JSONL record per analysed sentence.
 *
 * <pre>
 * ./gradlew extractCorpus -PvsmProject=doc/IntakeInterview,plugins/charamel-embed/ExampleProject \
 *                         -Pannotator=pg -Pout=build/corpus.jsonl
 * </pre>
 *
 * <h2>What a record is for</h2>
 * A record carries the whole decision an author faced, not only what they chose:
 * <ul>
 *   <li>{@code anchors} — every structural position a command <em>could</em> have taken. These are the
 *       negative examples, and a placement model is useless without them.</li>
 *   <li>{@code placements} — what the author actually did, each resolved to an anchor slot and to a
 *       NEUROGES Function via the behavior taxonomy.</li>
 * </ul>
 * Sentences with no commands are kept deliberately: "the author placed nothing here" is evidence.
 *
 * <h2>Multi-annotator from the start</h2>
 * {@code annotator} and {@code scenario} are recorded per record, because several people will annotate
 * the same scenarios and the agreement report (plan 2.3) needs to group by both. Neither is derivable
 * from the project files, so both come from the command line, defaulting to the project name and
 * {@code unknown}.
 *
 * <h2>Deterministic and re-derivable</h2>
 * Every record carries the analysis configuration that produced it — schema version, parser package,
 * taxonomy version. When the analysis changes, records can be regenerated and compared rather than
 * silently mixed.
 *
 * @author Patrick Gebhard
 */
public final class CorpusExtractCli {

    /** Bump when the record shape changes in a way consumers must notice. */
    private static final int CORPUS_VERSION = 1;

    /**
     * Which anchor label to record as the primary one when several share a position.
     *
     * <p>Ordered most specific first. A command sitting where both {@code before-object} and
     * {@code clause-initial} apply is better described by the object: the role-bearing label carries
     * the information a placement model can generalise from, while {@code utterance-initial} is true
     * of any first position. All matching labels are kept in {@code anchorCandidates} so this choice
     * can be revisited without re-extracting.</p>
     */
    private static final List<String> ANCHOR_PRIORITY = List.of(
            "before-object", "after-object",
            "before-subject", "after-subject",
            "before-predicate", "after-predicate",
            "after-address",
            "before-verb", "after-verb",
            "clause-initial",
            "before-final-punct",
            "utterance-initial", "utterance-final");

    private CorpusExtractCli() {
    }

    public static void main(String[] args) {
        List<String> projects = new ArrayList<>();
        String annotator = "unknown";
        String scenario = null;
        String out = null;
        String language = "de";

        for (String arg : args) {
            if (arg.startsWith("--project=")) {
                for (String part : arg.substring("--project=".length()).split(",")) {
                    if (!part.isBlank()) {
                        projects.add(part.trim());
                    }
                }
            } else if (arg.startsWith("--annotator=")) {
                annotator = arg.substring("--annotator=".length()).trim();
            } else if (arg.startsWith("--scenario=")) {
                scenario = arg.substring("--scenario=".length()).trim();
            } else if (arg.startsWith("--out=")) {
                out = arg.substring("--out=".length()).trim();
            } else if (arg.startsWith("--language=")) {
                language = arg.substring("--language=".length()).trim();
            } else {
                System.err.println("Unknown argument: " + arg);
                usage();
                System.exit(1);
            }
        }
        if (projects.isEmpty() || out == null || out.isEmpty()) {
            usage();
            System.exit(1);
        }

        BehaviorTaxonomy taxonomy;
        try {
            taxonomy = BehaviorTaxonomy.getDefault();
        } catch (RuntimeException exc) {
            System.err.println("Behavior taxonomy unavailable: " + exc.getMessage());
            System.exit(1);
            return;
        }

        WebUiServer server = WebUiServer.getInstance();
        JSONObject options = new JSONObject()
                .put("layers", new JSONObject().put("basic", true)
                        .put("dialogueAct", false).put("themeRheme", false))
                .put("useLlm", false)
                .put("persist", false)
                .put("language", language);

        int records = 0;
        int placements = 0;
        int unresolvedPlugin = 0;
        int unmatchedAnchor = 0;
        int failed = 0;

        try (BufferedWriter writer = Files.newBufferedWriter(Path.of(out), StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
                StandardOpenOption.WRITE)) {

            for (String projectPath : projects) {
                System.out.println("== " + projectPath);
                if (!Files.isDirectory(Path.of(projectPath))) {
                    System.err.println("   not a directory");
                    failed += 1;
                    continue;
                }
                try {
                    String pid = server.loadProjectForAnalysis(projectPath);
                    if (pid == null) {
                        System.err.println("   could not parse project");
                        failed += 1;
                        continue;
                    }
                    RunTimeProject project = server.projectForAnalysis(pid);
                    Map<String, String> actorToPlugin = actorPluginMap(server, project);
                    List<String> pluginIds = projectPluginIds(server, project);
                    String projectName = project != null && project.getProjectName() != null
                            ? project.getProjectName() : Path.of(projectPath).getFileName().toString();
                    String recordScenario = scenario != null && !scenario.isEmpty()
                            ? scenario : projectName;

                    int projectPlacements = 0;
                    JSONObject doc = server.analyzeScriptSemantics(pid, options);
                    String udPackage = doc.optJSONObject("provenance") == null
                            ? "" : doc.optJSONObject("provenance").optString("udPackage", "");
                    JSONArray annotations = doc.optJSONArray("annotations");

                    for (int i = 0; annotations != null && i < annotations.length(); i++) {
                        JSONObject ann = annotations.optJSONObject(i);
                        if (ann == null) {
                            continue;
                        }
                        JSONObject record = new JSONObject();
                        record.put("corpusVersion", CORPUS_VERSION);
                        record.put("annotator", annotator);
                        record.put("scenario", recordScenario);
                        record.put("project", projectName);
                        record.put("projectPath", projectPath);
                        record.put("speaker", ann.optString("speaker", ""));
                        record.put("line", ann.optInt("line", 0));
                        record.put("sentence", ann.optInt("sentence", 0));
                        record.put("cleanText", ann.optString("text", ""));
                        record.put("scriptFrom", ann.optInt("scriptFrom", 0));
                        record.put("scriptTo", ann.optInt("scriptTo", 0));
                        if (ann.optJSONArray("clauses") != null) {
                            record.put("clauses", ann.optJSONArray("clauses"));
                        }
                        JSONArray anchors = ann.optJSONArray("anchors");
                        if (anchors != null) {
                            record.put("anchors", anchors);
                        }

                        JSONArray placementsJson = new JSONArray();
                        JSONArray commands = ann.optJSONArray("commands");
                        for (int c = 0; commands != null && c < commands.length(); c++) {
                            JSONObject command = commands.optJSONObject(c);
                            if (command == null) {
                                continue;
                            }
                            JSONObject placement = buildPlacement(
                                    command, anchors, actorToPlugin, pluginIds, taxonomy);
                            if (placement.optString("plugin", "").isEmpty()) {
                                unresolvedPlugin += 1;
                            }
                            if (placement.isNull("anchor")) {
                                unmatchedAnchor += 1;
                            }
                            placementsJson.put(placement);
                            placements += 1;
                            projectPlacements += 1;
                        }
                        record.put("placements", placementsJson);

                        record.put("analysis", new JSONObject()
                                .put("schema", doc.optInt("version", 0))
                                .put("udPackage", udPackage)
                                .put("taxonomySystem", taxonomy.getSystem())
                                .put("taxonomyVersion", taxonomy.getSystemVersion()));

                        writer.write(record.toString());
                        writer.newLine();
                        records += 1;
                    }
                    System.out.printf("   %d sentence records, %d placements%n",
                            annotations == null ? 0 : annotations.length(), projectPlacements);
                } catch (Exception exc) {
                    System.err.println("   failed: " + exc);
                    failed += 1;
                }
            }
        } catch (Exception exc) {
            System.err.println("cannot write " + out + ": " + exc);
            System.exit(1);
        }

        System.out.println();
        System.out.printf("wrote %d records, %d placements to %s%n", records, placements, out);
        if (unresolvedPlugin > 0) {
            System.out.printf("  %d placement(s) could not be resolved to a plugin — not classified%n",
                    unresolvedPlugin);
        }
        if (unmatchedAnchor > 0) {
            System.out.printf("  %d placement(s) sit at no structural anchor — inspect these%n",
                    unmatchedAnchor);
        }
        System.exit(failed == 0 ? 0 : 2);
    }

    /**
     * Actor (agent) name → plugin id, via the project's agent → device → class chain.
     *
     * <p>Unqualified commands ({@code [background color='…']}, no actor) are the majority in practice
     * and are deliberately <em>not</em> in this map: the runtime dispatches them by command <em>name</em>
     * across the project's loaded plugins, so they are resolved that way in
     * {@link #resolveUnqualified}. An earlier version only handled them when the project had exactly one
     * classifiable plugin, which left most real commands unclassified.</p>
     */
    private static Map<String, String> actorPluginMap(WebUiServer server, RunTimeProject project) {
        Map<String, String> out = new LinkedHashMap<>();
        if (project == null || project.getProjectConfig() == null) {
            return out;
        }
        ProjectConfig config = project.getProjectConfig();
        for (AgentConfig agent : config.getAgentConfigList()) {
            if (agent == null || agent.getAgentName() == null) {
                continue;
            }
            PluginConfig plugin = config.getPluginConfig(agent.getDeviceName());
            String pluginId = plugin == null ? null : server.pluginIdForClassName(plugin.getClassName());
            if (pluginId != null) {
                out.put(agent.getAgentName(), pluginId);
            }
        }
        return out;
    }

    /** Plugin ids configured in the project, in declaration order. */
    private static List<String> projectPluginIds(WebUiServer server, RunTimeProject project) {
        List<String> out = new ArrayList<>();
        if (project == null || project.getProjectConfig() == null) {
            return out;
        }
        for (PluginConfig plugin : project.getProjectConfig().getPluginConfigList()) {
            if (plugin == null) {
                continue;
            }
            String pluginId = server.pluginIdForClassName(plugin.getClassName());
            if (pluginId != null && !out.contains(pluginId)) {
                out.add(pluginId);
            }
        }
        return out;
    }

    /**
     * Plugin for an unqualified command: the one project plugin that declares this command name.
     * Returns {@code null} when none or several do — an ambiguous attribution is worse than none,
     * because it would put a wrong NEUROGES Function on a training example.
     */
    private static String resolveUnqualified(String name, List<String> pluginIds,
                                             BehaviorTaxonomy taxonomy) {
        String found = null;
        for (String pluginId : pluginIds) {
            if (taxonomy.tagFor(pluginId, name) != null) {
                if (found != null) {
                    return null;
                }
                found = pluginId;
            }
        }
        return found;
    }

    private static JSONObject buildPlacement(JSONObject command, JSONArray anchors,
                                             Map<String, String> actorToPlugin,
                                             List<String> projectPluginIds,
                                             BehaviorTaxonomy taxonomy) {
        String name = command.optString("name", "");
        String actor = command.optString("actor", "");
        int tokenIndex = command.optInt("tokenIndex", -1);

        JSONObject placement = new JSONObject();
        placement.put("name", name);
        placement.put("actor", actor);
        placement.put("tokenIndex", tokenIndex);
        placement.put("cleanOffset", command.optInt("cleanOffset", 0));
        placement.put("scriptFrom", command.optInt("scriptFrom", 0));
        placement.put("scriptTo", command.optInt("scriptTo", 0));

        String pluginId = actor.isEmpty()
                ? resolveUnqualified(name, projectPluginIds, taxonomy)
                : actorToPlugin.get(actor);
        if (pluginId != null) {
            placement.put("plugin", pluginId);
            BehaviorTag tag = taxonomy.tagFor(pluginId, name);
            if (tag != null) {
                if (tag.getFunction() != null) {
                    placement.put("function", tag.getFunction());
                }
                if (tag.getType() != null) {
                    placement.put("neurogesType", tag.getType());
                }
                placement.put("channel", tag.getChannel());
                placement.put("cospeech", tag.isCoSpeech());
                if (tag.getAffiliate() != null) {
                    placement.put("affiliate", tag.getAffiliate());
                }
                placement.put("evidence", tag.getEvidence().getWireName());
            }
        } else {
            placement.put("plugin", "");
        }

        // Which structural slot the author used. All labels at that position are kept, because several
        // can legitimately coincide and the priority choice below should be revisable without
        // re-extracting the corpus.
        JSONArray candidates = new JSONArray();
        String primary = null;
        int primaryRank = Integer.MAX_VALUE;
        JSONObject primarySlot = null;
        for (int a = 0; anchors != null && a < anchors.length(); a++) {
            JSONObject slot = anchors.optJSONObject(a);
            if (slot == null || slot.optInt("tokenIndex", -2) != tokenIndex) {
                continue;
            }
            String label = slot.optString("slot", "");
            candidates.put(label);
            int rank = ANCHOR_PRIORITY.indexOf(label);
            if (rank < 0) {
                rank = ANCHOR_PRIORITY.size();
            }
            if (rank < primaryRank) {
                primaryRank = rank;
                primary = label;
                primarySlot = slot;
            }
        }
        placement.put("anchorCandidates", candidates);
        if (primary == null) {
            placement.put("anchor", JSONObject.NULL);
        } else {
            placement.put("anchor", primary);
            if (primarySlot != null) {
                if (!primarySlot.isNull("clauseId")) {
                    placement.put("clauseId", primarySlot.optString("clauseId"));
                }
                if (primarySlot.has("role")) {
                    placement.put("anchorRole", primarySlot.optString("role"));
                }
                if (primarySlot.has("kind")) {
                    placement.put("anchorKind", primarySlot.optString("kind"));
                }
            }
        }

        JSONObject params = new JSONObject();
        JSONObject given = command.optJSONObject("params");
        if (given != null) {
            for (String key : given.keySet()) {
                params.put(key, given.optString(key));
            }
        }
        placement.put("params", params);
        return placement;
    }

    private static void usage() {
        System.err.println("Usage: CorpusExtractCli --project=<dir>[,<dir>…] --out=<file.jsonl> "
                + "[--annotator=<id>] [--scenario=<name>] [--language=de]");
        System.err.println();
        System.err.println("  --annotator  who authored these scripts (grouping key for agreement)");
        System.err.println("  --scenario   scenario name; defaults to the project name");
        System.err.println("  --out        JSONL output file");
    }
}
