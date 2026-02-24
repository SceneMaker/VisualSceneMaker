package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowSituationPipelineTest {

    @Test
    void generatesFlowAndReportFromSituation() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");
        Path outProjectDir = tempDir.resolve("generated-project");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Okay button",
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.STANDALONE,
                        null),
                outProjectDir);

        assertEquals("success", report.optString("status"));
        assertTrue(Files.exists(outXml));
        assertTrue(Files.exists(reportJson));
        assertTrue(report.optInt("attemptCount", 0) >= 1);
        assertTrue(report.optJSONArray("assumptions").length() >= 1);
        assertEquals("SceneFlow", report.getJSONObject("availableGraphConfig").optString("rootId"));
        assertTrue(report.getJSONObject("availableGraphConfig").optJSONArray("nodes").length() >= 1);
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).has("candidate"));
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).has("activeSemanticRules"));
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).has("semanticRuleExecution"));
        assertTrue(report.has("activeSemanticRulesSummary"));
        assertTrue(report.has("semanticRuleExecutionSummary"));
        assertTrue(report.has("executedRuleCount"));
        assertTrue(report.has("violatedRuleCount"));
        assertTrue(report.has("interactivePatternCatalogPath"));
        assertEquals(outProjectDir.toAbsolutePath().toString(), report.optString("generatedProjectPath"));
        assertTrue(Files.exists(outProjectDir.resolve("project.xml")));
        assertTrue(Files.exists(outProjectDir.resolve("sceneflow.xml")));

        JSONObject idp = report.getJSONArray("attempts").getJSONObject(0).getJSONObject("interactiveDesignPattern");
        assertEquals(true, idp.optBoolean("available"));
        assertEquals("constrained_activity_base", idp.optString("selectedPatternId"));
        assertTrue(idp.getJSONArray("scientificSources").length() >= 1);
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).getJSONArray("activeSemanticRules").length() >= 1);
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).getJSONArray("activeSemanticRules").toString()
                .contains("SUPERNODE_EXIT_TARGET_OUTSIDE_SCOPE"));
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).getJSONArray("activeSemanticRules").toString()
                .contains("\"severity\":\"error\""));
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).getJSONArray("activeSemanticRules").toString()
                .contains("\"enabled\":true"));
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).getJSONArray("semanticRuleExecution").toString()
                .contains("\"executed\":true"));
        assertTrue(report.getJSONObject("activeSemanticRulesSummary").has("SUPERNODE_EXIT_TARGET_OUTSIDE_SCOPE"));
        assertTrue(report.optInt("executedRuleCount", 0) >= 1);

        JSONObject candidate = report.getJSONArray("attempts").getJSONObject(0).getJSONObject("candidate");
        assertTrue(candidate.getJSONArray("assumptions").toString().contains("Auto-created variable event"));
        assertTrue(candidate.getJSONArray("operations").toString().contains("\"op\":\"add_variable_definition\""));
        assertTrue(candidate.getJSONArray("operations").toString().contains("\"edgeType\":\"IEDGE\""));
        assertTrue(candidate.getJSONArray("operations").toString().contains("\"edgeType\":\"TEDGE\""));
        assertTrue(candidate.getJSONArray("operations").toString().contains("\"sourceNodeId\":\"S"));
        assertTrue(Files.readString(outXml).contains("Event(*, 10)"));
        assertTrue(Files.readString(outXml).contains("OkayButtonPressed"));
    }

    @Test
    void reminderSituationCreatesInternalReminderFlow() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-reminder-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Okay button and remind the user every 5 seconds",
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.STANDALONE,
                        null));

        assertEquals("success", report.optString("status"));
        JSONObject idp = report.getJSONArray("attempts").getJSONObject(0).getJSONObject("interactiveDesignPattern");
        assertEquals("periodic_reminder_while_waiting", idp.optString("selectedPatternId"));
        assertTrue(idp.getJSONArray("scientificSources").length() >= 1);
        String xml = Files.readString(outXml);
        assertTrue(xml.contains("Reminder"));
        assertTrue(xml.contains("timeout=\"5000\""));
        assertTrue(xml.contains("<TEdge"));
        assertTrue(xml.contains("<IEdge"));
    }

    @Test
    void hybridModeFallsBackToTemplateWhenLlmMissing() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-hybrid-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        SceneFlowSituationPipeline.Settings settings = new SceneFlowSituationPipeline.Settings(
                SceneFlowSituationPipeline.CandidateMode.HYBRID,
                SceneFlowSituationPipeline.OutputMode.STANDALONE,
                new SceneFlowIrLlmCandidateProvider.Config("", "", "", 5, 2)
        );

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Okay button",
                settings);

        assertEquals("success", report.optString("status"));
        assertTrue(report.optJSONArray("generationWarnings").length() >= 1);
        assertEquals("standalone", report.optString("outputMode"));
        assertTrue(report.has("availableGraphConfig"));
        assertTrue(Files.exists(outXml));
    }

    @Test
    void reportExposesAllConfiguredRuleDefinitionsInAttemptMetadata() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-rule-meta-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Okay button",
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.STANDALONE,
                        null));

        JSONObject mapping = new JSONObject(Files.readString(Path.of("doc/meta-to-sceneflow-mapping.json")));
        Set<String> configuredRuleIds = new HashSet<>();
        var defs = mapping.optJSONArray("ruleDefinitions");
        for (int i = 0; i < defs.length(); i++) {
            configuredRuleIds.add(defs.getJSONObject(i).optString("id", ""));
        }

        Set<String> reportedRuleIds = new HashSet<>();
        var reported = report.getJSONArray("attempts").getJSONObject(0).getJSONArray("activeSemanticRules");
        for (int i = 0; i < reported.length(); i++) {
            reportedRuleIds.add(reported.getJSONObject(i).optString("id", ""));
            assertTrue(reported.getJSONObject(i).has("severity"));
            assertTrue(reported.getJSONObject(i).has("enabled"));
        }

        assertEquals(configuredRuleIds, reportedRuleIds,
                "Attempt metadata should expose all configured rule definitions.");
    }
}
