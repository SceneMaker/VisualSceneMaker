package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
        assertTrue(report.getJSONArray("attempts").getJSONObject(0).has("promptResolution"));
        assertTrue(report.has("activeSemanticRulesSummary"));
        assertTrue(report.has("semanticRuleExecutionSummary"));
        assertTrue(report.has("executedRuleCount"));
        assertTrue(report.has("violatedRuleCount"));
        assertTrue(report.has("interactivePatternCatalogPath"));
        assertEquals(outProjectDir.toAbsolutePath().toString(), report.optString("generatedProjectPath"));
        assertTrue(Files.exists(outProjectDir.resolve("project.xml")));
        assertTrue(Files.exists(outProjectDir.resolve("sceneflow.xml")));
        String projectXml = Files.readString(outProjectDir.resolve("project.xml"));
        assertFalse(projectXml.contains("TimerExecutor"));
        assertFalse(projectXml.contains("<Agent "));

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
    void lowPromptResolutionConfidenceIsReportedAsGenerationWarning() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-confidence-warning-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait and remind while showing pictures",
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.STANDALONE,
                        null));

        assertEquals("success", report.optString("status"));
        assertTrue(report.optJSONArray("generationWarnings").toString().contains("low prompt-resolution confidence"));
        JSONObject attempt = report.getJSONArray("attempts").getJSONObject(0);
        assertTrue(attempt.getJSONObject("promptResolution").optDouble("confidence", 1.0) < 0.8);
        assertTrue(attempt.getJSONObject("promptResolution").getJSONArray("ambiguities").length() >= 1);
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

    @Test
    void canonicalizationRewritesAllSupernodeInterruptTargetsOutsideScope() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
        JSONObject candidate = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new org.json.JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_supernode")
                                .put("superNodeId", "S100")
                                .put("name", "Wait for Okay or Cancel")
                                .put("parentSuperNodeId", "SceneFlow"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N100")
                                .put("name", "Waiting")
                                .put("parentSuperNodeId", "S100"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N101")
                                .put("name", "Okay pressed")
                                .put("parentSuperNodeId", "S100"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N102")
                                .put("name", "Cancel pressed")
                                .put("parentSuperNodeId", "S100"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N1000")
                                .put("name", "After_OkayButtonPressed")
                                .put("parentSuperNodeId", "SceneFlow"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "E100")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "S100")
                                .put("targetNodeId", "N1000")
                                .put("payload", new JSONObject().put("conditionText", "event == \"OkayButtonPressed\"")))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "E101")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "S100")
                                .put("targetNodeId", "N102")
                                .put("payload", new JSONObject().put("conditionText", "event == \"CancelButtonPressed\"")))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "WaitTimeout")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "N100")
                                .put("targetNodeId", "N100")
                                .put("payload", new JSONObject().put("timeoutMs", 1000))));

        SceneFlowSituationPipeline pipeline = new SceneFlowSituationPipeline();
        Method canonicalize = SceneFlowSituationPipeline.class.getDeclaredMethod(
                "enforceWaitLoopCanonicalShape",
                JSONObject.class,
                JSONObject.class,
                String.class,
                SceneFlowSituationPipeline.OutputMode.class);
        canonicalize.setAccessible(true);
        JSONObject normalized = (JSONObject) canonicalize.invoke(
                pipeline,
                candidate,
                snapshot,
                "Wait until the user pressed the Okay button or the Cancel button",
                SceneFlowSituationPipeline.OutputMode.STANDALONE);

        Set<String> childNodeIds = new HashSet<>();
        org.json.JSONArray operations = normalized.getJSONArray("operations");
        for (int i = 0; i < operations.length(); i++) {
            JSONObject op = operations.getJSONObject(i);
            if ("create_node".equals(op.optString("op", "")) && "S100".equals(op.optString("parentSuperNodeId", ""))) {
                childNodeIds.add(op.optString("nodeId", ""));
            }
        }
        for (int i = 0; i < operations.length(); i++) {
            JSONObject op = operations.getJSONObject(i);
            if (!"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"IEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            if (!"S100".equals(op.optString("sourceNodeId", ""))) {
                continue;
            }
            assertFalse(childNodeIds.contains(op.optString("targetNodeId", "")),
                    "Interrupt edge target must be outside supernode scope.");
        }

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(normalized, snapshot);
        assertFalse(result.getIssues().stream().anyMatch(i -> "SUPERNODE_EXIT_TARGET_IN_SCOPE".equals(i.getCode())));
        assertFalse(result.getIssues().stream().anyMatch(i -> "NODE_REF_UNKNOWN".equals(i.getCode())));
    }

    @Test
    void canonicalizationAddsSelfTimeoutForFlatInterruptWaitNode() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
        JSONObject candidate = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new org.json.JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N100")
                                .put("name", "Wait for Okay or Cancel")
                                .put("parentSuperNodeId", "SceneFlow"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N101")
                                .put("name", "Okay pressed")
                                .put("parentSuperNodeId", "SceneFlow"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("nodeId", "N102")
                                .put("name", "Cancel pressed")
                                .put("parentSuperNodeId", "SceneFlow"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "E100")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "N100")
                                .put("targetNodeId", "N101")
                                .put("payload", new JSONObject().put("conditionText", "event == \"OkayButtonPressed\"")))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "E101")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "N100")
                                .put("targetNodeId", "N102")
                                .put("payload", new JSONObject().put("conditionText", "event == \"CancelButtonPressed\""))));

        SceneFlowSituationPipeline pipeline = new SceneFlowSituationPipeline();
        Method canonicalize = SceneFlowSituationPipeline.class.getDeclaredMethod(
                "enforceWaitLoopCanonicalShape",
                JSONObject.class,
                JSONObject.class,
                String.class,
                SceneFlowSituationPipeline.OutputMode.class);
        canonicalize.setAccessible(true);
        JSONObject normalized = (JSONObject) canonicalize.invoke(
                pipeline,
                candidate,
                snapshot,
                "Wait until the user pressed the Okay button or the Cancel button",
                SceneFlowSituationPipeline.OutputMode.STANDALONE);

        org.json.JSONArray operations = normalized.getJSONArray("operations");
        boolean hasSelfTimeout = false;
        for (int i = 0; i < operations.length(); i++) {
            JSONObject op = operations.getJSONObject(i);
            if (!"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"TEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            if ("N100".equals(op.optString("sourceNodeId", ""))
                    && "N100".equals(op.optString("targetNodeId", ""))) {
                hasSelfTimeout = true;
                break;
            }
        }
        assertTrue(hasSelfTimeout, "Flat interrupt wait source node must get a self timeout loop.");
    }

    @Test
    void strictConstraintResolutionRejectsUnresolvedLabels() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-strict-resolution-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Foobar button",
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.STANDALONE,
                        null,
                        ConstraintResolutionMode.STRICT));

        assertEquals("failed", report.optString("status"));
        JSONObject attempt = report.getJSONArray("attempts").getJSONObject(0);
        assertEquals("semantic_rejected", attempt.optString("status"));
        assertTrue(attempt.getJSONArray("semanticIssues").toString().contains("UNRESOLVED_CONSTRAINT_LABEL"));
        assertTrue(attempt.getJSONObject("constraintResolution").getJSONArray("unresolvedLabels").length() >= 1);
    }
}
