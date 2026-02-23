package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowSituationPipelineTest {

    @Test
    void generatesFlowAndReportFromSituation() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        JSONObject report = new SceneFlowSituationPipeline().run(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                outXml,
                reportJson,
                "Wait until the user pressed the Okay button");

        assertEquals("success", report.optString("status"));
        assertTrue(Files.exists(outXml));
        assertTrue(Files.exists(reportJson));
        assertTrue(report.optInt("attemptCount", 0) >= 1);
        assertTrue(report.optJSONArray("assumptions").length() >= 1);
    }

    @Test
    void hybridModeFallsBackToTemplateWhenLlmMissing() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-pipeline-hybrid-test");
        Path outXml = tempDir.resolve("generated.xml");
        Path reportJson = tempDir.resolve("report.json");

        SceneFlowSituationPipeline.Settings settings = new SceneFlowSituationPipeline.Settings(
                SceneFlowSituationPipeline.CandidateMode.HYBRID,
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
        assertTrue(Files.exists(outXml));
    }
}
