package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowIrOrchestratorTest {

    @Test
    void retriesAcrossCandidatesAndSucceedsOnSecond() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-ir-orchestrator-test");
        Path badIr = tempDir.resolve("bad-ir.json");
        Path goodIr = tempDir.resolve("good-ir.json");
        Path out = tempDir.resolve("out-sceneflow.xml");

        JSONObject invalid = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Bad1")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "N1")
                                .put("targetNodeId", "N1")
                                .put("payload", new JSONObject()
                                        .put("timeoutMs", -1))));
        JSONObject valid = new JSONObject(Files.readString(Path.of("doc/sceneflow-ir.wait-for-ok-button.example.json")));

        Files.writeString(badIr, invalid.toString(2));
        Files.writeString(goodIr, valid.toString(2));

        SceneFlowGenerationResult result = new SceneFlowIrOrchestrator().generateFlowFromSituation(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                out,
                List.of(badIr, goodIr));

        assertTrue(result.isSuccess());
        assertTrue(Files.exists(out));
        assertEquals(2, result.getAttempts().size());
        assertFalse(result.getAttempts().get(0).isSuccess());
        assertTrue(result.getAttempts().get(0).getSemanticIssues().stream()
                .anyMatch(issue -> "EDGE_TIMEOUT_INVALID".equals(issue.getCode())));
        assertTrue(result.getAttempts().get(1).isSuccess());
    }

    @Test
    void failsWhenAllCandidatesInvalid() throws Exception {
        Path tempDir = Files.createTempDirectory("sceneflow-ir-orchestrator-fail-test");
        Path badIr = tempDir.resolve("bad-ir.json");
        Path out = tempDir.resolve("out-sceneflow.xml");

        JSONObject invalid = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Bad1")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "N1")
                                .put("targetNodeId", "N1")
                                .put("payload", new JSONObject().put("timeoutMs", -1))));
        Files.writeString(badIr, invalid.toString(2));

        SceneFlowGenerationResult result = new SceneFlowIrOrchestrator().generateFlowFromSituation(
                Path.of("doc/capability-snapshot.designpatterns.json"),
                Path.of("doc/DesignPatterns/sceneflow.xml"),
                out,
                List.of(badIr));

        assertFalse(result.isSuccess());
        assertEquals(1, result.getAttempts().size());
        assertNotNull(result.getAttempts().get(0).getSemanticIssues());
        assertFalse(result.getAttempts().get(0).getSemanticIssues().isEmpty());
        assertFalse(Files.exists(out));
    }
}
