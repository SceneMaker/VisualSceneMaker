package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SceneFlowIrSemanticValidatorTest {

    @Test
    void validatesSimpleValidPatch() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", "SceneFlow")
                                .put("nodeId", "WaitLoop")
                                .put("name", "WaitLoop"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "WaitTimeout")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "WaitLoop")
                                .put("targetNodeId", "WaitLoop")
                                .put("payload", new JSONObject().put("timeoutMs", 1000))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertFalse(result.hasErrors(), "Expected no semantic issues");
    }

    @Test
    void rejectsUnknownVariableInCondition() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Interrupt1")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "SceneFlow")
                                .put("targetNodeId", "N1")
                                .put("payload", new JSONObject()
                                        .put("conditionText", "MissingVar == \"OkayButton\""))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(result.hasErrors(), "Expected semantic issues");
        assertTrue(result.getIssues().stream().anyMatch(issue -> "VAR_REF_UNKNOWN".equals(issue.getCode())));
    }

    @Test
    void rejectsInvalidEdgePayloadForType() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Timeout1")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "SceneFlow")
                                .put("targetNodeId", "N1")
                                .put("payload", new JSONObject())));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(result.hasErrors(), "Expected semantic issues");
        assertTrue(result.getIssues().stream().anyMatch(issue -> "EDGE_TIMEOUT_MISSING".equals(issue.getCode())));
    }

    @Test
    void acceptsStringLiteralInConditionWithoutTreatingItAsVariable() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Interrupt2")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "SceneFlow")
                                .put("targetNodeId", "N1")
                                .put("payload", new JSONObject()
                                        .put("conditionText", "UIEvent == \"OkayButton\""))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertFalse(result.hasErrors(), "Expected no semantic issues");
    }

    private JSONObject baseSnapshot() {
        return new JSONObject()
                .put("snapshotVersion", "1.0")
                .put("generatedAt", "2026-02-22T12:00:00Z")
                .put("project", new JSONObject().put("name", "Test"))
                .put("flow", new JSONObject()
                        .put("rootId", "SceneFlow")
                        .put("startNodeIds", new JSONArray().put("N1"))
                        .put("variables", new JSONArray()
                                .put(new JSONObject()
                                        .put("name", "UIEvent")
                                        .put("type", "Event")
                                        .put("scope", "global")))
                        .put("allowedEdgeTypes", new JSONArray()
                                .put("EEDGE")
                                .put("CEDGE")
                                .put("TEDGE")
                                .put("IEDGE")
                                .put("PEDGE")
                                .put("FEDGE"))
                        .put("nodes", new JSONArray()
                                .put(new JSONObject()
                                        .put("id", "N1")
                                        .put("name", "N1")
                                        .put("parentSuperNodeId", "SceneFlow")
                                        .put("isSuperNode", false)
                                        .put("isHistoryNode", false)))
                        .put("edges", new JSONArray()));
    }
}
