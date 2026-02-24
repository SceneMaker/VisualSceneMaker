package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

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
    void reportsUnknownVariableInConditionAsWarningByDefault() {
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
        assertFalse(result.hasErrors(), "Unknown variable should be reported as warning by default");
        assertTrue(result.hasIssues(), "Expected semantic warning issues");
        assertTrue(result.getIssues().stream().anyMatch(issue ->
                "VAR_REF_UNKNOWN".equals(issue.getCode())
                        && "warning".equalsIgnoreCase(issue.getSeverity())));
    }

    @Test
    void canDisableRuleByIdViaMappingConfig() throws Exception {
        Path mapping = Files.createTempFile("semantic-rules", ".json");
        Files.writeString(mapping, """
                {
                  "ruleDefinitions": [
                    { "id": "VAR_REF_UNKNOWN", "scope": "general", "activation": {} }
                  ],
                  "disabledRules": ["VAR_REF_UNKNOWN"]
                }
                """);

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

        SemanticValidationResult result = new SceneFlowIrSemanticValidator(mapping).validate(ir, snapshot);
        assertFalse(result.getIssues().stream().anyMatch(issue -> "VAR_REF_UNKNOWN".equals(issue.getCode())));
    }

    @Test
    void warningSeverityDoesNotTriggerSemanticErrorRejection() throws Exception {
        Path mapping = Files.createTempFile("semantic-rules-warning", ".json");
        Files.writeString(mapping, """
                {
                  "ruleDefinitions": [
                    {
                      "id": "VAR_REF_UNKNOWN",
                      "scope": "general",
                      "severity": "warning",
                      "activation": {}
                    }
                  ],
                  "disabledRules": []
                }
                """);

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

        SemanticValidationResult result = new SceneFlowIrSemanticValidator(mapping).validate(ir, snapshot);
        assertFalse(result.hasErrors(), "Warning-only findings should not count as semantic errors");
        assertTrue(result.hasIssues(), "Warning should still be reported");
        assertTrue(result.getIssues().stream()
                .anyMatch(issue -> "VAR_REF_UNKNOWN".equals(issue.getCode())
                        && "warning".equalsIgnoreCase(issue.getSeverity())));
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

    @Test
    void rejectsConstrainedSupernodeWithoutInternalLivenessLoop() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_supernode")
                                .put("parentSuperNodeId", "SceneFlow")
                                .put("superNodeId", "S100")
                                .put("name", "WaitForEvent"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", "S100")
                                .put("nodeId", "N100")
                                .put("name", "Waiting"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", "SceneFlow")
                                .put("nodeId", "N101")
                                .put("name", "After"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Interrupt100")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "S100")
                                .put("targetNodeId", "N101")
                                .put("payload", new JSONObject()
                                        .put("conditionText", "UIEvent == \"OkayButton\""))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(result.hasErrors(), "Expected semantic issues");
        assertTrue(result.getIssues().stream().anyMatch(issue ->
                "SUPERNODE_INTERNAL_LIVENESS_MISSING".equals(issue.getCode())));
    }

    @Test
    void rejectsSupernodeExitTargetInsideSupernodeScope() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_supernode")
                                .put("parentSuperNodeId", "SceneFlow")
                                .put("superNodeId", "S100")
                                .put("name", "WaitForEvent"))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", "S100")
                                .put("nodeId", "N100")
                                .put("name", "Waiting"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "WaitLoop100")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "N100")
                                .put("targetNodeId", "N100")
                                .put("payload", new JSONObject().put("timeoutMs", 1000)))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "Interrupt100")
                                .put("edgeType", "IEDGE")
                                .put("sourceNodeId", "S100")
                                .put("targetNodeId", "N100")
                                .put("payload", new JSONObject()
                                        .put("conditionText", "UIEvent == \"OkayButton\""))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(result.hasErrors(), "Expected semantic issues");
        assertTrue(result.getIssues().stream().anyMatch(issue ->
                "SUPERNODE_EXIT_TARGET_IN_SCOPE".equals(issue.getCode())));
    }

    @Test
    void mappingRuleDefinitionsAreKnownToValidator() throws Exception {
        JSONObject mapping = new JSONObject(Files.readString(Path.of("doc/meta-to-sceneflow-mapping.json")));
        JSONArray defs = mapping.optJSONArray("ruleDefinitions");
        assertNotNull(defs);
        Set<String> known = SceneFlowIrSemanticValidator.knownRuleIds();
        for (int i = 0; i < defs.length(); i++) {
            JSONObject def = defs.getJSONObject(i);
            String id = def.optString("id", "");
            String severity = def.optString("severity", "");
            assertTrue(known.contains(id), "Unknown ruleDefinitions id in mapping: " + id);
            assertTrue("error".equals(severity) || "warning".equals(severity),
                    "Rule severity must be error|warning for " + id + " but was: " + severity);
        }
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
