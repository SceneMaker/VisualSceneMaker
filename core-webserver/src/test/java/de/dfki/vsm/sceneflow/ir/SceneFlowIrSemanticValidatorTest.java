package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.testsupport.TestRepoPaths;

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

    /**
     * A duplicate global variable name used to survive validation and only surface as
     * Environment.create() throwing "Variable already defined" the first time Runtime.Play
     * processed the Declare list — silently aborting the interpreter thread before any node
     * ever ran (e.g. re-adding a plugin's declared variables to a project that already had them).
     */
    @Test
    void rejectsVariableDefinitionThatAlreadyExists() {
        JSONObject snapshot = baseSnapshot();
        JSONObject ir = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "add_variable_definition")
                                .put("ownerNodeId", "SceneFlow")
                                .put("varDef", new JSONObject()
                                        .put("name", "UIEvent")
                                        .put("type", "Event"))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(result.hasErrors(), "Expected re-declaring an existing variable to be an error");
        assertTrue(result.getIssues().stream().anyMatch(issue -> "VARDEF_NAME_DUPLICATE".equals(issue.getCode())));
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
        JSONObject mapping = new JSONObject(Files.readString(TestRepoPaths.doc("meta-to-sceneflow-mapping.json")));
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

    @Test
    void reportsUnknownLiteralSceneAsWarning() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Nonexistent\")"), snapshotWithScenes());

        assertFalse(result.hasErrors(), "An unknown scene must not block acceptance");
        assertTrue(issueCodes(result).contains("SCENE_REF_UNKNOWN"),
                "Expected SCENE_REF_UNKNOWN for an undeclared scene");
    }

    @Test
    void acceptsKnownLiteralScene() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Welcome\")"), snapshotWithScenes());
        assertFalse(issueCodes(result).contains("SCENE_REF_UNKNOWN"));
    }

    /** Scene arguments are a struct in braces. Brackets are not glue syntax. */
    @Test
    void acceptsKnownScenePlayedWithArguments() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Address\", { user = username })"), snapshotWithScenes());
        assertTrue(issueCodes(result).isEmpty(),
                "A correct call with its declared parameter must be clean, was: " + issueCodes(result));
    }

    @Test
    void reportsSceneParameterThatIsNotSupplied() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Address\")"), snapshotWithScenes());

        assertTrue(issueCodes(result).contains("SCENE_PARAM_MISSING"),
                "Address declares 'user', so playing it with no arguments must be reported");
        assertFalse(issueCodes(result).contains("SCENE_REF_UNKNOWN"));
    }

    @Test
    void reportsStructFieldThatIsNotADeclaredParameter() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Address\", { usr = username })"), snapshotWithScenes());

        assertTrue(issueCodes(result).contains("SCENE_PARAM_UNKNOWN"), "Misspelled parameter");
        assertTrue(issueCodes(result).contains("SCENE_PARAM_MISSING"), "'user' is still unsupplied");
    }

    /**
     * Binding is by name, and the runtime ignores any argument that is not a struct, so a bare value
     * supplies nothing despite looking like it passes one.
     */
    @Test
    void reportsNonStructArgumentToAParameterisedScene() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Address\", username)"), snapshotWithScenes());

        assertTrue(issueCodes(result).contains("SCENE_ARG_NOT_STRUCT"));
        assertTrue(issueCodes(result).contains("SCENE_PARAM_MISSING"));
    }

    @Test
    void acceptsSceneWithoutParametersPlayedWithoutArguments() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Welcome\")"), snapshotWithScenes());
        assertTrue(issueCodes(result).isEmpty(), "Was: " + issueCodes(result));
    }

    /**
     * A scene name may legitimately be a variable or built at runtime. Neither can be resolved
     * statically, so the rule must stay silent rather than report a scene that does not exist yet.
     */
    @Test
    void ignoresSceneNamesThatAreNotLiterals() {
        SceneFlowIrSemanticValidator validator = new SceneFlowIrSemanticValidator();
        for (String commandText : new String[] {
                "PlayScene(topic)",
                "PlayScene(\"Topic_\" + topic)",
                "PlayScene(prefix + suffix)"
        }) {
            SemanticValidationResult result = validator.validate(
                    irWithCommand(commandText), snapshotWithScenes());
            assertFalse(issueCodes(result).contains("SCENE_REF_UNKNOWN"),
                    "Must not report a non-literal scene name: " + commandText);
        }
    }

    /**
     * Snapshots taken before the scene inventory existed carry no script section. Treating that as
     * "no scenes declared" would report every scene in the flow as missing.
     */
    @Test
    void staysSilentWhenSnapshotHasNoSceneInventory() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene(\"Anything\")"), baseSnapshot());
        assertFalse(issueCodes(result).contains("SCENE_REF_UNKNOWN"));
    }

    /**
     * A single-quoted scene name used to compile successfully into a reference to a variable of that
     * name, with nothing reporting it. It must be an error, not a warning: no bare single quote is
     * ever correct in command text.
     */
    @Test
    void rejectsSingleQuotedStringsInCommandText() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayScene('Welcome')"), snapshotWithScenes());

        assertTrue(issueCodes(result).contains("COMMAND_TEXT_INVALID_QUOTE"));
        assertTrue(result.hasErrors(), "A single-quoted string must block acceptance");
    }

    /**
     * Single quotes inside a double-quoted string are legitimate and common in embedded action text.
     */
    @Test
    void allowsSingleQuotesInsideDoubleQuotedActionText() {
        SemanticValidationResult result = new SceneFlowIrSemanticValidator()
                .validate(irWithCommand("PlayAction(\"[background color='#77bb41']\")"),
                        snapshotWithScenes());
        assertFalse(issueCodes(result).contains("COMMAND_TEXT_INVALID_QUOTE"));
        assertFalse(result.hasErrors());
    }

    private Set<String> issueCodes(final SemanticValidationResult result) {
        java.util.Set<String> codes = new java.util.LinkedHashSet<>();
        for (SemanticIssue issue : result.getIssues()) {
            codes.add(issue.getCode());
        }
        return codes;
    }

    private JSONObject irWithCommand(final String commandText) {
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "add_node_command")
                                .put("nodeId", "N1")
                                .put("commandText", commandText)));
    }

    private JSONObject snapshotWithScenes() {
        return baseSnapshot()
                .put("snapshotVersion", "1.1")
                .put("script", new JSONObject()
                        .put("sections", new JSONArray())
                        .put("scenes", new JSONArray()
                                .put(new JSONObject().put("name", "Welcome"))
                                .put(new JSONObject().put("name", "Address")
                                        .put("parameters", new JSONArray().put("user")))
                                .put(new JSONObject().put("name", "Topic_Weather"))));
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

    /**
     * Two default edges on one node used to surface only as a compile exception, several stages later
     * than the diagnostic that could have explained it.
     */
    @Test
    void rejectsTwoDefaultEdgesOnTheSameNode() {
        JSONObject ir = irWithOps(new JSONArray()
                .put(createNode("A", true))
                .put(createNode("B", false))
                .put(edge("e1", "EEDGE", "A", "B"))
                .put(edge("e2", "TEDGE", "A", "B").put("payload", new JSONObject().put("timeoutMs", 500))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, baseSnapshot());
        assertTrue(issueCodes(result).contains("EDGE_DEFAULT_DUPLICATE"));
        assertTrue(result.hasErrors(), "The compiler would throw, so this must block acceptance");
    }

    /** A guarded edge lives in its own list, so it does not compete for the default-edge slot. */
    @Test
    void allowsAGuardedEdgeAlongsideADefaultEdge() {
        JSONObject ir = irWithOps(new JSONArray()
                .put(createNode("A", true))
                .put(createNode("B", false))
                .put(edge("e1", "EEDGE", "A", "B"))
                .put(edge("e2", "CEDGE", "A", "B")
                        .put("payload", new JSONObject().put("conditionText", "UIEvent != \"\""))));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, baseSnapshot());
        assertFalse(issueCodes(result).contains("EDGE_DEFAULT_DUPLICATE"));
    }

    @Test
    void detectsADefaultEdgeAddedToANodeThatAlreadyHasOne() throws Exception {
        // DesignPatterns N31 already leaves via an epsilon edge to N32.
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject ir = irWithOps(new JSONArray()
                .put(edge("extra", "EEDGE", "N31", "N32")));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertTrue(issueCodes(result).contains("EDGE_DEFAULT_DUPLICATE"),
                "Existing edges must be counted, was: " + issueCodes(result));
    }

    /**
     * A delete_edge names only an edge id and snapshot edges carry none, so the existing flow cannot
     * be reconciled with the patch. Reporting a conflict the deletion resolves would be worse than
     * missing one.
     */
    @Test
    void doesNotGuessAboutDefaultEdgesWhenThePatchDeletesEdges() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject ir = irWithOps(new JSONArray()
                .put(new JSONObject().put("op", "delete_edge").put("edgeId", "whichever"))
                .put(edge("replacement", "EEDGE", "N31", "N32")));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, snapshot);
        assertFalse(issueCodes(result).contains("EDGE_DEFAULT_DUPLICATE"));
    }

    @Test
    void reportsACreatedNodeNothingCanReach() {
        JSONObject ir = irWithOps(new JSONArray()
                .put(createNode("A", true))
                .put(createNode("Orphan", false)));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, baseSnapshot());
        assertTrue(issueCodes(result).contains("NODE_UNREACHABLE"));
        assertFalse(result.hasErrors(), "An unreachable node is inert rather than broken");
    }

    @Test
    void aStartNodeAndAnEdgeTargetAreBothReachable() {
        JSONObject ir = irWithOps(new JSONArray()
                .put(createNode("A", true))
                .put(createNode("B", false))
                .put(edge("e1", "EEDGE", "A", "B")));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, baseSnapshot());
        assertFalse(issueCodes(result).contains("NODE_UNREACHABLE"));
    }

    @Test
    void reportsWhereExecutionStops() {
        JSONObject ir = irWithOps(new JSONArray()
                .put(createNode("A", true))
                .put(createNode("B", false))
                .put(edge("e1", "EEDGE", "A", "B")));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(ir, baseSnapshot());
        assertTrue(issueCodes(result).contains("NODE_DEAD_END"), "B has no outgoing edge");
        assertFalse(result.hasErrors(), "A terminal step is legitimate");
    }

    private JSONObject irWithOps(final JSONArray operations) {
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", operations);
    }

    private JSONObject createNode(final String nodeId, final boolean isStartNode) {
        JSONObject op = new JSONObject()
                .put("op", "create_node")
                .put("parentSuperNodeId", "SceneFlow")
                .put("nodeId", nodeId)
                .put("name", nodeId);
        if (isStartNode) {
            op.put("isStartNode", true);
        }
        return op;
    }

    private JSONObject edge(final String edgeId, final String type, final String from, final String to) {
        return new JSONObject()
                .put("op", "create_edge")
                .put("edgeId", edgeId)
                .put("edgeType", type)
                .put("sourceNodeId", from)
                .put("targetNodeId", to);
    }
}
