package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SceneFlowIrSemanticValidator {
    private static final Path DEFAULT_META_MAPPING_PATH = Path.of("doc", "meta-to-sceneflow-mapping.json");

    private static final Set<String> EDGE_TYPES = Set.of("EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE");
    private static final Set<String> VARIABLE_TYPES = Set.of("Int", "Bool", "Float", "String", "Event");
    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("\\b[A-Za-z_][A-Za-z0-9_]*\\b");
    private static final Set<String> RESERVED_TOKENS = Set.of(
            "true", "false", "null", "and", "or", "not", "in", "if", "then", "else");
    private static final String RULE_EXIT_TARGET_OUTSIDE_SCOPE = "SUPERNODE_EXIT_TARGET_OUTSIDE_SCOPE";
    private static final String RULE_INTERNAL_LIVENESS_REQUIRED = "SUPERNODE_INTERNAL_LIVENESS_REQUIRED";
    private static final Set<String> KNOWN_RULE_IDS = Set.copyOf(new LinkedHashSet<>(Set.of(
            "IR_EMPTY",
            "OP_INVALID",
            "OP_MISSING_KIND",
            "OP_UNSUPPORTED",
            "NODE_ID_MISSING",
            "NODE_DUPLICATE",
            "NODE_REF_MISSING",
            "NODE_REF_UNKNOWN",
            "EDGE_ID_MISSING",
            "EDGE_DUPLICATE",
            "EDGE_REF_MISSING",
            "EDGE_REF_UNKNOWN",
            "EDGE_TYPE_INVALID",
            "EDGE_TYPE_NOT_ALLOWED",
            "EDGE_CONDITION_MISSING",
            "EDGE_TIMEOUT_MISSING",
            "EDGE_TIMEOUT_INVALID",
            "EDGE_PROBABILITY_MISSING",
            "EDGE_PROBABILITY_INVALID",
            "EDGE_CROSS_SUPERNODE_FORBIDDEN",
            "VARDEF_MISSING",
            "VARDEF_NAME_MISSING",
            "VARDEF_TYPE_INVALID",
            "VAR_REF_UNKNOWN",
            RULE_EXIT_TARGET_OUTSIDE_SCOPE,
            RULE_INTERNAL_LIVENESS_REQUIRED,
            "SUPERNODE_EXIT_TARGET_IN_SCOPE",
            "SUPERNODE_INTERNAL_LIVENESS_MISSING"
    )));
    private final RuleConfig ruleConfig;
    private final Map<String, InvariantRuleHandler> invariantRuleHandlers;
    private final Map<String, OperationRuleHandler> operationRuleHandlers;

    public SceneFlowIrSemanticValidator() {
        this(DEFAULT_META_MAPPING_PATH);
    }

    SceneFlowIrSemanticValidator(final Path metaMappingPath) {
        this.ruleConfig = loadRuleConfig(metaMappingPath);
        this.invariantRuleHandlers = createInvariantRuleHandlers();
        this.operationRuleHandlers = createOperationRuleHandlers();
    }

    public static Set<String> knownRuleIds() {
        return KNOWN_RULE_IDS;
    }

    public SemanticValidationResult validate(final JSONObject ir, final JSONObject snapshot) {
        final SemanticValidationResult result = new SemanticValidationResult();
        final Set<String> nodeIds = new HashSet<>();
        final Set<String> edgeIds = new HashSet<>();
        final Set<String> variableNames = new HashSet<>();
        final Set<String> allowedEdgeTypes = new HashSet<>();
        final Map<String, String> nodeParentById = new HashMap<>();
        final Set<String> superNodeIds = new HashSet<>();
        final JSONArray operations = ir.optJSONArray("operations");

        bootstrapContext(snapshot, nodeIds, edgeIds, variableNames, allowedEdgeTypes, nodeParentById, superNodeIds);
        if (operations == null || operations.length() == 0) {
            emitIssue(result, "IR_EMPTY", "/operations", "IR operations list is empty.");
            return result;
        }

        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            final String path = "/operations/" + i;
            if (op == null) {
                emitIssue(result, "OP_INVALID", path, "Operation is not an object.");
                continue;
            }
            validateOperation(op, path, nodeIds, edgeIds, variableNames, allowedEdgeTypes,
                    nodeParentById, superNodeIds, result);
        }

        validateConfiguredInvariants(ir, operations, nodeParentById, superNodeIds, result);
        return result;
    }

    public JSONArray describeActiveRules(final JSONObject ir, final JSONObject snapshot) {
        final JSONArray out = new JSONArray();
        final Set<String> nodeIds = new HashSet<>();
        final Set<String> edgeIds = new HashSet<>();
        final Set<String> variableNames = new HashSet<>();
        final Set<String> allowedEdgeTypes = new HashSet<>();
        final Map<String, String> nodeParentById = new HashMap<>();
        final Set<String> superNodeIds = new HashSet<>();
        bootstrapContext(snapshot, nodeIds, edgeIds, variableNames, allowedEdgeTypes, nodeParentById, superNodeIds);
        final JSONArray operations = ir == null ? null : ir.optJSONArray("operations");
        if (operations == null) {
            return out;
        }
        final Map<String, String> active = activeRuleReasons(ir, operations, nodeParentById, superNodeIds);
        for (Map.Entry<String, RuleDefinition> entry : ruleConfig.rules().entrySet()) {
            final RuleDefinition def = entry.getValue();
            final String ruleId = def.id();
            final JSONObject item = new JSONObject()
                    .put("id", ruleId)
                    .put("scope", def.scope())
                    .put("severity", ruleConfig.severityByRuleId().getOrDefault(ruleId, "error"))
                    .put("enabled", !ruleConfig.disabledRules().contains(ruleId))
                    .put("active", active.containsKey(def.id()))
                    .put("activationReason", active.getOrDefault(def.id(), ""));
            out.put(item);
        }
        return out;
    }

    public JSONArray describeRuleExecution(
            final JSONObject ir,
            final JSONObject snapshot,
            final SemanticValidationResult validationResult) {
        final JSONArray out = describeActiveRules(ir, snapshot);
        final Map<String, Integer> violations = new HashMap<>();
        final Map<String, String> firstViolationPath = new HashMap<>();
        final List<SemanticIssue> issues = validationResult == null ? List.of() : validationResult.getIssues();
        for (SemanticIssue issue : issues) {
            if (issue == null) {
                continue;
            }
            final String code = issue.getCode();
            if (code == null || code.isBlank()) {
                continue;
            }
            violations.put(code, violations.getOrDefault(code, 0) + 1);
            firstViolationPath.putIfAbsent(code, issue.getPath());
        }
        for (int i = 0; i < out.length(); i++) {
            final JSONObject item = out.optJSONObject(i);
            if (item == null) {
                continue;
            }
            final String id = item.optString("id", "");
            final boolean active = item.optBoolean("active", false);
            final boolean enabled = !ruleConfig.disabledRules().contains(id);
            item.put("executed", active && enabled);
            item.put("violatedCount", violations.getOrDefault(id, 0));
            item.put("firstViolationPath", firstViolationPath.containsKey(id)
                    ? firstViolationPath.get(id)
                    : JSONObject.NULL);
        }
        return out;
    }

    private void validateConfiguredInvariants(
            final JSONObject ir,
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final Map<String, String> active = activeRuleReasons(ir, operations, nodeParentById, superNodeIds);
        for (String ruleId : active.keySet()) {
            applyRule(ruleId, operations, nodeParentById, superNodeIds, result);
        }
    }

    private void applyRule(
            final String ruleId,
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final InvariantRuleHandler handler = invariantRuleHandlers.get(ruleId);
        if (handler != null) {
            handler.validate(operations, nodeParentById, superNodeIds, result);
        }
    }

    private Map<String, InvariantRuleHandler> createInvariantRuleHandlers() {
        return Map.of(
                RULE_INTERNAL_LIVENESS_REQUIRED, this::validateSuperNodeLiveness,
                RULE_EXIT_TARGET_OUTSIDE_SCOPE, this::validateSuperNodeExitTargetScope
        );
    }

    private Map<String, String> activeRuleReasons(
            final JSONObject ir,
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds) {
        final Map<String, String> active = new LinkedHashMap<>();
        for (Map.Entry<String, RuleDefinition> entry : ruleConfig.rules().entrySet()) {
            final String reason = ruleActivationReason(entry.getValue(), ir, operations, nodeParentById, superNodeIds);
            if (reason != null) {
                active.put(entry.getKey(), reason);
            }
        }
        return active;
    }

    private String ruleActivationReason(
            final RuleDefinition rule,
            final JSONObject ir,
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds) {
        if (rule == null) {
            return null;
        }
        final String scope = rule.scope().toLowerCase();
        if ("general".equals(scope)) {
            return "scope=general";
        }
        final String selectedPatternId = selectedPatternId(ir);
        if ("pattern".equals(scope)) {
            if (rule.patterns().isEmpty()) {
                return "scope=pattern (no pattern filter)";
            }
            if (!selectedPatternId.isBlank() && rule.patterns().contains(selectedPatternId)) {
                return "scope=pattern, selectedPatternId=" + selectedPatternId;
            }
            return null;
        }
        if ("context".equals(scope)) {
            boolean contextMatches = false;
            final String context = rule.context().toLowerCase();
            if ("constrained_wait_supernode".equals(context)) {
                contextMatches = hasConstrainedWaitContext(operations, nodeParentById, superNodeIds);
            }
            if (!contextMatches) {
                return null;
            }
            if (rule.patterns().isEmpty()) {
                return "scope=context, context=" + context;
            }
            if (selectedPatternId.isBlank()) {
                return "scope=context, context=" + context + ", selectedPatternId=unknown";
            }
            if (rule.patterns().contains(selectedPatternId)) {
                return "scope=context, context=" + context + ", selectedPatternId=" + selectedPatternId;
            }
            return null;
        }
        return "scope=" + scope;
    }

    private boolean isRuleActive(
            final String ruleId,
            final JSONObject ir,
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds) {
        final RuleDefinition rule = ruleConfig.rules().get(ruleId);
        return ruleActivationReason(rule, ir, operations, nodeParentById, superNodeIds) != null;
    }

    private String selectedPatternId(final JSONObject ir) {
        final JSONObject metadata = ir == null ? null : ir.optJSONObject("metadata");
        final JSONObject idp = metadata == null ? null : metadata.optJSONObject("interactiveDesignPattern");
        return idp == null ? "" : idp.optString("selectedPatternId", "").trim();
    }

    private boolean hasConstrainedWaitContext(
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"IEDGE".equals(op.optString("edgeType", "").trim())) {
                continue;
            }
            final String source = op.optString("sourceNodeId", "").trim();
            if (!superNodeIds.contains(source)) {
                continue;
            }
            final String sourceParent = nodeParentById.getOrDefault(source, "");
            if (!sourceParent.isBlank()) {
                return true;
            }
        }
        return false;
    }

    private void bootstrapContext(
            final JSONObject snapshot,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds) {
        final JSONObject flow = snapshot.optJSONObject("flow");
        if (flow == null) {
            return;
        }

        final String rootId = flow.optString("rootId", "").trim();
        if (!rootId.isEmpty()) {
            nodeIds.add(rootId);
            nodeParentById.put(rootId, "");
            superNodeIds.add(rootId);
        }

        final JSONArray nodes = flow.optJSONArray("nodes");
        if (nodes != null) {
            for (int i = 0; i < nodes.length(); i++) {
                final JSONObject node = nodes.optJSONObject(i);
                if (node == null) {
                    continue;
                }
                final String id = node.optString("id", "").trim();
                if (!id.isEmpty()) {
                    nodeIds.add(id);
                    nodeParentById.put(id, node.optString("parentSuperNodeId", "").trim());
                    if (node.optBoolean("isSuperNode", false)) {
                        superNodeIds.add(id);
                    }
                }
            }
        }

        final JSONArray edges = flow.optJSONArray("edges");
        if (edges != null) {
            for (int i = 0; i < edges.length(); i++) {
                final JSONObject edge = edges.optJSONObject(i);
                if (edge == null) {
                    continue;
                }
                final String id = edge.optString("id", "").trim();
                if (!id.isEmpty()) {
                    edgeIds.add(id);
                }
            }
        }

        final JSONArray variables = flow.optJSONArray("variables");
        if (variables != null) {
            for (int i = 0; i < variables.length(); i++) {
                final JSONObject variable = variables.optJSONObject(i);
                if (variable == null) {
                    continue;
                }
                final String name = variable.optString("name", "").trim();
                if (!name.isEmpty()) {
                    variableNames.add(name);
                }
            }
        }

        final JSONArray allowed = flow.optJSONArray("allowedEdgeTypes");
        if (allowed != null) {
            for (int i = 0; i < allowed.length(); i++) {
                final String edgeType = allowed.optString(i, "").trim();
                if (!edgeType.isEmpty()) {
                    allowedEdgeTypes.add(edgeType);
                }
            }
        }
    }

    private void validateOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final String kind = op.optString("op", "").trim();
        if (kind.isEmpty()) {
            emitIssue(result, "OP_MISSING_KIND", path + "/op", "Operation kind is missing.");
            return;
        }

        final OperationRuleHandler handler = operationRuleHandlers.get(kind);
        if (handler == null) {
            emitIssue(result, "OP_UNSUPPORTED", path + "/op", "Unsupported operation type: " + kind);
            return;
        }
        handler.validate(op, path, nodeIds, edgeIds, variableNames, allowedEdgeTypes, nodeParentById, superNodeIds, result);
    }

    private Map<String, OperationRuleHandler> createOperationRuleHandlers() {
        final Map<String, OperationRuleHandler> handlers = new HashMap<>();
        handlers.put("create_supernode", this::validateCreateSuperNodeOperation);
        handlers.put("create_node", this::validateCreateNodeOperation);
        handlers.put("update_node", this::validateUpdateOrDeleteNodeOperation);
        handlers.put("delete_node", this::validateUpdateOrDeleteNodeOperation);
        handlers.put("create_edge", this::validateCreateEdgeOperation);
        handlers.put("update_edge", this::validateUpdateOrDeleteEdgeOperation);
        handlers.put("delete_edge", this::validateUpdateOrDeleteEdgeOperation);
        handlers.put("add_variable_definition", this::validateAddVariableDefinitionOperation);
        handlers.put("update_variable_definition", this::validateUpdateOrDeleteVariableDefinitionOperation);
        handlers.put("delete_variable_definition", this::validateUpdateOrDeleteVariableDefinitionOperation);
        handlers.put("add_node_command", this::validateNodeCommandOperation);
        handlers.put("update_node_command", this::validateNodeCommandOperation);
        handlers.put("delete_node_command", this::validateNodeCommandOperation);
        return handlers;
    }

    private void validateCreateSuperNodeOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        validateNodeCreate(op, path, "superNodeId", "parentSuperNodeId",
                nodeIds, nodeParentById, superNodeIds, true, result);
        requireExistingNode(op, path, "parentSuperNodeId", nodeIds, result);
    }

    private void validateCreateNodeOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        validateNodeCreate(op, path, "nodeId", "parentSuperNodeId",
                nodeIds, nodeParentById, superNodeIds, false, result);
        requireExistingNode(op, path, "parentSuperNodeId", nodeIds, result);
    }

    private void validateUpdateOrDeleteNodeOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        requireExistingNode(op, path, "nodeId", nodeIds, result);
    }

    private void validateCreateEdgeOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        validateCreateEdge(op, path, nodeIds, edgeIds, variableNames, allowedEdgeTypes,
                nodeParentById, superNodeIds, result);
    }

    private void validateUpdateOrDeleteEdgeOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        requireExistingEdge(op, path, "edgeId", edgeIds, result);
        final String kind = op.optString("op", "");
        if ("update_edge".equals(kind) && op.has("payload")) {
            final JSONObject payload = op.optJSONObject("payload");
            validateEdgePayload(payload, path + "/payload", op.optString("edgeType", ""), variableNames, result);
        }
    }

    private void validateAddVariableDefinitionOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        validateVariableDefinition(op, path, nodeIds, variableNames, result);
    }

    private void validateUpdateOrDeleteVariableDefinitionOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        requireExistingNode(op, path, "ownerNodeId", nodeIds, result);
    }

    private void validateNodeCommandOperation(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        requireExistingNode(op, path, "nodeId", nodeIds, result);
    }

    private void validateNodeCreate(
            final JSONObject op,
            final String path,
            final String idField,
            final String parentField,
            final Set<String> nodeIds,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final boolean isSuperNode,
            final SemanticValidationResult result) {
        final String nodeId = op.optString(idField, "").trim();
        if (nodeId.isEmpty()) {
            emitIssue(result, "NODE_ID_MISSING", path + "/" + idField, "Node id is missing.");
            return;
        }
        if (nodeIds.contains(nodeId)) {
            emitIssue(result, "NODE_DUPLICATE", path + "/" + idField, "Node id already exists: " + nodeId);
            return;
        }
        nodeIds.add(nodeId);
        nodeParentById.put(nodeId, op.optString(parentField, "").trim());
        if (isSuperNode) {
            superNodeIds.add(nodeId);
        }
    }

    private void validateCreateEdge(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final String edgeId = op.optString("edgeId", "").trim();
        if (edgeId.isEmpty()) {
            emitIssue(result, "EDGE_ID_MISSING", path + "/edgeId", "Edge id is missing.");
        } else if (edgeIds.contains(edgeId)) {
            emitIssue(result, "EDGE_DUPLICATE", path + "/edgeId", "Edge id already exists: " + edgeId);
        } else {
            edgeIds.add(edgeId);
        }

        final String edgeType = op.optString("edgeType", "").trim();
        if (!EDGE_TYPES.contains(edgeType)) {
            emitIssue(result, "EDGE_TYPE_INVALID", path + "/edgeType", "Unsupported edge type: " + edgeType);
        } else if (!allowedEdgeTypes.isEmpty() && !allowedEdgeTypes.contains(edgeType)) {
            emitIssue(result, "EDGE_TYPE_NOT_ALLOWED", path + "/edgeType",
                    "Edge type not allowed by snapshot: " + edgeType);
        }

        requireExistingNode(op, path, "sourceNodeId", nodeIds, result);
        requireExistingNode(op, path, "targetNodeId", nodeIds, result);
        validateEdgeScope(op, path, nodeParentById, superNodeIds, result);

        final JSONObject payload = op.optJSONObject("payload");
        validateEdgePayload(payload, path + "/payload", edgeType, variableNames, result);
    }

    private void validateEdgeScope(
            final JSONObject op,
            final String path,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final String sourceId = op.optString("sourceNodeId", "").trim();
        final String targetId = op.optString("targetNodeId", "").trim();
        if (sourceId.isEmpty() || targetId.isEmpty()) {
            return;
        }
        final String sourceParent = nodeParentById.getOrDefault(sourceId, "");
        final String targetParent = nodeParentById.getOrDefault(targetId, "");
        if (sourceParent.isEmpty()) {
            return;
        }
        if (sourceParent.equals(targetParent)) {
            return;
        }
        if (superNodeIds.contains(sourceId)) {
            return;
        }
        emitIssue(result, "EDGE_CROSS_SUPERNODE_FORBIDDEN", path,
                "Node-in-supernode edges must stay in same supernode. Source=" + sourceId + ", target=" + targetId);
    }

    private void validateEdgePayload(
            final JSONObject payload,
            final String path,
            final String edgeType,
            final Set<String> variableNames,
            final SemanticValidationResult result) {
        if (payload == null) {
            if ("CEDGE".equals(edgeType) || "IEDGE".equals(edgeType)) {
                emitIssue(result, "EDGE_CONDITION_MISSING", path, "Condition payload is required for " + edgeType);
            } else if ("TEDGE".equals(edgeType)) {
                emitIssue(result, "EDGE_TIMEOUT_MISSING", path, "Timeout payload is required for TEDGE");
            } else if ("PEDGE".equals(edgeType)) {
                emitIssue(result, "EDGE_PROBABILITY_MISSING", path, "Probability payload is required for PEDGE");
            }
            return;
        }

        if ("CEDGE".equals(edgeType) || "IEDGE".equals(edgeType)) {
            final String condition = payload.optString("conditionText", "").trim();
            if (condition.isEmpty()) {
                emitIssue(result, "EDGE_CONDITION_MISSING", path + "/conditionText",
                        "Condition text is required for " + edgeType);
            } else {
                validateConditionVariables(condition, path + "/conditionText", variableNames, result);
            }
        }
        if ("TEDGE".equals(edgeType)) {
            if (!payload.has("timeoutMs")) {
                emitIssue(result, "EDGE_TIMEOUT_MISSING", path + "/timeoutMs", "timeoutMs is required for TEDGE");
            } else if (payload.optInt("timeoutMs", -1) < 0) {
                emitIssue(result, "EDGE_TIMEOUT_INVALID", path + "/timeoutMs", "timeoutMs must be >= 0");
            }
        }
        if ("PEDGE".equals(edgeType)) {
            final int probability = payload.optInt("probability", -1);
            if (probability < 0 || probability > 100) {
                emitIssue(result, "EDGE_PROBABILITY_INVALID", path + "/probability",
                        "probability must be in range [0,100]");
            }
        }
    }

    private void validateSuperNodeLiveness(
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        final Set<String> constrainedSuperNodes = new HashSet<>();
        final Set<String> internallyAliveSuperNodes = new HashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            final String edgeType = op.optString("edgeType", "").trim();
            final String source = op.optString("sourceNodeId", "").trim();
            final String target = op.optString("targetNodeId", "").trim();
            if (source.isEmpty() || target.isEmpty()) {
                continue;
            }
            if ("IEDGE".equals(edgeType) && superNodeIds.contains(source)) {
                final String sourceParent = nodeParentById.getOrDefault(source, "");
                if (sourceParent.isBlank()) {
                    continue;
                }
                constrainedSuperNodes.add(source);
            }
            if (!"TEDGE".equals(edgeType)) {
                continue;
            }
            final String sourceParent = nodeParentById.getOrDefault(source, "");
            final String targetParent = nodeParentById.getOrDefault(target, "");
            if (!sourceParent.isEmpty() && sourceParent.equals(targetParent)) {
                internallyAliveSuperNodes.add(sourceParent);
            }
        }
        for (String superNodeId : constrainedSuperNodes) {
            if (!internallyAliveSuperNodes.contains(superNodeId)) {
                emitIssue(result,
                        "SUPERNODE_INTERNAL_LIVENESS_MISSING",
                        "/operations",
                        "Constrained supernode " + superNodeId + " has no internal TEDGE liveness flow.");
            }
        }
    }

    private void validateSuperNodeExitTargetScope(
            final JSONArray operations,
            final Map<String, String> nodeParentById,
            final Set<String> superNodeIds,
            final SemanticValidationResult result) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            final String edgeType = op.optString("edgeType", "").trim();
            if (!"IEDGE".equals(edgeType) && !"CEDGE".equals(edgeType)) {
                continue;
            }
            final String source = op.optString("sourceNodeId", "").trim();
            final String target = op.optString("targetNodeId", "").trim();
            if (!superNodeIds.contains(source) || target.isEmpty()) {
                continue;
            }
            final String sourceParent = nodeParentById.getOrDefault(source, "");
            if (sourceParent.isBlank()) {
                continue;
            }
            final String targetParent = nodeParentById.getOrDefault(target, "");
            if (source.equals(targetParent)) {
                emitIssue(result,
                        "SUPERNODE_EXIT_TARGET_IN_SCOPE",
                        "/operations/" + i + "/targetNodeId",
                        "Supernode exit edge target must be outside supernode scope: " + source + " -> " + target);
            }
        }
    }

    private void validateVariableDefinition(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> variableNames,
            final SemanticValidationResult result) {
        requireExistingNode(op, path, "ownerNodeId", nodeIds, result);
        final JSONObject varDef = op.optJSONObject("varDef");
        if (varDef == null) {
            emitIssue(result, "VARDEF_MISSING", path + "/varDef", "varDef is required.");
            return;
        }
        final String varName = varDef.optString("name", "").trim();
        final String varType = varDef.optString("type", "").trim();
        if (varName.isEmpty()) {
            emitIssue(result, "VARDEF_NAME_MISSING", path + "/varDef/name", "Variable name is required.");
        } else {
            variableNames.add(varName);
        }
        if (!VARIABLE_TYPES.contains(varType)) {
            emitIssue(result, "VARDEF_TYPE_INVALID", path + "/varDef/type", "Unsupported variable type: " + varType);
        }
    }

    private void requireExistingNode(
            final JSONObject op,
            final String path,
            final String field,
            final Set<String> nodeIds,
            final SemanticValidationResult result) {
        final String nodeId = op.optString(field, "").trim();
        if (nodeId.isEmpty()) {
            emitIssue(result, "NODE_REF_MISSING", path + "/" + field, "Node reference is missing.");
        } else if (!nodeIds.contains(nodeId)) {
            emitIssue(result, "NODE_REF_UNKNOWN", path + "/" + field, "Unknown node reference: " + nodeId);
        }
    }

    private void requireExistingEdge(
            final JSONObject op,
            final String path,
            final String field,
            final Set<String> edgeIds,
            final SemanticValidationResult result) {
        final String edgeId = op.optString(field, "").trim();
        if (edgeId.isEmpty()) {
            emitIssue(result, "EDGE_REF_MISSING", path + "/" + field, "Edge reference is missing.");
        } else if (!edgeIds.contains(edgeId)) {
            emitIssue(result, "EDGE_REF_UNKNOWN", path + "/" + field, "Unknown edge reference: " + edgeId);
        }
    }

    private void validateConditionVariables(
            final String condition,
            final String path,
            final Set<String> variableNames,
            final SemanticValidationResult result) {
        final String sanitizedCondition = stripQuotedContent(condition);
        final Matcher matcher = IDENTIFIER_PATTERN.matcher(sanitizedCondition);
        final Set<String> reportedUnknowns = new HashSet<>();
        while (matcher.find()) {
            final String token = matcher.group();
            if (RESERVED_TOKENS.contains(token.toLowerCase())) {
                continue;
            }
            if (!variableNames.contains(token) && reportedUnknowns.add(token)) {
                emitIssue(result, "VAR_REF_UNKNOWN", path, "Unknown variable in condition: " + token);
            }
        }
    }

    private void emitIssue(
            final SemanticValidationResult result,
            final String issueCode,
            final String path,
            final String message) {
        if (ruleConfig.disabledRules().contains(issueCode)) {
            return;
        }
        result.addIssue(issueCode, path, message, ruleConfig.severityByRuleId().getOrDefault(issueCode, "error"));
    }

    private String stripQuotedContent(final String text) {
        final StringBuilder out = new StringBuilder(text.length());
        boolean inSingleQuote = false;
        boolean inDoubleQuote = false;
        boolean escaped = false;
        for (int i = 0; i < text.length(); i++) {
            final char c = text.charAt(i);
            if (escaped) {
                out.append(' ');
                escaped = false;
                continue;
            }
            if (c == '\\') {
                if (inSingleQuote || inDoubleQuote) {
                    out.append(' ');
                    escaped = true;
                } else {
                    out.append(c);
                }
                continue;
            }
            if (c == '\'' && !inDoubleQuote) {
                inSingleQuote = !inSingleQuote;
                out.append(' ');
                continue;
            }
            if (c == '"' && !inSingleQuote) {
                inDoubleQuote = !inDoubleQuote;
                out.append(' ');
                continue;
            }
            out.append((inSingleQuote || inDoubleQuote) ? ' ' : c);
        }
        return out.toString();
    }

    private RuleConfig loadRuleConfig(final Path mappingPath) {
        final RuleConfig defaultConfig = defaultRuleConfig();
        if (mappingPath == null || !Files.exists(mappingPath)) {
            return defaultConfig;
        }
        try {
            final JSONObject mapping;
            try (var reader = Files.newBufferedReader(mappingPath)) {
                mapping = new JSONObject(new JSONTokener(reader));
            }
            final JSONArray definitions = mapping.optJSONArray("ruleDefinitions");
            final Set<String> disabledRules = new HashSet<>();
            final JSONArray disabled = mapping.optJSONArray("disabledRules");
            if (disabled != null) {
                for (int i = 0; i < disabled.length(); i++) {
                    final String ruleId = disabled.optString(i, "").trim();
                    if (!ruleId.isBlank()) {
                        disabledRules.add(ruleId);
                    }
                }
            }
            if (definitions == null || definitions.length() == 0) {
                return new RuleConfig(defaultConfig.rules(), disabledRules, defaultConfig.severityByRuleId());
            }
            final Map<String, RuleDefinition> rules = new HashMap<>();
            final Map<String, String> severityByRuleId = new HashMap<>(defaultConfig.severityByRuleId());
            for (int i = 0; i < definitions.length(); i++) {
                final JSONObject def = definitions.optJSONObject(i);
                if (def == null) {
                    continue;
                }
                final String id = def.optString("id", "").trim();
                if (id.isBlank()) {
                    continue;
                }
                final String scope = def.optString("scope", "general").trim().toLowerCase();
                final String severity = normalizeSeverity(def.optString("severity", "error"));
                final JSONObject activation = def.optJSONObject("activation");
                final String context = activation == null ? "" : activation.optString("context", "").trim();
                final Set<String> patterns = new HashSet<>();
                if (activation != null) {
                    final JSONArray rawPatterns = activation.optJSONArray("patterns");
                    if (rawPatterns != null) {
                        for (int j = 0; j < rawPatterns.length(); j++) {
                            final String patternId = rawPatterns.optString(j, "").trim();
                            if (!patternId.isBlank()) {
                                patterns.add(patternId);
                            }
                        }
                    }
                }
                rules.put(id, new RuleDefinition(id, scope, context, patterns));
                severityByRuleId.put(id, severity);
            }
            if (rules.isEmpty()) {
                return defaultConfig;
            }
            return new RuleConfig(rules, disabledRules, severityByRuleId);
        } catch (IOException | RuntimeException ignored) {
            return defaultConfig;
        }
    }

    private String normalizeSeverity(final String value) {
        return "warning".equalsIgnoreCase(value) ? "warning" : "error";
    }

    private RuleConfig defaultRuleConfig() {
        final Map<String, RuleDefinition> rules = Map.of(
                RULE_EXIT_TARGET_OUTSIDE_SCOPE,
                new RuleDefinition(RULE_EXIT_TARGET_OUTSIDE_SCOPE, "general", "", Set.of()),
                RULE_INTERNAL_LIVENESS_REQUIRED,
                new RuleDefinition(RULE_INTERNAL_LIVENESS_REQUIRED, "context",
                        "constrained_wait_supernode",
                        Set.of("constrained_activity_base", "periodic_reminder_while_waiting"))
        );
        final Map<String, String> severityByRuleId = new HashMap<>();
        for (String ruleId : KNOWN_RULE_IDS) {
            severityByRuleId.put(ruleId, "error");
        }
        return new RuleConfig(rules, Set.of(), severityByRuleId);
    }

    private record RuleConfig(
            Map<String, RuleDefinition> rules,
            Set<String> disabledRules,
            Map<String, String> severityByRuleId) {
    }

    private record RuleDefinition(String id, String scope, String context, Set<String> patterns) {
    }

    @FunctionalInterface
    private interface InvariantRuleHandler {
        void validate(
                JSONArray operations,
                Map<String, String> nodeParentById,
                Set<String> superNodeIds,
                SemanticValidationResult result);
    }

    @FunctionalInterface
    private interface OperationRuleHandler {
        void validate(
                JSONObject op,
                String path,
                Set<String> nodeIds,
                Set<String> edgeIds,
                Set<String> variableNames,
                Set<String> allowedEdgeTypes,
                Map<String, String> nodeParentById,
                Set<String> superNodeIds,
                SemanticValidationResult result);
    }
}
