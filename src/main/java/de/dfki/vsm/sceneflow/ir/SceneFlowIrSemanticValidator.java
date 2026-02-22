package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SceneFlowIrSemanticValidator {

    private static final Set<String> EDGE_TYPES = Set.of("EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE");
    private static final Set<String> VARIABLE_TYPES = Set.of("Int", "Bool", "Float", "String", "Event");
    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("\\b[A-Za-z_][A-Za-z0-9_]*\\b");
    private static final Set<String> RESERVED_TOKENS = Set.of(
            "true", "false", "null", "and", "or", "not", "in", "if", "then", "else");

    public SemanticValidationResult validate(final JSONObject ir, final JSONObject snapshot) {
        final SemanticValidationResult result = new SemanticValidationResult();
        final Set<String> nodeIds = new HashSet<>();
        final Set<String> edgeIds = new HashSet<>();
        final Set<String> variableNames = new HashSet<>();
        final Set<String> allowedEdgeTypes = new HashSet<>();

        bootstrapContext(snapshot, nodeIds, edgeIds, variableNames, allowedEdgeTypes);

        final JSONArray operations = ir.optJSONArray("operations");
        if (operations == null || operations.length() == 0) {
            result.addIssue("IR_EMPTY", "/operations", "IR operations list is empty.");
            return result;
        }

        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            final String path = "/operations/" + i;
            if (op == null) {
                result.addIssue("OP_INVALID", path, "Operation is not an object.");
                continue;
            }
            validateOperation(op, path, nodeIds, edgeIds, variableNames, allowedEdgeTypes, result);
        }

        return result;
    }

    private void bootstrapContext(
            final JSONObject snapshot,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes) {
        final JSONObject flow = snapshot.optJSONObject("flow");
        if (flow == null) {
            return;
        }

        final String rootId = flow.optString("rootId", "").trim();
        if (!rootId.isEmpty()) {
            nodeIds.add(rootId);
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
            final SemanticValidationResult result) {
        final String kind = op.optString("op", "").trim();
        if (kind.isEmpty()) {
            result.addIssue("OP_MISSING_KIND", path + "/op", "Operation kind is missing.");
            return;
        }

        switch (kind) {
            case "create_supernode":
                validateNodeCreate(op, path, "superNodeId", nodeIds, result);
                requireExistingNode(op, path, "parentSuperNodeId", nodeIds, result);
                break;
            case "create_node":
                validateNodeCreate(op, path, "nodeId", nodeIds, result);
                requireExistingNode(op, path, "parentSuperNodeId", nodeIds, result);
                break;
            case "update_node":
            case "delete_node":
                requireExistingNode(op, path, "nodeId", nodeIds, result);
                break;
            case "create_edge":
                validateCreateEdge(op, path, nodeIds, edgeIds, variableNames, allowedEdgeTypes, result);
                break;
            case "update_edge":
            case "delete_edge":
                requireExistingEdge(op, path, "edgeId", edgeIds, result);
                if ("update_edge".equals(kind) && op.has("payload")) {
                    final JSONObject payload = op.optJSONObject("payload");
                    validateEdgePayload(payload, path + "/payload", op.optString("edgeType", ""), variableNames, result);
                }
                break;
            case "add_variable_definition":
                validateVariableDefinition(op, path, nodeIds, variableNames, result);
                break;
            case "update_variable_definition":
            case "delete_variable_definition":
                requireExistingNode(op, path, "ownerNodeId", nodeIds, result);
                break;
            case "add_node_command":
            case "update_node_command":
            case "delete_node_command":
                requireExistingNode(op, path, "nodeId", nodeIds, result);
                break;
            default:
                result.addIssue("OP_UNSUPPORTED", path + "/op", "Unsupported operation type: " + kind);
                break;
        }
    }

    private void validateNodeCreate(
            final JSONObject op,
            final String path,
            final String idField,
            final Set<String> nodeIds,
            final SemanticValidationResult result) {
        final String nodeId = op.optString(idField, "").trim();
        if (nodeId.isEmpty()) {
            result.addIssue("NODE_ID_MISSING", path + "/" + idField, "Node id is missing.");
            return;
        }
        if (nodeIds.contains(nodeId)) {
            result.addIssue("NODE_DUPLICATE", path + "/" + idField, "Node id already exists: " + nodeId);
            return;
        }
        nodeIds.add(nodeId);
    }

    private void validateCreateEdge(
            final JSONObject op,
            final String path,
            final Set<String> nodeIds,
            final Set<String> edgeIds,
            final Set<String> variableNames,
            final Set<String> allowedEdgeTypes,
            final SemanticValidationResult result) {
        final String edgeId = op.optString("edgeId", "").trim();
        if (edgeId.isEmpty()) {
            result.addIssue("EDGE_ID_MISSING", path + "/edgeId", "Edge id is missing.");
        } else if (edgeIds.contains(edgeId)) {
            result.addIssue("EDGE_DUPLICATE", path + "/edgeId", "Edge id already exists: " + edgeId);
        } else {
            edgeIds.add(edgeId);
        }

        final String edgeType = op.optString("edgeType", "").trim();
        if (!EDGE_TYPES.contains(edgeType)) {
            result.addIssue("EDGE_TYPE_INVALID", path + "/edgeType", "Unsupported edge type: " + edgeType);
        } else if (!allowedEdgeTypes.isEmpty() && !allowedEdgeTypes.contains(edgeType)) {
            result.addIssue("EDGE_TYPE_NOT_ALLOWED", path + "/edgeType",
                    "Edge type not allowed by snapshot: " + edgeType);
        }

        requireExistingNode(op, path, "sourceNodeId", nodeIds, result);
        requireExistingNode(op, path, "targetNodeId", nodeIds, result);

        final JSONObject payload = op.optJSONObject("payload");
        validateEdgePayload(payload, path + "/payload", edgeType, variableNames, result);
    }

    private void validateEdgePayload(
            final JSONObject payload,
            final String path,
            final String edgeType,
            final Set<String> variableNames,
            final SemanticValidationResult result) {
        if (payload == null) {
            if ("CEDGE".equals(edgeType) || "IEDGE".equals(edgeType)) {
                result.addIssue("EDGE_CONDITION_MISSING", path, "Condition payload is required for " + edgeType);
            } else if ("TEDGE".equals(edgeType)) {
                result.addIssue("EDGE_TIMEOUT_MISSING", path, "Timeout payload is required for TEDGE");
            } else if ("PEDGE".equals(edgeType)) {
                result.addIssue("EDGE_PROBABILITY_MISSING", path, "Probability payload is required for PEDGE");
            }
            return;
        }

        if ("CEDGE".equals(edgeType) || "IEDGE".equals(edgeType)) {
            final String condition = payload.optString("conditionText", "").trim();
            if (condition.isEmpty()) {
                result.addIssue("EDGE_CONDITION_MISSING", path + "/conditionText",
                        "Condition text is required for " + edgeType);
            } else {
                validateConditionVariables(condition, path + "/conditionText", variableNames, result);
            }
        }
        if ("TEDGE".equals(edgeType)) {
            if (!payload.has("timeoutMs")) {
                result.addIssue("EDGE_TIMEOUT_MISSING", path + "/timeoutMs", "timeoutMs is required for TEDGE");
            } else if (payload.optInt("timeoutMs", -1) < 0) {
                result.addIssue("EDGE_TIMEOUT_INVALID", path + "/timeoutMs", "timeoutMs must be >= 0");
            }
        }
        if ("PEDGE".equals(edgeType)) {
            final int probability = payload.optInt("probability", -1);
            if (probability < 0 || probability > 100) {
                result.addIssue("EDGE_PROBABILITY_INVALID", path + "/probability",
                        "probability must be in range [0,100]");
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
            result.addIssue("VARDEF_MISSING", path + "/varDef", "varDef is required.");
            return;
        }
        final String varName = varDef.optString("name", "").trim();
        final String varType = varDef.optString("type", "").trim();
        if (varName.isEmpty()) {
            result.addIssue("VARDEF_NAME_MISSING", path + "/varDef/name", "Variable name is required.");
        } else {
            variableNames.add(varName);
        }
        if (!VARIABLE_TYPES.contains(varType)) {
            result.addIssue("VARDEF_TYPE_INVALID", path + "/varDef/type", "Unsupported variable type: " + varType);
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
            result.addIssue("NODE_REF_MISSING", path + "/" + field, "Node reference is missing.");
        } else if (!nodeIds.contains(nodeId)) {
            result.addIssue("NODE_REF_UNKNOWN", path + "/" + field, "Unknown node reference: " + nodeId);
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
            result.addIssue("EDGE_REF_MISSING", path + "/" + field, "Edge reference is missing.");
        } else if (!edgeIds.contains(edgeId)) {
            result.addIssue("EDGE_REF_UNKNOWN", path + "/" + field, "Unknown edge reference: " + edgeId);
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
                result.addIssue("VAR_REF_UNKNOWN", path, "Unknown variable in condition: " + token);
            }
        }
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
}
