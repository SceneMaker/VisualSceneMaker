package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.util.llm.LLMSupport;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public final class SceneFlowIrLlmCandidateProvider {

    public static final class Config {
        private final String baseUrl;
        private final String apiKey;
        private final String modelId;
        private final int timeoutSeconds;
        private final int maxCandidates;

        public Config(
                final String baseUrl,
                final String apiKey,
                final String modelId,
                final int timeoutSeconds,
                final int maxCandidates) {
            this.baseUrl = baseUrl;
            this.apiKey = apiKey;
            this.modelId = modelId;
            this.timeoutSeconds = timeoutSeconds;
            this.maxCandidates = maxCandidates;
        }

        public String baseUrl() {
            return baseUrl;
        }

        public String apiKey() {
            return apiKey;
        }

        public String modelId() {
            return modelId;
        }

        public int timeoutSeconds() {
            return timeoutSeconds;
        }

        public int maxCandidates() {
            return maxCandidates;
        }

        public boolean isConfigured() {
            return baseUrl != null && !baseUrl.isBlank() && modelId != null && !modelId.isBlank();
        }
    }

    public List<JSONObject> generateCandidates(
            final String situation,
            final JSONObject snapshot,
            final Config config,
            final SceneFlowSituationPipeline.OutputMode outputMode) throws SceneFlowIrCompileException {
        if (config == null || !config.isConfigured()) {
            throw new SceneFlowIrCompileException("LLM provider requires baseUrl and modelId.");
        }
        final LLMSupport llm = new LLMSupport(
                config.baseUrl(),
                config.apiKey(),
                Duration.ofSeconds(Math.max(1, config.timeoutSeconds())));
        llm.setSelectedModel(config.modelId());
        llm.setDefaultTemperature(0.2);

        final String prompt = buildPrompt(
                situation, snapshot, Math.max(1, config.maxCandidates()), outputMode);
        final String content = sendPromptWithCompatibilityFallback(llm, prompt);

        final List<JSONObject> candidates = parseCandidates(content);
        if (candidates.isEmpty()) {
            throw new SceneFlowIrCompileException("LLM returned no parsable IR candidates.");
        }
        final int limit = Math.max(1, config.maxCandidates());
        final List<JSONObject> limited = candidates.subList(0, Math.min(limit, candidates.size()));
        for (int i = 0; i < limited.size(); i++) {
            final JSONObject candidate = limited.get(i);
            normalizeCandidate(candidate, i);
            if (!candidate.has("metadata") || candidate.optJSONObject("metadata") == null) {
                candidate.put("metadata", new JSONObject());
            }
            candidate.getJSONObject("metadata").put("source", "llm");
            if (!candidate.has("irVersion")) {
                candidate.put("irVersion", "1.0");
            }
            if (!candidate.has("mode")) {
                candidate.put("mode", "patch");
            }
        }
        return new ArrayList<>(limited);
    }

    private void normalizeCandidate(final JSONObject candidate, final int candidateIndex) {
        final JSONArray operations = candidate.optJSONArray("operations");
        if (operations == null) {
            return;
        }
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String normalizedOp = normalizeOperationName(op.optString("op", ""));
            if (!normalizedOp.isEmpty()) {
                op.put("op", normalizedOp);
            }
            normalizeOperationFields(op, normalizedOp, candidateIndex, i);
        }
    }

    private String normalizeOperationName(final String raw) {
        if (raw == null || raw.isBlank()) {
            return "";
        }
        final String collapsed = raw
                .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
                .replace('-', '_')
                .replace(' ', '_')
                .trim()
                .toLowerCase();
        switch (collapsed) {
            case "create_supernode":
            case "create_node":
            case "update_node":
            case "delete_node":
            case "create_edge":
            case "update_edge":
            case "delete_edge":
            case "add_node_command":
            case "update_node_command":
            case "delete_node_command":
            case "add_variable_definition":
            case "update_variable_definition":
            case "delete_variable_definition":
                return collapsed;
            default:
                return collapsed;
        }
    }

    private void normalizeOperationFields(final JSONObject op, final String opName, final int candidateIndex, final int opIndex) {
        if (opName == null || opName.isBlank()) {
            return;
        }
        final JSONObject nestedNode = resolveNestedNode(op, opName);
        final JSONObject nestedEdge = resolveNestedEdge(op, opName);
        switch (opName) {
            case "create_edge":
                copyIfMissing(op, "edgeId", firstNonBlank(op, "edgeId", "id", "edge_id"));
                copyIfMissing(op, "edgeId", firstNonBlankFrom(nestedEdge, "id", "edgeId", "edge_id"));
                copyIfMissing(op, "edgeType", firstNonBlank(op, "edgeType", "type", "edge_type"));
                copyIfMissing(op, "edgeType", firstNonBlankFrom(nestedEdge, "type", "edgeType", "edge_type"));
                copyIfMissing(op, "sourceNodeId", firstNonBlank(op, "sourceNodeId", "sourceId", "source", "from"));
                copyIfMissing(op, "sourceNodeId", firstNonBlankFrom(nestedEdge, "sourceNodeId", "sourceId", "source", "from"));
                copyIfMissing(op, "targetNodeId", firstNonBlank(op, "targetNodeId", "targetId", "target", "to"));
                copyIfMissing(op, "targetNodeId", firstNonBlankFrom(nestedEdge, "targetNodeId", "targetId", "target", "to"));
                if (!op.has("edgeId") || op.optString("edgeId", "").isBlank()) {
                    op.put("edgeId", "llm_edge_" + (candidateIndex + 1) + "_" + (opIndex + 1));
                }
                normalizeEdgePayload(op);
                break;
            case "update_edge":
            case "delete_edge":
                copyIfMissing(op, "edgeId", firstNonBlank(op, "edgeId", "id", "edge_id"));
                copyIfMissing(op, "edgeId", firstNonBlankFrom(nestedEdge, "id", "edgeId", "edge_id"));
                copyIfMissing(op, "edgeType", firstNonBlank(op, "edgeType", "type", "edge_type"));
                copyIfMissing(op, "edgeType", firstNonBlankFrom(nestedEdge, "type", "edgeType", "edge_type"));
                copyIfMissing(op, "sourceNodeId", firstNonBlank(op, "sourceNodeId", "sourceId", "source", "from"));
                copyIfMissing(op, "sourceNodeId", firstNonBlankFrom(nestedEdge, "sourceNodeId", "sourceId", "source", "from"));
                if ("update_edge".equals(opName)) {
                    copyIfMissing(op, "targetNodeId", firstNonBlank(op, "targetNodeId", "targetId", "target", "to"));
                    copyIfMissing(op, "targetNodeId", firstNonBlankFrom(nestedEdge, "targetNodeId", "targetId", "target", "to"));
                    normalizeEdgePayload(op);
                }
                // LLMs sometimes output "update_edge" without edgeId while describing a new edge.
                if (!op.has("edgeId") && hasCreateEdgeShape(op)) {
                    op.put("op", "create_edge");
                    copyIfMissing(op, "edgeId", "llm_edge_" + (candidateIndex + 1) + "_" + (opIndex + 1));
                    copyIfMissing(op, "edgeType", firstNonBlank(op, "edgeType", "type", "edge_type"));
                    copyIfMissing(op, "sourceNodeId", firstNonBlank(op, "sourceNodeId", "sourceId", "source", "from"));
                    copyIfMissing(op, "targetNodeId", firstNonBlank(op, "targetNodeId", "targetId", "target", "to"));
                }
                break;
            case "create_node":
            case "update_node":
            case "delete_node":
                if ("create_node".equals(opName) && nestedNode != null && nestedNode.optBoolean("isSuperNode", false)) {
                    op.put("op", "create_supernode");
                    copyIfMissing(op, "superNodeId", firstNonBlank(op, "superNodeId", "nodeId", "id", "super_node_id"));
                    copyIfMissing(op, "superNodeId", firstNonBlankFrom(nestedNode, "id", "nodeId", "superNodeId"));
                    copyIfMissing(op, "parentSuperNodeId", firstNonBlank(op, "parentSuperNodeId", "parentId", "parent", "parentNodeId"));
                    copyIfMissing(op, "parentSuperNodeId", firstNonBlankFrom(nestedNode, "parentSuperNodeId", "parentId", "parent", "parentNodeId"));
                    copyIfMissing(op, "name", firstNonBlank(op, "name"));
                    copyIfMissing(op, "name", firstNonBlankFrom(nestedNode, "name"));
                    break;
                }
                copyIfMissing(op, "nodeId", firstNonBlank(op, "nodeId", "id", "node_id"));
                copyIfMissing(op, "nodeId", firstNonBlankFrom(nestedNode, "id", "nodeId", "node_id"));
                if ("create_node".equals(opName)) {
                    copyIfMissing(op, "parentSuperNodeId",
                            firstNonBlank(op, "parentSuperNodeId", "parentId", "parent", "parentNodeId"));
                    copyIfMissing(op, "parentSuperNodeId",
                            firstNonBlankFrom(nestedNode, "parentSuperNodeId", "parentId", "parent", "parentNodeId"));
                    copyIfMissing(op, "name", firstNonBlank(op, "name"));
                    copyIfMissing(op, "name", firstNonBlankFrom(nestedNode, "name"));
                }
                break;
            case "create_supernode":
                copyIfMissing(op, "superNodeId", firstNonBlank(op, "superNodeId", "nodeId", "id", "super_node_id"));
                copyIfMissing(op, "parentSuperNodeId",
                        firstNonBlank(op, "parentSuperNodeId", "parentId", "parent", "parentNodeId"));
                break;
            case "add_variable_definition":
            case "update_variable_definition":
            case "delete_variable_definition":
                copyIfMissing(op, "ownerNodeId", firstNonBlank(op, "ownerNodeId", "ownerId", "nodeId", "owner"));
                break;
            case "add_node_command":
            case "update_node_command":
            case "delete_node_command":
                copyIfMissing(op, "nodeId", firstNonBlank(op, "nodeId", "id", "ownerNodeId"));
                copyIfMissing(op, "commandText", firstNonBlank(op, "commandText", "command", "text"));
                break;
            default:
                break;
        }
    }

    private boolean hasCreateEdgeShape(final JSONObject op) {
        final String source = firstNonBlank(op, "sourceNodeId", "sourceId", "source", "from");
        final String target = firstNonBlank(op, "targetNodeId", "targetId", "target", "to");
        final String edgeType = firstNonBlank(op, "edgeType", "type", "edge_type");
        return !source.isBlank() && !target.isBlank() && !edgeType.isBlank();
    }

    private void normalizeEdgePayload(final JSONObject op) {
        final JSONObject nestedEdge = resolveNestedEdge(op, op.optString("op", ""));
        JSONObject payload = op.optJSONObject("payload");
        if (payload == null && nestedEdge != null && nestedEdge.optJSONObject("payload") != null) {
            payload = new JSONObject(nestedEdge.getJSONObject("payload").toString());
            op.put("payload", payload);
        }
        if (payload == null) {
            payload = new JSONObject();
            boolean hasPayloadContent = false;
            final String condition = firstNonBlank(op, "conditionText", "condition", "guard");
            if (!condition.isBlank()) {
                payload.put("conditionText", condition);
                hasPayloadContent = true;
            } else {
                final String nestedCondition = firstNonBlankFromPayload(nestedEdge, "conditionText", "condition", "guard");
                if (!nestedCondition.isBlank()) {
                    payload.put("conditionText", nestedCondition);
                    hasPayloadContent = true;
                }
            }
            final Integer timeout = firstInt(op, "timeoutMs", "timeout");
            if (timeout != null) {
                payload.put("timeoutMs", timeout);
                hasPayloadContent = true;
            } else {
                final Integer nestedTimeout = firstIntFromPayload(nestedEdge, "timeoutMs", "timeout");
                if (nestedTimeout != null) {
                    payload.put("timeoutMs", nestedTimeout);
                    hasPayloadContent = true;
                }
            }
            final Integer probability = firstInt(op, "probability", "chance");
            if (probability != null) {
                payload.put("probability", probability);
                hasPayloadContent = true;
            } else {
                final Integer nestedProbability = firstIntFromPayload(nestedEdge, "probability", "chance");
                if (nestedProbability != null) {
                    payload.put("probability", nestedProbability);
                    hasPayloadContent = true;
                }
            }
            if (hasPayloadContent) {
                op.put("payload", payload);
            }
            return;
        }
        copyIfMissing(payload, "conditionText", firstNonBlank(payload, "conditionText", "condition", "guard"));
        final Integer timeout = firstInt(payload, "timeoutMs", "timeout");
        if (timeout != null && !payload.has("timeoutMs")) {
            payload.put("timeoutMs", timeout);
        }
        final Integer probability = firstInt(payload, "probability", "chance");
        if (probability != null && !payload.has("probability")) {
            payload.put("probability", probability);
        }
    }

    private String firstNonBlankFrom(final JSONObject source, final String... keys) {
        if (source == null) {
            return "";
        }
        for (String key : keys) {
            final String value = resolveStringValue(source, key);
            if (!value.isBlank()) {
                return value;
            }
        }
        return "";
    }

    private String firstNonBlankFromPayload(final JSONObject source, final String... keys) {
        if (source == null) {
            return "";
        }
        final JSONObject payload = source.optJSONObject("payload");
        if (payload == null) {
            return "";
        }
        return firstNonBlankFrom(payload, keys);
    }

    private Integer firstIntFromPayload(final JSONObject source, final String... keys) {
        if (source == null) {
            return null;
        }
        final JSONObject payload = source.optJSONObject("payload");
        if (payload == null) {
            return null;
        }
        for (String key : keys) {
            try {
                final Integer value = resolveIntValue(payload, key);
                if (value != null) {
                    return value;
                }
            } catch (RuntimeException ignored) {
                // continue
            }
        }
        return null;
    }

    private JSONObject resolveNestedNode(final JSONObject op, final String opName) {
        final JSONObject explicit = op.optJSONObject("node");
        if (explicit != null) {
            return explicit;
        }
        final JSONObject data = op.optJSONObject("data");
        if (data != null) {
            if ("create_node".equals(opName)
                    || "update_node".equals(opName)
                    || "delete_node".equals(opName)
                    || "create_supernode".equals(opName)) {
                if (data.has("id") || data.has("nodeId") || data.has("parentSuperNodeId")
                        || data.has("isSuperNode") || data.has("name")) {
                    return data;
                }
            }
        }
        final JSONObject payload = op.optJSONObject("payload");
        if (payload == null) {
            return null;
        }
        if ("create_node".equals(opName)
                || "update_node".equals(opName)
                || "delete_node".equals(opName)) {
            if (payload.has("id") || payload.has("nodeId") || payload.has("parentSuperNodeId")
                    || payload.has("isSuperNode") || payload.has("name")) {
                return payload;
            }
        }
        return null;
    }

    private JSONObject resolveNestedEdge(final JSONObject op, final String opName) {
        final JSONObject explicit = op.optJSONObject("edge");
        if (explicit != null) {
            return explicit;
        }
        final JSONObject data = op.optJSONObject("data");
        if (data != null) {
            if ("create_edge".equals(opName)
                    || "update_edge".equals(opName)
                    || "delete_edge".equals(opName)) {
                if (data.has("type") || data.has("edgeType") || data.has("sourceNodeId")
                        || data.has("targetNodeId") || data.has("id") || data.has("edgeId")
                        || data.has("payload")) {
                    return data;
                }
            }
        }
        final JSONObject payload = op.optJSONObject("payload");
        if (payload == null) {
            return null;
        }
        if ("create_edge".equals(opName)
                || "update_edge".equals(opName)
                || "delete_edge".equals(opName)) {
            if (payload.has("type") || payload.has("edgeType") || payload.has("sourceNodeId")
                    || payload.has("targetNodeId") || payload.has("id") || payload.has("edgeId")) {
                return payload;
            }
        }
        return null;
    }

    private void copyIfMissing(final JSONObject target, final String key, final String value) {
        if (target.has(key)) {
            return;
        }
        if (value != null && !value.isBlank()) {
            target.put(key, value);
        }
    }

    private String firstNonBlank(final JSONObject source, final String... keys) {
        final JSONObject nestedEdge = source.optJSONObject("edge");
        final JSONObject nestedData = source.optJSONObject("data");
        for (String key : keys) {
            final String value = resolveStringValue(source, key);
            if (!value.isBlank()) {
                return value;
            }
            if (nestedData != null) {
                final String nestedDataValue = resolveStringValue(nestedData, key);
                if (!nestedDataValue.isBlank()) {
                    return nestedDataValue;
                }
            }
            if (nestedEdge != null) {
                final String nestedValue = resolveStringValue(nestedEdge, key);
                if (!nestedValue.isBlank()) {
                    return nestedValue;
                }
            }
        }
        return "";
    }

    private Integer firstInt(final JSONObject source, final String... keys) {
        final JSONObject nestedEdge = source.optJSONObject("edge");
        final JSONObject nestedData = source.optJSONObject("data");
        for (String key : keys) {
            try {
                final Integer direct = resolveIntValue(source, key);
                if (direct != null) {
                    return direct;
                }
            } catch (RuntimeException ignored) {
                // continue
            }
            if (nestedData != null) {
                try {
                    final Integer nested = resolveIntValue(nestedData, key);
                    if (nested != null) {
                        return nested;
                    }
                } catch (RuntimeException ignored) {
                    // continue
                }
            }
            if (nestedEdge != null) {
                try {
                    final Integer nested = resolveIntValue(nestedEdge, key);
                    if (nested != null) {
                        return nested;
                    }
                } catch (RuntimeException ignored) {
                    // continue
                }
            }
        }
        return null;
    }

    private String resolveStringValue(final JSONObject source, final String alias) {
        final Object value = resolveValue(source, alias);
        if (value == null) {
            return "";
        }
        return String.valueOf(value).trim();
    }

    private Integer resolveIntValue(final JSONObject source, final String alias) {
        final Object value = resolveValue(source, alias);
        if (value == null) {
            return null;
        }
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        return Integer.parseInt(String.valueOf(value).trim());
    }

    private Object resolveValue(final JSONObject source, final String alias) {
        if (source.has(alias)) {
            return source.opt(alias);
        }
        final String target = normalizeKey(alias);
        final Iterator<String> keys = source.keys();
        while (keys.hasNext()) {
            final String actualKey = keys.next();
            if (normalizeKey(actualKey).equals(target)) {
                return source.opt(actualKey);
            }
        }
        return null;
    }

    private String normalizeKey(final String key) {
        return key == null ? "" : key.replaceAll("[^A-Za-z0-9]", "").toLowerCase();
    }

    private String sendPromptWithCompatibilityFallback(final LLMSupport llm, final String prompt)
            throws SceneFlowIrCompileException {
        final LLMSupport.LLMPrompt withMaxTokens = LLMSupport.LLMPrompt.builder()
                .addSystemMessage(
                        "You generate SceneFlow IR JSON only. Do not output markdown. " +
                                "Return either a single IR object or {\"candidates\":[...]} with up to the requested candidate count.")
                .addUserMessage(prompt)
                .maxTokens(2000)
                .build();
        try {
            return llm.sendPrompt(withMaxTokens).content();
        } catch (IOException | InterruptedException | RuntimeException firstExc) {
            if (!isUnsupportedMaxTokens(firstExc)) {
                throw new SceneFlowIrCompileException("LLM generation failed: " + firstExc.getMessage(), firstExc);
            }
        }

        final LLMSupport.LLMPrompt withoutMaxTokens = LLMSupport.LLMPrompt.builder()
                .addSystemMessage(
                        "You generate SceneFlow IR JSON only. Do not output markdown. " +
                                "Return either a single IR object or {\"candidates\":[...]} with up to the requested candidate count.")
                .addUserMessage(prompt)
                .build();
        try {
            return llm.sendPrompt(withoutMaxTokens).content();
        } catch (IOException | InterruptedException | RuntimeException exc) {
            throw new SceneFlowIrCompileException("LLM generation failed: " + exc.getMessage(), exc);
        }
    }

    private boolean isUnsupportedMaxTokens(final Throwable throwable) {
        if (throwable == null || throwable.getMessage() == null) {
            return false;
        }
        final String message = throwable.getMessage().toLowerCase();
        return message.contains("unsupported parameter")
                && message.contains("max_tokens");
    }

    private String buildPrompt(
            final String situation,
            final JSONObject snapshot,
            final int maxCandidates,
            final SceneFlowSituationPipeline.OutputMode outputMode) {
        final boolean standalone = outputMode == SceneFlowSituationPipeline.OutputMode.STANDALONE;
        return "Situation:\n" + (situation == null ? "" : situation) + "\n\n"
                + "Constraints:\n"
                + "- irVersion must be \"1.0\"\n"
                + "- mode must be \"patch\"\n"
                + "- operations must use allowed node/edge ids and edge types from snapshot\n"
                + "- for CEDGE/IEDGE provide payload.conditionText\n"
                + "- for TEDGE provide payload.timeoutMs >= 0\n"
                + "- for PEDGE provide payload.probability in [0,100]\n"
                + (standalone
                ? "- output target is standalone: only use create_* operations; do not use update_* or delete_* operations\n"
                + "- output target is standalone: create all required nodes and edges explicitly with fresh ids\n"
                : "")
                + "- keep candidate count <= " + maxCandidates + "\n\n"
                + "Snapshot:\n" + snapshot.toString(2) + "\n\n"
                + "Output JSON only.";
    }

    private List<JSONObject> parseCandidates(final String rawContent) throws SceneFlowIrCompileException {
        final String jsonText = extractJsonBlock(rawContent == null ? "" : rawContent.trim());
        if (jsonText.isEmpty()) {
            throw new SceneFlowIrCompileException("LLM response did not contain JSON.");
        }
        final List<JSONObject> results = new ArrayList<>();
        try {
            if (jsonText.startsWith("[")) {
                final JSONArray arr = new JSONArray(jsonText);
                for (int i = 0; i < arr.length(); i++) {
                    final JSONObject item = arr.optJSONObject(i);
                    if (item != null) {
                        results.add(item);
                    }
                }
                return dedupe(results);
            }
            final JSONObject root = new JSONObject(jsonText);
            final JSONArray candidates = root.optJSONArray("candidates");
            if (candidates != null) {
                for (int i = 0; i < candidates.length(); i++) {
                    final JSONObject candidate = candidates.optJSONObject(i);
                    if (candidate != null) {
                        results.add(candidate);
                    }
                }
                return dedupe(results);
            }
            if (root.has("operations")) {
                results.add(root);
                return results;
            }
            throw new SceneFlowIrCompileException("LLM JSON did not contain IR operations.");
        } catch (RuntimeException exc) {
            throw new SceneFlowIrCompileException("Unable to parse LLM JSON response.", exc);
        }
    }

    private List<JSONObject> dedupe(final List<JSONObject> input) {
        final Set<String> seen = new LinkedHashSet<>();
        final List<JSONObject> out = new ArrayList<>();
        for (JSONObject obj : input) {
            final String key = obj.toString();
            if (seen.add(key)) {
                out.add(obj);
            }
        }
        return out;
    }

    private String extractJsonBlock(final String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        String trimmed = text.trim();
        if (trimmed.startsWith("```")) {
            final int firstNewline = trimmed.indexOf('\n');
            if (firstNewline >= 0) {
                trimmed = trimmed.substring(firstNewline + 1);
            }
            final int closing = trimmed.lastIndexOf("```");
            if (closing >= 0) {
                trimmed = trimmed.substring(0, closing);
            }
            trimmed = trimmed.trim();
        }
        final int objStart = trimmed.indexOf('{');
        final int arrStart = trimmed.indexOf('[');
        int start = -1;
        if (objStart >= 0 && arrStart >= 0) {
            start = Math.min(objStart, arrStart);
        } else if (objStart >= 0) {
            start = objStart;
        } else if (arrStart >= 0) {
            start = arrStart;
        }
        if (start < 0) {
            return "";
        }
        return trimmed.substring(start).trim();
    }
}
