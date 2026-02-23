package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.util.llm.LLMSupport;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
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
            final Config config) throws SceneFlowIrCompileException {
        if (config == null || !config.isConfigured()) {
            throw new SceneFlowIrCompileException("LLM provider requires baseUrl and modelId.");
        }
        final LLMSupport llm = new LLMSupport(
                config.baseUrl(),
                config.apiKey(),
                Duration.ofSeconds(Math.max(1, config.timeoutSeconds())));
        llm.setSelectedModel(config.modelId());
        llm.setDefaultTemperature(0.2);

        final String prompt = buildPrompt(situation, snapshot, Math.max(1, config.maxCandidates()));
        final LLMSupport.LLMPrompt chatPrompt = LLMSupport.LLMPrompt.builder()
                .addSystemMessage(
                        "You generate SceneFlow IR JSON only. Do not output markdown. " +
                                "Return either a single IR object or {\"candidates\":[...]} with up to the requested candidate count.")
                .addUserMessage(prompt)
                .maxTokens(2000)
                .build();

        final String content;
        try {
            content = llm.sendPrompt(chatPrompt).content();
        } catch (IOException | InterruptedException | RuntimeException exc) {
            throw new SceneFlowIrCompileException("LLM generation failed: " + exc.getMessage(), exc);
        }

        final List<JSONObject> candidates = parseCandidates(content);
        if (candidates.isEmpty()) {
            throw new SceneFlowIrCompileException("LLM returned no parsable IR candidates.");
        }
        final int limit = Math.max(1, config.maxCandidates());
        final List<JSONObject> limited = candidates.subList(0, Math.min(limit, candidates.size()));
        for (int i = 0; i < limited.size(); i++) {
            final JSONObject candidate = limited.get(i);
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

    private String buildPrompt(final String situation, final JSONObject snapshot, final int maxCandidates) {
        return "Situation:\n" + (situation == null ? "" : situation) + "\n\n"
                + "Constraints:\n"
                + "- irVersion must be \"1.0\"\n"
                + "- mode must be \"patch\"\n"
                + "- operations must use allowed node/edge ids and edge types from snapshot\n"
                + "- for CEDGE/IEDGE provide payload.conditionText\n"
                + "- for TEDGE provide payload.timeoutMs >= 0\n"
                + "- for PEDGE provide payload.probability in [0,100]\n"
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

