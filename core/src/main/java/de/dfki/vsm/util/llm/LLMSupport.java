package de.dfki.vsm.util.llm;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Lightweight helper that talks to an LM Studio (or OpenAI-compatible) REST endpoint.
 * The class can enumerate available models and submit chat prompts using the selected model.
 */
public class LLMSupport {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static volatile Supplier<HttpTransport> sDefaultTransportFactory = null;

    private final HttpTransport mHttpTransport;
    private final URI mBaseUri;
    private final String mApiKey;
    private final Duration mRequestTimeout;
    private volatile LLMModel mSelectedModel;
    private volatile Double mDefaultTemperature = null;
    private volatile String mDefaultReasoningEffort = null;
    private volatile Boolean mDefaultDisableThinking = null;

    public LLMSupport() {
        this(defaultHttpTransport(), "http://localhost:8234/v1/", null, Duration.ofSeconds(30));
    }

    public LLMSupport(String baseUrl) {
        this(defaultHttpTransport(), baseUrl, null, Duration.ofSeconds(30));
    }

    public LLMSupport(String baseUrl, Duration requestTimeout) {
        this(defaultHttpTransport(), baseUrl, null, requestTimeout);
    }

    public LLMSupport(String baseUrl, String apiKey, Duration requestTimeout) {
        this(defaultHttpTransport(), baseUrl, apiKey, requestTimeout);
    }

    public LLMSupport(HttpTransport httpTransport, String baseUrl, String apiKey, Duration requestTimeout) {
        this.mHttpTransport = Objects.requireNonNull(httpTransport, "httpTransport");
        String normalized = Objects.requireNonNull(baseUrl, "baseUrl");
        if (!normalized.endsWith("/")) {
            normalized = normalized + "/";
        }
        this.mBaseUri = URI.create(normalized);
        this.mApiKey = apiKey;
        this.mRequestTimeout = Objects.requireNonNull(requestTimeout, "requestTimeout");
    }

    /**
     * Queries /models and returns the list advertised by the LLM server.
     */
    public List<LLMModel> fetchAvailableModels() throws IOException, InterruptedException {
        sLogger.message("Requesting LLM models from " + mBaseUri);
        HttpTransport.HttpResponseData response = mHttpTransport.get(
                mBaseUri.resolve("models"),
                headers("Accept", "application/json"),
                mRequestTimeout
        );
        if (response.statusCode() >= 300) {
            throw new IOException("LLM /models failed: " + response.statusCode() + " - " + response.body());
        }
        JSONObject root = new JSONObject(response.body());
        JSONArray data = root.optJSONArray("data");
        List<LLMModel> models = new ArrayList<>();
        if (data != null) {
            for (int i = 0; i < data.length(); i++) {
                JSONObject node = data.optJSONObject(i);
                if (node == null) continue;
                String id = node.optString("id", null);
                if (id == null) continue;
                String ownedBy = node.optString("owned_by", null);
                models.add(new LLMModel(id, node.optString("object", null), ownedBy));
            }
        }
        sLogger.message("LLM service returned " + models.size() + " models");
        return models;
    }

    public void setSelectedModel(LLMModel model) {
        this.mSelectedModel = Objects.requireNonNull(model, "model");
    }

    public void setSelectedModel(String modelId) {
        if (modelId == null || modelId.isBlank()) {
            throw new IllegalArgumentException("modelId must not be blank");
        }
        this.mSelectedModel = new LLMModel(modelId.trim(), null, null);
    }

    public LLMModel getSelectedModel() {
        return mSelectedModel;
    }

    public void setDefaultTemperature(Double temperature) {
        this.mDefaultTemperature = temperature;
    }

    /**
     * Reasoning effort applied to requests that don't set their own (e.g. "low", "medium", "high").
     * Understood by OpenAI's o-series/gpt-5 models and several OpenAI-compatible servers; ignored
     * by servers/models that don't support it.
     */
    public void setDefaultReasoningEffort(String reasoningEffort) {
        this.mDefaultReasoningEffort = (reasoningEffort == null || reasoningEffort.isBlank()) ? null : reasoningEffort.trim();
    }

    /**
     * When true, requests that don't override it ask the model to skip its reasoning/thinking pass
     * (sent as {@code "enable_thinking": false}, the convention used by Qwen3 and other
     * thinking-capable models served via vLLM/SGLang/Ollama/LM Studio).
     */
    public void setDefaultDisableThinking(Boolean disableThinking) {
        this.mDefaultDisableThinking = disableThinking;
    }

    public URI getBaseUri() {
        return mBaseUri;
    }

    /**
     * Sends a single user prompt to the configured model using the chat completion route.
     */
    public LLMCompletion sendPrompt(String prompt) throws IOException, InterruptedException {
        return sendPrompt(LLMPrompt.of(prompt.trim()));
    }

    public LLMCompletion sendPrompt(LLMPrompt prompt) throws IOException, InterruptedException {
        Objects.requireNonNull(prompt, "prompt");
        LLMModel model = mSelectedModel;
        if (model == null) {
            throw new IllegalStateException("No LLM model selected.");
        }
        sLogger.message("Sending LLM prompt to model " + model.id());
        JSONObject payload = prompt.toJson(model.id(), mDefaultTemperature, mDefaultReasoningEffort, mDefaultDisableThinking);
        HttpTransport.HttpResponseData response = mHttpTransport.postJson(
                mBaseUri.resolve("chat/completions"),
                payload.toString(),
                headers(
                        "Content-Type", "application/json",
                        "Accept", "application/json"
                ),
                mRequestTimeout
        );
        if (response.statusCode() >= 300) {
            throw new IOException("LLM chat completion failed: " + response.statusCode() + " - " + response.body());
        }
        return parseCompletion(response.body());
    }

    private LLMCompletion parseCompletion(String body) {
        JSONObject root = new JSONObject(body);
        JSONArray choices = root.optJSONArray("choices");
        String content = "";
        if (choices != null && choices.length() > 0) {
            JSONObject choice = choices.optJSONObject(0);
            if (choice != null) {
                content = choice.optJSONObject("message") != null
                        ? choice.getJSONObject("message").optString("content", "")
                        : "";
            }
        }
        String modelId = root.optString("model", null);
        long created = root.optLong("created", System.currentTimeMillis() / 1000);
        JSONObject usageNode = root.optJSONObject("usage");
        Usage usage;
        if (usageNode != null) {
            usage = new Usage(
                    usageNode.optInt("prompt_tokens", 0),
                    usageNode.optInt("completion_tokens", 0),
                    usageNode.optInt("total_tokens", 0)
            );
        } else {
            usage = new Usage(0, 0, 0);
        }
        return new LLMCompletion(modelId, content, Instant.ofEpochSecond(created), usage, body);
    }

    private Map<String, String> headers(String... keyValues) {
        Map<String, String> headers = new LinkedHashMap<>();
        for (int i = 0; i + 1 < keyValues.length; i += 2) {
            headers.put(keyValues[i], keyValues[i + 1]);
        }
        if (mApiKey != null && !mApiKey.isBlank()) {
            headers.put("Authorization", "Bearer " + mApiKey);
        }
        return headers;
    }

    public static String normalizeBaseUrl(String baseUrl, Integer port) {
        Objects.requireNonNull(baseUrl, "baseUrl");
        try {
            URI uri = URI.create(baseUrl.trim());
            if (uri.getScheme() == null || uri.getHost() == null) {
                throw new IllegalArgumentException("Base URL must include scheme and host");
            }
            int effectivePort = (port != null) ? port : uri.getPort();
            URI withPort = new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(), effectivePort,
                    uri.getPath(), null, null);
            String path = withPort.getPath();
            if (path == null || path.isBlank() || "/".equals(path)) {
                path = "/v1/";
            } else {
                if (!path.endsWith("/")) {
                    path = path + "/";
                }
                if (!path.contains("/v1")) {
                    path = path + "v1/";
                }
            }
            URI normalized = new URI(withPort.getScheme(), withPort.getUserInfo(), withPort.getHost(),
                    withPort.getPort(), path, null, null);
            String normalizedUrl = normalized.toString();
            if (!normalizedUrl.endsWith("/")) {
                normalizedUrl = normalizedUrl + "/";
            }
            return normalizedUrl;
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException("Invalid LLM base URL", e);
        }
    }

    private static HttpTransport defaultHttpTransport() {
        Supplier<HttpTransport> factory = sDefaultTransportFactory;
        if (factory != null) {
            return Objects.requireNonNull(factory.get(), "default HttpTransport factory returned null");
        }
        try {
            Class<?> clazz = Class.forName("de.dfki.vsm.util.llm.JdkHttpTransport");
            Object instance = clazz.getDeclaredConstructor().newInstance();
            if (instance instanceof HttpTransport) {
                return (HttpTransport) instance;
            }
        } catch (Throwable ignored) {
            // Fall through to explicit error.
        }
        throw new IllegalStateException("No default HttpTransport available. Provide an explicit transport.");
    }

    public static void setDefaultTransportFactory(Supplier<HttpTransport> factory) {
        sDefaultTransportFactory = factory;
    }

    // --- Value types ---

    public record LLMModel(String id, String objectType, String ownedBy) {}

    public record Usage(int promptTokens, int completionTokens, int totalTokens) {}

    public record LLMCompletion(String modelId, String content, Instant createdAt, Usage usage, String rawBody) {}

    public record ChatMessage(String role, String content) {
        public ChatMessage {
            if (role == null || role.isBlank()) {
                throw new IllegalArgumentException("role must not be blank");
            }
            if (content == null || content.isBlank()) {
                throw new IllegalArgumentException("content must not be blank");
            }
        }
    }

    public static final class LLMPrompt {
        private final List<ChatMessage> mMessages;
        private final Double mTemperature;
        private final Integer mMaxTokens;
        private final String mReasoningEffort;
        private final Boolean mDisableThinking;

        private LLMPrompt(List<ChatMessage> messages, Double temperature, Integer maxTokens,
                           String reasoningEffort, Boolean disableThinking) {
            if (messages == null || messages.isEmpty()) {
                throw new IllegalArgumentException("messages must not be empty");
            }
            this.mMessages = List.copyOf(messages);
            this.mTemperature = temperature;
            this.mMaxTokens = maxTokens;
            this.mReasoningEffort = reasoningEffort;
            this.mDisableThinking = disableThinking;
        }

        public static LLMPrompt of(String userPrompt) {
            String trimmed = Objects.requireNonNull(userPrompt, "userPrompt").trim();
            if (trimmed.isEmpty()) {
                throw new IllegalArgumentException("userPrompt must not be blank");
            }
            return new LLMPrompt(List.of(new ChatMessage("user", trimmed)), null, null, null, null);
        }

        public static Builder builder() {
            return new Builder();
        }

        JSONObject toJson(String modelId, Double defaultTemperature) {
            return toJson(modelId, defaultTemperature, null, null);
        }

        JSONObject toJson(String modelId, Double defaultTemperature, String defaultReasoningEffort, Boolean defaultDisableThinking) {
            JSONObject root = new JSONObject();
            root.put("model", modelId);
            JSONArray array = new JSONArray();
            for (ChatMessage message : mMessages) {
                JSONObject obj = new JSONObject();
                obj.put("role", message.role());
                obj.put("content", message.content());
                array.put(obj);
            }
            root.put("messages", array);
            Double temp = mTemperature != null ? mTemperature : defaultTemperature;
            if (temp != null) {
                root.put("temperature", temp);
            }
            if (mMaxTokens != null) {
                root.put("max_tokens", mMaxTokens);
            }
            String reasoningEffort = mReasoningEffort != null ? mReasoningEffort : defaultReasoningEffort;
            if (reasoningEffort != null) {
                root.put("reasoning_effort", reasoningEffort);
            }
            Boolean disableThinking = mDisableThinking != null ? mDisableThinking : defaultDisableThinking;
            if (disableThinking != null) {
                root.put("enable_thinking", !disableThinking);
            }
            return root;
        }

        public static final class Builder {
            private final List<ChatMessage> mMessages = new ArrayList<>();
            private Double mTemperature;
            private Integer mMaxTokens;
            private String mReasoningEffort;
            private Boolean mDisableThinking;

            public Builder temperature(double value) {
                this.mTemperature = value;
                return this;
            }

            public Builder maxTokens(int value) {
                this.mMaxTokens = value;
                return this;
            }

            /** Per-request override of the reasoning effort (e.g. "low", "medium", "high"). */
            public Builder reasoningEffort(String value) {
                this.mReasoningEffort = value;
                return this;
            }

            /** Per-request override of whether the model's thinking/reasoning pass is disabled. */
            public Builder disableThinking(boolean value) {
                this.mDisableThinking = value;
                return this;
            }

            public Builder addSystemMessage(String text) {
                if (text == null || text.isBlank()) {
                    return this;
                }
                this.mMessages.add(new ChatMessage("system", text.trim()));
                return this;
            }

            public Builder addUserMessage(String text) {
                if (text == null || text.isBlank()) {
                    return this;
                }
                this.mMessages.add(new ChatMessage("user", text.trim()));
                return this;
            }

            public Builder addAssistantMessage(String text) {
                if (text == null || text.isBlank()) {
                    return this;
                }
                this.mMessages.add(new ChatMessage("assistant", text.trim()));
                return this;
            }

            public LLMPrompt build() {
                return new LLMPrompt(mMessages, mTemperature, mMaxTokens, mReasoningEffort, mDisableThinking);
            }
        }
    }
}
