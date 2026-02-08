package de.dfki.vsm.util.llm;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Lightweight helper that talks to an LM Studio (or OpenAI-compatible) REST endpoint.
 * The class can enumerate available models and submit chat prompts using the selected model.
 */
public class LLMSupport {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    private final HttpClient mHttpClient;
    private final URI mBaseUri;
    private final String mApiKey;
    private final Duration mRequestTimeout;
    private volatile LLMModel mSelectedModel;
    private volatile Double mDefaultTemperature = null;

    public LLMSupport() {
        this(defaultHttpClient(), "http://localhost:8234/v1/", null, Duration.ofSeconds(30));
    }

    public LLMSupport(String baseUrl) {
        this(defaultHttpClient(), baseUrl, null, Duration.ofSeconds(30));
    }

    public LLMSupport(String baseUrl, Duration requestTimeout) {
        this(defaultHttpClient(), baseUrl, null, requestTimeout);
    }

    public LLMSupport(String baseUrl, String apiKey, Duration requestTimeout) {
        this(defaultHttpClient(), baseUrl, apiKey, requestTimeout);
    }

    public LLMSupport(HttpClient httpClient, String baseUrl, String apiKey, Duration requestTimeout) {
        this.mHttpClient = Objects.requireNonNull(httpClient, "httpClient");
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
        HttpRequest request = baseRequest("models")
                .header("Accept", "application/json")
                .GET()
                .build();
        HttpResponse<String> response = mHttpClient.send(request, HttpResponse.BodyHandlers.ofString());
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
        JSONObject payload = prompt.toJson(model.id(), mDefaultTemperature);
        HttpRequest request = baseRequest("chat/completions")
                .header("Content-Type", "application/json")
                .header("Accept", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                .build();
        HttpResponse<String> response = mHttpClient.send(request, HttpResponse.BodyHandlers.ofString());
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

    private HttpRequest.Builder baseRequest(String path) {
        URI target = mBaseUri.resolve(path);
        HttpRequest.Builder builder = HttpRequest.newBuilder(target)
                .timeout(mRequestTimeout)
                .version(HttpClient.Version.HTTP_1_1);
        if (mApiKey != null && !mApiKey.isBlank()) {
            builder.header("Authorization", "Bearer " + mApiKey);
        }
        return builder;
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

    private static HttpClient defaultHttpClient() {
        return HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_1_1)
                .connectTimeout(Duration.ofSeconds(10))
                .build();
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

        private LLMPrompt(List<ChatMessage> messages, Double temperature, Integer maxTokens) {
            if (messages == null || messages.isEmpty()) {
                throw new IllegalArgumentException("messages must not be empty");
            }
            this.mMessages = List.copyOf(messages);
            this.mTemperature = temperature;
            this.mMaxTokens = maxTokens;
        }

        public static LLMPrompt of(String userPrompt) {
            String trimmed = Objects.requireNonNull(userPrompt, "userPrompt").trim();
            if (trimmed.isEmpty()) {
                throw new IllegalArgumentException("userPrompt must not be blank");
            }
            return new LLMPrompt(List.of(new ChatMessage("user", trimmed)), null, null);
        }

        public static Builder builder() {
            return new Builder();
        }

        JSONObject toJson(String modelId, Double defaultTemperature) {
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
            return root;
        }

        public static final class Builder {
            private final List<ChatMessage> mMessages = new ArrayList<>();
            private Double mTemperature;
            private Integer mMaxTokens;

            public Builder temperature(double value) {
                this.mTemperature = value;
                return this;
            }

            public Builder maxTokens(int value) {
                this.mMaxTokens = value;
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
                return new LLMPrompt(mMessages, mTemperature, mMaxTokens);
            }
        }
    }
}
