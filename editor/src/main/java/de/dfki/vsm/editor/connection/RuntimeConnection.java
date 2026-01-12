package de.dfki.vsm.editor.connection;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONObject;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.WebSocket;
import java.nio.ByteBuffer;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;

/**
 * Represents a connection to a remote RuntimeServer.
 * Provides methods for sending runtime control commands and monitoring runtime state.
 *
 * @author Phase 5 Refactoring - 2026-01-12
 */
public class RuntimeConnection {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final long REQUEST_TIMEOUT_MS = 5000;

    private final String mName;
    private final String mUrl;
    private final String mToken;
    private final HttpClient mHttpClient;

    private WebSocket mWebSocket;
    private ConnectionState mState = ConnectionState.DISCONNECTED;
    private RuntimeStatus mLastStatus;
    private final AtomicLong mRequestIdCounter = new AtomicLong(0);
    private final Map<String, CompletableFuture<JSONObject>> mPendingRequests = new ConcurrentHashMap<>();

    // Event listeners
    private Consumer<RuntimeEvent> mEventListener;

    /**
     * Connection states.
     */
    public enum ConnectionState {
        DISCONNECTED,
        CONNECTING,
        CONNECTED,
        ERROR
    }

    /**
     * Runtime status information.
     */
    public static class RuntimeStatus {
        public final String state;           // "stopped", "loaded", "running", "paused"
        public final String projectPath;
        public final String projectName;
        public final boolean isRunning;
        public final boolean isPaused;
        public final boolean wasExecuted;

        public RuntimeStatus(String state, String projectPath, String projectName,
                           boolean isRunning, boolean isPaused, boolean wasExecuted) {
            this.state = state;
            this.projectPath = projectPath;
            this.projectName = projectName;
            this.isRunning = isRunning;
            this.isPaused = isPaused;
            this.wasExecuted = wasExecuted;
        }

        public static RuntimeStatus fromJson(JSONObject json) {
            return new RuntimeStatus(
                json.optString("state", "stopped"),
                json.optString("projectPath", ""),
                json.optString("projectName", ""),
                json.optBoolean("isRunning", false),
                json.optBoolean("isPaused", false),
                json.optBoolean("wasExecuted", false)
            );
        }
    }

    /**
     * Runtime event received via WebSocket.
     */
    public static class RuntimeEvent {
        public final String type;
        public final JSONObject data;

        public RuntimeEvent(String type, JSONObject data) {
            this.type = type;
            this.data = data;
        }
    }

    /**
     * Creates a new runtime connection.
     *
     * @param name Display name for this connection
     * @param url Base URL of runtime server (e.g., "http://192.168.1.100:8091")
     * @param token Authentication token
     */
    public RuntimeConnection(String name, String url, String token) {
        this.mName = name;
        this.mUrl = url.endsWith("/") ? url.substring(0, url.length() - 1) : url;
        this.mToken = token;
        this.mHttpClient = HttpClient.newBuilder()
            .connectTimeout(Duration.ofMillis(REQUEST_TIMEOUT_MS))
            .build();
    }

    /**
     * Connects to the runtime server.
     *
     * @return true if connection successful
     */
    public boolean connect() {
        if (mState == ConnectionState.CONNECTED) {
            sLogger.warning("Already connected to " + mName);
            return true;
        }

        mState = ConnectionState.CONNECTING;
        sLogger.message("Connecting to runtime server: " + mName + " at " + mUrl);

        try {
            // First verify server is reachable by fetching info
            JSONObject info = sendRestRequest("GET", "/api/v1/info", null, false);
            if (info == null) {
                mState = ConnectionState.ERROR;
                sLogger.failure("Failed to connect to " + mUrl);
                return false;
            }

            sLogger.message("Connected to: " + info.optString("name", "Unknown") +
                          " (mode: " + info.optString("mode", "unknown") + ")");

            // Connect WebSocket for real-time events
            connectWebSocket();

            // Get initial status
            refreshStatus();

            mState = ConnectionState.CONNECTED;
            return true;

        } catch (Exception e) {
            sLogger.failure("Error connecting to " + mUrl + ": " + e.getMessage());
            mState = ConnectionState.ERROR;
            return false;
        }
    }

    /**
     * Disconnects from the runtime server.
     */
    public void disconnect() {
        if (mWebSocket != null) {
            mWebSocket.sendClose(WebSocket.NORMAL_CLOSURE, "Client disconnecting");
            mWebSocket = null;
        }

        mState = ConnectionState.DISCONNECTED;
        sLogger.message("Disconnected from " + mName);
    }

    /**
     * Checks if currently connected.
     */
    public boolean isConnected() {
        return mState == ConnectionState.CONNECTED;
    }

    /**
     * Gets the connection state.
     */
    public ConnectionState getState() {
        return mState;
    }

    /**
     * Gets the connection name.
     */
    public String getName() {
        return mName;
    }

    /**
     * Gets the connection URL.
     */
    public String getUrl() {
        return mUrl;
    }

    /**
     * Gets the last known runtime status.
     */
    public RuntimeStatus getStatus() {
        return mLastStatus;
    }

    /**
     * Refreshes runtime status from server.
     */
    public boolean refreshStatus() {
        try {
            JSONObject response = sendRestRequest("GET", "/api/v1/runtime/status", null, true);
            if (response != null) {
                mLastStatus = RuntimeStatus.fromJson(response);
                return true;
            }
        } catch (Exception e) {
            sLogger.warning("Failed to refresh status: " + e.getMessage());
        }
        return false;
    }

    /**
     * Sets an event listener for runtime events.
     */
    public void setEventListener(Consumer<RuntimeEvent> listener) {
        mEventListener = listener;
    }

    // ========== Runtime Control Commands ==========

    /**
     * Loads a project on the runtime server.
     *
     * @param projectPath Path to project (accessible from runtime server's filesystem)
     * @return true if successful
     */
    public boolean loadProject(String projectPath) {
        JSONObject body = new JSONObject();
        body.put("projectPath", projectPath);

        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/load", body, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Starts runtime execution.
     */
    public boolean start() {
        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/start", null, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Pauses runtime execution.
     */
    public boolean pause() {
        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/pause", null, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Resumes paused runtime execution.
     */
    public boolean resume() {
        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/resume", null, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Stops runtime execution.
     */
    public boolean stop() {
        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/stop", null, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Unloads the current project.
     */
    public boolean unload() {
        JSONObject response = sendRestRequest("POST", "/api/v1/runtime/unload", null, true);
        if (response != null && "ok".equals(response.optString("status"))) {
            refreshStatus();
            return true;
        }
        return false;
    }

    /**
     * Gets runtime variables.
     */
    public JSONObject getVariables() {
        return sendRestRequest("GET", "/api/v1/runtime/variables", null, true);
    }

    /**
     * Gets sceneflow structure (read-only).
     */
    public JSONObject getSceneflow() {
        return sendRestRequest("GET", "/api/v1/runtime/sceneflow", null, true);
    }

    // ========== Internal Methods ==========

    /**
     * Sends a REST request to the runtime server.
     *
     * @param method HTTP method (GET, POST, etc.)
     * @param path API path (e.g., "/api/v1/runtime/status")
     * @param body Request body (null for GET requests)
     * @param requireAuth Whether to include authentication token
     * @return Response JSON or null on error
     */
    private JSONObject sendRestRequest(String method, String path, JSONObject body, boolean requireAuth) {
        try {
            String url = mUrl + path;
            HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofMillis(REQUEST_TIMEOUT_MS));

            // Add authentication if required
            if (requireAuth) {
                builder.header("Authorization", "Bearer " + mToken);
            }

            // Set method and body
            if ("GET".equals(method)) {
                builder.GET();
            } else if ("POST".equals(method)) {
                if (body != null) {
                    builder.header("Content-Type", "application/json");
                    builder.POST(HttpRequest.BodyPublishers.ofString(body.toString()));
                } else {
                    builder.POST(HttpRequest.BodyPublishers.noBody());
                }
            }

            HttpRequest request = builder.build();
            HttpResponse<String> response = mHttpClient.send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() >= 200 && response.statusCode() < 300) {
                String responseBody = response.body();
                if (responseBody != null && !responseBody.isEmpty()) {
                    return new JSONObject(responseBody);
                }
                return new JSONObject();
            } else {
                sLogger.warning("HTTP " + response.statusCode() + " from " + url + ": " + response.body());
                return null;
            }

        } catch (IOException | InterruptedException e) {
            sLogger.warning("Request failed to " + path + ": " + e.getMessage());
            return null;
        }
    }

    /**
     * Connects WebSocket for real-time event streaming.
     */
    private void connectWebSocket() {
        try {
            String wsUrl = mUrl.replace("http://", "ws://").replace("https://", "wss://") +
                          "/ws?token=" + mToken;

            mWebSocket = HttpClient.newHttpClient().newWebSocketBuilder()
                .buildAsync(URI.create(wsUrl), new WebSocket.Listener() {
                    private StringBuilder messageBuffer = new StringBuilder();

                    @Override
                    public void onOpen(WebSocket webSocket) {
                        sLogger.message("WebSocket connected to " + mName);
                        webSocket.request(1);
                    }

                    @Override
                    public CompletionStage<?> onText(WebSocket webSocket, CharSequence data, boolean last) {
                        messageBuffer.append(data);

                        if (last) {
                            String message = messageBuffer.toString();
                            messageBuffer.setLength(0);
                            handleWebSocketMessage(message);
                        }

                        webSocket.request(1);
                        return null;
                    }

                    @Override
                    public CompletionStage<?> onBinary(WebSocket webSocket, ByteBuffer data, boolean last) {
                        webSocket.request(1);
                        return null;
                    }

                    @Override
                    public CompletionStage<?> onClose(WebSocket webSocket, int statusCode, String reason) {
                        sLogger.message("WebSocket closed: " + reason);
                        return null;
                    }

                    @Override
                    public void onError(WebSocket webSocket, Throwable error) {
                        sLogger.warning("WebSocket error: " + error.getMessage());
                    }
                }).join();

        } catch (Exception e) {
            sLogger.warning("Failed to connect WebSocket: " + e.getMessage());
        }
    }

    /**
     * Handles incoming WebSocket messages.
     */
    private void handleWebSocketMessage(String message) {
        try {
            JSONObject msg = new JSONObject(message);

            // Check if this is a response to a pending request
            String id = msg.optString("id", null);
            if (id != null && mPendingRequests.containsKey(id)) {
                CompletableFuture<JSONObject> future = mPendingRequests.remove(id);
                if (future != null) {
                    future.complete(msg.optJSONObject("result"));
                }
                return;
            }

            // Check if this is an event
            String event = msg.optString("event", null);
            if (event != null) {
                handleRuntimeEvent(event, msg);
                return;
            }

        } catch (Exception e) {
            sLogger.warning("Error parsing WebSocket message: " + e.getMessage());
        }
    }

    /**
     * Handles runtime events from WebSocket.
     */
    private void handleRuntimeEvent(String eventType, JSONObject data) {
        sLogger.message("Runtime event: " + eventType);

        // Update local state based on event
        if ("runtime.state".equals(eventType)) {
            String state = data.optString("state", "");
            if (!state.isEmpty() && mLastStatus != null) {
                // Update cached status
                mLastStatus = new RuntimeStatus(
                    state,
                    data.optString("projectPath", mLastStatus.projectPath),
                    data.optString("projectName", mLastStatus.projectName),
                    "running".equals(state),
                    "paused".equals(state),
                    !"stopped".equals(state)
                );
            }
        }

        // Notify listener
        if (mEventListener != null) {
            mEventListener.accept(new RuntimeEvent(eventType, data));
        }
    }

    @Override
    public String toString() {
        return mName + " (" + mUrl + ") - " + mState;
    }
}
