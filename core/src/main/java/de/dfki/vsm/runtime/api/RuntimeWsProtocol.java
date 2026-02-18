package de.dfki.vsm.runtime.api;

import org.json.JSONObject;

/**
 * Shared WebSocket command protocol codec for runtime command envelopes.
 *
 * Expected command request format:
 * - preferred: { "id": "...", "method": "...", "params": { ... } }
 * - accepted aliases:
 *   - command name in "name"
 *   - params in "payload"
 *
 * Response format:
 * - success: { "type": "response", "id": "...", "payload": { ... }, "status": "ok" }
 * - error:   { "type": "error", "payload": { "message": "..." }, "status": "error" }
 */
public final class RuntimeWsProtocol {

    public static final class CommandRequest {
        private final String id;
        private final String method;
        private final JSONObject params;

        CommandRequest(final String id, final String method, final JSONObject params) {
            this.id = id;
            this.method = method;
            this.params = params;
        }

        public String id() {
            return id;
        }

        public String method() {
            return method;
        }

        public JSONObject params() {
            return params;
        }
    }

    private RuntimeWsProtocol() {
    }

    public static CommandRequest parseRequest(final String raw) {
        final JSONObject msg = new JSONObject(raw);
        final String id = msg.optString("id", "");
        String method = msg.optString("method", "");
        if (method.isEmpty()) {
            method = msg.optString("name", "");
        }
        JSONObject params = msg.optJSONObject("params");
        if (params == null) {
            params = msg.optJSONObject("payload");
        }
        if (params == null) {
            params = new JSONObject();
        }
        return new CommandRequest(id, method, params);
    }

    public static JSONObject successResponse(final String id, final JSONObject payload) {
        final JSONObject response = new JSONObject();
        response.put("type", "response");
        if (id != null && !id.isEmpty()) {
            response.put("id", id);
        }
        response.put("payload", payload != null ? payload : new JSONObject());
        response.put("status", "ok");
        return response;
    }

    public static JSONObject errorResponse(final String message) {
        final JSONObject response = new JSONObject();
        response.put("type", "error");
        final JSONObject payload = new JSONObject();
        payload.put("message", message == null ? "Unknown error" : message);
        response.put("payload", payload);
        response.put("status", "error");
        return response;
    }
}
