package de.dfki.vsm.web;

import org.json.JSONObject;

/**
 * An immutable record of one mutating editing command that was accepted by the
 * server and appended to an {@link OperationLog}.
 *
 * <p>Fields follow the collaborative protocol described in
 * {@code doc/collaborative-multisession-plan.md} Component 3.</p>
 */
public final class SceneFlowOperation {

    /** Monotonically increasing sequence number within one project's log. */
    public final long seq;

    /** Opaque user identifier; the WS session-id until Component 7 (SessionGate) is implemented. */
    public final String userId;

    /** Wall-clock milliseconds when the server committed this operation. */
    public final long timestamp;

    /** WebSocket method name, e.g. {@code "SceneFlow.Node.Move"}. */
    public final String method;

    /**
     * Defensive copy of the command parameters as received from the client.
     * Never {@code null}; may be an empty object.
     */
    public final JSONObject params;

    /**
     * The sequence number the client reported as its last-known state when it
     * sent the command. {@code -1} means the client does not support the
     * collaborative protocol (legacy mode).
     */
    public final long basedOnSeq;

    public SceneFlowOperation(long seq, String userId, long timestamp,
                               String method, JSONObject params, long basedOnSeq) {
        this.seq = seq;
        this.userId = userId != null ? userId : "";
        this.timestamp = timestamp;
        this.method = method != null ? method : "";
        // Defensive copy so callers cannot mutate the stored params.
        this.params = params != null ? new JSONObject(params.toString()) : new JSONObject();
        this.basedOnSeq = basedOnSeq;
    }

    /** Serialises the operation to JSON for REST responses and WS broadcasts. */
    public JSONObject toJson() {
        JSONObject obj = new JSONObject();
        obj.put("seq", seq);
        obj.put("userId", userId);
        obj.put("ts", timestamp);
        obj.put("method", method);
        obj.put("params", params);
        obj.put("basedOnSeq", basedOnSeq);
        return obj;
    }
}
