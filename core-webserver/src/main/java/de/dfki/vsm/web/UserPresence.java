package de.dfki.vsm.web;

import org.json.JSONObject;

/**
 * Ephemeral, per-user awareness state within one {@link CollaborationSession}.
 *
 * <p>Instances are created by {@link PresenceManager#join} and removed by
 * {@link PresenceManager#leave}.  They are never persisted to disk.</p>
 *
 * <p>The mutable fields ({@code activeNodeId}, {@code viewport},
 * {@code lastSeen}) are only written by
 * {@link PresenceManager#update} which is itself synchronized, so no
 * additional locking is required here.</p>
 */
public final class UserPresence {

    /** Stable identifier — the WS session-id until Component 7 (SessionGate). */
    public final String userId;

    /** Human-readable name shown in the UI. */
    public final String displayName;

    /**
     * Hex color string assigned from the palette at join time and stable for
     * the lifetime of this presence record (e.g. {@code "#e07b54"}).
     */
    public final String color;

    /** Node the user is currently hovering or editing; {@code null} if none. */
    volatile String activeNodeId;

    /**
     * Current canvas viewport as a JSON object with keys
     * {@code x, y, width, height}; {@code null} if unknown.
     */
    volatile JSONObject viewport;

    /** Wall-clock ms of the last received update for this user. */
    volatile long lastSeen;

    UserPresence(String userId, String displayName, String color) {
        this.userId = userId;
        this.displayName = displayName != null ? displayName : userId;
        this.color = color != null ? color : "#7a7d81";
        this.lastSeen = System.currentTimeMillis();
    }

    /** Serialises presence state to JSON for REST responses and WS broadcasts. */
    public JSONObject toJson() {
        JSONObject obj = new JSONObject();
        obj.put("userId", userId);
        obj.put("displayName", displayName);
        obj.put("color", color);
        obj.put("lastSeen", lastSeen);
        if (activeNodeId != null) {
            obj.put("activeNodeId", activeNodeId);
        }
        if (viewport != null) {
            obj.put("viewport", viewport);
        }
        return obj;
    }
}
