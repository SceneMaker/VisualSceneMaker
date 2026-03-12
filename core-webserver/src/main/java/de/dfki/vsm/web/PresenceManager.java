package de.dfki.vsm.web;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Tracks ephemeral per-user awareness state within one
 * {@link CollaborationSession}.
 *
 * <p>State is never persisted to disk and exists only while at least one user
 * is connected.  All mutating methods are {@code synchronized}; read-only
 * accessors return unmodifiable snapshots.</p>
 *
 * <h2>Color assignment</h2>
 * <p>Each user joining the session is assigned the next color from
 * {@link #COLOR_PALETTE}, cycling modulo the palette length.  The counter
 * only ever increments so a reconnecting user may receive a different color
 * than their previous session — acceptable until Component 7 (SessionGate)
 * provides stable user identities.</p>
 */
public final class PresenceManager {

    // ------------------------------------------------------------------
    // Color palette
    // ------------------------------------------------------------------

    static final String[] COLOR_PALETTE = {
        "#e07b54", "#5b8edc", "#ffc857", "#5bae7a",
        "#a06a4b", "#e26d5a", "#9b59b6", "#1abc9c",
        "#e74c3c", "#3498db"
    };

    // ------------------------------------------------------------------
    // State
    // ------------------------------------------------------------------

    /** Insertion-ordered so iteration reflects join order. */
    private final Map<String, UserPresence> users = new LinkedHashMap<>();
    private final AtomicInteger colorIndex = new AtomicInteger(0);

    // ------------------------------------------------------------------
    // Join / leave
    // ------------------------------------------------------------------

    /**
     * Registers a user as present in this session.  If the user is already
     * present the existing record is returned unchanged.
     *
     * @param userId      stable identifier (WS session-id or future user token)
     * @param displayName optional; derived from {@code userId} when {@code null}
     * @return the (possibly new) {@link UserPresence} record
     */
    public synchronized UserPresence join(String userId, String displayName) {
        if (userId == null || userId.isBlank()) {
            throw new IllegalArgumentException("userId must not be blank");
        }
        UserPresence existing = users.get(userId);
        if (existing != null) {
            existing.lastSeen = System.currentTimeMillis();
            return existing;
        }
        String resolvedName = (displayName != null && !displayName.isBlank())
                ? displayName
                : deriveDisplayName(userId);
        String color = COLOR_PALETTE[colorIndex.getAndIncrement() % COLOR_PALETTE.length];
        UserPresence presence = new UserPresence(userId, resolvedName, color);
        users.put(userId, presence);
        return presence;
    }

    /**
     * Removes a user's presence record.
     *
     * @return the removed record, or {@code null} if the user was not present
     */
    public synchronized UserPresence leave(String userId) {
        return users.remove(userId);
    }

    // ------------------------------------------------------------------
    // Update
    // ------------------------------------------------------------------

    /**
     * Updates the mutable awareness fields for a user.  Silently ignored if
     * the user is not currently present.
     *
     * @param userId       the user whose state is being updated
     * @param activeNodeId node being hovered/edited, or {@code null} to clear
     * @param viewport     current canvas viewport, or {@code null} to leave unchanged
     * @return the updated record, or {@code null} if user not present
     */
    public synchronized UserPresence update(String userId, String activeNodeId, JSONObject viewport) {
        UserPresence p = users.get(userId);
        if (p == null) return null;
        p.activeNodeId = activeNodeId;
        if (viewport != null) {
            p.viewport = new JSONObject(viewport.toString()); // defensive copy
        }
        p.lastSeen = System.currentTimeMillis();
        return p;
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns a snapshot of all currently present users, ordered by join time.
     * The list is unmodifiable.
     */
    public synchronized List<UserPresence> getAll() {
        return Collections.unmodifiableList(new ArrayList<>(users.values()));
    }

    /**
     * Returns the presence record for {@code userId}, or {@code null} if not
     * present.
     */
    public synchronized UserPresence get(String userId) {
        return users.get(userId);
    }

    /** {@code true} if the user is currently registered as present. */
    public synchronized boolean isPresent(String userId) {
        return users.containsKey(userId);
    }

    /** Number of users currently present. */
    public synchronized int size() {
        return users.size();
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    /**
     * Derives a short display name from the userId.  Uses the first 8
     * characters of the ID so the UI has something human-readable before
     * Component 7 provides real names.
     */
    static String deriveDisplayName(String userId) {
        if (userId == null || userId.isBlank()) return "User";
        String trimmed = userId.trim();
        int end = Math.min(8, trimmed.length());
        return "User-" + trimmed.substring(0, end);
    }
}
