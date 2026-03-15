package de.dfki.vsm.web;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Managed lifecycle registry for {@link CollaborationSession} instances.
 *
 * <p>Replaces the bare {@code Map<String, ProjectRef>} in {@link WebUiServer}
 * with a typed API that handles session creation, user join/leave, and
 * destruction.  Corresponds to Component 5 in
 * {@code doc/collaborative-multisession-plan.md}.</p>
 *
 * <h2>Thread safety</h2>
 * <p>All public methods are thread-safe.  The internal map is a
 * {@link ConcurrentHashMap}; atomic compound operations (e.g.
 * {@link #getOrCreate}) use {@code computeIfAbsent}.</p>
 */
public final class SessionRegistry {

    // ------------------------------------------------------------------
    // SessionSummary — immutable snapshot for list responses
    // ------------------------------------------------------------------

    /**
     * Snapshot of a session's key properties suitable for REST list responses
     * and the {@code GET /api/v1/sessions} endpoint.  Immutable.
     */
    public static final class SessionSummary {

        /** The session's project identifier. */
        public final String projectId;

        /** Number of WebSocket clients currently subscribed to this session. */
        public final int subscriberCount;

        /** Number of users currently registered as present in this session. */
        public final int presenceCount;

        /**
         * Latest committed sequence number in the session's operation log.
         * {@code 0} means no operations have been applied yet.
         */
        public final long operationSeq;

        private SessionSummary(String projectId, int subscriberCount,
                               int presenceCount, long operationSeq) {
            this.projectId = projectId;
            this.subscriberCount = subscriberCount;
            this.presenceCount = presenceCount;
            this.operationSeq = operationSeq;
        }

        /** Serialises the summary to JSON for REST responses. */
        public JSONObject toJson() {
            JSONObject obj = new JSONObject();
            obj.put("projectId", projectId);
            obj.put("subscriberCount", subscriberCount);
            obj.put("presenceCount", presenceCount);
            obj.put("operationSeq", operationSeq);
            return obj;
        }
    }

    // ------------------------------------------------------------------
    // State
    // ------------------------------------------------------------------

    private final ConcurrentHashMap<String, CollaborationSession> sessions =
            new ConcurrentHashMap<>();

    // ------------------------------------------------------------------
    // Session lifecycle
    // ------------------------------------------------------------------

    /**
     * Creates a new {@link CollaborationSession} for {@code projectId} and
     * registers it in the registry.
     *
     * @param projectId unique project identifier; must be non-blank
     * @return the newly created session
     * @throws IllegalArgumentException if {@code projectId} is blank or null
     * @throws IllegalStateException    if a session for that project already exists
     */
    public CollaborationSession create(String projectId) {
        requireNonBlank(projectId, "projectId");
        CollaborationSession session = new CollaborationSession(projectId);
        CollaborationSession existing = sessions.putIfAbsent(projectId, session);
        if (existing != null) {
            throw new IllegalStateException(
                    "Session already exists for projectId: " + projectId);
        }
        return session;
    }

    /**
     * Returns an existing session or atomically creates a new one for
     * {@code projectId}.  Useful for idempotent open-or-create patterns.
     *
     * @param projectId unique project identifier; must be non-blank
     * @return the existing or newly created session; never {@code null}
     * @throws IllegalArgumentException if {@code projectId} is blank or null
     */
    public CollaborationSession getOrCreate(String projectId) {
        requireNonBlank(projectId, "projectId");
        return sessions.computeIfAbsent(projectId, CollaborationSession::new);
    }

    /**
     * Destroys the session for {@code projectId}, removing it from the
     * registry.  Does nothing if no session exists.
     *
     * @param projectId the project whose session should be removed
     * @return the removed session, or {@code null} if no session existed
     */
    public CollaborationSession destroy(String projectId) {
        return sessions.remove(projectId);
    }

    // ------------------------------------------------------------------
    // User join / leave
    // ------------------------------------------------------------------

    /**
     * Registers a user as present in the given project's session.
     *
     * @param projectId   the session to join; must have been created first
     * @param userId      user identifier (WS session-id until Component 7)
     * @param displayName optional display name; derived from {@code userId} when null
     * @return the session that was joined; never {@code null}
     * @throws IllegalArgumentException if {@code projectId} or {@code userId} is blank
     * @throws IllegalStateException    if no session exists for {@code projectId}
     */
    public CollaborationSession join(String projectId, String userId, String displayName) {
        requireNonBlank(projectId, "projectId");
        CollaborationSession session = sessions.get(projectId);
        if (session == null) {
            throw new IllegalStateException(
                    "No session found for projectId: " + projectId);
        }
        session.getPresenceManager().join(userId, displayName);
        return session;
    }

    /**
     * Removes a user's presence from the given project's session.  Silently
     * ignored if the session or the user does not exist.
     *
     * @param projectId the session to leave
     * @param userId    the user leaving
     * @return the removed {@link UserPresence}, or {@code null} if the user
     *         was not present or the session does not exist
     */
    public UserPresence leave(String projectId, String userId) {
        CollaborationSession session = sessions.get(projectId);
        if (session == null) return null;
        return session.getPresenceManager().leave(userId);
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns the session for {@code projectId}, or {@code null} if none
     * exists.
     */
    public CollaborationSession get(String projectId) {
        return sessions.get(projectId);
    }

    /** {@code true} if a session exists for {@code projectId}. */
    public boolean contains(String projectId) {
        return sessions.containsKey(projectId);
    }

    /** Number of currently registered sessions. */
    public int size() {
        return sessions.size();
    }

    /**
     * Returns an unmodifiable snapshot of all currently registered project
     * IDs.
     */
    public Set<String> projectIds() {
        return Collections.unmodifiableSet(new LinkedHashSet<>(sessions.keySet()));
    }

    /**
     * Returns a snapshot list of {@link SessionSummary} objects for all
     * currently registered sessions.
     *
     * <p>Note: {@link ConcurrentHashMap} does not guarantee insertion order,
     * so the returned list order may vary.  Phase E (SessionRegistry with
     * checkpointing) will add an explicit ordering layer if needed.</p>
     */
    public List<SessionSummary> list() {
        List<SessionSummary> summaries = new ArrayList<>();
        for (CollaborationSession s : sessions.values()) {
            summaries.add(new SessionSummary(
                    s.getProjectId(),
                    s.subscriberCount(),
                    s.getPresenceManager().size(),
                    s.getOperationLog().currentSeq()
            ));
        }
        return Collections.unmodifiableList(summaries);
    }

    /**
     * Removes all sessions from the registry.  Intended for graceful server
     * shutdown and testing.
     */
    public void clear() {
        sessions.clear();
    }

    // ------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------

    private static void requireNonBlank(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
    }
}
