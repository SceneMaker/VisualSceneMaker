package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Orchestrates multiple parallel runtime sessions for independent lifecycle
 * management, exclusive hardware-resource arbitration, and cross-session
 * messaging.
 *
 * <p>Corresponds to Component 6 in
 * {@code doc/collaborative-multisession-plan.md}.</p>
 *
 * <h2>Design responsibilities</h2>
 * <ol>
 *   <li><b>State tracking</b> — each registered project transitions through
 *       {@link RuntimeState} ({@code IDLE → RUNNING → PAUSED → RUNNING → STOPPED}).
 *       Crashes in one session never affect another; calling {@link #setState}
 *       is the only mutation path.</li>
 *   <li><b>Resource arbitration</b> — hardware-exclusive resources (e.g. a
 *       microphone, camera, or serial-port plugin) may only be held by one
 *       session at a time.  {@link #acquireExclusiveResource} returns
 *       {@code false} when the resource is already taken; the caller is
 *       responsible for deciding whether to abort or retry.  Resources are
 *       automatically released when a session is stopped or unregistered.</li>
 *   <li><b>Cross-session messaging</b> — a shared {@link CrossSessionBus}
 *       lets one runtime emit named events that other sessions have subscribed
 *       to (e.g. a "handoff" signal in multi-agent installations).</li>
 * </ol>
 *
 * <h2>Thread safety</h2>
 * <p>State transitions and resource arbitration use a per-instance
 * {@code synchronized} lock on the {@code exclusiveResources} map so that
 * acquire/release pairs are atomic.  Session registration uses
 * {@link ConcurrentHashMap#putIfAbsent} for lock-free idempotent inserts.</p>
 */
public final class RuntimeOrchestrator {

    // ------------------------------------------------------------------
    // RuntimeState
    // ------------------------------------------------------------------

    /** Lifecycle states for a registered runtime session. */
    public enum RuntimeState {
        /** Session registered but runtime not yet started. */
        IDLE,
        /** Interpreter is actively running the SceneFlow. */
        RUNNING,
        /** Interpreter is temporarily paused (can resume). */
        PAUSED,
        /** Runtime has finished or been stopped; cannot be resumed. */
        STOPPED
    }

    // ------------------------------------------------------------------
    // RuntimeStatus — immutable snapshot
    // ------------------------------------------------------------------

    /**
     * Snapshot of a single session's runtime state for {@link #listRuntimes()}
     * and REST responses.
     */
    public static final class RuntimeStatus {

        public final String projectId;
        public final RuntimeState state;
        /** Wall-clock ms when state first became {@link RuntimeState#RUNNING}; {@code 0} if never started. */
        public final long startedAt;
        /** Unmodifiable snapshot of exclusive resources held by this session. */
        public final Set<String> heldResources;

        private RuntimeStatus(String projectId, RuntimeState state,
                               long startedAt, Set<String> heldResources) {
            this.projectId = projectId;
            this.state = state;
            this.startedAt = startedAt;
            this.heldResources = Collections.unmodifiableSet(new HashSet<>(heldResources));
        }

        /** Serialises to JSON for REST list responses. */
        public JSONObject toJson() {
            JSONObject obj = new JSONObject();
            obj.put("projectId", projectId);
            obj.put("state", state.name());
            obj.put("startedAt", startedAt);
            JSONArray resources = new JSONArray();
            for (String r : heldResources) {
                resources.put(r);
            }
            obj.put("heldResources", resources);
            return obj;
        }
    }

    // ------------------------------------------------------------------
    // Internal per-session entry
    // ------------------------------------------------------------------

    private static final class RuntimeEntry {
        volatile RuntimeState state = RuntimeState.IDLE;
        volatile long startedAt = 0L;
        // Thread-safe set; mutations protected by exclusiveResources lock when
        // coordinating with the resource map.
        final Set<String> heldResources = ConcurrentHashMap.newKeySet();
    }

    // ------------------------------------------------------------------
    // State
    // ------------------------------------------------------------------

    private final ConcurrentHashMap<String, RuntimeEntry> entries =
            new ConcurrentHashMap<>();

    /**
     * Maps resourceId → projectId of the holder.
     * All acquire/release operations synchronize on this map for atomicity.
     */
    private final ConcurrentHashMap<String, String> exclusiveResources =
            new ConcurrentHashMap<>();

    private final CrossSessionBus crossSessionBus = new CrossSessionBus();

    // ------------------------------------------------------------------
    // Registration
    // ------------------------------------------------------------------

    /**
     * Registers a session in the orchestrator with initial state
     * {@link RuntimeState#IDLE}.  If the session is already registered the
     * call is silently ignored (idempotent).
     *
     * @param projectId project identifier; must be non-blank
     */
    public void register(String projectId) {
        requireNonBlank(projectId, "projectId");
        entries.putIfAbsent(projectId, new RuntimeEntry());
    }

    /**
     * Unregisters a session, releasing all exclusive resources it holds.
     * The session's subscriptions on the {@link CrossSessionBus} are
     * <em>not</em> removed automatically — call
     * {@link CrossSessionBus#unsubscribeAll} separately if needed.
     *
     * @param projectId project to unregister
     */
    public void unregister(String projectId) {
        RuntimeEntry entry = entries.remove(projectId);
        if (entry != null) {
            releaseAllFor(projectId, entry);
        }
    }

    /** {@code true} if a session with {@code projectId} is registered. */
    public boolean contains(String projectId) {
        return entries.containsKey(projectId);
    }

    /** Number of currently registered sessions. */
    public int size() {
        return entries.size();
    }

    // ------------------------------------------------------------------
    // State transitions
    // ------------------------------------------------------------------

    /**
     * Updates the runtime state of a registered session.
     *
     * <p>Transitioning to {@link RuntimeState#RUNNING} for the first time
     * records {@code startedAt}.  Transitioning to
     * {@link RuntimeState#STOPPED} automatically releases all exclusive
     * resources held by the session.</p>
     *
     * @param projectId the session to update; must be registered
     * @param state     new state; must not be {@code null}
     * @throws IllegalArgumentException if {@code projectId} is blank
     * @throws IllegalStateException    if the session is not registered
     */
    public void setState(String projectId, RuntimeState state) {
        requireNonBlank(projectId, "projectId");
        if (state == null) throw new IllegalArgumentException("state must not be null");
        RuntimeEntry entry = entries.get(projectId);
        if (entry == null) {
            throw new IllegalStateException(
                    "No runtime registered for projectId: " + projectId);
        }
        entry.state = state;
        if (state == RuntimeState.RUNNING && entry.startedAt == 0L) {
            entry.startedAt = System.currentTimeMillis();
        } else if (state == RuntimeState.STOPPED) {
            releaseAllFor(projectId, entry);
        }
    }

    /**
     * Returns the current {@link RuntimeState} of the session, or
     * {@code null} if the session is not registered.
     */
    public RuntimeState getState(String projectId) {
        RuntimeEntry entry = entries.get(projectId);
        return entry != null ? entry.state : null;
    }

    /**
     * Convenience: {@code true} if the session is registered and in state
     * {@link RuntimeState#RUNNING}.
     */
    public boolean isRunning(String projectId) {
        return RuntimeState.RUNNING.equals(getState(projectId));
    }

    // ------------------------------------------------------------------
    // Resource arbitration
    // ------------------------------------------------------------------

    /**
     * Attempts to acquire an exclusive resource for a session.
     *
     * <p>Only one session can hold a given {@code resourceId} at a time.
     * Re-acquiring the same resource by the same session is idempotent and
     * returns {@code true}.</p>
     *
     * @param projectId  the session requesting the resource; must be registered
     * @param resourceId opaque identifier for the hardware resource (e.g.
     *                   {@code "microphone"}, {@code "camera"}); must be non-blank
     * @return {@code true} if the resource was acquired (or already held by
     *         this session); {@code false} if it is currently held by another
     *         session
     * @throws IllegalArgumentException if either argument is blank
     * @throws IllegalStateException    if the session is not registered
     */
    public boolean acquireExclusiveResource(String projectId, String resourceId) {
        requireNonBlank(projectId, "projectId");
        requireNonBlank(resourceId, "resourceId");
        if (!entries.containsKey(projectId)) {
            throw new IllegalStateException(
                    "No runtime registered for projectId: " + projectId);
        }
        synchronized (exclusiveResources) {
            String holder = exclusiveResources.get(resourceId);
            if (holder == null) {
                exclusiveResources.put(resourceId, projectId);
                entries.get(projectId).heldResources.add(resourceId);
                return true;
            }
            if (holder.equals(projectId)) {
                // Idempotent re-acquire by the same session.
                return true;
            }
            return false;
        }
    }

    /**
     * Releases a resource held by {@code projectId}.  Silently ignored if the
     * resource is not held by that session.
     *
     * @param projectId  the session releasing the resource
     * @param resourceId resource to release
     */
    public void releaseExclusiveResource(String projectId, String resourceId) {
        requireNonBlank(projectId, "projectId");
        requireNonBlank(resourceId, "resourceId");
        synchronized (exclusiveResources) {
            exclusiveResources.remove(resourceId, projectId);
        }
        RuntimeEntry entry = entries.get(projectId);
        if (entry != null) {
            entry.heldResources.remove(resourceId);
        }
    }

    /**
     * Returns the project ID of the session currently holding
     * {@code resourceId}, or {@code null} if the resource is free.
     */
    public String resourceHolder(String resourceId) {
        return exclusiveResources.get(resourceId);
    }

    /** {@code true} if {@code resourceId} is currently held by any session. */
    public boolean isResourceHeld(String resourceId) {
        return exclusiveResources.containsKey(resourceId);
    }

    /**
     * Returns an unmodifiable snapshot of the exclusive resources currently
     * held by {@code projectId}.  Returns an empty set if the session is not
     * registered.
     */
    public Set<String> heldResources(String projectId) {
        RuntimeEntry entry = entries.get(projectId);
        if (entry == null) return Collections.emptySet();
        return Collections.unmodifiableSet(new HashSet<>(entry.heldResources));
    }

    // ------------------------------------------------------------------
    // Cross-session bus
    // ------------------------------------------------------------------

    /**
     * Returns the shared {@link CrossSessionBus} for inter-session event
     * delivery.
     */
    public CrossSessionBus getCrossSessionBus() {
        return crossSessionBus;
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns an unmodifiable snapshot of {@link RuntimeStatus} for all
     * registered sessions.
     */
    public List<RuntimeStatus> listRuntimes() {
        List<RuntimeStatus> result = new ArrayList<>();
        for (java.util.Map.Entry<String, RuntimeEntry> e : entries.entrySet()) {
            RuntimeEntry re = e.getValue();
            result.add(new RuntimeStatus(e.getKey(), re.state, re.startedAt,
                    re.heldResources));
        }
        return Collections.unmodifiableList(result);
    }

    /**
     * Returns the number of sessions currently in state
     * {@link RuntimeState#RUNNING}.
     */
    public int activeCount() {
        int count = 0;
        for (RuntimeEntry entry : entries.values()) {
            if (entry.state == RuntimeState.RUNNING) count++;
        }
        return count;
    }

    // ------------------------------------------------------------------
    // Lifecycle
    // ------------------------------------------------------------------

    /**
     * Releases all exclusive resources across all sessions and clears the
     * registry.  Intended for graceful server shutdown.  Does not unsubscribe
     * sessions from the {@link CrossSessionBus}.
     */
    public void shutdown() {
        for (java.util.Map.Entry<String, RuntimeEntry> e : entries.entrySet()) {
            releaseAllFor(e.getKey(), e.getValue());
        }
        entries.clear();
    }

    // ------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------

    private void releaseAllFor(String projectId, RuntimeEntry entry) {
        synchronized (exclusiveResources) {
            for (String resourceId : new HashSet<>(entry.heldResources)) {
                exclusiveResources.remove(resourceId, projectId);
            }
        }
        entry.heldResources.clear();
    }

    private static void requireNonBlank(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
    }
}
