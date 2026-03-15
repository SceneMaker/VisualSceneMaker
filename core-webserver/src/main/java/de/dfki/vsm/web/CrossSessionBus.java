package de.dfki.vsm.web;

import org.json.JSONObject;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Lightweight pub/sub bus that lets one runtime session trigger named events
 * in other sessions.
 *
 * <p>Typical use: a finished dialogue agent in session A sends a
 * {@code "handoff"} event; session B's runtime has subscribed to that event
 * and reacts by starting a different scenario.  This enables multi-agent
 * installations where several parallel {@link RuntimeOrchestrator} sessions
 * coordinate without sharing state or an interpreter thread.</p>
 *
 * <h2>Thread safety</h2>
 * <p>All operations are non-blocking and safe for concurrent use.
 * {@link ConcurrentHashMap} ensures visibility of subscribe/unsubscribe from
 * any thread.  Listener invocations during {@link #emit} hold no locks, so
 * slow listeners do not block the emitter.</p>
 *
 * <h2>Error isolation</h2>
 * <p>Exceptions thrown by a listener are caught and silently discarded so that
 * a misbehaving subscriber cannot prevent other subscribers from receiving the
 * same event.</p>
 */
public final class CrossSessionBus {

    // ------------------------------------------------------------------
    // Listener interface
    // ------------------------------------------------------------------

    /**
     * Receives a cross-session event emitted by another project's runtime.
     */
    @FunctionalInterface
    public interface Listener {
        /**
         * @param fromProjectId project that emitted the event
         * @param eventName     name of the event (e.g. {@code "handoff"})
         * @param payload       event data; never {@code null} (empty object if
         *                      the emitter passed {@code null})
         */
        void onEvent(String fromProjectId, String eventName, JSONObject payload);
    }

    // ------------------------------------------------------------------
    // State:  eventName → (subscriberId → Listener)
    // ------------------------------------------------------------------

    private final ConcurrentHashMap<String, ConcurrentHashMap<String, Listener>> subs =
            new ConcurrentHashMap<>();

    // ------------------------------------------------------------------
    // Subscribe / unsubscribe
    // ------------------------------------------------------------------

    /**
     * Registers {@code listener} to receive events named {@code eventName}.
     * If {@code subscriberId} already has a listener for that event it is
     * silently replaced.
     *
     * @param eventName    name of the event to subscribe to; must be non-blank
     * @param subscriberId stable subscriber identifier (usually a project ID);
     *                     must be non-blank
     * @param listener     callback to invoke; must not be {@code null}
     */
    public void subscribe(String eventName, String subscriberId, Listener listener) {
        requireNonBlank(eventName, "eventName");
        requireNonBlank(subscriberId, "subscriberId");
        Objects.requireNonNull(listener, "listener must not be null");
        subs.computeIfAbsent(eventName, k -> new ConcurrentHashMap<>())
            .put(subscriberId, listener);
    }

    /**
     * Removes the subscription of {@code subscriberId} for {@code eventName}.
     * Silently ignored if the subscription does not exist.
     */
    public void unsubscribe(String eventName, String subscriberId) {
        ConcurrentHashMap<String, Listener> byEvent = subs.get(eventName);
        if (byEvent != null) {
            byEvent.remove(subscriberId);
        }
    }

    /**
     * Removes all subscriptions for {@code subscriberId} across all event
     * names.  Call this when a session shuts down.
     *
     * @param subscriberId subscriber to remove; must be non-blank
     */
    public void unsubscribeAll(String subscriberId) {
        requireNonBlank(subscriberId, "subscriberId");
        for (ConcurrentHashMap<String, Listener> byEvent : subs.values()) {
            byEvent.remove(subscriberId);
        }
    }

    // ------------------------------------------------------------------
    // Emit
    // ------------------------------------------------------------------

    /**
     * Delivers {@code eventName} to every subscriber registered for that
     * event.  Exceptions thrown by individual listeners are caught and
     * suppressed so all subscribers receive the event regardless.
     *
     * @param fromProjectId project emitting the event (may be {@code null}
     *                      for system-generated events)
     * @param eventName     name of the event; must be non-blank
     * @param payload       arbitrary event data; {@code null} is treated as
     *                      an empty JSON object
     * @return the number of listeners that were successfully notified
     */
    public int emit(String fromProjectId, String eventName, JSONObject payload) {
        requireNonBlank(eventName, "eventName");
        ConcurrentHashMap<String, Listener> byEvent = subs.get(eventName);
        if (byEvent == null || byEvent.isEmpty()) {
            return 0;
        }
        JSONObject safePayload = payload != null ? payload : new JSONObject();
        int count = 0;
        for (Listener listener : byEvent.values()) {
            try {
                listener.onEvent(fromProjectId, eventName, safePayload);
                count++;
            } catch (Exception ignored) {
                // Listener errors must not propagate to the emitter.
            }
        }
        return count;
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns the number of subscribers currently registered for
     * {@code eventName}.
     */
    public int subscriberCount(String eventName) {
        ConcurrentHashMap<String, Listener> byEvent = subs.get(eventName);
        return byEvent != null ? byEvent.size() : 0;
    }

    /** {@code true} if at least one subscriber is registered for {@code eventName}. */
    public boolean hasSubscribers(String eventName) {
        return subscriberCount(eventName) > 0;
    }

    /** Removes all subscriptions for all events. */
    public void clear() {
        subs.clear();
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
