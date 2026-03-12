package de.dfki.vsm.web;

import de.dfki.vsm.event.EventListener;
import io.javalin.websocket.WsContext;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages the set of WebSocket subscribers for a single project and holds the
 * per-project {@link EventListener} that forwards runtime events to those
 * subscribers.
 *
 * <p>Each {@code CollaborationSession} is created together with its
 * {@link de.dfki.vsm.web.WebUiServer.ProjectRef} and lives for the same
 * lifetime as the project registration.  Clients join a session by sending a
 * {@code Session.Subscribe} WebSocket command.  Once subscribed, runtime
 * events produced by that project are routed exclusively to the session's
 * subscriber set; unsubscribed clients continue to receive server-wide
 * broadcasts via the legacy {@code broadcastToAll} path.</p>
 */
public class CollaborationSession {

    private final String projectId;
    private final Set<WsContext> subscribers = ConcurrentHashMap.newKeySet();
    private volatile EventListener eventForwarder;
    private final OperationLog operationLog = new OperationLog();
    private final PresenceManager presenceManager = new PresenceManager();

    public CollaborationSession(String projectId) {
        this.projectId = projectId;
    }

    // -------------------------------------------------------------------------
    // Subscription management
    // -------------------------------------------------------------------------

    public void subscribe(WsContext ctx) {
        subscribers.add(ctx);
    }

    public void unsubscribe(WsContext ctx) {
        subscribers.remove(ctx);
    }

    public int subscriberCount() {
        return subscribers.size();
    }

    public Set<WsContext> getSubscribers() {
        return Collections.unmodifiableSet(subscribers);
    }

    // -------------------------------------------------------------------------
    // Broadcast helpers
    // -------------------------------------------------------------------------

    /** Send {@code message} to every subscriber of this project. */
    public void broadcast(String message) {
        for (WsContext ctx : subscribers) {
            trySend(ctx, message);
        }
    }

    /** Send {@code message} to every subscriber except {@code origin}. */
    public void broadcastExcept(WsContext origin, String message) {
        for (WsContext ctx : subscribers) {
            if (ctx != origin) {
                trySend(ctx, message);
            }
        }
    }

    private static void trySend(WsContext ctx, String message) {
        try {
            if (ctx.session.isOpen()) {
                ctx.send(message);
            }
        } catch (Exception ignored) {
            // Silently drop; session will be cleaned up on close/error.
        }
    }

    // -------------------------------------------------------------------------
    // Event forwarder (set by WebUiServer when project is registered)
    // -------------------------------------------------------------------------

    public EventListener getEventForwarder() {
        return eventForwarder;
    }

    public void setEventForwarder(EventListener forwarder) {
        this.eventForwarder = forwarder;
    }

    // -------------------------------------------------------------------------
    // Operation log
    // -------------------------------------------------------------------------

    public OperationLog getOperationLog() {
        return operationLog;
    }

    // -------------------------------------------------------------------------
    // Presence
    // -------------------------------------------------------------------------

    public PresenceManager getPresenceManager() {
        return presenceManager;
    }

    // -------------------------------------------------------------------------
    // Accessors
    // -------------------------------------------------------------------------

    public String getProjectId() {
        return projectId;
    }
}
