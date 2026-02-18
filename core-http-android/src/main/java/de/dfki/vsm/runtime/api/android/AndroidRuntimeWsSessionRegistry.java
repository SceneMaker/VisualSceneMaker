package de.dfki.vsm.runtime.api.android;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * Tracks active Android WebSocket sessions and provides a safe broadcaster.
 */
public final class AndroidRuntimeWsSessionRegistry {

    private final Set<AndroidRuntimeWsSession> sessions = ConcurrentHashMap.newKeySet();

    public void add(final AndroidRuntimeWsSession session) {
        if (session != null) {
            sessions.add(session);
        }
    }

    public void remove(final AndroidRuntimeWsSession session) {
        if (session != null) {
            sessions.remove(session);
        }
    }

    /**
     * Returns a broadcaster that emits to all currently registered sessions.
     */
    public Consumer<String> broadcaster() {
        return message -> {
            if (message == null) {
                return;
            }
            for (AndroidRuntimeWsSession session : sessions) {
                try {
                    session.sendText(message);
                } catch (Exception ignored) {
                    // Session transport errors are ignored; caller can remove dead sessions via onClose hooks.
                }
            }
        };
    }
}
