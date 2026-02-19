package de.dfki.vsm.runtime.api.android;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.function.Consumer;

/**
 * Tracks active Android WebSocket sessions and provides a safe broadcaster.
 */
public final class AndroidRuntimeWsSessionRegistry {

    private final Set<AndroidRuntimeWsSession> sessions = ConcurrentHashMap.newKeySet();
    private final ExecutorService broadcastExecutor = Executors.newSingleThreadExecutor(new ThreadFactory() {
        @Override
        public Thread newThread(final Runnable runnable) {
            Thread thread = new Thread(runnable, "vsm-android-ws-broadcast");
            thread.setDaemon(true);
            return thread;
        }
    });

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
            broadcastExecutor.execute(() -> {
                for (AndroidRuntimeWsSession session : sessions) {
                    try {
                        session.sendText(message);
                    } catch (Exception ignored) {
                        // Drop broken sessions to avoid repeated failures.
                        sessions.remove(session);
                    }
                }
            });
        };
    }

    public void shutdown() {
        broadcastExecutor.shutdownNow();
        sessions.clear();
    }
}
