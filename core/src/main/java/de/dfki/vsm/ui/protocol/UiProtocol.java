package de.dfki.vsm.ui.protocol;

import de.dfki.vsm.event.EventDispatcher;

import java.util.concurrent.atomic.AtomicBoolean;

public final class UiProtocol {
    public static final int VERSION = 1;
    private static final UiEventBus EVENT_BUS = new UiEventBus();
    // Used only for the legacy singleton fallback path.
    private static final AtomicBoolean BRIDGE_INSTALLED = new AtomicBoolean(false);

    private UiProtocol() {
    }

    public static UiEventBus getEventBus() {
        ensureBridge(null);
        return EVENT_BUS;
    }

    public static UiEventSink getEventSink() {
        return getEventBus();
    }

    /**
     * Installs a {@link UiEventBridge} on {@code dispatcher}.
     * <p>
     * If {@code dispatcher} is non-null (per-project instance), a new bridge is
     * registered immediately — one bridge per dispatcher, no global guard needed.
     * <p>
     * If {@code dispatcher} is null the legacy singleton fallback is used, and the
     * bridge is installed at most once (AtomicBoolean guard).
     */
    public static void ensureBridge(final EventDispatcher dispatcher) {
        if (dispatcher != null) {
            // Per-project dispatcher: always install a fresh bridge on it.
            dispatcher.register(new UiEventBridge(EVENT_BUS));
            return;
        }
        // Legacy singleton fallback — install once.
        if (!BRIDGE_INSTALLED.compareAndSet(false, true)) {
            return;
        }
        EventDispatcher.getInstance().register(new UiEventBridge(EVENT_BUS));
    }
}
