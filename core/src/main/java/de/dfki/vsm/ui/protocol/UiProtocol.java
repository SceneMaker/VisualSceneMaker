package de.dfki.vsm.ui.protocol;

import de.dfki.vsm.event.EventDispatcher;

import java.util.concurrent.atomic.AtomicBoolean;

public final class UiProtocol {
    public static final int VERSION = 1;
    private static final UiEventBus EVENT_BUS = new UiEventBus();
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

    public static void ensureBridge(final EventDispatcher dispatcher) {
        if (!BRIDGE_INSTALLED.compareAndSet(false, true)) {
            return;
        }
        EventDispatcher target = dispatcher != null ? dispatcher : EventDispatcher.getInstance();
        target.register(new UiEventBridge(EVENT_BUS));
    }
}
