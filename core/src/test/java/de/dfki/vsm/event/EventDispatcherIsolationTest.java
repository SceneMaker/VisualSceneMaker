package de.dfki.vsm.event;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Verifies that two independent EventDispatcher instances do not share events.
 * This is the core correctness invariant of Phase A (de-singletoning).
 */
class EventDispatcherIsolationTest {

    // Minimal concrete EventObject for testing
    static class PingEvent extends EventObject {
        PingEvent(Object source) { super(source); }
    }

    // Counting listener
    static class CountingListener implements EventListener {
        final AtomicInteger count = new AtomicInteger(0);
        @Override
        public void update(EventObject event) { count.incrementAndGet(); }
    }

    @Test
    void eventsDoNotCrossDispatcherBoundary() {
        EventDispatcher dispatcherA = new EventDispatcher();
        EventDispatcher dispatcherB = new EventDispatcher();

        CountingListener listenerA = new CountingListener();
        CountingListener listenerB = new CountingListener();

        dispatcherA.register(listenerA);
        dispatcherB.register(listenerB);

        // Fire on A — only A's listener should receive it
        dispatcherA.convey(new PingEvent(this));

        assertEquals(1, listenerA.count.get(), "listenerA should receive the event fired on dispatcherA");
        assertEquals(0, listenerB.count.get(), "listenerB must NOT receive events from dispatcherA");
    }

    @Test
    void eachDispatcherIsIndependent() {
        EventDispatcher d1 = new EventDispatcher();
        EventDispatcher d2 = new EventDispatcher();
        EventDispatcher d3 = new EventDispatcher();

        CountingListener l1 = new CountingListener();
        CountingListener l2 = new CountingListener();
        CountingListener l3 = new CountingListener();

        d1.register(l1);
        d2.register(l2);
        d3.register(l3);

        d2.convey(new PingEvent(this));
        d2.convey(new PingEvent(this));

        assertEquals(0, l1.count.get());
        assertEquals(2, l2.count.get());
        assertEquals(0, l3.count.get());
    }

    @Test
    void listenerRemovalIsPerDispatcher() {
        EventDispatcher dispatcher = new EventDispatcher();
        CountingListener listener = new CountingListener();

        dispatcher.register(listener);
        dispatcher.convey(new PingEvent(this));
        assertEquals(1, listener.count.get());

        dispatcher.remove(listener);
        dispatcher.convey(new PingEvent(this));
        assertEquals(1, listener.count.get(), "Removed listener must not receive further events");
    }

    @Test
    void singletonRemainsIndependentOfNewInstances() {
        // Ensure the deprecated singleton does not receive events from a new instance
        CountingListener singletonListener = new CountingListener();
        @SuppressWarnings("deprecation")
        EventDispatcher singleton = EventDispatcher.getInstance();
        singleton.register(singletonListener);

        try {
            EventDispatcher fresh = new EventDispatcher();
            fresh.convey(new PingEvent(this));

            assertEquals(0, singletonListener.count.get(),
                "Singleton listener must not receive events from a separate EventDispatcher instance");
        } finally {
            singleton.remove(singletonListener);
        }
    }
}
