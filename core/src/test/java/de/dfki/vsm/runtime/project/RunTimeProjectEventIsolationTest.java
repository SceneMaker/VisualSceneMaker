package de.dfki.vsm.runtime.project;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Verifies that each RunTimeProject owns an independent EventDispatcher,
 * so events fired in one project's runtime do not reach listeners of another project.
 */
class RunTimeProjectEventIsolationTest {

    static class TestEvent extends EventObject {
        TestEvent(Object source) { super(source); }
    }

    static class CountingListener implements EventListener {
        final AtomicInteger count = new AtomicInteger(0);
        @Override
        public void update(EventObject event) { count.incrementAndGet(); }
    }

    @Test
    void eachProjectHasItsOwnDispatcher() {
        RunTimeProject projectA = new RunTimeProject();
        RunTimeProject projectB = new RunTimeProject();

        EventDispatcher dA = projectA.getEventDispatcher();
        EventDispatcher dB = projectB.getEventDispatcher();

        assertNotNull(dA, "Project A must have a non-null EventDispatcher");
        assertNotNull(dB, "Project B must have a non-null EventDispatcher");
        assertNotSame(dA, dB, "Projects must NOT share the same EventDispatcher instance");
    }

    @Test
    void eventsFromProjectADoNotReachProjectBListeners() {
        RunTimeProject projectA = new RunTimeProject();
        RunTimeProject projectB = new RunTimeProject();

        CountingListener listenerOnB = new CountingListener();
        projectB.getEventDispatcher().register(listenerOnB);

        // Fire an event on project A's dispatcher
        projectA.getEventDispatcher().convey(new TestEvent(this));

        assertEquals(0, listenerOnB.count.get(),
            "A listener registered on project B must not receive events fired on project A");
    }

    @Test
    void eventsReachCorrectProjectListeners() {
        RunTimeProject projectA = new RunTimeProject();
        RunTimeProject projectB = new RunTimeProject();

        CountingListener listenerOnA = new CountingListener();
        CountingListener listenerOnB = new CountingListener();

        projectA.getEventDispatcher().register(listenerOnA);
        projectB.getEventDispatcher().register(listenerOnB);

        projectA.getEventDispatcher().convey(new TestEvent(this));
        projectB.getEventDispatcher().convey(new TestEvent(this));
        projectB.getEventDispatcher().convey(new TestEvent(this));

        assertEquals(1, listenerOnA.count.get(), "Project A listener should receive exactly 1 event");
        assertEquals(2, listenerOnB.count.get(), "Project B listener should receive exactly 2 events");
    }
}
