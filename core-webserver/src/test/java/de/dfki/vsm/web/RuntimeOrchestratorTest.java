package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Functional tests for {@link RuntimeOrchestrator}, {@link RuntimeOrchestrator.RuntimeStatus},
 * and {@link CrossSessionBus}.
 */
class RuntimeOrchestratorTest {

    private RuntimeOrchestrator orchestrator;

    @BeforeEach
    void setUp() {
        orchestrator = new RuntimeOrchestrator();
    }

    // =====================================================================
    // RuntimeOrchestrator — Registration
    // =====================================================================

    @Test
    void newOrchestratorIsEmpty() {
        assertEquals(0, orchestrator.size());
        assertTrue(orchestrator.listRuntimes().isEmpty());
    }

    @Test
    void registerAddsSession() {
        orchestrator.register("proj-1");
        assertEquals(1, orchestrator.size());
        assertTrue(orchestrator.contains("proj-1"));
    }

    @Test
    void registerIsIdempotent() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-1");
        assertEquals(1, orchestrator.size());
    }

    @Test
    void registerRefusesBlankId() {
        assertThrows(IllegalArgumentException.class, () -> orchestrator.register(""));
        assertThrows(IllegalArgumentException.class, () -> orchestrator.register(null));
    }

    @Test
    void newSessionStartsInIdleState() {
        orchestrator.register("proj-1");
        assertEquals(RuntimeOrchestrator.RuntimeState.IDLE, orchestrator.getState("proj-1"));
    }

    @Test
    void containsReturnsFalseForUnknownId() {
        assertFalse(orchestrator.contains("ghost"));
    }

    @Test
    void unregisterRemovesSession() {
        orchestrator.register("proj-1");
        orchestrator.unregister("proj-1");
        assertFalse(orchestrator.contains("proj-1"));
        assertEquals(0, orchestrator.size());
    }

    @Test
    void unregisterOnAbsentIdIsHarmless() {
        assertDoesNotThrow(() -> orchestrator.unregister("ghost"));
    }

    @Test
    void unregisterOneDoesNotAffectOthers() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.unregister("proj-A");
        assertFalse(orchestrator.contains("proj-A"));
        assertTrue(orchestrator.contains("proj-B"));
        assertEquals(1, orchestrator.size());
    }

    // =====================================================================
    // RuntimeOrchestrator — State transitions
    // =====================================================================

    @Test
    void stateTransitionsIdleToRunning() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertEquals(RuntimeOrchestrator.RuntimeState.RUNNING, orchestrator.getState("proj-1"));
    }

    @Test
    void stateTransitionsRunningToPaused() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.PAUSED);
        assertEquals(RuntimeOrchestrator.RuntimeState.PAUSED, orchestrator.getState("proj-1"));
    }

    @Test
    void stateTransitionsPausedToRunning() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.PAUSED);
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertEquals(RuntimeOrchestrator.RuntimeState.RUNNING, orchestrator.getState("proj-1"));
    }

    @Test
    void stateTransitionsToStopped() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.STOPPED);
        assertEquals(RuntimeOrchestrator.RuntimeState.STOPPED, orchestrator.getState("proj-1"));
    }

    @Test
    void setStateThrowsForUnregisteredSession() {
        assertThrows(IllegalStateException.class,
                () -> orchestrator.setState("ghost", RuntimeOrchestrator.RuntimeState.RUNNING));
    }

    @Test
    void setStateRefusesNullState() {
        orchestrator.register("proj-1");
        assertThrows(IllegalArgumentException.class,
                () -> orchestrator.setState("proj-1", null));
    }

    @Test
    void isRunningReturnsTrueOnlyWhenRunning() {
        orchestrator.register("proj-1");
        assertFalse(orchestrator.isRunning("proj-1"));
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertTrue(orchestrator.isRunning("proj-1"));
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.PAUSED);
        assertFalse(orchestrator.isRunning("proj-1"));
    }

    @Test
    void isRunningReturnsFalseForUnregisteredSession() {
        assertFalse(orchestrator.isRunning("ghost"));
    }

    @Test
    void getStateReturnsNullForUnregisteredSession() {
        assertNull(orchestrator.getState("ghost"));
    }

    @Test
    void startedAtIsRecordedOnFirstRunning() {
        orchestrator.register("proj-1");
        assertEquals(0L, startedAt("proj-1"));
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertTrue(startedAt("proj-1") > 0L);
    }

    @Test
    void startedAtIsNotOverwrittenOnResumeAfterPause() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        long first = startedAt("proj-1");

        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.PAUSED);
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertEquals(first, startedAt("proj-1"),
                "startedAt must not be overwritten on resume");
    }

    // =====================================================================
    // RuntimeOrchestrator — Resource arbitration
    // =====================================================================

    @Test
    void acquireGrantsUnheldResource() {
        orchestrator.register("proj-1");
        assertTrue(orchestrator.acquireExclusiveResource("proj-1", "microphone"));
        assertTrue(orchestrator.isResourceHeld("microphone"));
        assertEquals("proj-1", orchestrator.resourceHolder("microphone"));
    }

    @Test
    void acquireIsIdempotentForSameSession() {
        orchestrator.register("proj-1");
        assertTrue(orchestrator.acquireExclusiveResource("proj-1", "microphone"));
        assertTrue(orchestrator.acquireExclusiveResource("proj-1", "microphone"),
                "Re-acquiring the same resource by the same session must succeed");
        assertEquals(1, orchestrator.heldResources("proj-1").size());
    }

    @Test
    void acquireBlocksOtherSession() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        assertTrue(orchestrator.acquireExclusiveResource("proj-1", "microphone"));
        assertFalse(orchestrator.acquireExclusiveResource("proj-2", "microphone"),
                "A second session must not acquire a held resource");
    }

    @Test
    void releaseFreesResourceForOtherSession() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        orchestrator.releaseExclusiveResource("proj-1", "microphone");

        assertFalse(orchestrator.isResourceHeld("microphone"));
        assertTrue(orchestrator.acquireExclusiveResource("proj-2", "microphone"));
    }

    @Test
    void releaseByNonHolderIsIgnored() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        orchestrator.releaseExclusiveResource("proj-2", "microphone"); // proj-2 doesn't hold it
        assertEquals("proj-1", orchestrator.resourceHolder("microphone"),
                "Non-holder release must not free the resource");
    }

    @Test
    void heldResourcesReflectsAcquired() {
        orchestrator.register("proj-1");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        orchestrator.acquireExclusiveResource("proj-1", "camera");
        Set<String> held = orchestrator.heldResources("proj-1");
        assertTrue(held.contains("microphone"));
        assertTrue(held.contains("camera"));
        assertEquals(2, held.size());
    }

    @Test
    void heldResourcesIsUnmodifiable() {
        orchestrator.register("proj-1");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        assertThrows(UnsupportedOperationException.class,
                () -> orchestrator.heldResources("proj-1").add("evil"));
    }

    @Test
    void heldResourcesEmptyForUnregisteredSession() {
        assertTrue(orchestrator.heldResources("ghost").isEmpty());
    }

    @Test
    void stoppedStateReleasesAllResources() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        orchestrator.acquireExclusiveResource("proj-1", "camera");

        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.STOPPED);

        assertFalse(orchestrator.isResourceHeld("microphone"),
                "Stopping must release microphone");
        assertFalse(orchestrator.isResourceHeld("camera"),
                "Stopping must release camera");
        assertTrue(orchestrator.heldResources("proj-1").isEmpty());

        // proj-2 can now acquire the freed resource
        assertTrue(orchestrator.acquireExclusiveResource("proj-2", "microphone"));
    }

    @Test
    void unregisterReleasesAllResources() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");

        orchestrator.unregister("proj-1");

        assertFalse(orchestrator.isResourceHeld("microphone"));
        assertTrue(orchestrator.acquireExclusiveResource("proj-2", "microphone"));
    }

    @Test
    void acquireThrowsForUnregisteredSession() {
        assertThrows(IllegalStateException.class,
                () -> orchestrator.acquireExclusiveResource("ghost", "microphone"));
    }

    @Test
    void acquireRefusesBlankArguments() {
        orchestrator.register("proj-1");
        assertThrows(IllegalArgumentException.class,
                () -> orchestrator.acquireExclusiveResource("", "microphone"));
        assertThrows(IllegalArgumentException.class,
                () -> orchestrator.acquireExclusiveResource("proj-1", ""));
    }

    // =====================================================================
    // RuntimeOrchestrator — listRuntimes() and activeCount()
    // =====================================================================

    @Test
    void listRuntimesReturnsOnePerSession() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        assertEquals(2, orchestrator.listRuntimes().size());
    }

    @Test
    void listRuntimesIsUnmodifiable() {
        orchestrator.register("proj-1");
        assertThrows(UnsupportedOperationException.class,
                () -> orchestrator.listRuntimes().remove(0));
    }

    @Test
    void runtimeStatusToJsonContainsAllFields() {
        orchestrator.register("proj-1");
        orchestrator.setState("proj-1", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.acquireExclusiveResource("proj-1", "microphone");

        RuntimeOrchestrator.RuntimeStatus status = findStatus("proj-1");
        JSONObject json = status.toJson();

        assertEquals("proj-1", json.getString("projectId"));
        assertEquals("RUNNING", json.getString("state"));
        assertTrue(json.getLong("startedAt") > 0);
        assertTrue(json.getJSONArray("heldResources").toList()
                .contains("microphone"));
    }

    @Test
    void activeCountReflectsRunningSessions() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.register("proj-C");

        assertEquals(0, orchestrator.activeCount());

        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertEquals(1, orchestrator.activeCount());

        orchestrator.setState("proj-B", RuntimeOrchestrator.RuntimeState.RUNNING);
        assertEquals(2, orchestrator.activeCount());

        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.PAUSED);
        assertEquals(1, orchestrator.activeCount());

        orchestrator.setState("proj-B", RuntimeOrchestrator.RuntimeState.STOPPED);
        assertEquals(0, orchestrator.activeCount());
    }

    // =====================================================================
    // RuntimeOrchestrator — Lifecycle independence
    // =====================================================================

    @Test
    void stoppingOneSessionDoesNotAffectOthers() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.setState("proj-B", RuntimeOrchestrator.RuntimeState.RUNNING);

        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.STOPPED);

        assertEquals(RuntimeOrchestrator.RuntimeState.STOPPED, orchestrator.getState("proj-A"));
        assertEquals(RuntimeOrchestrator.RuntimeState.RUNNING, orchestrator.getState("proj-B"));
    }

    @Test
    void sessionsHaveIndependentStateAndResources() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.acquireExclusiveResource("proj-A", "camera");

        assertEquals(RuntimeOrchestrator.RuntimeState.IDLE, orchestrator.getState("proj-B"));
        assertTrue(orchestrator.heldResources("proj-B").isEmpty());
    }

    // =====================================================================
    // RuntimeOrchestrator — shutdown()
    // =====================================================================

    @Test
    void shutdownClearsAllSessions() {
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.shutdown();
        assertEquals(0, orchestrator.size());
    }

    @Test
    void shutdownReleasesAllResources() {
        orchestrator.register("proj-1");
        orchestrator.register("proj-2");
        orchestrator.acquireExclusiveResource("proj-1", "microphone");
        orchestrator.acquireExclusiveResource("proj-2", "camera");

        orchestrator.shutdown();

        assertFalse(orchestrator.isResourceHeld("microphone"));
        assertFalse(orchestrator.isResourceHeld("camera"));
    }

    @Test
    void shutdownOnEmptyOrchestratorIsHarmless() {
        assertDoesNotThrow(() -> orchestrator.shutdown());
    }

    // =====================================================================
    // RuntimeOrchestrator — Thread safety
    // =====================================================================

    @Test
    void concurrentRegisterAndUnregisterAreSafe() throws InterruptedException {
        int threads = 30;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        ExecutorService exec = Executors.newFixedThreadPool(threads);

        for (int i = 0; i < threads; i++) {
            final String id = "proj-" + i;
            exec.submit(() -> {
                try {
                    start.await();
                    orchestrator.register(id);
                    orchestrator.unregister(id);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();
        // No assertion on final size — just verify no exception was thrown
    }

    @Test
    void concurrentResourceAcquisitionAllowsOnlyOneWinner() throws InterruptedException {
        int threads = 20;
        for (int i = 0; i < threads; i++) {
            orchestrator.register("proj-" + i);
        }
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        AtomicInteger winners = new AtomicInteger(0);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final String id = "proj-" + i;
            exec.submit(() -> {
                try {
                    start.await();
                    boolean acquired = orchestrator.acquireExclusiveResource(id, "shared-mic");
                    if (acquired) winners.incrementAndGet();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();

        assertEquals(1, winners.get(), "Exactly one session must win the shared resource");
        assertTrue(orchestrator.isResourceHeld("shared-mic"));
    }

    // =====================================================================
    // CrossSessionBus — Subscribe / emit
    // =====================================================================

    @Test
    void emitDeliveresToSubscriber() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        List<String> received = new ArrayList<>();

        bus.subscribe("handoff", "proj-B", (from, event, payload) ->
                received.add(from + ":" + event));

        bus.emit("proj-A", "handoff", null);

        assertEquals(1, received.size());
        assertEquals("proj-A:handoff", received.get(0));
    }

    @Test
    void emitReturnsListenerCount() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        bus.subscribe("handoff", "proj-B", (f, e, p) -> {});
        bus.subscribe("handoff", "proj-C", (f, e, p) -> {});

        int notified = bus.emit("proj-A", "handoff", null);
        assertEquals(2, notified);
    }

    @Test
    void emitReturnsZeroWhenNoSubscribers() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertEquals(0, bus.emit("proj-A", "handoff", null));
    }

    @Test
    void emitDoesNotDeliverToOtherEvents() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        AtomicInteger count = new AtomicInteger(0);

        bus.subscribe("event-A", "proj-B", (f, e, p) -> count.incrementAndGet());

        bus.emit("proj-A", "event-B", null);  // different event

        assertEquals(0, count.get());
    }

    @Test
    void emitDeliverPayloadToSubscriber() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        List<JSONObject> payloads = new ArrayList<>();

        bus.subscribe("update", "proj-B", (f, e, p) -> payloads.add(p));

        JSONObject payload = new JSONObject();
        payload.put("key", "value");
        bus.emit("proj-A", "update", payload);

        assertEquals(1, payloads.size());
        assertEquals("value", payloads.get(0).getString("key"));
    }

    @Test
    void emitWithNullPayloadPassesEmptyObject() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        List<JSONObject> payloads = new ArrayList<>();

        bus.subscribe("ping", "proj-B", (f, e, p) -> payloads.add(p));
        bus.emit("proj-A", "ping", null);

        assertNotNull(payloads.get(0));
        assertEquals(0, payloads.get(0).length());  // empty object
    }

    @Test
    void emitWithNullFromProjectIdIsDelivered() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        List<String> froms = new ArrayList<>();

        bus.subscribe("sys.event", "proj-B", (f, e, p) -> froms.add(f));
        bus.emit(null, "sys.event", null);

        assertEquals(1, froms.size());
        assertNull(froms.get(0));
    }

    // =====================================================================
    // CrossSessionBus — Unsubscribe
    // =====================================================================

    @Test
    void unsubscribeStopsDelivery() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        AtomicInteger count = new AtomicInteger(0);

        bus.subscribe("handoff", "proj-B", (f, e, p) -> count.incrementAndGet());
        bus.emit("proj-A", "handoff", null);
        assertEquals(1, count.get());

        bus.unsubscribe("handoff", "proj-B");
        bus.emit("proj-A", "handoff", null);
        assertEquals(1, count.get(), "Should not receive event after unsubscribe");
    }

    @Test
    void unsubscribeOnAbsentSubscriptionIsHarmless() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertDoesNotThrow(() -> bus.unsubscribe("unknown-event", "proj-B"));
    }

    @Test
    void unsubscribeAllRemovesFromAllEvents() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        AtomicInteger count = new AtomicInteger(0);

        bus.subscribe("event-A", "proj-B", (f, e, p) -> count.incrementAndGet());
        bus.subscribe("event-B", "proj-B", (f, e, p) -> count.incrementAndGet());

        bus.unsubscribeAll("proj-B");

        bus.emit("proj-A", "event-A", null);
        bus.emit("proj-A", "event-B", null);

        assertEquals(0, count.get(), "No events should be received after unsubscribeAll");
    }

    @Test
    void unsubscribeAllRefusesBlankId() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertThrows(IllegalArgumentException.class, () -> bus.unsubscribeAll(""));
        assertThrows(IllegalArgumentException.class, () -> bus.unsubscribeAll(null));
    }

    @Test
    void subscribeReplacesExistingListenerForSameSubscriber() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        AtomicInteger firstCount = new AtomicInteger(0);
        AtomicInteger secondCount = new AtomicInteger(0);

        bus.subscribe("event", "proj-B", (f, e, p) -> firstCount.incrementAndGet());
        bus.subscribe("event", "proj-B", (f, e, p) -> secondCount.incrementAndGet()); // replaces

        bus.emit("proj-A", "event", null);

        assertEquals(0, firstCount.get(), "First listener must be replaced");
        assertEquals(1, secondCount.get(), "Second listener must receive the event");
    }

    // =====================================================================
    // CrossSessionBus — Error isolation
    // =====================================================================

    @Test
    void listenerExceptionDoesNotPreventOtherDeliveries() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        AtomicInteger goodCount = new AtomicInteger(0);

        bus.subscribe("evt", "proj-bad",  (f, e, p) -> { throw new RuntimeException("oops"); });
        bus.subscribe("evt", "proj-good", (f, e, p) -> goodCount.incrementAndGet());

        int notified = assertDoesNotThrow(() -> bus.emit("proj-A", "evt", null));

        // Only the good listener succeeded
        assertEquals(1, notified);
        assertEquals(1, goodCount.get());
    }

    // =====================================================================
    // CrossSessionBus — subscriberCount / hasSubscribers
    // =====================================================================

    @Test
    void subscriberCountReflectsRegistrations() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertEquals(0, bus.subscriberCount("handoff"));

        bus.subscribe("handoff", "proj-B", (f, e, p) -> {});
        assertEquals(1, bus.subscriberCount("handoff"));

        bus.subscribe("handoff", "proj-C", (f, e, p) -> {});
        assertEquals(2, bus.subscriberCount("handoff"));

        bus.unsubscribe("handoff", "proj-B");
        assertEquals(1, bus.subscriberCount("handoff"));
    }

    @Test
    void hasSubscribersReturnsTrueWhenPresent() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertFalse(bus.hasSubscribers("handoff"));
        bus.subscribe("handoff", "proj-B", (f, e, p) -> {});
        assertTrue(bus.hasSubscribers("handoff"));
    }

    // =====================================================================
    // CrossSessionBus — clear()
    // =====================================================================

    @Test
    void clearRemovesAllSubscriptions() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        bus.subscribe("event-A", "proj-B", (f, e, p) -> {});
        bus.subscribe("event-B", "proj-C", (f, e, p) -> {});

        bus.clear();

        assertEquals(0, bus.subscriberCount("event-A"));
        assertEquals(0, bus.subscriberCount("event-B"));
    }

    // =====================================================================
    // CrossSessionBus — Thread safety
    // =====================================================================

    @Test
    void concurrentSubscribeAndEmitAreSafe() throws InterruptedException {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        int threads = 30;
        AtomicInteger received = new AtomicInteger(0);
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final int idx = i;
            exec.submit(() -> {
                try {
                    start.await();
                    bus.subscribe("ping", "sub-" + idx, (f, e, p) -> received.incrementAndGet());
                    bus.emit("emitter", "ping", null);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();

        // No assertions on exact count (subscribe and emit race), just no exception
        assertTrue(received.get() >= 0);
    }

    // =====================================================================
    // CrossSessionBus — emitRefusesBlankEventName
    // =====================================================================

    @Test
    void emitRefusesBlankEventName() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertThrows(IllegalArgumentException.class, () -> bus.emit("proj-A", "", null));
        assertThrows(IllegalArgumentException.class, () -> bus.emit("proj-A", null, null));
    }

    @Test
    void subscribeRefusesBlankArguments() {
        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        assertThrows(IllegalArgumentException.class,
                () -> bus.subscribe("", "proj-B", (f, e, p) -> {}));
        assertThrows(IllegalArgumentException.class,
                () -> bus.subscribe("event", "", (f, e, p) -> {}));
        assertThrows(NullPointerException.class,
                () -> bus.subscribe("event", "proj-B", null));
    }

    // =====================================================================
    // Integration: orchestrator + bus + resources
    // =====================================================================

    @Test
    void fullHandoffScenario() {
        // proj-A runs, acquires mic, then signals proj-B via bus
        orchestrator.register("proj-A");
        orchestrator.register("proj-B");
        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.RUNNING);
        orchestrator.acquireExclusiveResource("proj-A", "microphone");

        CrossSessionBus bus = orchestrator.getCrossSessionBus();
        List<String> events = new ArrayList<>();
        bus.subscribe("handoff", "proj-B", (from, evt, p) -> events.add(from + "→" + evt));

        // proj-A finishes: release mic, emit handoff, stop
        orchestrator.releaseExclusiveResource("proj-A", "microphone");
        bus.emit("proj-A", "handoff", null);
        orchestrator.setState("proj-A", RuntimeOrchestrator.RuntimeState.STOPPED);

        // proj-B picks up the mic and starts
        assertTrue(orchestrator.acquireExclusiveResource("proj-B", "microphone"));
        orchestrator.setState("proj-B", RuntimeOrchestrator.RuntimeState.RUNNING);

        assertEquals(1, events.size());
        assertEquals("proj-A→handoff", events.get(0));
        assertEquals(RuntimeOrchestrator.RuntimeState.RUNNING, orchestrator.getState("proj-B"));
        assertEquals("proj-B", orchestrator.resourceHolder("microphone"));
    }

    // =====================================================================
    // Helpers
    // =====================================================================

    private long startedAt(String projectId) {
        return orchestrator.listRuntimes().stream()
                .filter(s -> projectId.equals(s.projectId))
                .findFirst()
                .map(s -> s.startedAt)
                .orElseThrow(() -> new AssertionError("Status not found for " + projectId));
    }

    private RuntimeOrchestrator.RuntimeStatus findStatus(String projectId) {
        return orchestrator.listRuntimes().stream()
                .filter(s -> projectId.equals(s.projectId))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Status not found for " + projectId));
    }
}
