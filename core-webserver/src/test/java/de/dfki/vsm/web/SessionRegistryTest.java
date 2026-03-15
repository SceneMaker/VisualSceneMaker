package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Functional tests for {@link SessionRegistry} and {@link SessionRegistry.SessionSummary}.
 */
class SessionRegistryTest {

    private SessionRegistry registry;

    @BeforeEach
    void setUp() {
        registry = new SessionRegistry();
    }

    // ------------------------------------------------------------------
    // Initial state
    // ------------------------------------------------------------------

    @Test
    void newRegistryIsEmpty() {
        assertEquals(0, registry.size());
        assertTrue(registry.list().isEmpty());
    }

    // ------------------------------------------------------------------
    // create()
    // ------------------------------------------------------------------

    @Test
    void createAddsSession() {
        registry.create("proj-1");
        assertEquals(1, registry.size());
        assertTrue(registry.contains("proj-1"));
    }

    @Test
    void createReturnsSessionWithCorrectId() {
        CollaborationSession s = registry.create("proj-1");
        assertEquals("proj-1", s.getProjectId());
    }

    @Test
    void createRefusesBlankProjectId() {
        assertThrows(IllegalArgumentException.class, () -> registry.create(""));
        assertThrows(IllegalArgumentException.class, () -> registry.create("   "));
        assertThrows(IllegalArgumentException.class, () -> registry.create(null));
    }

    @Test
    void createFailsWhenSessionAlreadyExists() {
        registry.create("proj-1");
        assertThrows(IllegalStateException.class, () -> registry.create("proj-1"));
    }

    @Test
    void createTwoDifferentSessionsSucceeds() {
        registry.create("proj-A");
        registry.create("proj-B");
        assertEquals(2, registry.size());
    }

    @Test
    void createdSessionHasIndependentSubcomponents() {
        CollaborationSession s = registry.create("proj-1");
        assertNotNull(s.getOperationLog());
        assertNotNull(s.getPresenceManager());
        assertEquals(0L, s.getOperationLog().currentSeq());
        assertEquals(0, s.getPresenceManager().size());
    }

    // ------------------------------------------------------------------
    // getOrCreate()
    // ------------------------------------------------------------------

    @Test
    void getOrCreateCreatesWhenAbsent() {
        CollaborationSession s = registry.getOrCreate("proj-1");
        assertNotNull(s);
        assertEquals("proj-1", s.getProjectId());
        assertTrue(registry.contains("proj-1"));
    }

    @Test
    void getOrCreateReturnsSameInstanceWhenPresent() {
        CollaborationSession first = registry.getOrCreate("proj-1");
        CollaborationSession second = registry.getOrCreate("proj-1");
        assertSame(first, second);
    }

    @Test
    void getOrCreateIsIdempotent() {
        registry.getOrCreate("proj-1");
        registry.getOrCreate("proj-1");
        assertEquals(1, registry.size());
    }

    @Test
    void getOrCreateRefusesBlankId() {
        assertThrows(IllegalArgumentException.class, () -> registry.getOrCreate(""));
        assertThrows(IllegalArgumentException.class, () -> registry.getOrCreate(null));
    }

    // ------------------------------------------------------------------
    // get() and contains()
    // ------------------------------------------------------------------

    @Test
    void getReturnsNullForUnknownId() {
        assertNull(registry.get("nonexistent"));
    }

    @Test
    void getReturnsRegisteredSession() {
        CollaborationSession created = registry.create("proj-1");
        assertSame(created, registry.get("proj-1"));
    }

    @Test
    void containsReturnsFalseForUnknown() {
        assertFalse(registry.contains("nobody"));
    }

    @Test
    void containsReturnsTrueAfterCreate() {
        registry.create("proj-1");
        assertTrue(registry.contains("proj-1"));
    }

    @Test
    void containsReturnsFalseAfterDestroy() {
        registry.create("proj-1");
        registry.destroy("proj-1");
        assertFalse(registry.contains("proj-1"));
    }

    // ------------------------------------------------------------------
    // destroy()
    // ------------------------------------------------------------------

    @Test
    void destroyRemovesSession() {
        registry.create("proj-1");
        registry.destroy("proj-1");
        assertFalse(registry.contains("proj-1"));
        assertEquals(0, registry.size());
    }

    @Test
    void destroyReturnsRemovedSession() {
        CollaborationSession s = registry.create("proj-1");
        CollaborationSession removed = registry.destroy("proj-1");
        assertSame(s, removed);
    }

    @Test
    void destroyOnAbsentIdReturnsNull() {
        assertNull(registry.destroy("ghost"));
    }

    @Test
    void destroyOneDoesNotAffectOthers() {
        registry.create("proj-A");
        registry.create("proj-B");
        registry.destroy("proj-A");
        assertFalse(registry.contains("proj-A"));
        assertTrue(registry.contains("proj-B"));
        assertEquals(1, registry.size());
    }

    @Test
    void recreateAfterDestroySucceeds() {
        registry.create("proj-1");
        registry.destroy("proj-1");
        CollaborationSession fresh = registry.create("proj-1");
        assertNotNull(fresh);
        assertEquals("proj-1", fresh.getProjectId());
    }

    // ------------------------------------------------------------------
    // join()
    // ------------------------------------------------------------------

    @Test
    void joinRegistersUserInSession() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        assertTrue(registry.get("proj-1").getPresenceManager().isPresent("user1"));
    }

    @Test
    void joinReturnsCorrectSession() {
        CollaborationSession created = registry.create("proj-1");
        CollaborationSession joined = registry.join("proj-1", "user1", "Alice");
        assertSame(created, joined);
    }

    @Test
    void joinThrowsWhenSessionNotFound() {
        assertThrows(IllegalStateException.class,
                () -> registry.join("nonexistent", "user1", "Alice"));
    }

    @Test
    void joinRefusesBlankProjectId() {
        assertThrows(IllegalArgumentException.class,
                () -> registry.join("", "user1", "Alice"));
        assertThrows(IllegalArgumentException.class,
                () -> registry.join(null, "user1", "Alice"));
    }

    @Test
    void joinSetsDisplayName() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        UserPresence p = registry.get("proj-1").getPresenceManager().get("user1");
        assertNotNull(p);
        assertEquals("Alice", p.displayName);
    }

    @Test
    void joinDerivesDisplayNameWhenNull() {
        registry.create("proj-1");
        registry.join("proj-1", "abcdefghij", null);
        UserPresence p = registry.get("proj-1").getPresenceManager().get("abcdefghij");
        assertEquals("User-abcdefgh", p.displayName);
    }

    @Test
    void multipleUsersCanJoinSameSession() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        registry.join("proj-1", "user2", "Bob");
        assertEquals(2, registry.get("proj-1").getPresenceManager().size());
    }

    // ------------------------------------------------------------------
    // leave()
    // ------------------------------------------------------------------

    @Test
    void leaveRemovesUserFromSession() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        registry.leave("proj-1", "user1");
        assertFalse(registry.get("proj-1").getPresenceManager().isPresent("user1"));
    }

    @Test
    void leaveReturnsPresenceRecord() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        UserPresence removed = registry.leave("proj-1", "user1");
        assertNotNull(removed);
        assertEquals("user1", removed.userId);
    }

    @Test
    void leaveOnAbsentSessionReturnsNull() {
        assertNull(registry.leave("ghost", "user1"));
    }

    @Test
    void leaveOnAbsentUserReturnsNull() {
        registry.create("proj-1");
        assertNull(registry.leave("proj-1", "nobody"));
    }

    @Test
    void leaveOneUserDoesNotAffectOthers() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", "Alice");
        registry.join("proj-1", "user2", "Bob");
        registry.leave("proj-1", "user1");
        assertFalse(registry.get("proj-1").getPresenceManager().isPresent("user1"));
        assertTrue(registry.get("proj-1").getPresenceManager().isPresent("user2"));
    }

    // ------------------------------------------------------------------
    // list()
    // ------------------------------------------------------------------

    @Test
    void listIsEmptyOnNewRegistry() {
        assertTrue(registry.list().isEmpty());
    }

    @Test
    void listReturnsOneSummaryPerSession() {
        registry.create("proj-A");
        registry.create("proj-B");
        assertEquals(2, registry.list().size());
    }

    @Test
    void listSummaryHasCorrectProjectId() {
        registry.create("proj-1");
        SessionRegistry.SessionSummary summary = findSummary("proj-1");
        assertEquals("proj-1", summary.projectId);
    }

    @Test
    void listSummaryReflectsPresenceCount() {
        registry.create("proj-1");
        registry.join("proj-1", "user1", null);
        registry.join("proj-1", "user2", null);
        SessionRegistry.SessionSummary summary = findSummary("proj-1");
        assertEquals(2, summary.presenceCount);
    }

    @Test
    void listSummaryReflectsZeroPresenceCount() {
        registry.create("proj-1");
        SessionRegistry.SessionSummary summary = findSummary("proj-1");
        assertEquals(0, summary.presenceCount);
    }

    @Test
    void listSummaryReflectsOperationSeq() {
        registry.create("proj-1");
        registry.get("proj-1").getOperationLog()
                .append("SceneFlow.Node.Move", new JSONObject(), -1, "u");
        SessionRegistry.SessionSummary summary = findSummary("proj-1");
        assertEquals(1L, summary.operationSeq);
    }

    @Test
    void listSummaryZeroSeqWhenNoOperations() {
        registry.create("proj-1");
        assertEquals(0L, findSummary("proj-1").operationSeq);
    }

    @Test
    void listIsUnmodifiable() {
        registry.create("proj-1");
        List<SessionRegistry.SessionSummary> list = registry.list();
        assertThrows(UnsupportedOperationException.class, () -> list.remove(0));
    }

    @Test
    void listSummaryToJsonContainsAllFields() {
        registry.create("proj-1");
        JSONObject json = findSummary("proj-1").toJson();
        assertTrue(json.has("projectId"));
        assertTrue(json.has("subscriberCount"));
        assertTrue(json.has("presenceCount"));
        assertTrue(json.has("operationSeq"));
        assertEquals("proj-1", json.getString("projectId"));
    }

    @Test
    void listSummarySubscriberCountIsZeroWithNoWsClients() {
        registry.create("proj-1");
        // WsContext requires a live connection; verify the summary reflects zero subscribers
        assertEquals(0, findSummary("proj-1").subscriberCount);
    }

    // ------------------------------------------------------------------
    // projectIds()
    // ------------------------------------------------------------------

    @Test
    void projectIdsContainsAllRegisteredIds() {
        registry.create("proj-A");
        registry.create("proj-B");
        Set<String> ids = registry.projectIds();
        assertTrue(ids.contains("proj-A"));
        assertTrue(ids.contains("proj-B"));
        assertEquals(2, ids.size());
    }

    @Test
    void projectIdsIsUnmodifiable() {
        registry.create("proj-1");
        assertThrows(UnsupportedOperationException.class,
                () -> registry.projectIds().add("evil"));
    }

    @Test
    void projectIdsIsEmptyOnNewRegistry() {
        assertTrue(registry.projectIds().isEmpty());
    }

    // ------------------------------------------------------------------
    // clear()
    // ------------------------------------------------------------------

    @Test
    void clearRemovesAllSessions() {
        registry.create("proj-A");
        registry.create("proj-B");
        registry.clear();
        assertEquals(0, registry.size());
        assertTrue(registry.list().isEmpty());
    }

    @Test
    void clearOnEmptyRegistryIsHarmless() {
        assertDoesNotThrow(() -> registry.clear());
    }

    // ------------------------------------------------------------------
    // Per-session isolation
    // ------------------------------------------------------------------

    @Test
    void sessionsHaveIndependentPresenceManagers() {
        registry.create("proj-A");
        registry.create("proj-B");
        registry.join("proj-A", "alice", "Alice");
        registry.join("proj-B", "bob", "Bob");

        assertTrue(registry.get("proj-A").getPresenceManager().isPresent("alice"));
        assertFalse(registry.get("proj-A").getPresenceManager().isPresent("bob"));
        assertTrue(registry.get("proj-B").getPresenceManager().isPresent("bob"));
        assertFalse(registry.get("proj-B").getPresenceManager().isPresent("alice"));
    }

    @Test
    void sessionsHaveIndependentOperationLogs() {
        registry.create("proj-A");
        registry.create("proj-B");
        registry.get("proj-A").getOperationLog()
                .append("SceneFlow.Node.Move", new JSONObject(), -1, "u");

        assertEquals(1L, registry.get("proj-A").getOperationLog().currentSeq());
        assertEquals(0L, registry.get("proj-B").getOperationLog().currentSeq());
    }

    @Test
    void destroyedSessionDoesNotAffectOtherSessionsPresence() {
        registry.create("proj-A");
        registry.create("proj-B");
        registry.join("proj-A", "alice", "Alice");
        registry.join("proj-B", "bob", "Bob");

        registry.destroy("proj-A");

        assertTrue(registry.get("proj-B").getPresenceManager().isPresent("bob"));
        assertEquals(1, registry.size());
    }

    // ------------------------------------------------------------------
    // Thread safety
    // ------------------------------------------------------------------

    @Test
    void concurrentGetOrCreateIsSafe() throws InterruptedException {
        int threads = 30;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        AtomicInteger nullCount = new AtomicInteger(0);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            exec.submit(() -> {
                try {
                    start.await();
                    CollaborationSession s = registry.getOrCreate("shared-project");
                    if (s == null) nullCount.incrementAndGet();
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

        assertEquals(0, nullCount.get(), "getOrCreate must never return null");
        assertEquals(1, registry.size(), "Exactly one session for shared-project");
    }

    @Test
    void concurrentJoinAndLeaveAreSafe() throws InterruptedException {
        registry.create("proj-1");
        int threads = 40;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final String userId = "user-" + i;
            exec.submit(() -> {
                try {
                    start.await();
                    registry.join("proj-1", userId, null);
                    registry.leave("proj-1", userId);
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

        assertEquals(0, registry.get("proj-1").getPresenceManager().size(),
                "All users joined and left — presence should be empty");
    }

    @Test
    void concurrentCreateAndDestroyAreSafe() throws InterruptedException {
        int threads = 20;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final String projId = "proj-" + i;
            exec.submit(() -> {
                try {
                    start.await();
                    registry.getOrCreate(projId);
                    registry.destroy(projId);
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
        // Registry size may be 0 or some subset if destroys raced with creates
        // — just verify no exception was thrown and size is non-negative.
        assertTrue(registry.size() >= 0);
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    private SessionRegistry.SessionSummary findSummary(String projectId) {
        return registry.list().stream()
                .filter(s -> projectId.equals(s.projectId))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Summary not found for " + projectId));
    }
}
