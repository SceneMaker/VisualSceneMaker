package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Functional tests for {@link OperationLog} and {@link SceneFlowOperation}.
 */
class OperationLogTest {

    private OperationLog log;

    @BeforeEach
    void setUp() {
        log = new OperationLog();
    }

    // ------------------------------------------------------------------
    // Initial state
    // ------------------------------------------------------------------

    @Test
    void newLogHasZeroSeq() {
        assertEquals(0L, log.currentSeq());
    }

    @Test
    void newLogHasZeroSize() {
        assertEquals(0, log.size());
    }

    @Test
    void sinceOnEmptyLogReturnsEmpty() {
        assertTrue(log.since(-1).isEmpty());
        assertTrue(log.since(0).isEmpty());
    }

    // ------------------------------------------------------------------
    // Append — clean apply (no conflict)
    // ------------------------------------------------------------------

    @Test
    void appendIncrementsSeq() {
        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "user1");
        assertEquals(1L, log.currentSeq());

        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "user1");
        assertEquals(2L, log.currentSeq());
    }

    @Test
    void appendWithBasedOnSeqEqualToCurrentIsApplied() {
        // First op establishes seq=1
        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "u1");

        // Client is up to date: basedOnSeq == currentSeq before this call
        OperationLog.AppendResult r = log.append("SceneFlow.Node.Move", params("nodeId", "n1"), 1L, "u1");
        assertEquals(OperationLog.Resolution.APPLIED, r.resolution);
        assertEquals(2L, r.seq);
    }

    @Test
    void appendReturnsCorrectSeq() {
        OperationLog.AppendResult r1 = log.append("SceneFlow.Node.Move", params(), -1, "u");
        OperationLog.AppendResult r2 = log.append("SceneFlow.Node.Move", params(), -1, "u");
        assertEquals(1L, r1.seq);
        assertEquals(2L, r2.seq);
    }

    @Test
    void appendedOperationStoredCorrectly() {
        JSONObject p = params("nodeId", "n42");
        log.append("SceneFlow.Node.Move", p, -1, "alice");

        List<SceneFlowOperation> ops = log.since(0);
        assertEquals(1, ops.size());
        SceneFlowOperation op = ops.get(0);
        assertEquals(1L, op.seq);
        assertEquals("alice", op.userId);
        assertEquals("SceneFlow.Node.Move", op.method);
        assertEquals("n42", op.params.optString("nodeId"));
        assertEquals(-1L, op.basedOnSeq);
    }

    // ------------------------------------------------------------------
    // since() — catch-up query
    // ------------------------------------------------------------------

    @Test
    void sinceReturnsOnlyOperationsAfterGivenSeq() {
        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "u");   // seq=1
        log.append("SceneFlow.Node.Move", params("nodeId", "n2"), -1, "u");   // seq=2
        log.append("SceneFlow.Node.Move", params("nodeId", "n3"), -1, "u");   // seq=3

        List<SceneFlowOperation> after1 = log.since(1);
        assertEquals(2, after1.size());
        assertEquals(2L, after1.get(0).seq);
        assertEquals(3L, after1.get(1).seq);
    }

    @Test
    void sinceMinusOneReturnsAll() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");  // seq=1
        log.append("SceneFlow.Node.Move", params(), -1, "u");  // seq=2
        assertEquals(2, log.since(-1).size());
    }

    @Test
    void sinceCurrentSeqReturnsEmpty() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");  // seq=1
        assertTrue(log.since(1).isEmpty());
    }

    @Test
    void sinceListIsUnmodifiable() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");
        List<SceneFlowOperation> ops = log.since(-1);
        assertThrows(UnsupportedOperationException.class, () -> ops.remove(0));
    }

    // ------------------------------------------------------------------
    // Conflict check — clean (no concurrent ops)
    // ------------------------------------------------------------------

    @Test
    void checkConflictWithLegacyClientIsAccepted() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");  // seq=1

        OperationLog.AppendResult r = log.checkConflict("SceneFlow.Node.Move", params(), -1L);
        assertTrue(r.isAccepted());
        assertEquals(OperationLog.Resolution.APPLIED, r.resolution);
    }

    @Test
    void checkConflictWhenUpToDateIsApplied() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");  // seq=1

        OperationLog.AppendResult r = log.checkConflict("SceneFlow.Node.Move", params(), 1L);
        assertTrue(r.isAccepted());
        assertEquals(OperationLog.Resolution.APPLIED, r.resolution);
    }

    // ------------------------------------------------------------------
    // Conflict resolution — last-write-wins
    // ------------------------------------------------------------------

    @Test
    void moveVsMoveLastWriteWinsIsMerged() {
        // seq=1: user A moves n1
        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "userA");

        // user B, based on seq=0 (hasn't seen seq=1), also moves n1
        OperationLog.AppendResult r = log.append("SceneFlow.Node.Move", params("nodeId", "n1"), 0L, "userB");
        assertTrue(r.isAccepted());
        assertEquals(OperationLog.Resolution.CONFLICT_MERGED, r.resolution);
        assertEquals(2L, r.seq);
    }

    @Test
    void renameVsRenameIsMerged() {
        log.append("SceneFlow.Node.Rename", params("nodeId", "n1", "name", "OldName"), -1, "userA");

        OperationLog.AppendResult r = log.append("SceneFlow.Node.Rename",
                params("nodeId", "n1", "name", "NewName"), 0L, "userB");
        assertTrue(r.isAccepted());
        assertEquals(OperationLog.Resolution.CONFLICT_MERGED, r.resolution);
    }

    @Test
    void addNodeVsAddNodeIsAppliedAsCommutative() {
        log.append("SceneFlow.Node.Add", params("nodeId", "n1"), -1, "userA");

        // Different node, so commutative
        OperationLog.AppendResult r = log.append("SceneFlow.Node.Add", params("nodeId", "n2"), 0L, "userB");
        assertTrue(r.isAccepted());
    }

    // ------------------------------------------------------------------
    // Conflict resolution — rejection
    // ------------------------------------------------------------------

    @Test
    void addEdgeAfterDeleteNodeIsRejected() {
        // seq=1: server deleted node n1
        log.append("SceneFlow.Node.Delete", params("nodeId", "n1"), -1, "userA");

        // user B, based on seq=0, tries to add an edge from n1 (now deleted)
        JSONObject edgeParams = params("sourceId", "n1", "targetId", "n2");
        OperationLog.AppendResult check = log.checkConflict("SceneFlow.Edge.Add", edgeParams, 0L);
        assertFalse(check.isAccepted());
        assertEquals(OperationLog.Resolution.CONFLICT_REJECTED, check.resolution);
        assertNotNull(check.rejectionReason);
        assertTrue(check.rejectionReason.contains("n1"));
    }

    @Test
    void addEdgeAfterDeleteNodeTargetIsRejected() {
        // node n2 (the target) was deleted
        log.append("SceneFlow.Node.Delete", params("nodeId", "n2"), -1, "userA");

        JSONObject edgeParams = params("sourceId", "n1", "targetId", "n2");
        OperationLog.AppendResult check = log.checkConflict("SceneFlow.Edge.Add", edgeParams, 0L);
        assertFalse(check.isAccepted());
        assertEquals(OperationLog.Resolution.CONFLICT_REJECTED, check.resolution);
    }

    @Test
    void addEdgeAfterDeleteOfUnrelatedNodeIsAccepted() {
        // n99 deleted — unrelated to the edge being added
        log.append("SceneFlow.Node.Delete", params("nodeId", "n99"), -1, "userA");

        JSONObject edgeParams = params("sourceId", "n1", "targetId", "n2");
        OperationLog.AppendResult check = log.checkConflict("SceneFlow.Edge.Add", edgeParams, 0L);
        assertTrue(check.isAccepted());
    }

    @Test
    void rejectedOperationIsNotCommitted() {
        log.append("SceneFlow.Node.Delete", params("nodeId", "n1"), -1, "userA");
        long seqBefore = log.currentSeq();

        // checkConflict does not commit
        log.checkConflict("SceneFlow.Edge.Add", params("sourceId", "n1", "targetId", "n2"), 0L);
        assertEquals(seqBefore, log.currentSeq());
        assertEquals(1, log.size());
    }

    @Test
    void noConflictWhenBasedOnSeqBeforeAnyDeleteOfUnrelatedNode() {
        // Multiple unrelated ops between basedOnSeq=0 and current
        log.append("SceneFlow.Node.Move", params("nodeId", "n5"), -1, "userA");
        log.append("SceneFlow.Node.Move", params("nodeId", "n6"), -1, "userA");

        JSONObject edgeParams = params("sourceId", "n1", "targetId", "n2");
        OperationLog.AppendResult check = log.checkConflict("SceneFlow.Edge.Add", edgeParams, 0L);
        assertTrue(check.isAccepted());
    }

    // ------------------------------------------------------------------
    // Method classifiers
    // ------------------------------------------------------------------

    @Test
    void isAddEdgeMatchesVariants() {
        assertTrue(OperationLog.isAddEdge("SceneFlow.Edge.Add"));
        assertTrue(OperationLog.isAddEdge("SceneFlow.Edge.AddEpsilon"));
        assertFalse(OperationLog.isAddEdge("SceneFlow.Node.Add"));
        assertFalse(OperationLog.isAddEdge(null));
    }

    @Test
    void isDeleteNodeMatchesExactNames() {
        assertTrue(OperationLog.isDeleteNode("SceneFlow.Node.Delete"));
        assertTrue(OperationLog.isDeleteNode("SceneFlow.Node.Remove"));
        assertFalse(OperationLog.isDeleteNode("SceneFlow.Node.DeleteEdge"));
        assertFalse(OperationLog.isDeleteNode(null));
    }

    // ------------------------------------------------------------------
    // Bounded log eviction
    // ------------------------------------------------------------------

    @Test
    void oldestEntryEvictedWhenMaxSizeExceeded() {
        OperationLog small = new OperationLog(3);
        small.append("SceneFlow.Node.Move", params("nodeId", "n1"), -1, "u");  // seq=1
        small.append("SceneFlow.Node.Move", params("nodeId", "n2"), -1, "u");  // seq=2
        small.append("SceneFlow.Node.Move", params("nodeId", "n3"), -1, "u");  // seq=3
        small.append("SceneFlow.Node.Move", params("nodeId", "n4"), -1, "u");  // seq=4 → evicts seq=1

        assertEquals(3, small.size());
        assertEquals(4L, small.currentSeq());

        // seq=1 was evicted; since(-1) starts at seq=2
        List<SceneFlowOperation> ops = small.since(-1);
        assertEquals(3, ops.size());
        assertEquals(2L, ops.get(0).seq);
    }

    // ------------------------------------------------------------------
    // clear()
    // ------------------------------------------------------------------

    @Test
    void clearResetsSeqAndEntries() {
        log.append("SceneFlow.Node.Move", params(), -1, "u");
        log.append("SceneFlow.Node.Move", params(), -1, "u");

        log.clear();
        assertEquals(0L, log.currentSeq());
        assertEquals(0, log.size());
        assertTrue(log.since(-1).isEmpty());
    }

    // ------------------------------------------------------------------
    // SceneFlowOperation.toJson()
    // ------------------------------------------------------------------

    @Test
    void operationToJsonContainsAllFields() {
        log.append("SceneFlow.Node.Move", params("nodeId", "n1"), 3L, "bob");
        SceneFlowOperation op = log.since(-1).get(0);
        JSONObject json = op.toJson();

        assertEquals(1L, json.getLong("seq"));
        assertEquals("bob", json.getString("userId"));
        assertEquals("SceneFlow.Node.Move", json.getString("method"));
        assertEquals(3L, json.getLong("basedOnSeq"));
        assertTrue(json.has("ts"));
        assertTrue(json.has("params"));
    }

    // ------------------------------------------------------------------
    // Thread safety
    // ------------------------------------------------------------------

    @Test
    void concurrentAppendsProduceUniqueMonotonicSeqs() throws InterruptedException {
        int threadCount = 20;
        int appendsPerThread = 50;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threadCount);
        AtomicLong maxSeq = new AtomicLong(0);

        ExecutorService exec = Executors.newFixedThreadPool(threadCount);
        for (int i = 0; i < threadCount; i++) {
            exec.submit(() -> {
                try {
                    start.await();
                    for (int j = 0; j < appendsPerThread; j++) {
                        OperationLog.AppendResult r =
                                log.append("SceneFlow.Node.Move", params(), -1, "t");
                        maxSeq.updateAndGet(cur -> Math.max(cur, r.seq));
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(10, TimeUnit.SECONDS));
        exec.shutdown();

        long expected = (long) threadCount * appendsPerThread;
        assertEquals(expected, log.currentSeq());
        assertEquals(expected, maxSeq.get());
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    private static JSONObject params() {
        return new JSONObject();
    }

    private static JSONObject params(String k1, String v1) {
        JSONObject p = new JSONObject();
        p.put(k1, v1);
        return p;
    }

    private static JSONObject params(String k1, String v1, String k2, String v2) {
        JSONObject p = new JSONObject();
        p.put(k1, v1);
        p.put(k2, v2);
        return p;
    }
}
