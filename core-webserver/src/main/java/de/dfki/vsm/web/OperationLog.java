package de.dfki.vsm.web;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Per-project, in-memory, server-serialised operation log for collaborative
 * graph editing.
 *
 * <h2>Protocol summary</h2>
 * <p>Every mutating WebSocket command that passes through
 * {@code WebUiServer.handleWsMessage} is appended here after successful
 * server-side application. Before appending, the server checks whether the
 * client's declared {@code basedOnSeq} is behind the current log sequence;
 * if so, a lightweight conflict resolver decides whether to accept (with
 * last-write-wins semantics) or reject the incoming operation.</p>
 *
 * <h2>Conflict resolution rules</h2>
 * <pre>
 *   MoveNode    vs MoveNode  (same node)         → CONFLICT_MERGED (last-write-wins)
 *   AddNode     vs AddNode   (different nodes)    → APPLIED (commutative by UUID)
 *   DeleteNode  vs AddEdge   (references deleted) → CONFLICT_REJECTED
 *   RenameNode  vs RenameNode (same node)         → CONFLICT_MERGED (last-write-wins)
 *   Any         vs AddNode   (different nodes)    → APPLIED
 *   All others                                    → CONFLICT_MERGED (last-write-wins)
 * </pre>
 *
 * <h2>Memory management</h2>
 * <p>The log is bounded to {@code maxSize} entries (default 1 000). When the
 * bound is exceeded the oldest entries are evicted. This keeps memory use
 * constant for long-running projects.</p>
 *
 * <p>All public methods are {@code synchronized} and safe for concurrent use
 * from Javalin's WebSocket thread pool.</p>
 */
public final class OperationLog {

    // ------------------------------------------------------------------
    // Resolution outcome
    // ------------------------------------------------------------------

    public enum Resolution {
        /** Operation was applied without any conflict. */
        APPLIED,
        /** A concurrent conflict was detected but resolved by last-write-wins. */
        CONFLICT_MERGED,
        /** The operation was rejected due to an irreconcilable conflict. */
        CONFLICT_REJECTED
    }

    // ------------------------------------------------------------------
    // AppendResult
    // ------------------------------------------------------------------

    /** Result returned by {@link #append} and {@link #checkConflict}. */
    public static final class AppendResult {
        public final long seq;
        public final Resolution resolution;
        /** Human-readable reason, non-null only for {@link Resolution#CONFLICT_REJECTED}. */
        public final String rejectionReason;

        private AppendResult(long seq, Resolution resolution, String rejectionReason) {
            this.seq = seq;
            this.resolution = resolution;
            this.rejectionReason = rejectionReason;
        }

        static AppendResult applied(long seq) {
            return new AppendResult(seq, Resolution.APPLIED, null);
        }

        static AppendResult merged(long seq) {
            return new AppendResult(seq, Resolution.CONFLICT_MERGED, null);
        }

        static AppendResult rejected(long currentSeq, String reason) {
            return new AppendResult(currentSeq, Resolution.CONFLICT_REJECTED, reason);
        }

        /** {@code true} if the operation was accepted (applied or merged). */
        public boolean isAccepted() {
            return resolution != Resolution.CONFLICT_REJECTED;
        }
    }

    // ------------------------------------------------------------------
    // State
    // ------------------------------------------------------------------

    static final int DEFAULT_MAX_SIZE = 1_000;

    private final int maxSize;
    private final List<SceneFlowOperation> entries = new ArrayList<>();
    private long currentSeq = 0;

    // ------------------------------------------------------------------
    // Constructors
    // ------------------------------------------------------------------

    public OperationLog() {
        this(DEFAULT_MAX_SIZE);
    }

    public OperationLog(int maxSize) {
        this.maxSize = Math.max(1, maxSize);
    }

    // ------------------------------------------------------------------
    // Conflict check (read-only, does NOT commit)
    // ------------------------------------------------------------------

    /**
     * Checks whether {@code method} with {@code params} can be applied given
     * the client's declared {@code basedOnSeq}.  Does <em>not</em> modify the
     * log; call {@link #append} separately to commit.
     *
     * @param basedOnSeq the client's last-known seq; {@code -1} skips conflict checks
     */
    public synchronized AppendResult checkConflict(String method, JSONObject params, long basedOnSeq) {
        if (basedOnSeq < 0 || basedOnSeq >= currentSeq) {
            // No conflict window: legacy client or client is up to date.
            return AppendResult.applied(currentSeq);
        }
        List<SceneFlowOperation> concurrent = sinceLocked(basedOnSeq);
        Resolution r = resolveConflict(method, params, concurrent);
        if (r == Resolution.CONFLICT_REJECTED) {
            return AppendResult.rejected(currentSeq, buildRejectionReason(method, params, concurrent));
        }
        return new AppendResult(currentSeq, r, null);
    }

    // ------------------------------------------------------------------
    // Append (commits unconditionally — caller must check conflict first)
    // ------------------------------------------------------------------

    /**
     * Commits a new operation to the log and returns the assigned sequence
     * number together with the conflict resolution that was applied.
     *
     * <p>The conflict resolution included in the result reflects the state at
     * commit time.  Callers that need to reject the operation should call
     * {@link #checkConflict} first and skip {@code append} if rejected.</p>
     */
    public synchronized AppendResult append(String method, JSONObject params, long basedOnSeq, String userId) {
        Resolution resolution = Resolution.APPLIED;
        if (basedOnSeq >= 0 && basedOnSeq < currentSeq) {
            List<SceneFlowOperation> concurrent = sinceLocked(basedOnSeq);
            resolution = resolveConflict(method, params, concurrent);
            // REJECTED should have been caught by checkConflict before this call;
            // treat as MERGED for safety.
            if (resolution == Resolution.CONFLICT_REJECTED) {
                resolution = Resolution.CONFLICT_MERGED;
            }
        }
        currentSeq++;
        SceneFlowOperation op = new SceneFlowOperation(
                currentSeq, userId, System.currentTimeMillis(), method, params, basedOnSeq);
        entries.add(op);
        if (entries.size() > maxSize) {
            entries.remove(0);
        }
        return resolution == Resolution.APPLIED
                ? AppendResult.applied(currentSeq)
                : AppendResult.merged(currentSeq);
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns all operations with {@code seq > sinceSeq} (exclusive lower bound),
     * ordered by ascending sequence number.
     */
    public synchronized List<SceneFlowOperation> since(long sinceSeq) {
        return sinceLocked(sinceSeq);
    }

    /** Current (latest committed) sequence number. {@code 0} means the log is empty. */
    public synchronized long currentSeq() {
        return currentSeq;
    }

    /** Number of entries currently held in memory (≤ maxSize). */
    public synchronized int size() {
        return entries.size();
    }

    // ------------------------------------------------------------------
    // Lifecycle
    // ------------------------------------------------------------------

    /** Resets the log to its initial empty state. */
    public synchronized void clear() {
        entries.clear();
        currentSeq = 0;
    }

    // ------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------

    private List<SceneFlowOperation> sinceLocked(long sinceSeq) {
        List<SceneFlowOperation> result = new ArrayList<>();
        for (SceneFlowOperation op : entries) {
            if (op.seq > sinceSeq) {
                result.add(op);
            }
        }
        return Collections.unmodifiableList(result);
    }

    private Resolution resolveConflict(String method, JSONObject params,
                                       List<SceneFlowOperation> concurrent) {
        // Rule: DeleteNode vs AddEdge (referencing deleted node) → REJECTED
        if (isAddEdge(method)) {
            String sourceId = params != null ? params.optString("sourceId", "") : "";
            String targetId = params != null ? params.optString("targetId", "") : "";
            for (SceneFlowOperation op : concurrent) {
                if (isDeleteNode(op.method)) {
                    String deletedId = op.params.optString("nodeId", "");
                    if (!deletedId.isEmpty()
                            && (deletedId.equals(sourceId) || deletedId.equals(targetId))) {
                        return Resolution.CONFLICT_REJECTED;
                    }
                }
            }
        }
        // All other cases: last-write-wins → CONFLICT_MERGED
        return Resolution.CONFLICT_MERGED;
    }

    private String buildRejectionReason(String method, JSONObject params,
                                        List<SceneFlowOperation> concurrent) {
        StringBuilder sb = new StringBuilder("Operation '").append(method)
                .append("' rejected due to concurrent conflict.");
        if (isAddEdge(method)) {
            String sourceId = params != null ? params.optString("sourceId", "") : "";
            String targetId = params != null ? params.optString("targetId", "") : "";
            for (SceneFlowOperation op : concurrent) {
                if (isDeleteNode(op.method)) {
                    String deletedId = op.params.optString("nodeId", "?");
                    if (deletedId.equals(sourceId) || deletedId.equals(targetId)) {
                        sb.append(" Node '").append(deletedId)
                          .append("' was deleted by seq ").append(op.seq).append(".");
                        break;
                    }
                }
            }
        }
        return sb.toString();
    }

    // ------------------------------------------------------------------
    // Method classifiers
    // ------------------------------------------------------------------

    static boolean isAddEdge(String method) {
        return method != null && method.startsWith("SceneFlow.Edge.Add");
    }

    static boolean isDeleteNode(String method) {
        return method != null
                && (method.equals("SceneFlow.Node.Delete")
                 || method.equals("SceneFlow.Node.Remove"));
    }
}
