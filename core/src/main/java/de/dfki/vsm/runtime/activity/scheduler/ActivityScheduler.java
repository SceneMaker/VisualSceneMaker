package de.dfki.vsm.runtime.activity.scheduler;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityScheduler<T extends AbstractActivity> {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The list of detected marks
    private final HashMap<String, ActivityWorker<T>> mWorkerMap = new HashMap<>();

    // Batched marker -> "run every accumulated action" closures — see registerBatch()'s docs.
    private final HashMap<String, Runnable> mBatchMap = new HashMap<>();

    // One single-threaded queue per target ActivityExecutor, lazily created — see handle()'s docs
    // for why marker-triggered activities are dispatched here instead of via ActivityWorker.start().
    private final Map<ActivityExecutor, ExecutorService> mExecutorQueues = new ConcurrentHashMap<>();

    private ExecutorService queueFor(final ActivityExecutor executor) {
        return mExecutorQueues.computeIfAbsent(executor, e -> Executors.newSingleThreadExecutor(r -> {
            final Thread t = new Thread(r, "activity-queue-" + e);
            t.setDaemon(true);
            return t;
        }));
    }

    // Handle activity feedback
    public final void handle(final String marker) {
        // Get the activity
        //final AbstractActivity activity = object.getActivity();
        // Check the feedback
        //if (object instanceof StatusFeedback) {
        //final StatusFeedback feedback = (StatusFeedback) object;
        // Get the status name
        //final Status status = feedback.getStatus();
        // TODO:
        // Notify the waiting thread that its feedback is there
        //} else if (object instanceof MarkerFeedback) {
        //final MarkerFeedback feedback = (MarkerFeedback) object;
        //final String marker = feedback.getMarker();
        final Runnable batch;
        final ActivityWorker<T> worker;
        synchronized (mWorkerMap) {
            batch = mBatchMap.remove(marker);
            worker = (batch != null) ? null : mWorkerMap.remove(marker);
        }
        if (batch != null) {
            // Already dispatches each accumulated action via queueFor() itself (see
            // registerBatch()) — nothing further to do on this thread.
            batch.run();
        } else if (worker != null) {
            // Dispatched onto a single-threaded queue keyed by the TARGET executor, running
            // worker.run() directly (not .start()) so no new ad-hoc Thread races another marker's.
            // Two reasons this shape specifically, both confirmed 2026-07-23:
            //   1. Ordering: markers embedded in the same utterance (e.g. a Timer plugin's
            //      [init]...[time] pair either side of a pause) are echoed back by the engine in
            //      strict text order, but a bare worker.start() per marker is fire-and-forget — a
            //      later marker's thread can run before an earlier one's has finished, silently
            //      reordering script-order-dependent actions ("time" read its map before "init"
            //      had written to it, two markers echoed only 1ms apart). Serializing per-executor
            //      fixes this without over-serializing unrelated executors against each other.
            //   2. No blocking here: an earlier attempt made handle() itself join() the worker
            //      before returning, which deadlocked — a same-actor marker action (e.g.
            //      "background", targeting the speaking character's OWN executor) can need that
            //      executor's dispatch lock, which the character's ongoing SpeechActivity is
            //      already holding while it blocks waiting for the engine's speech-stop feedback.
            //      Blocking the WebSocket message thread inside handle() blocked the very thread
            //      needed to ever deliver that stop feedback. Submitting to a queue and returning
            //      immediately keeps this thread free regardless of what the queued activity does.
            queueFor(worker.getExecutor()).submit(worker::run);
        } else {
            mLogger.warning("ActivityScheduler: no worker registered for marker '" + marker + "'");
        }
        //}
    }


    //Check if there is an marker / activity.
    public boolean hasMarker(String marker) {
        synchronized (mWorkerMap) {
            return mWorkerMap.containsKey(marker) || mBatchMap.containsKey(marker);
        }
    }

    // Schedule an activity on an executor with a timeout
    public final void schedule(
            final long timeout,
            final List<ActivityWorker<T>> list,
            final T activity,
            final ActivityExecutor executor) {
        // Create a new activity task
        final ActivityWorker<T> task = new ActivityWorker<T>(
                timeout, list, activity, executor);
        // Start the activity task
        task.start();
        // Check if we need to wait
        if (activity.getType() == Type.blocking) {
            // Wait for termination
            boolean finished = false;
            while (!finished) {
                try {
                    // Join the job worker
                    task.join();
                    // Finish this execution
                    // after an interruption
                    finished = true;
                } catch (final InterruptedException exc) {
                    // Terminate job worker
                    task.abort();
                }
            }
        }
    }

    // Register an activity on an executor with a marker
    public final ActivityWorker<T> register(
            final String marker,
            final T activity,
            final ActivityExecutor executor) {
        // Create a new activity task
        final ActivityWorker<T> task = new ActivityWorker<>(
                -1, null, activity, executor);
        // Add the task to the mapping
        synchronized (mWorkerMap) {
            mWorkerMap.put(marker, task);
        }
        // Return the task for joining
        return task;
    }

    /**
     * Registers a marker that, when it fires, dispatches every accumulated (activity, executor)
     * pair in {@code batch}, each still routed through {@link #queueFor} (so per-executor ordering
     * and failure isolation match a plain {@link #register}'d single action exactly).
     *
     * <p>Needed because the character engine's onMarker only ever echoes back the LAST of several
     * bare marker tokens sitting adjacent with no real word in between (e.g. two {@code background}
     * commands from different actors at the very start of a turn, before any speech) — silently
     * dropping every earlier one. Bundling the whole run under one marker guarantees the engine
     * only ever sees a single token at that position. Mirrors {@code
     * CharamelEmbedExecutor.previewTurn()}'s identical fix (applied there 2026-07-21) — this is the
     * real-playback ({@code ReactivePlayer}) counterpart, confirmed missing and reproduced
     * 2026-07-23 with a turn opening on two adjacent background commands for different actors.
     */
    public final void registerBatch(final String marker, final List<Map.Entry<T, ActivityExecutor>> batch) {
        final Runnable runnable = () -> {
            for (final Map.Entry<T, ActivityExecutor> entry : batch) {
                final T activity = entry.getKey();
                final ActivityExecutor executor = entry.getValue();
                queueFor(executor).submit(() -> {
                    try {
                        executor.execute(activity);
                    } catch (Exception exc) {
                        mLogger.failure("Activity '" + activity + "' failed on executor '"
                                + executor + "': " + exc.getMessage());
                    }
                });
            }
        };
        synchronized (mWorkerMap) {
            mBatchMap.put(marker, runnable);
        }
    }
}
