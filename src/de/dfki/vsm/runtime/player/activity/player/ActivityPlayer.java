package de.dfki.vsm.runtime.player.activity.player;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.ActionActivity;
import static de.dfki.vsm.runtime.player.activity.player.ActivityPlayer.SchedulingPolicy.BLOCKING;
import de.dfki.vsm.runtime.player.executor.ActivityExecutor;
import de.dfki.vsm.runtime.player.executor.feedback.ExecutorFeedback;
import de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback;
import de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback.ExecutionStatus;
import de.dfki.vsm.runtime.player.executor.feedback.MarkerFeedback;
import de.dfki.vsm.runtime.player.activity.trigger.MarkerTrigger;
import de.dfki.vsm.runtime.player.activity.trigger.TimeoutTrigger;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityPlayer {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // Scheduling policy
    public enum SchedulingPolicy {

        BLOCKING,
        PARALLEL
    }
    // The list of detected marks
    private final HashMap<String, Thread> mActivityMap
            = new HashMap<String, Thread>();

    public final void feedback(final ExecutorFeedback object) {
        //
        if (object instanceof StatusFeedback) {
            final StatusFeedback feedback = (StatusFeedback) object;
            final AbstractActivity activity = feedback.getActivity();
            final ExecutionStatus status = feedback.getStatus();
            // Print some information
            mLogger.message("Status feedback '" + feedback + "'"
                    + " for activity '" + activity + "'"
                    + " with status '" + status + "'");
        }
        //
        if (object instanceof MarkerFeedback) {
            final MarkerFeedback feedback = (MarkerFeedback) object;
            final AbstractActivity activity = feedback.getActivity();
            final MarkerTrigger trigger = feedback.getTrigger();
            final String marker = ((MarkerTrigger) trigger).toString();
            // Print some information
            mLogger.message("Marker feedback '" + feedback + "'"
                    + " in activity '" + activity + "'"
                    + " with marker '" + marker + "'");
            synchronized (mActivityMap) {
                // Get the marker to queue
                mActivityMap.get(trigger.getMarker()).start();
            }
        }
    }

    // Schedule an activity
    public final void schedule(
            final TimeoutTrigger trigger,
            final SchedulingPolicy policy,
            final AbstractActivity activity,
            final ActivityExecutor executor) {
        // Print some information
        mLogger.message("Scheduling '" + activity + "'"
                + " with policy '" + policy + "'"
                + " and trigger '" + trigger + "'"
                + " on executor '" + executor + "'");
        // Create a new activity task
        final Task task = new Task(trigger, activity, executor, this);
        // Start the activity task
        task.start();
        // Check if we need to wait
        if (policy == BLOCKING) {
            // Wait for termination
            boolean finished = false;
            while (!finished) {
                try {
                    // Print some information
                    mLogger.warning("Waiting for '" + task + "'");
                    // Join the job worker
                    task.join();
                    // Finish this execution
                    // after an interruption
                    finished = true;
                } catch (final InterruptedException exc) {
                    // Print some information
                    mLogger.warning("Interrupting '" + task + "'");
                    // Terminate job worker
                    task.abort();
                }
            }
            // Print some information
            mLogger.warning("Continuing '" + Thread.currentThread() + "'");
        }
    }

    // Register an activity on an executor with a certain trigger 
    public final void register(
            final MarkerTrigger trigger,
            final SchedulingPolicy policy,
            final ActionActivity activity,
            final ActivityExecutor executor) {
        final ActivityPlayer player = this;
        synchronized (mActivityMap) {
            mActivityMap.put(trigger.getMarker(), new Thread(new Runnable() {

                @Override
                public void run() {
                    // TODO:
                    executor.execute(activity, player);
                }
            }));
        }
    }

    // The activity task
    private final class Task extends Thread {

        // The termination flag
        private boolean mDone;
        // The execution delay
        private final TimeoutTrigger mTrigger;
        private final AbstractActivity mActivity;
        private final ActivityExecutor mExecutor;
        private final ActivityPlayer mPlayer;

        // Abort the execution
        public final void abort() {
            // Set termination flag
            mDone = true;
            // Interrupt the thread
            interrupt();
        }

        // Check execution status
        public final boolean isDone() {
            return mDone;
        }

        // Construct with a name
        private Task(
                final TimeoutTrigger trigger,
                final AbstractActivity activity,
                final ActivityExecutor executor,
                final ActivityPlayer player) {
            super(activity.toString());
            // Initialize the flag
            mDone = false;
            // Initialize the data
            mTrigger = trigger;
            mActivity = activity;
            mExecutor = executor;
            mPlayer = player;
        }

        @Override
        public final void run() {
            //
            //if (mTrigger instanceof TimeoutTrigger) {
            //    final TimeoutTrigger trigger = (TimeoutTrigger) mTrigger;
            try {
                // Wait for the delay
                Thread.sleep(mTrigger.getTimeout());
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning(exc.toString());
                // Return when aborted
                if (mDone) {
                    return;
                }
            }
            //}
            //
            /*
             if (mTrigger instanceof MarkerTrigger) {
             final MarkerTrigger trigger = (MarkerTrigger) mTrigger;
             synchronized (mMarks) {
             while (!mMarks.contains(trigger)) {
             try {
             // Wait until notification
             mMarks.wait();
             } catch (InterruptedException exc) {
             // Print some information
             mLogger.warning(exc.toString());
             // Return when aborted
             if (mDone) {
             return;
             }
             }
             }
             }
             }
             */
            // Execute the activity
            mExecutor.execute(mActivity, mPlayer);
        }
    }
}
