package de.dfki.vsm.runtime.activity.manager;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Policy;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.feedback.ActivityFeedback;
import de.dfki.vsm.runtime.activity.feedback.StatusFeedback;
import de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status;
import de.dfki.vsm.runtime.activity.feedback.MarkerFeedback;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.List;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityManager {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The list of detected marks
    private final HashMap<String, ActivityWorker> mWorkerMap = new HashMap();

    // Handle activity feedback
    public final void handle(final ActivityFeedback object) {
        // Get the activity 
        final AbstractActivity activity = object.getActivity();
        // Check the feedback
        if (object instanceof StatusFeedback) {
            final StatusFeedback feedback = (StatusFeedback) object;
            // Get the status name 
            final Status status = feedback.getStatus();
            // Print some information
            mLogger.message("Status of activity '" + feedback + "' is '" + status + "'");
            // TODO: 
            // Notify the waiting thread that its feedback is there
        } else if (object instanceof MarkerFeedback) {
            final MarkerFeedback feedback = (MarkerFeedback) object;
            final String marker = feedback.getMarker();
            // Print some information
            mLogger.message("Marker '" + marker + "' detected during '" + activity + "'");
            // Start the assigned task
            synchronized (mWorkerMap) {
                mWorkerMap.remove(marker).start();
            }
        }
    }

    // Schedule an activity on an executor with a timeout
    public final void schedule(
            final long timeout, // after a timeout
            final List<ActivityWorker> list,
            final AbstractActivity activity,
            final ActivityExecutor executor) {
        // Print some information
        mLogger.message("Scheduling '" + activity + "'"
                + " with timeout '" + timeout + "'"
                + " on executor '" + executor + "'");
        // Create a new activity task
        final ActivityWorker task = new ActivityWorker(
                timeout, list, activity, executor, this);
        // Start the activity task
        task.start();
        // Check if we need to wait
        if (activity.getType() == Policy.BLOCKING) {
            // Print some information
            mLogger.warning("Blocking calling thread'" + Thread.currentThread() + "'");
            // Wait for termination
            boolean finished = false;
            while (!finished) {
                try {
                    // Print some information
                    mLogger.warning("Awaiting activity worker '" + task + "'");
                    // Join the job worker
                    task.join();
                    // Finish this execution
                    // after an interruption
                    finished = true;
                    // Print some information
                    mLogger.warning("Joining activity worker '" + task + "'");
                } catch (final InterruptedException exc) {
                    // Print some information
                    mLogger.warning("Aborting activity worker '" + task + "'");
                    // Terminate job worker
                    task.abort();
                }
            }
            // Print some information
            mLogger.warning("Continuing calling thread'" + Thread.currentThread() + "'");
        }
    }

    // Register an activity on an executor with a marker
    public final ActivityWorker register(
            final String marker, // at some marker
            final ActionActivity activity,
            final ActivityExecutor executor) {
        // Print some information
        mLogger.message("Registering '" + activity + "'"
                + " with marker '" + marker + "'"
                + " on executor '" + executor + "'");
        // Create a new activity task
        final ActivityWorker task = new ActivityWorker(
                -1, null, activity, executor, this);
        // Add the task to the mapping
        synchronized (mWorkerMap) {
            mWorkerMap.put(marker, task);
        }
        // Return the task for joining
        return task;
    }

}
