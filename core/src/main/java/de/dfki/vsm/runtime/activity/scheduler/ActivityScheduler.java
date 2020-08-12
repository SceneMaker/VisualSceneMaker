package de.dfki.vsm.runtime.activity.scheduler;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.HashMap;
import java.util.List;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityScheduler {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The list of detected marks
    private final HashMap<String, ActivityWorker> mWorkerMap = new HashMap<>();

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
        // Start the assigned task
        synchronized (mWorkerMap) {
            mWorkerMap.remove(marker).start();
        }
        //}
    }


    //Check if there is an marker / activity.
    public boolean hasMaker(String marker) {
        synchronized (mWorkerMap) {
            return mWorkerMap.containsKey(marker);
        }
    }

    // Schedule an activity on an executor with a timeout
    public final void schedule(
            final long timeout,
            final List<ActivityWorker> list,
            final AbstractActivity activity,
            final ActivityExecutor executor) {
        // Create a new activity task
        final ActivityWorker task = new ActivityWorker(
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
    public final ActivityWorker register(
            final String marker,
            final ActionActivity activity,
            final ActivityExecutor executor) {

        // PG 12.8.2020 debug: mLogger.message("ActivityScheduler registers activity " + activity.getText() + " with marker >" + marker + "< and executor " + executor.getClass().getCanonicalName());
        // Create a new activity task
        final ActivityWorker task = new ActivityWorker(
                -1, null, activity, executor);
        // Add the task to the mapping
        synchronized (mWorkerMap) {
            mWorkerMap.put(marker, task);
        }
        // Return the task for joining
        return task;
    }
}
