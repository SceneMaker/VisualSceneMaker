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
    private final HashMap<String, ActivityWorker> mWorkerMap = new HashMap();

    // Handle activity feedback
    public final void handle(final String marker) {
        // Get the activity 
        //final AbstractActivity activity = object.getActivity();
        // Check the feedback
        //if (object instanceof StatusFeedback) {
        //final StatusFeedback feedback = (StatusFeedback) object;
        // Get the status name 
        //final Status status = feedback.getStatus();
        // Print some information
        //mLogger.message("Status of activity '" + feedback + "' is '" + status + "'");
        // TODO: 
        // Notify the waiting thread that its feedback is there
        //} else if (object instanceof MarkerFeedback) {
        //final MarkerFeedback feedback = (MarkerFeedback) object;
        //final String marker = feedback.getMarker();
        // Print some information
        //mLogger.message("Marker '" + marker + "' detected");
        // Start the assigned task
        synchronized (mWorkerMap) {
            mWorkerMap.remove(marker).start();
        }
        //}
    }

    // Schedule an activity on an executor with a timeout
    public final void schedule(
            final long timeout,
            final List<ActivityWorker> list,
            final AbstractActivity activity,
            final ActivityExecutor executor) {
        // Print some information
        //mLogger.message("Scheduling '" + activity + "'"
        //        + " with timeout '" + timeout + "'"
        //        + " on executor '" + executor + "'");
        // Create a new activity task
        final ActivityWorker task = new ActivityWorker(
                timeout, list, activity, executor);
        // Start the activity task
        task.start();
        // Check if we need to wait
        if (activity.getType() == Type.blocking) {
            // Print some information
            //mLogger.message("Blocking calling thread'" + Thread.currentThread() + "'");
            // Wait for termination
            boolean finished = false;
            while (!finished) {
                try {
                    // Print some information
                    //mLogger.message("Awaiting activity worker '" + task + "'");
                    // Join the job worker
                    task.join();
                    // Finish this execution
                    // after an interruption
                    finished = true;
                    // Print some information
                     // mLogger.message("Joining activity worker '" + task + "'");
                } catch (final InterruptedException exc) {
                    // Print some information
                     // mLogger.warning("Aborting activity worker '" + task + "'");
                    // Terminate job worker
                    task.abort();
                }
            }
            // Print some information
            //mLogger.message("Continuing calling thread'" + Thread.currentThread() + "'");
        }
    }

    // Register an activity on an executor with a marker
    public final ActivityWorker register(
            final String marker,
            final ActionActivity activity,
            final ActivityExecutor executor) {
        // Print some information
         //mLogger.message("Registering '" + activity + "'"
         //        + " with marker '" + marker + "'"
         //        + " on executor '" + executor + "'");
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

//    // get Activity from Worker related to Marker - added by PG 5.4.2016
//    public final AbstractActivity getMarkerActivity(final String marker) {
//        synchronized (mWorkerMap) {
//            if (mWorkerMap.containsKey(marker)) {
//                ActivityWorker task = mWorkerMap.get(marker);
//                return task.getActivity();
//            } else {
//                return null;
//            }
//        }
//    }
}
