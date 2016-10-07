package de.dfki.vsm.runtime.activity.scheduler;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.List;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityWorker extends Thread {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The termination flag
    private boolean mDone;
    // The execution delay
    private final long mTimeout;
    // The worker thread list
    private final List<ActivityWorker> mList;
    // The activity data
    private final AbstractActivity mActivity;
    private final ActivityExecutor mExecutor;

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
    public ActivityWorker(
            final long timeout,
            final List<ActivityWorker> workers,
            final AbstractActivity activity,
            final ActivityExecutor executor) {
        super(activity.toString());
        // Initialize the flag
        mDone = false;
        // Initialize the list
        mList = workers;
        // Initialize the data
        mTimeout = timeout;
        mActivity = activity;
        mExecutor = executor;
    }

    @Override
    public final void run() {
        // Wait for the timeout
        if (mTimeout > 0) {
            try {
                // Sleep for the timeout
                Thread.sleep(mTimeout);
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning(exc.toString());
            }
        }
        // Return when aborted
        if (!mDone) {
            // Print some information
            //mLogger.message("Executing activity '" + mActivity + "' on executor '" + mExecutor + "'");
            // Execute the activity
            mExecutor.execute(mActivity);
        }
        // Wait for workers
        if (mList != null) {
            for (final ActivityWorker worker : mList) {
                try {
                    // Print some information
                     //mLogger.message("Awaiting activity worker '" + worker + "'");
                    // Wait for the delay
                    worker.join();
                    // Print some information
                     //mLogger.message("Joining activity worker '" + worker + "'");
                    // TODO: Remove the worker from thread
                    // or clear the list after joinig all
                } catch (final InterruptedException exc) {
                    // Print some information
                    mLogger.warning(exc.toString());
                }
            }
        }
    }
}
