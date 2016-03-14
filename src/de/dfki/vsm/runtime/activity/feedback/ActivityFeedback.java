package de.dfki.vsm.runtime.activity.feedback;

import de.dfki.vsm.runtime.activity.AbstractActivity;

/**
 * @author Gregor Mehlmann
 */
public abstract class ActivityFeedback {

    // The feedback's activity
    protected final AbstractActivity mActivity;

    // Construct the executor
    public ActivityFeedback(final AbstractActivity activity) {
        // Initialize the activity
        mActivity = activity;
    }

    // Get the activity
    public AbstractActivity getActivity() {
        return mActivity;
    }
}
