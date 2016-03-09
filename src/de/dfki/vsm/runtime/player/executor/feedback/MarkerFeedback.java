package de.dfki.vsm.runtime.player.executor.feedback;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.trigger.MarkerTrigger;

/**
 * This class represents a feedback message of the executor which contains the
 * information about the detection of some marker during the execution of some
 * activity.
 *
 * @author Gregor Mehlmann
 */
public final class MarkerFeedback implements ExecutorFeedback {

    private final MarkerTrigger mTrigger;
    private final AbstractActivity mActivity;

    public MarkerFeedback(
            final MarkerTrigger trigger,
            final AbstractActivity activity) {
        mTrigger = trigger;
        mActivity = activity;
    }

    public final MarkerTrigger getTrigger() {
        return mTrigger;
    }

    public final AbstractActivity getActivity() {
        return mActivity;
    }
}
