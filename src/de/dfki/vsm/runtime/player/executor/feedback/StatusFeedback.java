package de.dfki.vsm.runtime.player.executor.feedback;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;

/**
 * This class represents a feedback message of the executor which contains the
 * information about the status of the execution of an activity. The execution
 * status of an activity may either be STARTED, STOPPED, RUNNING or ABORTED.
 *
 * @author Gregor Mehlmann
 */
public final class StatusFeedback implements ExecutorFeedback {

    public enum ExecutionStatus {

        STARTED,
        STOPPED,
        RUNNING,
        ABORTED
    }

    private final ExecutionStatus mStatus;
    private final AbstractActivity mActivity;

    public StatusFeedback(
            final AbstractActivity activity,
            final ExecutionStatus status) {
        mActivity = activity;
        mStatus = status;
    }

    public final AbstractActivity getActivity() {
        return mActivity;
    }

    public final ExecutionStatus getStatus() {
        return mStatus;
    }

    @Override
    public String toString() {
        return mStatus.name();
    }
}
