//package de.dfki.vsm.runtime.activity.feedback;
//
//import de.dfki.vsm.runtime.activity.AbstractActivity;
//
///**
// * This class represents a feedback message of the executor which contains the
// * information about the status of the execution of an activity. The execution
// * status of an activity may either be STARTED, STOPPED, RUNNING or ABORTED.
// *
// * @author Gregor Mehlmann
// */
//public final class StatusFeedback extends ActivityFeedback {
//
//    public enum Status {
//
//        STARTED,
//        STOPPED,
//        RUNNING,
//        ABORTED
//    }
//
//    private final Status mStatus;
//
//    public StatusFeedback(
//            final AbstractActivity activity, final Status status) {
//        // Initialize the feedback
//        super(activity);
//        // Initialize the status
//        mStatus = status;
//    }
//
//    public final Status getStatus() {
//        return mStatus;
//    }
//
//    @Override
//    public String toString() {
//        return mStatus.name();
//    }
//}
