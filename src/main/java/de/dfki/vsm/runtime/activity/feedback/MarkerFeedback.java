//package de.dfki.vsm.runtime.activity.feedback;
//
//import de.dfki.vsm.runtime.activity.AbstractActivity;
//
///**
// * This class represents a feedback message of the executor which contains the
// * information about the detection of some marker during the execution of some
// * activity.
// *
// * @author Gregor Mehlmann
// */
//public final class MarkerFeedback extends ActivityFeedback {
//
//    private final String mMarker;
//
//    public MarkerFeedback(final String marker) {
//        // Initialize the feedback
//        super();
//        // Initialize the marker
//        mMarker = marker;
//    }
//    
//   
//    public MarkerFeedback(
//            final AbstractActivity activity, final String marker) {
//        // Initialize the feedback
//        super(activity);
//        // Initialize the marker
//        mMarker = marker;
//    }
//    
//    
//    // Get the marker
//    public final String getMarker() {
//        return mMarker;
//    }
//
//    @Override
//    public String toString() {
//        return mMarker;
//    }
//}
