package de.dfki.vsm.runtime.activity;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractActivity {

    // The activity type
    public enum Policy {

        BLOCKING, // The calling task has to wait for temination
        PARALLEL // The calling task doesn't need to join this
    }

    // The activity type
    protected final Policy mType;
    // The activity actor
    protected final String mActor;

    // Construct the activity
    public AbstractActivity(final Policy type, final String actor) {
        mType = type;
        mActor = actor;
    }

    // Get the scheduling type
    public final Policy getType() {
        return mType;
    }

    // Get the activity actor
    public final String getActor() {
        return mActor;
    }
}
