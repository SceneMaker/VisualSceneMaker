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
    // The activity mode
    protected final String mMode;
    // The activity name
    protected final String mName;

    // Construct the activity
    public AbstractActivity(
            final Policy type,
            final String actor,
            final String mode,
            final String name) {
        mType = type;
        mActor = actor;
        mMode = mode;
        mName = name;
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
