package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import java.util.LinkedList;

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
    protected Policy mType;
    // The activity actor
    protected final String mActor;
    // The activity mode
    protected final String mMode;
    // The activity name
    protected final String mName;
    // The related action features
    protected final LinkedList<ActionFeature> mFeatureList;

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
        mFeatureList = null;
    }

    // Construct the activity (added PG)
    public AbstractActivity(
            final Policy type,
            final String actor,
            final String mode,
            final String name,
            final LinkedList<ActionFeature> featureList) {
        mType = type;
        mActor = actor;
        mMode = mode;
        mName = name;
        mFeatureList = featureList;
    }

    // Get the scheduling type
    public final Policy getType() {
        return mType;
    }

    // added PG  - 21.4.2016 (play action activities with sceneflow play cmd)
    public final void setTyp(Policy type) {
        mType = type;
    }

    // Get the activity actor
    public final String getActor() {
        return mActor;
    }

    // Get the activity mode (added PG)
    public final String getMode() {
        return mMode;
    }

    // Get the activity name (added PG)
    public final String getName() {
        return mName;
    }

    // Get the activity name (added PG)
    public final LinkedList<ActionFeature> getFeatureList() {
        return mFeatureList;
    }
}
