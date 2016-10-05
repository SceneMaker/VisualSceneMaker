package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractActivity {

    // The activity type
    public enum Type {

        // The calling task has to wait for temrination
        blocking,
        // The calling task doesn't need to join this
        parallel
    }

    // The activity type
    protected Type mType;
    // The activity actor
    protected final String mActor;
    // The activity name
    protected final String mName;
    // The activity mode
    protected final String mMode;
    // The related action features
    protected final LinkedList<ActionFeature> mFeatures;

    // Construct the activity
    public AbstractActivity(
            final Type type,
            final String actor,
            final String mode,
            final String name) {
        mType = type;
        mActor = actor;
        mMode = mode;
        mName = name;
        mFeatures = null;
    }

    // Construct the activity (added PG)
    public AbstractActivity(
            final Type type,
            final String actor,
            final String mode,
            final String name,
            final LinkedList<ActionFeature> featureList) {
        mType = type;
        mActor = actor;
        mMode = mode;
        mName = name;
        mFeatures = featureList;
    }

    // Get the scheduling type
    public final Type getType() {
        return mType;
    }

    // added PG  - 21.4.2016 (play action activities with sceneflow play cmd)
    public final void setType(final Type type) {
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
    public final LinkedList<ActionFeature> getFeatures() {
        return mFeatures;
    }

    // Get the value of a feature
    public final String getValueOf(final String key) {
        for (final ActionFeature feature : mFeatures) {
            if (feature.getKey().equals(key)) {
                return feature.getVal();
            }
        }
        return null;
    }

    // Get the textual representation
    public abstract String getText();

}
