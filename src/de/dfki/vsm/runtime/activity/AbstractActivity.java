package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractActivity {

    // The activity type
    public enum Type {

        // The calling task has to wait for termination
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
    // The action features
    protected final LinkedList<ActionFeature> mFeatures;
    // The substitutions
    protected final HashMap<String, String> mSubstitutions;

    // Construct the activity
    public AbstractActivity(
            final Type type,
            final String actor,
            final String name) {
        mType = type;
        mActor = actor;
        mName = name;
        mFeatures = null;
        mSubstitutions = null;
    }

    // Construct the activity
    public AbstractActivity(
            final Type type,
            final String actor,
            final String name,
            final LinkedList<ActionFeature> featureList,
            final HashMap<String, String> substitutions) {
        mType = type;
        mActor = actor;
        mName = name;
        mFeatures = featureList;
        mSubstitutions = substitutions;
    }

    // Get the scheduling type
    public final Type getType() {
        return mType;
    }

    // Set the scheduling type
    public final void setType(final Type type) {
        mType = type;
    }

    // Get the activity actor
    public final String getActor() {
        return mActor;
    }

    // Get the activity name
    public final String getName() {
        return mName;
    }

    // Get the features
    public final LinkedList<ActionFeature> getFeatures() {
        return mFeatures;
    }

    // Get the sunstitutions
    public final HashMap<String, String> getSubstitutions() {
        return mSubstitutions;
    }

    // Get the value of a feature
    public final String get(final String key) {
        for (final ActionFeature feature : mFeatures) {
            if (feature.getKey().equals(key)) {
                return feature.getVal(mSubstitutions);
            }
        }
        return null;
    }

    // Get the textual representation
    public abstract String getText();

}
