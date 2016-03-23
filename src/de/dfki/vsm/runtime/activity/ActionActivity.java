package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ActionActivity extends AbstractActivity {

    private final String mText;

    public ActionActivity(
            final String actor,
            final String mode,
            final String name,
            final String text) {
        super(Policy.PARALLEL, actor, mode, name);
        // Initialize the text
        mText = text;
    }

    public ActionActivity( // (added PG)
            final String actor,
            final String mode,
            final String name,
            final String text,
            final LinkedList<ActionFeature> featureList) {
        super(Policy.PARALLEL, actor, mode, name, featureList);
        // Initialize the text
        mText = text;
    }

    public final String getText() {
        return mText;
    }
}
