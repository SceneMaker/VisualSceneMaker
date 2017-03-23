package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ActionActivity extends AbstractActivity {

    // The context type
    public enum Context {

        // The calling task has to wait for termination
        nested,
        // The calling task doesn't need to join this
        single
    }

    // TODO: The context of the activity is either nested or standalone
    // If nested, then the actor is optional and otherwise mandatory, so
    // maybe we need another enumeration with the type of the context
    private final Context mContext = Context.nested;

    // The textual representation
    protected final String mText;

    // Construct the activity
    public ActionActivity( // (added PG)
            final String actor,
            //final String mode,
            final String name,
            final String text,
            final LinkedList<ActionFeature> featureList,
            final HashMap<String, String> substitutions            ) {
        super(Type.parallel, actor,/* mode,*/ name, featureList);
        // Initialize the text
        mText = text;
    }

    // Get the textual representation
    @Override
    public final String getText() {
        return mText;
    }
}
