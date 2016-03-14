package de.dfki.vsm.runtime.activity;

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
        super(Policy.PARALLEL, actor);
        // Initialize the text
        mText = text;
    }

    public final String getText() {
        return mText;
    }
}
