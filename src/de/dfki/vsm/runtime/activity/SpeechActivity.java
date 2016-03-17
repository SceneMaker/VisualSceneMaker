package de.dfki.vsm.runtime.activity;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class SpeechActivity extends AbstractActivity {

    private final LinkedList mList;
    private final String mMark;

    // Construct the speech activity
    public SpeechActivity(
            final String actor,
            final LinkedList list,
            final String mark) {
        super(Policy.BLOCKING, actor, "SPEECH", "SPEAK");
        // Initialize the content
        mList = list;
        mMark = mark;
    }

    // Get the policy
    //public Policy getPolicy() {
    //    return mPolicy;
    //}
    // Get representation
    @Override
    public final String toString() {
        final StringBuilder builder = new StringBuilder();
        for (final Object item : mList) {
            builder.append(item.toString());
            if (!item.equals(mList.getLast())) {
                builder.append(' ');
            } else {
                builder.append(mMark);
            }
        }
        return builder.toString();
    }
}
