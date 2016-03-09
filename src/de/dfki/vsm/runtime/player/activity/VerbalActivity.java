package de.dfki.vsm.runtime.player.activity;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class VerbalActivity implements AbstractActivity {

    private final LinkedList<String> mList;
    private final String mPunct;
    private final String mSpeaker;

    public VerbalActivity(final String speaker, final LinkedList<String> list, final String punct) {
        // Initialize the text
        mList = list;
        mPunct = punct;
        mSpeaker = speaker;
    }

    @Override
    public final String getText() {
        final StringBuilder builder = new StringBuilder();
        for (final String item : mList) {
            builder.append(item);
            if (!item.equals(mList.getLast())) {
                builder.append(' ');
            }
        }
        return builder.append(mPunct).toString();
    }

    public final String getSpeaker() {
        return mSpeaker;
    }

    @Override
    public final String toString() {
        return getText();
    }
}
