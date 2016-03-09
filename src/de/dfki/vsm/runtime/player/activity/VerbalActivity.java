package de.dfki.vsm.runtime.player.activity;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class VerbalActivity implements AbstractActivity {

    private final LinkedList<String> mList;
    private final String mMark;

    public VerbalActivity(final LinkedList<String> list, final String mark) {
        // Initialize the text
        mList = list;
        mMark = mark;
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
        return builder.toString();
    }

    @Override
    public final String toString() {
        return getText();
    }
}
