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
    // Get the text and time mark blocks (added by PG)
    public final LinkedList getBlocks() {
        return mList;
    }

    // Get the text only - without time mark blocks (added by PG)
    public final String getTextOnly(String markerSign) {
        final StringBuilder builder = new StringBuilder();
        for (final Object item : mList) {
            if (!item.toString().contains(markerSign)) {
                builder.append(item.toString());
                if (!item.equals(mList.getLast())) {
                    builder.append(' ');
                } else {
                    builder.append(mMark);
                }
            }
        }
        return builder.toString();
    }

     // Get the punctuation information (added by PG 20.4.2016)
    public final LinkedList<String> getTimeMarks(String markerSign) {
        final LinkedList<String> tms = new LinkedList<>();
        for (final Object item : mList) {
            if (item.toString().contains(markerSign)) {
                tms.add(item.toString());
            }
        }

        return tms;
    }

    // Get the punctuation information (added by PG)
    public final String getPunctuation() {
        return mMark;
    }

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
