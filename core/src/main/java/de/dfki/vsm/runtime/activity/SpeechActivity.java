package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;

/**
 * @author Gregor Mehlmann
 */
public final class SpeechActivity extends AbstractActivity {

    // The logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();

    // The list of blocks
    private LinkedList mBlocks;
    // The punctuation mark
    private final String mPunct;

    // Construct the activity
    public SpeechActivity(
            final String actor,
            final LinkedList list,
            final String punct) {
        super(Type.blocking, actor, /*"speech",*/ "speak");
        // Initialize the content
        mBlocks = list;
        mPunct = punct;
    }

    // Get the text and time mark blocks (added by PG)
    public final LinkedList getBlocks() {
        return mBlocks;
    }

    // Get the punctuation information (added by PG)
    public final String getPunct() {
        return mPunct;
    }

    /**
     * Get the text only - without time mark blocks (added by PG)
     *
     * @param markerSign The beginning of the marker
     * @return The text without speech markers
     */
    public final String getTextOnly(final String markerSign) {
        final StringBuilder builder = new StringBuilder();
        for (final Object item : mBlocks) {
            if (!item.toString().contains(markerSign)) {
                builder.append(item.toString());
                if (!item.equals(mBlocks.getLast())) {
                    builder.append(' ');
                } else {
                    builder.append(mPunct);
                }
            }
        }
        return builder.toString();
    }

    /**
     * Do pronounciation mapping for a better text to speech output.
     *
     * @author PG - 2.5.2016
     */
    public final void doPronounciationMapping(Properties pronunciationMap) {
        if (pronunciationMap == null) {
            return;
        }

        LinkedList replaced = new LinkedList();
        for (final Object item : mBlocks) {
            String text = item.toString();
            //mLogger.success("text to be checked and maybe replaced " + text);
            for (Map.Entry<Object, Object> entry : pronunciationMap.entrySet()) {
                if (text != null && entry.getKey() != null && entry.getValue() != null) {
                    text = text.replaceAll("\\b" + entry.getKey() + "\\b", (String) entry.getValue());
                }
            }
            //mLogger.success(" with " + text);
            replaced.add(text);
        }
        mBlocks = replaced;
    }

    /**
     * Get the punctuation information
     *
     * @param markerSign The beginning of the marker
     * @return
     * @author PG 20.4.2016
     */
    public final LinkedList<String> getTimeMarks(final String markerSign) {
        final LinkedList<String> tms = new LinkedList<>();
        for (final Object item : mBlocks) {
            if (item.toString().contains(markerSign)) {
                tms.add(item.toString());
            }
        }

        return tms;
    }

    /**
     * Get textual representation
     *
     * @return
     */
    @Override
    public final String getText() {
        final StringBuilder builder = new StringBuilder();
        for (final Object item : mBlocks) {
            if (item.equals(mBlocks.getLast())) {
                if (item.toString().startsWith("${'")) { // PG 14.7.2020 quick and dirty solution if there is a marker at the end, the punctuation comes before.
                    if( builder.length() > 0 )
                        builder.deleteCharAt(builder.length() - 1); // remove the last space
                        builder.append(mPunct).append(' ').append(item.toString());
                } else {
                    builder.append(item.toString()).append(mPunct);
                }
            } else {
                builder.append(item.toString()).append(' ');
            }

            //builder.append(item.toString());
            //if (!item.equals(mBlocks.getLast())) {
            //    builder.append(' ');
            //} else {
            //    builder.append(mPunct);
            //}
        }
        return builder.toString();
    }
}
