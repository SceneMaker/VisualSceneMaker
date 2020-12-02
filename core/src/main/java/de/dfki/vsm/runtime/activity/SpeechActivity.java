package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.SceneTurn;
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
    private LinkedList<String> mBlocks;
    // The punctuation mark
    private final String mPunct;
    // Turn information
    private final SceneTurn mTurn;
    private final int mTurnNum;
    private final int mTotalTurns;
    // Utterance information
    private final int mUtteranceNum;
    private final int mTotalUtterances;

    // Construct the activity
    public SpeechActivity(
            final String actor,
            final LinkedList<String> list,
            final String punct,
            final SceneTurn turn,
            final int turn_num,
            final int total_turns,
            final int utterance_num,
            final int total_utterances) {
        super(Type.blocking, actor, /*"speech",*/ "speak");
        // Initialize the content
        mBlocks = list;
        mPunct = punct;
        mTurn = turn;
        mTurnNum = turn_num;
        mTotalTurns = total_turns;
        mUtteranceNum = utterance_num;
        mTotalUtterances = total_utterances;
    }

    // Get the text and time mark blocks (added by PG)
    public final LinkedList<String> getBlocks() {
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
     * Get the actual turn number (as index)
     *
     * @return
     */
    public final int getTurnNumber() {
        return mTurnNum;
    }

    /**
     * Get the total turn numbers
     *
     * @return
     */
    public final int getTotalTurns() {
        return mTotalTurns;
    }

    /**
     * Get the actual utterance number (as index)
     *
     * @return
     */
    public final int getUtteranceNumber() {
        return mUtteranceNum;
    }

    /**
     * Get the total utterances number
     *
     * @return
     */
    public final int getTotalUtterances() {
        return mTotalUtterances;
    }

    /**
     * Get the whole SceneTurn object
     *
     * @return
     */
    public final SceneTurn getSceneTurn() {
        return mTurn;
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
            builder.append(item.toString());
            if (!item.equals(mBlocks.getLast())) {
                builder.append(' ');
            } else {
                builder.append(mPunct);
            }
        }
        return builder.toString();
    }
}
