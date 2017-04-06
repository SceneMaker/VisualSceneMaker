package de.dfki.vsm.xtension.ssi.logger;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import java.io.File;
import java.util.NavigableSet;

/**
 * @author Gregor Mehlmann
 */
public final class SSILoggerScanner {

    // The singelton logger
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    //
    private final static SSIEventArray sEventArray = new SSIEventArray();
    
    public static boolean load(final String file) {
        //
        sLogger.message("Loading SSI event record file '" + file + "'");
        // Clear the array
        sEventArray.clear();
        // And parse it then
        if (XMLUtilities.parseFromXMLFile(sEventArray, new File(file))) {
            //
            sLogger.message(sEventArray.toString());
            //
            return true;
        }
        return false;
    }

    public static int getNumberOf(final String topic, final String event) {
        // Get all events during that topic
        final NavigableSet<SSIEventEntry> set = getRange("topic", topic);
        //
        int number = 0;
        //
        if (set != null) {
            sLogger.message("Inspecting topic set \n'" + set.toString() + "'");
            // Increment the number of head nod events
            for (final SSIEventEntry entry : set) {
                if (entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase("continued")) {
                    sLogger.success("Found \n'" + entry.toString() + "'");
                    number++;
                }
            }
        }
        return number;
    }

    public static int getDurationOf(final String topic, final String event) {
        // Get all events during that topic
        final NavigableSet<SSIEventEntry> set = getRange("topic", topic);
        //
        int duration = 0;
        //
        if (set != null) {
            // Accumulate the durations of head nod events
            for (final SSIEventEntry entry : set) {
                if (entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase("completed")) {
                    sLogger.success("Found \n'" + entry.toString() + "'");
                    duration += Integer.parseInt(entry.getDur());
                }
            }
        }
        return duration;
    }

    // Get a single event entry
    private static SSIEventEntry getEvent(
            final String event,
            final String state,
            final String topic) {
        for (final SSIEventEntry entry : sEventArray.getTreeSet()) {
            if (entry.getData() instanceof SSIStringData) {
                if (entry.getData().toString().equalsIgnoreCase(topic)
                        && entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase(state)) {
                    return entry;
                }
            }
        }
        return null;
    }

    // Get a range of event entries
    private static NavigableSet<SSIEventEntry> getRange(
            final String event,
            final String topic) {
        final SSIEventEntry lower = getEvent(event, "continued", topic);
        final SSIEventEntry higher = getEvent(event, "completed", topic);
        sLogger.message("Lower bound of range is '" + lower.toString() + "'");
        sLogger.message("Higher bound of range is '" + higher.toString() + "'");
        if (lower != null && higher != null) {
            return sEventArray.getTreeSet().subSet(lower, true, higher, true);
        }
        // Return at failure
        return null;
    }
}
