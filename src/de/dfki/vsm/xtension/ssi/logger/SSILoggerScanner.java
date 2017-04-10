package de.dfki.vsm.xtension.ssi.logger;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;
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

    public static int getNumberOfEventInContext(
            final String event,
            final String context,
            final int id) {
        // Get all relevant the events 
        final NavigableSet<SSIEventEntry> set = findEventRangeInEventRecord(context, String.valueOf(id));
        // Count the number of events
        int number = 0;
        if (set != null) {
            // Increment the number of head nod events
            for (final SSIEventEntry entry : set) {
                if (entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase("continued")) {
                    number++;
                }
            }
        }
        return number;
    }

    public static int getDurationOfEventInContext(
            final String event,
            final String context,
            final int id) {
        // Get all relevant the events 
        final NavigableSet<SSIEventEntry> set = findEventRangeInEventRecord(context, String.valueOf(id));
        // Count the number of events
        int duration = 0;
        if (set != null) {
            for (final SSIEventEntry entry : set) {
                if (entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase("completed")) {
                    duration += Integer.parseInt(entry.getDur());
                }
            }
        }
        return duration;
    }

    public static float getMaximumOfEventInContext(
            final String event,
            final String value,
            final String context,
            final int id) {
        // Get all relevant the events 
        final NavigableSet<SSIEventEntry> set = findEventRangeInEventRecord(context, String.valueOf(id));
        // Count the number of events
        float maximum = 0.0F;
        if (set != null) {
            for (final SSIEventEntry entry : set) {
                if (entry.getEvent().equalsIgnoreCase(event)
                        && entry.getType().equalsIgnoreCase("map")
                        && entry.getState().equalsIgnoreCase("completed")) {
                    maximum = Math.max(maximum,
                            Float.parseFloat(((SSITupleData) entry.getData()).get(value.toLowerCase())));
                }
            }
        }
        return maximum;
    }

    // Get the duration of a context event with the given identifier
    public static int getDurationOfContext(
            final String event, final int id) {
        // Find the context completed event itself
        final SSIEventEntry entry = findEventEntryInEventRecord(event, "completed", String.valueOf(id));
        // And then return the duration of this event
        return Integer.parseInt(entry.getDur());
    }

    // Get a single event entry from the 
    // context with the given identifier
    private static SSIEventEntry findEventEntryInEventRecord(
            final String event, final String state, final String id) {
        for (final SSIEventEntry entry : sEventArray.getTreeSet()) {
            if (entry.getData() instanceof SSIStringData) {
                if (entry.getData().toString().equalsIgnoreCase(id)
                        && entry.getEvent().equalsIgnoreCase(event)
                        && entry.getState().equalsIgnoreCase(state)) {
                    return entry;
                }
            }
        }
        return null;
    }

    // Get a range of event entries from the 
    // context with the given identifier
    private static NavigableSet<SSIEventEntry> findEventRangeInEventRecord(
            final String event, final String id) {
        final SSIEventEntry lower = findEventEntryInEventRecord(event, "continued", id);
        final SSIEventEntry upper = findEventEntryInEventRecord(event, "completed", id);
        if (lower != null && upper != null) {
            return sEventArray.getTreeSet().subSet(lower, true, upper, true);
        }
        // Return at failure
        return null;
    }
}
