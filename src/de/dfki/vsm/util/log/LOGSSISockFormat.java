package de.dfki.vsm.util.log;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;

//~--- JDK imports ------------------------------------------------------------

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * @author Gregor Mehlmann
 */
public class LOGSSISockFormat extends Formatter {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String format(final LogRecord record) {

        // Get SSI Version String
        final String version = "V2";

        // Set The SSI Event Fields
        final String sender  = "VSM";
        final String event   = "LOG";
        final String from    = "0.0";
        final String dur     = "0.0";
        final String prob    = "1.0";
        final String type    = "STRING";
        final String state   = "CONTINUED";
        final String glue    = "0";
        final String message = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "<events ssi-v=\"" + version + "\">"
                               + "<event " + "sender=\"" + sender + "\" " + "ev cent=\"" + event + "\" " + "from=\""
                               + from + "\" " + "dur=\"" + dur + "\" " + "prob=\"" + prob + "\" " + "type=\"" + type
                               + "\" " + "state=\"" + state + "\" " + "glue=\"" + glue + "\">" + record.getMessage()
                               + "</event>" + "</events>" + Preferences.sSYSPROPS_LINE_SEPR;

        // Create The Date Of Logging
        // Date date = new Date(record.getMillis());
        // Create The Thread Of Logging
        // Thread thread = Thread.currentThread();
        // Create The Name Of Logger
        // String name = record.getLoggerName();
        // Create The Stack Trace
        // Object[] trace = record.getParameters();
        // Create The Method Name
        // Object method = trace[2];
        // Create The String For Logging
        // String message = record.getLevel()
        // + " to " + "SOCKET"
        // + " on " + date
        // + " by " + name
        // + " in " + thread
        // + " at " + method;
        // Append The User Message
        // message += SYSUtilities.sSYSPROPS_LINE_SEPR
        // + record.getMessage() // The Message
        // + SYSUtilities.sSYSPROPS_LINE_SEPR
        // + SYSUtilities.sSYSPROPS_LINE_SEPR;
        // Return The Final Log Message
        return record.getMessage();
    }
}
