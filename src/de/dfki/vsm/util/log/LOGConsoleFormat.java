package de.dfki.vsm.util.log;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;
import java.text.SimpleDateFormat;

//~--- JDK imports ------------------------------------------------------------

import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author Gregor Mehlmann
 */
public class LOGConsoleFormat extends Formatter {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String format(final LogRecord record) {

        // Create The Date Of Logging
        Date date = new Date(record.getMillis());

        // Create The Thread Of Logging
        Thread thread = Thread.currentThread();

        // Create The Name Of Logger
        String name = record.getLoggerName();

        // Create The Stack Trace
        Object[] trace = record.getParameters();

        // Create The Method Name
        Object method = trace[2];

        String message = "";
        if (record.getLevel() == Level.SEVERE) {
            message += "\033[31m";
        } else if (record.getLevel() == Level.WARNING) {
            message += "\033[1;33m";
        } else if (record.getLevel() == Level.INFO) {
            message += "\033[1;36m";
        }  else if (record.getLevel() == Level.ALL) {
            message += "\033[1;32m";
        } else {
            message += "\033[1;37m";
        }
        
        // PG added milliseconds
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        
        
        // Create The String For Logging
        message += record.getLevel() + " to " + "STDERR" + " on " + df.format(date) + " by " + name + " in " + thread
                         + " at " + method;

        // Append The User Message
        message += Preferences.sSYSPROPS_LINE_SEPR + record.getMessage()    // The Message
                   + Preferences.sSYSPROPS_LINE_SEPR + Preferences.sSYSPROPS_LINE_SEPR + "\033[0m";

        // return The Final Log Message
        return message;
    }
}
