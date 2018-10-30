package de.dfki.vsm.util.log;

import de.dfki.vsm.Preferences;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/*
Vordergrundfarbe
\[\033[0;30m\]	schwarz
\[\033[1;30m\]	dunkelgrau
\[\033[0;31m\]	rot
\[\033[1;31m\]	helles rot
\[\033[0;32m\]	grün
\[\033[1;32m\]	hellgrün
\[\033[0;33m\]	braun
\[\033[1;33m\]	gelb
\[\033[0;34m\]	blau
\[\033[1;34m\]	hellblau
\[\033[0;35m\]	dunkellila
\[\033[1;35m\]	helllila
\[\033[0;36m\]	Dunkeltürkis
\[\033[1;36m\]	Türkis
\[\033[0;37m\]	Hellgrau
\[\033[1;37m\]	weiß
\[\033[0m\]	farblos

Hintergrundfarbe
\[\033[XXm\]	Keine Hintergrundfarbe
\[\033[40;XXm\]	Schwarzer Hintergrund
\[\033[41;XXm\]	Roter Hintergrund
\[\033[42;XXm\]	Grüner Hintergrund
\[\033[43;XXm\]	Hellbrauner Hintergrund
\[\033[44;XXm\]	Blauer Hintergrund
\[\033[45;XXm\]	Lila Hintergrund
\[\033[46;XXm\]	Türkis Hintergrund
\[\033[47;XXm\]	Hellgrau Hintergrund
*/
/**
 * @author Gregor Mehlmann
 */
public final class LOGConsoleFormat extends Formatter {

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
        //  
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
