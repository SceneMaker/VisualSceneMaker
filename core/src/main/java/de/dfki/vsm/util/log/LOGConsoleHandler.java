package de.dfki.vsm.util.log;

import de.dfki.vsm.Preferences;

import java.util.logging.ConsoleHandler;

/**
 * @author Gregor Mehlmann
 */
public class LOGConsoleHandler extends ConsoleHandler {

    public LOGConsoleHandler() {

        // Install A New Console Formatter
        setFormatter(new LOGConsoleFormat());
        // Log The Messages From All Levels
        setLevel(Preferences.getLogLevel());
        // Set Logger Encoding To UTF-8
        try {
            setEncoding("UTF-8");
        } catch (final Exception exc) {
            exc.printStackTrace();
        }
    }
}
