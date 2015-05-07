package de.dfki.vsm.util.log;

//~--- JDK imports ------------------------------------------------------------

import java.util.logging.ConsoleHandler;
import java.util.logging.Level;

/**
 * @author Gregor Mehlmann
 */
public class LOGConsoleHandler extends ConsoleHandler {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public LOGConsoleHandler() {

        // Install A New Console Formatter
        setFormatter(new LOGConsoleFormat());

        // Log The Messages From All Levels
        setLevel(Level.ALL);
    }
}
