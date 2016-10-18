package de.dfki.vsm.util.log;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;

//~--- JDK imports ------------------------------------------------------------

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Gregor Mehlmann
 */
public class LOGSSISockLogger {

    // The Singelton Console Logger Instance
    private static LOGSSISockLogger sInstance = null;

    // Construct The Java Console Logger
    private static final Logger sLogger = Logger.getLogger(LOGSSISockLogger.class.getName());

    // Connection Information
    private final String mHost;
    private final int    mPort;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Construct The Default Logger
    private LOGSSISockLogger(final String host, final int port) {

        // Initialize Connection Information
        mHost = host;
        mPort = port;

        // Log The Messages From All Levels
        sLogger.setLevel(Level.ALL);

        // Do Not Propagate The Messages
        sLogger.setUseParentHandlers(false);

        try {

            // Install The Console Handler
            install(new LOGSSISockHandler(mHost, mPort));

            // Install The Logfile Handler
            install(new LOGLogFileHandler(Preferences.sSOCKFILE_FILE_NAME, 10485760, 1, true));    // 10 MB Size
        } catch (Exception exc) {
            sLogger.severe(exc.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Get The Singelton Logger Instance
    public static synchronized LOGSSISockLogger getInstance(final String host, final int port) {
        if (sInstance == null) {
            sInstance = new LOGSSISockLogger(host, port);
        }

        // TODO: Remove Old Handlers And Install New Ones
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Install A Handler
    public final synchronized void install(final Handler handler) {
        sLogger.addHandler(handler);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Remove A Handler
    public final synchronized void remove(final Handler handler) {
        sLogger.removeHandler(handler);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Log A Severe Message
    public final synchronized void failure(final String msg) {
        sLogger.log(Level.SEVERE, msg, Thread.currentThread().getStackTrace());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Log A Warning Message
    public final synchronized void warning(final String msg) {
        sLogger.log(Level.WARNING, msg, Thread.currentThread().getStackTrace());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Log An Inform Message
    public final synchronized void message(final String msg) {
        sLogger.log(Level.INFO, msg, Thread.currentThread().getStackTrace());
    }
}
