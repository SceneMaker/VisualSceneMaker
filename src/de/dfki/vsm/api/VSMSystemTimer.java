package de.dfki.vsm.api;

import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public class VSMSystemTimer extends Thread {

    // Termination Flag
    private boolean mDone = false;
    // The System Logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Scene Player
    private final VSMScenePlayer mPlayer;
    // The Wait Interval
    private final long mInterval;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VSMSystemTimer(final VSMScenePlayer player, final long interval) {
        // Initialize The Player
        mPlayer = player;
        // Initialize The Interval
        mInterval = interval;
        // Debug Some Information
        mLogger.message("Creating System Timer");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void abort() {
        // Debug Some Information
        mLogger.message("Aborting System Timer");
        // Set Termination Flag
        mDone = true;
        // Interrupt Thread State
        interrupt();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {
        // Debug Some Information
        mLogger.message("Starting System Timer");
        // Initialize Startup Time
        mPlayer.setStartupTime(System.currentTimeMillis());
        // Update The Current Time
        while (!mDone) {
            // Sleep For Some Time
            try {
                Thread.sleep(mInterval);
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
                // Exit On An Interrupt
                mDone = true;
            }
            // Update The Current Time
            mPlayer.setCurrentTime(
                    System.currentTimeMillis() - mPlayer.getStartupTime());
            // Assert The New Time
            mPlayer.query("retractall(now(_)),"
                    + " assertz(now(" + mPlayer.getCurrentTime() + ")).");
        }
        // Print Some Information
        mLogger.message("Stopping System Timer");
    }
}
