package de.dfki.vsm.api;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Not me
 */
public final class VSMSystemTimer extends Thread {

    // Termination Flag
    private volatile boolean mDone = false;

    // The System Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Scene Player
    private final VSMScenePlayer mPlayer;

    // The Wait Interval
    private final long mTimerInterval;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VSMSystemTimer(final VSMScenePlayer player, final long interval) {
        
        super("VSMSystemTimer");

        // Initialize The Player
        mPlayer = player;

        // Initialize The Interval
        mTimerInterval = interval;

        // Debug Some Information
        mLogger.message("Creating VSM System Timer");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void abort() {

        // Debug Some Information
        mLogger.message("Aborting VSM System Timer");

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
        mLogger.message("Starting VSM System Timer");

        // Initialize Startup Time
        mPlayer.setStartupTime(System.currentTimeMillis());

        // Update The Current Time
        while (!mDone) {

            // Sleep For Some Time
            try {
                Thread.sleep(mTimerInterval);
            } catch (InterruptedException exc) {

                // Debug Some Information
                mLogger.warning(exc.toString());

                // Debug Some Information
                mLogger.warning("Interrupting VSM System Timer");

                // Exit On An Interrupt
                mDone = true;
            }

            // Update The Current Time
            mPlayer.setCurrentTime(System.currentTimeMillis() - mPlayer.getStartupTime());

            // Assert The New Time
            mPlayer.query("retractall(now(_))," + " assertz(now(" + mPlayer.getCurrentTime() + ")).");
        }

        // Print Some Information
        mLogger.message("Stopping VSM System Timer");
    }
}
