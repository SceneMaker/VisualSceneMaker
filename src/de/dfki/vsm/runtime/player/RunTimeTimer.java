package de.dfki.vsm.runtime.player;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann TODO: This should basically be a singelton thread
 * factory in the JPL package
 */
public final class RunTimeTimer extends Thread {

    // The singelton system togger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The thread termination flag
    private volatile boolean mDone = false;
    // The timer wait interval
    private final long mTimerInterval;
    // The launch startup time
    private volatile long mStartupTime;
    // The current player time
    private volatile long mCurrentTime;
    //The flag if we use the JPL
    private final boolean mUserJPLEngine;

    // Construct the system timer
    public RunTimeTimer(final long interval, final boolean usejpl) {
        super("WizardTimer");
        // Initialize the interval
        mTimerInterval = interval;
        // INitialize the JPL flag
        mUserJPLEngine = usejpl;
        // Print some Information
        mLogger.message("Creating System Timer");
    }

    // Abort the system timer
    public final void abort() {
        // Print some Information
        mLogger.message("Aborting System Timer");
        // Set termination flag
        mDone = true;
        // Interrupt thread state
        interrupt();
    }

    public final long getTime() {
        // Reurn the JPL time
        return mCurrentTime;
    }

    // Execute the system timer
    @Override
    public final void run() {
        // Print some information
        mLogger.message("Starting System Timer");
        // Set the JPL start time
        mStartupTime = System.currentTimeMillis();
        // Then update the JPL time
        while (!mDone) {
            // Sleep for some very short time
            try {
                // Eventually change interval 
                Thread.sleep(mTimerInterval);
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning(exc.toString());
                // Exit on an interrupt
                mDone = true;
            }
            // Update the player time
            mCurrentTime
                    = System.currentTimeMillis() - mStartupTime;
            if (mUserJPLEngine) {
                // Assert the new time now
                JPLEngine.query("retractall(now(_)),"
                        + "assertz(now(" + mCurrentTime + ")).");
            }
        }
        // Print some information
        mLogger.message("Stopping System Timer");
    }
}
