package de.dfki.vsm.xtension.wizard;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public final class WizardTimer extends Thread {

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

    // Construct the system timer
    public WizardTimer(final long interval) {
        super("WizardTimer");
        // Initialize the interval
        mTimerInterval = interval;
        // Print some Information
        mLogger.message("Creating Wizard Timer");
    }

    // Abort the system timer
    public final void abort() {
        // Print some Information
        mLogger.message("Aborting Wizard Timer");
        // Set termination flag
        mDone = true;
        // Interrupt thread state
        interrupt();
    }

    // Execute the system timer
    @Override
    public final void run() {
        // Print some Information
        mLogger.message("Starting Wizard Timer");
        // Set the player start time
        mStartupTime = System.currentTimeMillis();
        // Then update the player time
        while (!mDone) {
            // Sleep for some very short time
            try {
                // Eventually change interval 
                Thread.sleep(mTimerInterval);
            } catch (final Exception exc) {
                // Print some Information
                mLogger.warning(exc.toString());
                // Print some Information
                //mLogger.warning("Interrupting VSM System Timer");
                // Exit on an interrupt
                mDone = true;
            }
            // Update the player time
            mCurrentTime
                    = System.currentTimeMillis() - mStartupTime;
            // Assert the new time now
            final JPLResult result = JPLEngine.query("retractall(now(_)),assertz(now(" + mCurrentTime + ")).");

        }
        // Print some information
        mLogger.message("Stopping Wizard Timer");
    }
}
