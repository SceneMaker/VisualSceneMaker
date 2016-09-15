//package de.dfki.vsm.xapistuff;
//
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//
///**
// * @author Gregor Mehlmann
// */
//public final class VSMSystemTimer extends Thread {
//
//    // The singelton system togger
//    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
//    // The thread termination flag
//    private volatile boolean mDone = false;
//    // The scene player reference
//    private final VSMScenePlayer mPlayer;
//    // The timer wait interval
//    private final long mTimerInterval;
//
//    // Construct the system timer
//    public VSMSystemTimer(final VSMScenePlayer player, final long interval) {
//        super("VSMSystemTimer");
//        // Initialize the player
//        mPlayer = player;
//        // Initialize the interval
//        mTimerInterval = interval;
//        // Print some Information
//        mLogger.message("Creating VSM System Timer");
//    }
//
//    // Abort the system timer
//    public final void abort() {
//        // Print some Information
//        mLogger.message("Aborting VSM System Timer");
//        // Set termination flag
//        mDone = true;
//        // Interrupt thread state
//        interrupt();
//    }
//
//    // Execute the system timer
//    @Override
//    public final void run() {
//        // Print some Information
//        mLogger.message("Starting VSM System Timer");
//        // Set the player start time
//        mPlayer.setStartupTime(System.currentTimeMillis());
//        // Then update the player time
//        while (!mDone) {
//            // Sleep for some very short time
//            try {
//                // Eventually change interval 
//                Thread.sleep(mTimerInterval);
//            } catch (final Exception exc) {
//                // Print some Information
//                mLogger.warning(exc.toString());
//                // Print some Information
//                //mLogger.warning("Interrupting VSM System Timer");
//                // Exit on an interrupt
//                mDone = true;
//            }
//            // Update the player time
//            mPlayer.setCurrentTime(
//                    System.currentTimeMillis() - mPlayer.getStartupTime());
//            // Assert the new time now
//            mPlayer.query("retractall(now(_)),"
//                    + " assertz(now(" + mPlayer.getCurrentTime() + ")).");
//        }
//        // Print some information
//        mLogger.message("Stopping VSM System Timer");
//    }
//}
