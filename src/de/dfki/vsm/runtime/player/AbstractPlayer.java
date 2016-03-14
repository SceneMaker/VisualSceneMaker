package de.dfki.vsm.runtime.player;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractPlayer {

    public void launch();

    public void unload();

    public void play(final String name, final LinkedList args);

    // A player task
    public class PlayerWorker extends Thread {

        // The termination flag
        private volatile boolean mDone;

        // Abort the execution
        public final void abort() {
            // Set termination flag
            mDone = true;
            // And interrupt thread
            interrupt();
        }

        // Check execution status
        public final boolean isDone() {
            return mDone;
        }

        // Construct with a name
        protected PlayerWorker(final String name) {
            super(name);
            // Initialize the flag
            mDone = false;
        }
    }
}
