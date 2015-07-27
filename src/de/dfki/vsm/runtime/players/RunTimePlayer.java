package de.dfki.vsm.runtime.players;

import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.plugins.RunTimePlugin;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public interface RunTimePlayer extends RunTimePlugin {

    // Play with given arguments
    public void play(final String name, final LinkedList<AbstractValue> args);

    // A single task of the player
    public static class Task extends Thread {

        // The termination flag
        private boolean mIsDone;

        // Abort the execution
        public final void abort() {
            mIsDone = true;
        }

        // Check execution status
        public final boolean isDone() {
            return mIsDone;
        }

        // Construct with a name
        public Task(final String name) {
            super(name);
            // Initialize the flag
            mIsDone = false;
        }
    }
}
