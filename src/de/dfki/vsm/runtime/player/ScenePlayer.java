package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class ScenePlayer extends RunTimePlugin {

    // Construct the player
    public ScenePlayer(final RunTimeProject project) {
        super(project);
    }

    // Call the playback method
    public abstract void play(final String name, final LinkedList args);

    // The scene player worker
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
