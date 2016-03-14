package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class ScenePlayer {

    // The defaut system logger
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The runtime project
    protected final RunTimeProject mProject;

    // Construct the player
    public ScenePlayer(final RunTimeProject project) {
        mProject = project;
    }

    // Launch the scene player
    public abstract void launch();

    // Unload the scene player
    public abstract void unload();

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
