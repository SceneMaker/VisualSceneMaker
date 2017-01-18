package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.activity.scheduler.ActivityScheduler;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimePlayer {

    // The activity manager
    protected final ActivityScheduler mScheduler = new ActivityScheduler();
    // The runtime project
    protected final RunTimeProject mProject;
    // The plugin's name
    protected final PlayerConfig mConfig;

    // Construct the player
    public RunTimePlayer(
            final PlayerConfig config,
            final RunTimeProject project) {
        // Initializ the config
        mConfig = config;
        // Initialize the project
        mProject = project;
    }

    // The system logger
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // Launch the plugin
    public abstract void launch();

    // Unload the plugin
    public abstract void unload();

    //public abstract long getTime();

    // Get the activity scheduler
    public ActivityScheduler getActivityScheduler() {
        return mScheduler;
    }

    // Call the play action activity method
    public abstract void playActionActivity(final String name, final LinkedList args);

    // Call the play scenegroup method
    public abstract void playSceneGroup(final String name, final LinkedList args);

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
