package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.scheduler.ActivityScheduler;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimePlayer extends RunTimePlugin {

    // The activity manager
    protected final ActivityScheduler mScheduler = new ActivityScheduler();

    // Construct the player
    public RunTimePlayer(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
    }

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
