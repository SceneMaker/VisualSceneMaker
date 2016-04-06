package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.manager.ActivityScheduler;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimePlayer extends RunTimePlugin {
	
    // The activity manager
    protected final ActivityScheduler mScheduler = new ActivityScheduler(); // moved from ReactivePlayer to here PG 4.4.2016

    // Construct the player
    public RunTimePlayer(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    // Call the playback method
    public abstract void play(final String name, final LinkedList args);
	
	// get the ActivityScheduler for handling activities ...
	public ActivityScheduler getActivityManager() {
		return mScheduler;
	}

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
