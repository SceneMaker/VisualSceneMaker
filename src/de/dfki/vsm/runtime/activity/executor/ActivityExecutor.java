package de.dfki.vsm.runtime.activity.executor;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.scheduler.ActivityScheduler;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public abstract class ActivityExecutor extends RunTimePlugin {

    // The runtime player
    protected final RunTimePlayer mPlayer;
    // The activity scheduler
    protected final ActivityScheduler mScheduler;

    public ActivityExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Initialize runtime player
        mPlayer = mProject.getRunTimePlayer();
        // Initialize activity scheduler
        mScheduler = mPlayer.getActivityScheduler();

    }

    public abstract String marker(final long id);

    public abstract void execute(final AbstractActivity activity);
}
