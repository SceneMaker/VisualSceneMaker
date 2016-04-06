package de.dfki.vsm.runtime.activity.executor;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityScheduler;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public abstract class ActivityExecutor extends RunTimePlugin {
	
    public ActivityExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    public abstract String marker(final long id);

    public abstract void execute(
            final AbstractActivity activity,
            final ActivityScheduler player); // this can be removed ...

}
