package de.dfki.vsm.xtension.mindbotrobot;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

public final class MindbotRobotExecutor extends ActivityExecutor {

    // Construct executor
    public MindbotRobotExecutor(final PluginConfig config, final RunTimeProject project) { super(config, project); }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public final void launch() { }

    @Override
    public final void unload() { }

    @Override
    public void execute(final AbstractActivity activity) { }
}
