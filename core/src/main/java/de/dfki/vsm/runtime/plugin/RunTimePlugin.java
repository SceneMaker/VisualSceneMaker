package de.dfki.vsm.runtime.plugin;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimePlugin {

    // The system logger
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The runtime project
    protected final RunTimeProject mProject;
    // The plugin's name
    protected final PluginConfig mConfig;

    // Construct the plugin
    public RunTimePlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initializ the config
        mConfig = config;
        // Initialize the project
        mProject = project;
    }

    // Launch the plugin
    public abstract void launch();

    // Unload the plugin
    public abstract void unload();
}
