package de.dfki.vsm.runtime.plugin;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimePlugin {

    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The runtime project
    protected final RunTimeProject mProject;

    // Construct the plugin
    public RunTimePlugin(final RunTimeProject project) {
        mProject = project;
    }

    // Launch the plugin
    public abstract void launch();

    // Unload the plugin
    public abstract void unload();
}
