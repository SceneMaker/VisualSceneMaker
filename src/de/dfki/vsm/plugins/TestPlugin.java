package de.dfki.vsm.plugins;

import de.dfki.vsm.model.config.ConfigData;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.runtime.plugin.Plugin;

/**
 * @author Gregor Mehlmann
 */
public final class TestPlugin implements Plugin {

    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The Project Data
    final ProjectData mProject;
    // The Config Data
    final ConfigData mConfig;

    public TestPlugin(
            final ProjectData project,
            final ConfigData config) {
        //
        mProject = project;
        mConfig = config;
        // Print Some Information
        mLogger.message("Creating Plugin '" + this + "' With Project '" + mProject.toString() + "' And Config\n" + mConfig.toString());
    }

    @Override
    public void launch() {
        // Print Some Information
        mLogger.message("Launching Plugin '" + this + "'");

    }

    @Override
    public void unload() {
        // Print Some Information
        mLogger.message("Unloading Plugin '" + this + "'");
    }

}
