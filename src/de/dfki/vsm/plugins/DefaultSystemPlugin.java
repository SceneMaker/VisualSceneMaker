package de.dfki.vsm.plugins;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.plugin.Plugin;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public final class DefaultSystemPlugin implements Plugin {

    // The Singelton Instance
    public static DefaultSystemPlugin sInstance = null;
    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Project Data
    final RunTimeProject mProject;
    // The Config Data
    final PluginConfig mConfig;

    ////////////////////////////////////////////////////////////////////////////
    public static synchronized DefaultSystemPlugin getInstance(
            final RunTimeProject project,
            final PluginConfig config) {
        if (sInstance == null) {
            sInstance = new DefaultSystemPlugin(project, config);
        }
        // Return The Singelton Instance
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    private DefaultSystemPlugin(
            final RunTimeProject project,
            final PluginConfig config) {
        // Initialize the plugin members
        mProject = project;
        mConfig = config;
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public boolean launch() {
        // Print Some Information
        mLogger.message("Launching Plugin '" + this + "'");
        // Return true at success
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public boolean unload() {
        // Print Some Information
        mLogger.message("Unloading Plugin '" + this + "'");
        // Return true at success
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    public void test() {
        // Print Some Information
        mLogger.message("Accessing Plugin '" + this + "'");
    }
}
