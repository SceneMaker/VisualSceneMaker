package de.dfki.vsm.test.plugins;

import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.runtime.plugin.Plugin;

/**
 * @author Not me
 */
public final class TestPlugin implements Plugin {

    // The Singelton Instance
    public static TestPlugin sInstance = null;
    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Project Data
    final RunTimeProject mProject;
    // The Config Data
    final ConfigElement mConfig;

    ////////////////////////////////////////////////////////////////////////////
    public static synchronized TestPlugin getInstance(
            final RunTimeProject project,
            final PluginConfig config) {
        if (sInstance == null) {
            sInstance = new TestPlugin(project, config);
        }
        // Return The Singelton Instance
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    private TestPlugin(
            final RunTimeProject project,
            final PluginConfig config) {
        //
        mProject = project;
        mConfig = config;
        // Print Some Information
        mLogger.message("Creating Plugin '" + this
                + "' With Project '" + mProject.toString()
                + "' And Config\n" + mConfig.toString());
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void launch() {
        // Print Some Information
        mLogger.message("Launching Plugin '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void unload() {
        // Print Some Information
        mLogger.message("Unloading Plugin '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    public void test() {
        // Print Some Information
        mLogger.message("Accessing Plugin '" + this + "'");
    }
}
