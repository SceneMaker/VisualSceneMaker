package de.dfki.vsm.plugins;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.plugins.RunTimePlugin;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public final class DefaultSystemPlugin implements RunTimePlugin {

    // The Singelton Instance
    public static DefaultSystemPlugin sInstance = null;
    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The player's runtime project 
    private RunTimeProject mProject;
    // The project specific config
    private PluginConfig mPluginConfig;
    // The project specific name
    private String mPluginName;

    // Get the singelton instance
    public static synchronized DefaultSystemPlugin getInstance() {
        if (sInstance == null) {
            sInstance = new DefaultSystemPlugin();
        }
        // Return The Singelton Instance
        return sInstance;
    }

    // Construct the system plugin
    private DefaultSystemPlugin() {
    }

    // Launch the system plugin
    @Override
    public boolean launch(final RunTimeProject project) {
        // Initialize the project
        mProject = project;
        // Initialize the name
        mPluginName = project.getPluginName(this);
        // Initialize the config
        mPluginConfig = project.getPluginConfig(mPluginName);
        // Print some information
        mLogger.message("Launching system plugin '" + this + "' with configuration:\n" + mPluginConfig);
        // Return true at success
        return true;
    }

    // Unload the system plugin
    @Override
    public boolean unload() {
        // Print some information
        mLogger.message("Unloading system plugin '" + this + "' with configuration:\n" + mPluginConfig);
        // Return true at success
        return true;
    }

    // Some test member function
    public void test() {
        // Print some information
        mLogger.message("Accessing system plugin '" + this + "' with configuration:\n" + mPluginConfig);
    }
}
