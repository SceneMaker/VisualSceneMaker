package de.dfki.vsm.runtime.plugins;

import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public interface RunTimePlugin {

    // Launch the plugin
    public boolean launch(final RunTimeProject project);

    // Unload the plugin
    public boolean unload();
}
