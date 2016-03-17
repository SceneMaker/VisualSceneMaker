package de.dfki.vsm.runtime.plugin;

import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public interface DEPRECATEDRunTimePluginDEPRECATED {

    // Launch the plugin
    public boolean launch(final RunTimeProject project);

    // Unload the plugin
    public boolean unload();
}
