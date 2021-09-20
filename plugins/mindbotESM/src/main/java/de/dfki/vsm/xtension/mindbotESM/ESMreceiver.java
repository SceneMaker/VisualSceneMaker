package de.dfki.vsm.xtension.mindbotESM;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;


public class ESMreceiver extends RunTimePlugin {

    public ESMreceiver(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public void launch() {
        // TODO -- start the http server

        // TODO -- setup a REST service to get answers from the avatar

    }

    @Override
    public void unload() {
        // TODO -- stop the http server

    }
}
