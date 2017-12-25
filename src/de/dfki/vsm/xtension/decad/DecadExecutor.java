package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;


public class DecadExecutor extends ActivityExecutor{

    public static final String DECAD_MARKER_SEPARATOR = "#";

    public DecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }



    @Override
    public String marker(long id) {
        return DECAD_MARKER_SEPARATOR + id + DECAD_MARKER_SEPARATOR;
    }

    @Override
    public void execute(AbstractActivity activity) {

    }

    @Override
    public void launch() {

    }

    @Override
    public void unload() {

    }

}
