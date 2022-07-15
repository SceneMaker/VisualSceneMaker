package de.dfki.vsm.xtension.mithos;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

public class MithosExecutor extends ActivityExecutor {

    public MithosExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return null;
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