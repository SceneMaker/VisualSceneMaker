package de.dfki.vsm.xtension.yallah;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

public class YallahExecutor extends ActivityExecutor {

    public YallahExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    @Override
    public String marker(long id)
    {
        return "$"+id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        mLogger.message("YALLAH Agent " + activity.getActor() + " said: " + activity.getText());
    }

    @Override
    public void launch() {
        //
        // TODO --  launch either the desktop app or the web page

    }

    @Override
    public void unload() {

    }
}
