package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * This wraps the MindBotSSIPlugin, since we cannot inherit ActivityExecutor in it
 */
class MindBotExecutor extends ActivityExecutor {

    private MindBotSSIPlugin mPlugin;

    public MindBotExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        this.mPlugin = new MindBotSSIPlugin(config, project);
    }

    @Override
    public String marker(long id) {
        return "$"+id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity.getName().equals("ssi start_recording")) {
            mPlugin.sendStart();
        }

        else if(activity.getName().equals("ssi stop_recording")) {
            mPlugin.sendStop();
        }
    }




    @Override
    public void launch() {
        mPlugin.launch();
    }

    @Override
    public void unload() {
        mPlugin.unload();
    }
}