package de.dfki.vsm.xtension.SIAHomeConnection;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

public class SIAHomeConnectionExecutor extends ActivityExecutor {

    private static final String sAPP_INTENT_VAR = "app_intent_var";
    private static final String sAPP_INTENT_DEFAULT = "app_intent";

    private String app_intent_var;

    public SIAHomeConnectionExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }
    @Override
    public String marker(long id) {
        return null;
    }



    @Override
    public void execute(AbstractActivity activity) {

        final String action_name = activity.getName();
        if (action_name.equals("speak"))
            mLogger.message("intent val in set_app_intent(): " + mProject.getValueOf("app_intent").toString());

    }

    @Override
    public void launch() {
        app_intent_var = mConfig.getProperty(sAPP_INTENT_VAR, sAPP_INTENT_DEFAULT);
    }

    @Override
    public void unload() {

    }

    public void set_app_intent(String asr_result) {

        if (mProject.hasVariable("app_intent")) {
            mLogger.message("app_intent var detected");
        }
        mLogger.message("ASR val in set_transcript(): " + asr_result);
        mProject.setVariable("app_intent", asr_result);
    }

    public String get_app_intent() {
        return mProject.getValueOf("app_intent").toString();
    }
}