package de.dfki.vsm.xtension.DialogManager;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Chirag Bhuvaneshwara
 */
public class DialogManagerExecutor extends ActivityExecutor {

    /**
     * Default constructor, pass values to superclass.
     *
     * @param config  Plugin configuration, as it can be set via the extensions settings, or manually via xml.
     * @param project The running project the plugin is applied to.
     */
    public DialogManagerExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    DialogManagerListener client;
    private String asr_res_var;
    private static final String sASR_RES_VAR = "asr_result_var";
    private static final String sASR_RES_VAR_DEFAULT = "asr_result";


    private int port = 50000; // Fixed port at which the UDP socket is created for ASR input

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void launch() {
        DialogManagerListener client = new DialogManagerListener(port, this, true);
        client.start();
        asr_res_var = mConfig.getProperty(sASR_RES_VAR, sASR_RES_VAR_DEFAULT);
    }

    @Override
    public void unload() {
        client.killprocess();
    }

    @Override
    public void execute(AbstractActivity activity) {
//        if (activity instanceof SpeechActivity) {
//            mLogger.failure("DialogManager Plugin does not work with Scenes.");
//        } else {
//
//
//            final String action_name = activity.getName();
//            final LinkedList<ActionFeature> features = activity.getFeatures();
//            if (action_name.equals("REQUEST")) {
//
//                // Make text activity blocking
//                activity.setType(AbstractActivity.Type.blocking);
//
//            }
//        }
    }

    public void set_transcript(String asr_result) {

        if (mProject.hasVariable("asr_result")) {
            mLogger.message("asr_result var detected");
        }
        mLogger.message("ASR val in set_transcript(): " + asr_result);
        mProject.setVariable("asr_result", asr_result);
    }

    public String get_transcript() {
        return mProject.getValueOf("asr_result").toString();
    }

}
