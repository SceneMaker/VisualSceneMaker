package de.dfki.vsm.xtension.DialogManager;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
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

    private int port=50000;

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void launch() {
        DialogManagerListener client = new DialogManagerListener(port, this);
        client.start();
        asr_res_var = mConfig.getProperty(sASR_RES_VAR, sASR_RES_VAR_DEFAULT);
    }

    @Override
    public void unload() {
    }

    @Override
    public void execute(AbstractActivity activity) {
    }

    public void set_transcript(String asr_result) {

        if (mProject.hasVariable("asr_result")){
            System.out.println("asr_result var detected");
        }

        System.out.println("ASR val in set_transcript(): " + asr_result);

        mProject.setVariable("asr_result", asr_result);
//        System.out.println("asr_result var updated as " + mProject.getValueOf("asr_result").toString());
    }

    public String get_transcript() {
        return mProject.getValueOf("asr_result").toString();
    }

}
