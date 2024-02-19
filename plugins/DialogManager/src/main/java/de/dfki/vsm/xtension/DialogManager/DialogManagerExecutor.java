package de.dfki.vsm.xtension.DialogManager;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.LinkedList;


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
    public DialogManagerExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    DialogManagerListener listener;
    DialogManagerSender sender;

    private String asr_res_var;
    private static final String sASR_RES_VAR = "asr_result_var";
    private static final String sASR_RES_VAR_DEFAULT = "asr_result";


    private int get_port = 50000; // Fixed port at which the UDP socket is created for ASR input
    private int post_port = 50001; // Fixed port at which the UDP socket is created for sending VSM cmds to app
    private String post_address = "127.0.0.1"; // Fixed address at which the UDP socket is created for ASR input

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void launch() {
        listener = new DialogManagerListener(get_port, mLogger, this, true);
        listener.start();

        try {
            sender = new DialogManagerSender(post_port, post_address);
        } catch (SocketException | UnknownHostException e) {
            mLogger.failure("Failed to create DialogManagerSender: " + e);
        }

        asr_res_var = mConfig.getProperty(sASR_RES_VAR, sASR_RES_VAR_DEFAULT);
    }

    @Override
    public void unload() {
        listener.killprocess();
        sender.killprocess();
    }

    @Override
    public void execute(AbstractActivity activity) {

        final String action_name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatures();
        mLogger.message(features.toString());

        for (ActionFeature feature : features) {
            String key = feature.getKey();
            String value = feature.getVal();

            // Only if key and value is not empty
            if (key != null && !key.isEmpty() && value != null && !value.isEmpty()) {
                if (key.equalsIgnoreCase("action")) {
                    // Send value associated with faeture as VSM cmd to Ubidenz app
                    byte[] cmd = value.getBytes();
                    sender.SendInstruction(cmd);

                } else {
                    mLogger.failure("Unknown key value supplied to DialogManager plugin.");
                }
            }
        }
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
