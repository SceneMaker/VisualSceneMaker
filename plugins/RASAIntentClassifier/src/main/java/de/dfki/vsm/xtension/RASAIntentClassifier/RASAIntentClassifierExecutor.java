package de.dfki.vsm.xtension.RASAIntentClassifier;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.tpl.Tuple;
import org.eclipse.jetty.server.Server;

import java.net.InetAddress;

public class RASAIntentClassifierExecutor extends ActivityExecutor {

    private static final String sRASA_INTENT_VAR = "rasa_intent_var";
    private static final String sRASA_EVENT_DEFAULT = "rasa_intent";





    private Server server;

    private String rasa_intent_var;

    private RASAIntentClassifier intentClassifier;

    public RASAIntentClassifierExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }
    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String action_name = activity.getName();
        // Fetch the `asr_full` text from the VSM
        // For simple demo we are using the following template

        String asr_full = "Ja. Ja. Ja.";

        // Then call the intent classifier to send a request to the RASA classifier server
        Tuple<String, String> intent = intentClassifier.getIntent(asr_full);
    }

    @Override
    public void launch() {
        rasa_intent_var = mConfig.getProperty(sRASA_INTENT_VAR, sRASA_EVENT_DEFAULT);

        // Initialize the event receiver
        intentClassifier = new RASAIntentClassifier();
    }

    @Override
    public void unload() {

    }
}
