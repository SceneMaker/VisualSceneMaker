package de.dfki.vsm.xtension.RASAIntentClassifier;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.tpl.Tuple;
import org.eclipse.jetty.server.Server;

import java.net.InetAddress;
import java.util.LinkedList;

public class RASAIntentClassifierExecutor extends ActivityExecutor {

    public RASAIntentClassifierExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }
    private static final String sRASA_INTENT_VAR = "rasa_intent_var";
    private static final String sRASA_EVENT_DEFAULT = "rasa_intent";

    private Server server;

    private String rasa_intent_var;

    RASAIntentClassifier intentClassifier;


    @Override
    public String marker(long id) {
        return null;
    }


    @Override
    public void launch() {
        // Initialize the event receiver
        mLogger.message("[RASAIntentClassifier]: Starting RASA Agent.");
        System.out.println("[RASAIntentClassifier]: Starting RASA Agent.");
        intentClassifier = new RASAIntentClassifier();

        rasa_intent_var = mConfig.getProperty(sRASA_INTENT_VAR, sRASA_EVENT_DEFAULT);
    }

    @Override
    public void unload() {

    }
    @Override
    public void execute(AbstractActivity activity) {
        final String action_name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatures();

        // For simple demo we are using the following template
        for (ActionFeature feature : features) {
            String key = feature.getKey();
            String value = feature.getVal();

            // Only if key and value is not empty
            if (key != null && !key.isEmpty() && value != null && !value.isEmpty()) {
                if (key.equalsIgnoreCase("action")) {
                    fetchIntent();
                } else {
                    mLogger.failure("Unknown key value supplied to DialogManager plugin.");
                }
            }
        }
    }

    public void fetchIntent() {
        String asr_full = get_transcript();

        // Then call the intent classifier to send a request to the RASA classifier server
        Tuple<String, String> intent = intentClassifier.getIntent(asr_full);

        String intent_type = intent.getFirst();
        String intent_value = intent.getSecond();
        String intent_name = "";
        if (intent_type.equals("give_name")) {
            intent_name = "user_name";
            setVariable("user_name", intent_value);
        } else if (intent_type.equals("affirm") || intent_type.equals("deny")) {
            intent_name = "rasa_intent";
            setVariable("rasa_intent", intent_value);
        }
        //setVariable(intent_name, intent_value);

        System.out.println("[RASAIntentClassifier]: Message " + intent_name + " " + intent_value);
    }




    public void set_vsm_variable(String name, String value) {

        if (mProject.hasVariable(name)) {
            mLogger.message(name + "var detected");
        }
        mLogger.message(name + "in set_rasa_intent(): " + value);
        if (name.equals("user_name")) {
            mProject.setVariable("user_name", value);
        } else if (name.equals("rasa_intent")) {
            mProject.setVariable("rasa_intent", value);
        }
    }

    public void setVariable(String varName, String val) {
        mProject.setVariable(varName, val);
    }

    public String get_transcript() {
        String value = mProject.getValueOf("asr_full").getValue().toString();
        System.out.println("ASR Full: " + value);
        return value;
    }

}
