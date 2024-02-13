package de.dfki.vsm.xtension.IntentClassifier;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.eclipse.jetty.server.Server;

import java.net.InetAddress;

public class IntentClassifierExecutor extends ActivityExecutor {

    private static final String sBHOME_EVENT_VAR = "bhome_event_var";
    private static final String sBHOME_EVENT_DEFAULT = "bhome_event";

    private Server server;

    private String bhome_event_var;

    public IntentClassifierExecutor(PluginConfig config, RunTimeProject project) {
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
        bhome_event_var = mConfig.getProperty(sBHOME_EVENT_VAR, sBHOME_EVENT_DEFAULT);

        final String rlhost = mConfig.getProperty("rec_host"); // Receiver Local Host (VSM receiver host)
        final String rlport = mConfig.getProperty("rec_port"); // Receiver Local Port (VSM receiver port)

        // Initialize the event receiver
        server = new Server(Integer.parseInt(rlport));
        server.setHandler(new IntentClassifierJSONHandler(this));

        // Start the server here
        try {
            server.start();
            System.out.println("-- Running Intent Classifier Server at " + InetAddress.getLocalHost() + " The port is: " + rlport);
            server.join();
        } catch (Exception e) {
            mLogger.failure("Failed to run server: " + e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void unload() {

    }
//public void set_app_intent(String asr_result) {
//
//        if (mProject.hasVariable("app_intent")) {
//            mLogger.message("app_intent var detected");
//        }
//        mLogger.message("ASR val in set_transcript(): " + asr_result);
//        mProject.setVariable("app_intent", asr_result);
//    }
//
//    public String get_app_intent() {
//        return mProject.getValueOf("app_intent").toString();
//    }
}