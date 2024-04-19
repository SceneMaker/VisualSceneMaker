package de.dfki.vsm.xtension.SIAHomeConnection;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.eclipse.jetty.server.Server;

import java.net.InetAddress;
import java.util.LinkedList;

public class SIAHomeConnectionExecutor extends ActivityExecutor {

    private static final String sBHOME_EVENT_VAR = "bhome_event_var";
    private static final String sBHOME_EVENT_DEFAULT = "bhome_event";

    private SIAHomeConnectionServerThread server;
    private LOGDefaultLogger mLogger;

    private String bhome_event_var;

    RunTimeProject mProject;


    public SIAHomeConnectionExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        this.mProject = super.mProject;
    }
    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final LinkedList<ActionFeature> features = activity.getFeatures();

        // For simple demo we are using the following template
        for (ActionFeature feature : features) {
            String key = feature.getKey();
            String value = feature.getVal();

            // Only if key and value is not empty
            if (key != null && !key.isEmpty() && value != null && !value.isEmpty()) {
                if (key.equalsIgnoreCase("action")) {
                    // check if the action is wake up?
                    Boolean wakeUp = mProject.getValueOf("bhome_event").getValue().toString() == "woke_up";

                    System.out.println("bhome wake up: " + wakeUp);

                } else {
                    mLogger.failure("Unknown key value supplied to DialogManager plugin.");
                }
            }
        }

    }

    @Override
    public void launch() {
        bhome_event_var = mConfig.getProperty(sBHOME_EVENT_VAR, sBHOME_EVENT_DEFAULT);

        final String rlhost = mConfig.getProperty("rec_host"); // Receiver Local Host (VSM receiver host)
        final String rlport = mConfig.getProperty("rec_port"); // Receiver Local Port (VSM receiver port)

        // Initialize the event receiver
        int rlPort = Integer.parseInt(rlport);
        server = new SIAHomeConnectionServerThread(new SIAHomeConnectionJSONHandler(this), rlPort);

        // Start the server here
        try {
            server.start();
            System.out.println("-- Running Server at " + InetAddress.getLocalHost() + " The port is: " + rlport);
            server.join();
        } catch (Exception e) {
            mLogger.failure("Failed to run server: " + e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void unload() {
        server.interrupt();
        server.server.destroy();
    }
    public void set_app_intent(String event) {

        if (mProject.hasVariable("bhome_event")) {
            mLogger.message("app_intent var detected");
        }
        mLogger.message("event val in set_transcript(): " + event);
        mProject.setVariable("bhome_event", event);

    }
    
    public void setVariable(String varName, String val) {
        mProject.setVariable(varName, val);
    }

    public String getVariable() {
        return mProject.getValueOf("timeout_response").toString();
    }

}