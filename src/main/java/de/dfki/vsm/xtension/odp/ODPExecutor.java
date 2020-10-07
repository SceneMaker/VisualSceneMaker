package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONObject;

import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.List;


/**
 * Created by Patrick on 11/06/19.
 */
public class ODPExecutor extends ActivityExecutor {

    //private final String variableName;
    //private final int port;

    private UdpClient client;
    private UpdServer server;

    private String clientAddress;
    private String clientText;
    private String serverAddress;
    private int clientPort;
    private int serverPort;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ODPExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public final synchronized String marker(final long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        mLogger.message("Loading ODPExecutor ...");

        clientAddress = "127.0.0.1";
        clientPort = 4445;
        serverAddress = "127.0.0.1";
        serverPort = 4446;
        //clientText = "{task:\"greeting\", voice:\"Hallo, Wie heißen Sie?\"}";

        initClient();
        initServer();
    }

    private void initClient() {
        try {
            client = new UdpClient(clientAddress, clientPort);
            mLogger.message("Initialize udp client");
        } catch (SocketException | UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private void initServer() {
        if (server == null) {
            try {
                server = new UpdServer(serverPort, mProject);
                mLogger.message("Initialize udp server");
                server.start();
            } catch (SocketException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    @Override
    public void execute(AbstractActivity activity) {
        // Get action information
        final Type activity_type = activity.getType();
        final String activity_text = activity.getText();
        final String activity_name = activity.getName();
        //final String activity_mode = activity.getMode();
        final String activity_actor = activity.getActor();
        final List activity_features = activity.getFeatures();
        // set all activities blocking
        //activity.setType(Type.blocking);

        final String name = activity.getName();
        if (name.equalsIgnoreCase("charspeak")) {
            String state = (activity.get("state") != null) ? activity.get("state") : "";

            JSONObject jsonOut = new JSONObject();
            jsonOut.put("tts", (state.equalsIgnoreCase("start")) ? "start" : "end");

            sendToOPD(jsonOut);
        }

        if (name.equalsIgnoreCase("send")) {
            String task = activity.get("task");
            // abort if no task there
            if (task == null) {
                return;
            }
            String function = (activity.get("func") != null) ? activity.get("func") : "";
            String content = (activity.get("cont") != null) ? activity.get("cont") : "";

            JSONObject jsonOut = new JSONObject();

            jsonOut.put("task", task);
            jsonOut.put("function", function);
            jsonOut.put("content", content);

            sendToOPD(jsonOut);
        }



    }

    private void sendToOPD(JSONObject jo) {
        try {
            client.clientSend(jo.toString());
        } catch (IOException e) {
            mLogger.failure(e.getMessage());
        }
    }

    @Override
    public void unload() {
        // Initialize udp client
        if (client != null) {
            client.close();
            client = null;
        }

        if (server != null) {
            server.close();
            server = null;
        }
    }
}
