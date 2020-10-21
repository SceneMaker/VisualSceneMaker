package de.dfki.vsm.xtension.yallah;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import io.javalin.Javalin;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class YallahExecutor extends ActivityExecutor implements ExportableProperties {

    // The properties
    private final YallahProperties mYallahProperties = new YallahProperties() ;

    // The websocket server reading messages coming from the YALLAH visualizer
    private Javalin mWebSocketServer;

    // The connection instance associated to the connected client
    private WsConnectContext mSocketConnectCtx = null;


    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();


    public YallahExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    private final static String MARKER = "$" ;

    @Override
    public String marker(long id)
    {
        return MARKER+id;
    }


    @Override
    public void launch() {
        mLogger.message("YALLAH plugin launching...");

        //
        // Setup the WebSocket server to get messages from a YALLAH application.
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));
        mLogger.message("Starting the WebSocket server on port " + port + "...");

        //mWebSocketServer = Javalin.create(config -> config.enforceSsl = true).start(port);
        mWebSocketServer = Javalin.create().start(port);

        // This sequence sets up what the websocket server must do when events occur
        mWebSocketServer.ws("/", ws -> {

            // Client Connection
            ws.onConnect(ctx -> {
                mLogger.message("Getting connection request.");
                mSocketConnectCtx = ctx;
            });

            // Client Message
            ws.onMessage(this::handleMessage);

            // Client disconnection
            ws.onClose(ctx -> {
                mSocketConnectCtx = null;

                mLogger.message("Socket closed");

                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            });

            // Server error
            ws.onError(ctx -> mLogger.failure("Error handling ws message exchange:" + ctx));
        });


        //
        //
        String launch_mode = mConfig.getProperty("launchMode") ;
        YallahProperties.LaunchMode lm = YallahProperties.LaunchMode.valueOf(launch_mode) ;
        switch (lm) {
            case None:
                break ;
            case App:
                // TODO -- launch a standalone app
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
                break;
            case WebPage:
                // TODO -- run an internal web server that provides the webpages and open them in a browser.
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
                break;
            default:
                mLogger.warning("Unrecognized launch method " + lm.toString() );
        }


        //
        mLogger.message("YALLAH launched.");
    }

    @Override
    public void unload() {
        mLogger.message("YALLAH unloading....");

        //
        // TODO -- 1. Stop the YALLAH application (Not much to do if it is a WebPage)

        // 2. Shutdown the WebSocket server
        mWebSocketServer.stop();
        mSocketConnectCtx = null ;

        mLogger.message("YALLAH unloaded.");
    }


    @Override
    public void execute(AbstractActivity activity) {

        mLogger.message("YALLAH Agent '" + activity.getActor() + "' said: " + activity.getText());
        mLogger.message("Activity name: " + activity.getName() + ", type: " + activity.getType() + ", features: " + activity.getFeatures());

        //activity.getSubstitutions().forEach((k, v) -> subs += k + ", " + v + "; ");
        if(activity.getSubstitutions() != null) {
            String subs = "";
            for (Map.Entry<String, String> e : activity.getSubstitutions().entrySet()) {
                String k = e.getKey();
                String v = e.getValue();
                subs += k + ", " + v + "; ";
            }
            mLogger.message(subs);
        } else {
            mLogger.message("No substitutions");
        }

        if (activity instanceof SpeechActivity) {
            //
            // SPEECH activity
            SpeechActivity sa = (SpeechActivity) activity;

            String punct = sa.getPunct() ;
            String text_only = sa.getTextOnly(MARKER) ;
            LinkedList<String> time_marks = sa.getTimeMarks(MARKER);

            mLogger.message("This is a Speech Activity. text only: '" + text_only + "'; punct: '" + punct + "'"
                    + "There are " + time_marks.size() + " time marks") ;
            time_marks.forEach(mLogger::message);

            //
            // Send the sentence to the client
            if(mSocketConnectCtx != null) {
                String text = activity.getText() ;

                String json_string = "{\n"
                        + "\"type\": \"text\",\n"
                        + "\"text\": \"" + text + "\"\n"
                        + "}" ;

                mSocketConnectCtx.send(json_string);
            }
        } else {
            //
            // It is an [ACTION (with parameters)]

            String cmd = activity.getName() ;
            final LinkedList<ActionFeature> features = activity.getFeatures();

            String json_string = "{\n"
                    + "\"type\": \"command\",\n"
                    + "\"command\": \"" + cmd + "\",\n"
                    + "\"parameters\": \"" ;

            // The Action "features" are actually the parameters.
            for (int i = 0, featuresSize = features.size(); i < featuresSize; i++) {
                ActionFeature af = features.get(i);

                json_string += af.getKey() + "=" + af.getVal() ;
                // Add a comma only if it is not the last element
                if (i < featuresSize-1) json_string += "," ;
            }

            json_string += "\"\n"
                    + "}\n" ;

            if(mSocketConnectCtx != null) {
                mSocketConnectCtx.send(json_string);
            }

        }

    }


    /** Invoked every time the WebSocket server receives a message */
    private void handleMessage(@NotNull WsMessageContext wsMessageContext) {
        String message = wsMessageContext.message();

        mLogger.message("YALLAH: Tell VSM activity scheduler to handle action represented by time marker >" + message + "<");
        if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(message)) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(message);
        } else {
            mLogger.failure("Marker has already been processed!");
        }


    }


    //
    // Plugin Properties
    //

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return mYallahProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return mYallahProperties.getExportableAgentProperties();
    }
}
