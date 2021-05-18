/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.studymasterWeb;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Objects;

/**
 * @author Patrick Gebhard, Lenny Händler, Sarah Hoffmann, Fabrizio Nunnari
 * This plugin uses the Javalin web server to create a remote "Studymaster" connection over websocket
 * and serve a html/javascript interface for control. The interface source is located in the resources.
 */
public class WebStudyMasterExecutor extends ActivityExecutor {

    private static final String sMSG_SEPARATOR = "#";
    private static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;

    /** The name of the project variable holding the result of the user selection (SUBMIT/CANCEL) */
    private static final String sREQUEST_RESULT_VAR = "request_result" ;

    private static final String sGUI_CONNECTED_VAR_DEFAULT = "gui_connected" ;
    private static final String sJAVALIN_PORT_DEFAULT = "8080" ;


    /** The singleton logger instance.*/
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    /** The list of websocket connections. Yes, it will be possible to have several GUIs on the same scene flow.*/
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    /** The Javalin HTTP server. */
    private Javalin httpServer;


    private String mSceneflowVar;


    /** This is the name of a Boolean global project variable that will be bet to _true_ when at least one web app is connected.*/
    public String mGUIConnectedVar = "";

    /** This is a "cache" of the last request sent to all connected GUIs.
     * The string contains the message already formatted to the communication protocol.
     * It will be used to send again the request to GUIs connecting after the REQUEST action was issued.
     * If null, no pending request is active.
     */
    private String mLastRequestMessage = null;

    /**
     * Default constructor, pass values to superclass.
     *
     * @param config  Plugin configuration, as it can be set via the extensions settings, or manually via xml.
     * @param project The running project the plugin is applied to.
     */
    public WebStudyMasterExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }


    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }


    @Override
    public void launch() {
        mLogger.message("Loading StudyMaster message sender and receiver ...");

        mGUIConnectedVar = mConfig.getProperty("GUIStateVar", sGUI_CONNECTED_VAR_DEFAULT);
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port", sJAVALIN_PORT_DEFAULT)));

        mSceneflowVar = mConfig.getProperty("variable");

        // Start the HTTP server
        httpServer = Javalin.create(config -> {
            config.addStaticFiles("/react-studymaster/build");
            config.enforceSsl = true;
        }).start(port);

        // Set callbacks to manage WebSocket events
        httpServer.ws("/ws", ws -> {
            ws.onConnect(this::addWs);
            ws.onMessage(ctx -> this.handleGUIMessage(ctx.message()));
            ws.onClose(this::removeWs);
            ws.onError(ctx -> mLogger.failure("Errored: " + ctx.error()));
        });
    }


    @Override
    public void unload() {
        for (WsConnectContext ws: websockets) {
            if (ws.session.isOpen()) {
                ws.session.close();
            }
        }
        websockets.clear();
        httpServer.stop();
    }


    /** Invoked when a new websocket connection is opened (new web app is loaded).*/
    private synchronized void addWs(WsConnectContext ws) {
        mLogger.message("New WebSocket connection.");
        this.websockets.add(ws);

        // Update the connection status
        if (mProject.hasVariable(mGUIConnectedVar)) {
            mProject.setVariable(mGUIConnectedVar, true);
        }

        // If a request is pending, send it to the new client
        if(mLastRequestMessage != null) {
            ws.send(mLastRequestMessage) ;
        }

    }


    /** Invoked when a websocket connection is closed (web app is closed, or timeout?).*/
    private synchronized void removeWs(WsCloseContext ctx) {
        mLogger.message("Closed WebSocket connection.");
        websockets.remove(ctx);

        // Set the GUIState variable to false if there are no more GUI connections.
        if(websockets.size()==0) {
            if (mProject.hasVariable(mGUIConnectedVar)) {
                mProject.setVariable(mGUIConnectedVar, false);
            }

        }
    }



    @Override
    public void execute(AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String action_name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if (action_name.equalsIgnoreCase("stop")) {
                httpServer.stop();
            } else if (action_name.equals("REQUEST")) {
                mLastRequestMessage = null ; // In case a previous request was still active.
                try {
                    mLastRequestMessage = encodeRequest(activity, features);
                    synchronized (this) {
                        websockets.forEach(ws -> ws.send(mLastRequestMessage));
                    }
                } catch (IllegalArgumentException e) {
                    mLogger.failure("Malformed REQUEST");
                }
            } else {
                mLogger.warning("Unknown action '" + action_name + "'");
            }
        }
    }


    /** Given the activity and the feastures of a REQUEST action, convert it into a string in the protocol format.
     *
     * @param activity the invoked command ([<command> ...]). Accepted: REQUEST
     * @param features parameters to the command ([... x="hello world"...]).
     *                 this plugin takes arguments
     *                 - time
     *                 - var: variables
     *                 - value: value(s) for every variable.
     *                 - type: types for the inputs for every variable.
     * @return String that is sent to the js client, the format is the same as in the Studymaster plugin
     */
    @NotNull
    private String encodeRequest(AbstractActivity activity, LinkedList<ActionFeature> features) throws IllegalArgumentException {
        // var mMessage = activity.getName();
        var varRequest = getActionFeatureValue("var", features);
        var valuesRequest = getActionFeatureValue("values", features);
        var typeRequest = getActionFeatureValue("type", features);

        long timestamp = System.currentTimeMillis();

        if ((!varRequest.isEmpty()) && (!typeRequest.isEmpty()) && (!valuesRequest.isEmpty())) {
            return sMSG_HEADER
                    + "REQUEST"
                    + sMSG_SEPARATOR
                    + timestamp
                    + sMSG_SEPARATOR
                    + varRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + valuesRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + typeRequest.replace("'", "") ;

//      } else if (!mMessage.equalsIgnoreCase("REQUEST")) {
//            return message(mMessage, mMessageTimeInfo, timestamp);
        } else {
            throw new IllegalArgumentException("REQUEST message malformed") ;
        }
    }

    /**
     * Format a message to the client
     *
     * @param mMessage         Message to be displayed
     * @param mMessageTimeInfo action parameter "time"
     * @param timestamp        Time the message was sent
     * @return Message in Studymaster format
     */
    @NotNull
    private String message(String mMessage, String mMessageTimeInfo, long timestamp) {
        return (
                sMSG_HEADER
                        + mMessage
                        + sMSG_SEPARATOR
                        + timestamp
                        + ((!mMessageTimeInfo.isEmpty()) ? sMSG_SEPARATOR + mMessageTimeInfo : ""));
    }


    /** Invoked when a websocket connection gets a message from the web app.*/
    public void handleGUIMessage(String message) {
        if (message.startsWith(sMSG_HEADER)) {

            // parse message
            String[] msgParts = message.split(sMSG_SEPARATOR);

            if (msgParts.length > 1) {
                // String msgHeader = msgParts[0];
                String msg = msgParts[1];

                // MESSAGE VAR: VSMMessage#VAR#<var>#<value>
                if (msg.equalsIgnoreCase("VAR")) {
                    String var = msgParts[2];
                    String value = msgParts[3];

                    if (mProject.hasVariable(var)) {
                        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
                        mProject.setVariable(var, new StringValue(value));

                        // If we received the user choice, reset the request cache
                        if(var.equals(sREQUEST_RESULT_VAR)) {
                            mLastRequestMessage = null ;
                        }
                    } else {
                        mLogger.warning("Can't assign sceneflow variable " + var + " with value " + value + ": global project variable not defined");
                    }

                // MESSAGE GO: VSMMessage#Go
                } else if(msg.equalsIgnoreCase("GO")) {
                    mLogger.message("Assigning sceneflow variable " + mSceneflowVar + " with value " + message);
                    mProject.setVariable(mSceneflowVar, true);

                // MESSAGE UNKNOWN!!!
                } else {
                    mLogger.warning("Unsupported message '" + msg + "' received.");
                }

            } else {
                mLogger.warning("Message malformed: no proper separation with '" + sMSG_SEPARATOR + "'");
            }

        } else {
            mLogger.warning("Message malformed: no header '" + sMSG_HEADER + "'");
        }
    }
}
