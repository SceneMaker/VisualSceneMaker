/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.studymasterWeb;

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
import de.dfki.vsm.util.log.LOGConsoleLogger;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author Patrick Gebhard, Lenny Händler, Sarah Hoffmann, Fabrizio Nunnari, Chirag Bhuvaneshwara
 * This plugin uses the Javalin web server to create a remote "Studymaster" connection over websocket
 * and serve a html/javascript interface (developed with React) for control. The interface source is located in the resources.
 */
public class WebStudyMasterExecutor extends ActivityExecutor implements EventListener {

    static long sUtteranceId = 0;
    // The map of activity worker to keep track of all the active threads in the current scene
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();

    private static final String sMSG_SEPARATOR = "#";
    private static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;

    /**
     * Project variable set when the "GO" message is received.
     */
    private static final String sGO_VAR = "go_var";
    private static final String sGO_VAR_DEFAULT = "go";

    /**
     * Project variable holding the result of the user selection (SUBMIT/CANCEL)
     */
    private static final String sREQUEST_RESULT_VAR = "request_result_var";
    private static final String sREQUEST_RESULT_VAR_DEFAULT = "request_result";

    /**
     * Project variable set when the remote Web Studymaster connects via websocket.
     */
    private static final String sGUI_CONNECTED_VAR = "studymaster_connected_var";
    private static final String sGUI_CONNECTED_VAR_DEFAULT = "studymaster_connected";

    /**
     * Project variable storing the satus of the remote Web GUI .
     */
    private static final String sSTUDYMASTER_INFO_VAR = "studymaster_info";
//    private static final String sSTUDYMASTER_INFO_VAR_DEFAULT = "alive";

    /**
     * HTTP port
     */
    private static final String sJAVALIN_PORT_VAR = "port";
    private static final String sJAVALIN_PORT_VAR_DEFAULT = "8080";


    /**
     * The singleton logger instance.
     */
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    /**
     * The list of websocket connections. Yes, it will be possible to have several GUIs on the same scene flow.
     */
    private final ArrayList<WsConnectContext> mWebsockets = new ArrayList<>();
    /**
     * The Javalin HTTP server.
     */
    private Javalin mHttpServer;

    private String mRequestResultVar;

    private String mSceneflowGoVar;

    /**
     * This is the name of a Boolean global project variable that will be bet to _true_ when at least one web app is connected.
     */
    public String mGUIConnectedVar;

    /**
     * This is a "cache" of the last request sent to all connected GUIs.
     * The string contains the message already formatted to the communication protocol.
     * It will be used to send again the request to GUIs connecting after the REQUEST action was issued.
     * If null, no pending request is active.
     */
    private String mLastRequestMessage;
    private String mLastInformMessage;


    public synchronized Long getVMUtteranceId() {
        return ++sUtteranceId;
    }

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

        EventDispatcher.getInstance().register(this);

        // Retrieve property values
        mGUIConnectedVar = mConfig.getProperty(sGUI_CONNECTED_VAR, sGUI_CONNECTED_VAR_DEFAULT);
        final int http_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty(sJAVALIN_PORT_VAR, sJAVALIN_PORT_VAR_DEFAULT)));
        mRequestResultVar = mConfig.getProperty(sREQUEST_RESULT_VAR, sREQUEST_RESULT_VAR_DEFAULT);
        mSceneflowGoVar = mConfig.getProperty(sGO_VAR, sGO_VAR_DEFAULT);

        mLastInformMessage = null;
        mLastRequestMessage = null;

        // Start the HTTP server
        mHttpServer = Javalin.create(config -> {
            config.addStaticFiles("/react-studymaster/build");
            config.enforceSsl = false;
        }).start(http_port);

        // Set callbacks to manage WebSocket events
        mHttpServer.ws("/ws", ws -> {
            ws.onConnect(this::addWs);
            ws.onMessage(ctx -> this.handleGUIMessage(ctx.message()));
            ws.onClose(this::removeWs);
            ws.onError(ctx -> mLogger.failure("WebSocket Error: " + ctx.error()));
        });
    }


    @Override
    public void unload() {

        EventDispatcher.getInstance().remove(this);

        for (WsConnectContext ws : mWebsockets) {
            if (ws.session.isOpen()) {
                ws.session.close();
            }
        }
        mWebsockets.clear();
        mHttpServer.stop();

        mLastInformMessage = null;
        mLastRequestMessage = null;
    }


    /**
     * Invoked when a new websocket connection is opened (new web app is loaded).
     */
    private synchronized void addWs(WsConnectContext ws) {
        ws.session.setIdleTimeout(Long.MAX_VALUE);

        mLogger.message("New WebSocket connection made.");
//        this.mWebsockets.add(ws);
        if (!this.mWebsockets.contains(ws)) {
            // If not present, add it to the ArrayList
            this.mWebsockets.add(ws);
            mLogger.message("WebSocket added to mWebsockets");
        } else {
            mLogger.message("WebSocket already present in mWebsockets");
        }

        mLogger.message("Total number of web sockets: " + this.mWebsockets.size());

        // Update the connection status
        if (mProject.hasVariable(mGUIConnectedVar)) {
            mProject.setVariable(mGUIConnectedVar, true);
        }

        // If a request is pending, send it to the new client
        if (mLastRequestMessage != null) {
            ws.send(mLastRequestMessage);
        }
        // If an inform is pending, send it to the new client
        if (mLastInformMessage != null) {
            ws.send(mLastInformMessage);
        }

    }


    /**
     * Invoked when a websocket connection is closed (web app is closed, or timeout?).
     */
    private synchronized void removeWs(WsCloseContext ctx) {
        mLogger.message("Closed WebSocket connection.");
        mWebsockets.remove(ctx);
        mLogger.message("Total number of web sockets: " + this.mWebsockets.size());

        // Set the GUIState variable to false if there are no more GUI connections.
        if (mWebsockets.size() == 0) {
            if (mProject.hasVariable(mGUIConnectedVar)) {
                mProject.setVariable(mGUIConnectedVar, false);
            }
        }

//        mLastInformMessage = null; // In case a previous inform was still active.
//        mLastRequestMessage = null; // In case a previous request was still active.
    }


    @Override
    public void execute(AbstractActivity activity) {

        final String activity_actor = activity.getActor();

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

            String vmuid = activity_actor + "_utterance_" + getVMUtteranceId();

            final String action_name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if (action_name.equalsIgnoreCase("stop")) {
                mHttpServer.stop();
            } else if (action_name.equals("REQUEST")) {
                mLastRequestMessage = null; // In case a previous request was still active.
                try {
                    mLastRequestMessage = encodeRequest(features, vmuid);
                    synchronized (this) {
                        mWebsockets.forEach(ws -> ws.send(mLastRequestMessage));
                    }
                } catch (IllegalArgumentException e) {
                    mLogger.failure("Malformed REQUEST");
                }

                // Make text activity blocking
                activity.setType(AbstractActivity.Type.blocking);

                synchronized (mActivityWorkerMap) {
                    if (!mWebsockets.isEmpty()) { //only enable blocking method if at least one connection exists.
                        // TODO: make sure it is a valid connection with a valid Studymaster client

                        // organize wait for feedback if (activity instanceof SpeechActivity) {
                        ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                        mActivityWorkerMap.put(vmuid, cAW);

                        if (activity.getType() == AbstractActivity.Type.blocking) { // Wait only if activity is blocking
                            // wait until we got feedback
                            mLogger.message("ActivityWorker waiting for feedback on action with id " + vmuid + "...");
                            while (mActivityWorkerMap.containsValue(cAW)) {
                                try {
                                    mActivityWorkerMap.wait();
                                } catch (InterruptedException exc) {
                                    mLogger.failure(exc.toString());
                                }
                            }
                            mLogger.message("ActivityWorker proceed - got feedback on blocking action with id " + vmuid + "...");
                        } else {
                            mLogger.message("ActivityWorker does not feedback on action with id " + vmuid + " since action is non-blocking ...");
                        }
                    } else {
                        mLogger.warning("Blocking action command was send to nowhere. Executor will not wait. ");
                    }
                }

            } else if (action_name.equals("PROCEED")) {
                mLastInformMessage = null; // In case a previous inform was still active.
                try {
                    mLastInformMessage = encodeProceed(features, vmuid);
                    synchronized (this) {
                        mWebsockets.forEach(ws -> ws.send(mLastInformMessage));
                    }
                } catch (IllegalArgumentException e) {
                    mLogger.failure("Malformed PROCEED");
                }

                // Make text activity blocking
                activity.setType(AbstractActivity.Type.blocking);

                synchronized (mActivityWorkerMap) {
                    if (!mWebsockets.isEmpty()) { //only enable blocking method if at least one connection exists.
                        // TODO: make sure it is a valid connection with a valid Studymaster client

                        // organize wait for feedback if (activity instanceof SpeechActivity) {
                        ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                        mActivityWorkerMap.put(vmuid, cAW);

                        if (activity.getType() == AbstractActivity.Type.blocking) { // Wait only if activity is blocking
                            // wait until we got feedback
                            mLogger.message("ActivityWorker waiting for feedback on action with id " + vmuid + "...");
                            while (mActivityWorkerMap.containsValue(cAW)) {
                                try {
                                    mActivityWorkerMap.wait();
                                } catch (InterruptedException exc) {
                                    mLogger.failure(exc.toString());
                                }
                            }
                            mLogger.message("ActivityWorker proceed - got feedback on blocking action with id " + vmuid + "...");
                        } else {
                            mLogger.message("ActivityWorker does not feedback on action with id " + vmuid + " since action is non-blocking ...");
                        }
                    } else {
                        mLogger.warning("Blocking action command was send to nowhere. Executor will not wait. ");
                    }
                }

            } else if (action_name.equals("INFORM")) {
                mLastInformMessage = null; // In case a previous inform was still active.
                try {
                    mLastInformMessage = encodeInform(features);
                    synchronized (this) {
                        mWebsockets.forEach(ws -> ws.send(mLastInformMessage));
                    }
                } catch (IllegalArgumentException e) {
                    mLogger.failure("Malformed INFORM");
                }

            } else {
                mLogger.warning("Unknown action '" + action_name + "'");
            }
        }
    }


    /**
     * Given the features of a REQUEST action, convert it into a string in the protocol format.
     *
     * @param features parameters to the command ([... x="hello world"...]).
     *                 this plugin takes arguments
     *                 - time
     *                 - var: variables
     *                 - value: value(s) for every variable.
     *                 - type: types for the inputs for every variable.
     * @return String that is sent to the js client, the format is the same as in the Studymaster plugin
     */
    @NotNull
    private String encodeRequest(LinkedList<ActionFeature> features, String vm_uid) throws IllegalArgumentException {
        String varRequest = getActionFeatureValue("var", features);
        String valueRequest = getActionFeatureValue("value", features);
        String typeRequest = getActionFeatureValue("type", features);

        long timestamp = System.currentTimeMillis();

        if ((!varRequest.isEmpty()) && (!typeRequest.isEmpty()) && (!valueRequest.isEmpty())) {
            return sMSG_HEADER
                    + "REQUEST"
                    + sMSG_SEPARATOR
                    + timestamp
                    + sMSG_SEPARATOR
                    + varRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + valueRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + typeRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + vm_uid
                    ;

        } else {
            throw new IllegalArgumentException("REQUEST message malformed");
        }
    }

    /**
     * Given the features of an INFORM action, convert it into a string in the protocol format.
     *
     * @param features parameters to the command ([... x="hello world"...]).
     *                 this plugin takes arguments
     *                 - time
     *                 - var: variables
     *                 - value: value(s) for every variable.
     *                 - type: types for the inputs for every variable.
     * @return String that is sent to the js client to provide information in the INFORM box of the Web GUI.
     */
    private String encodeInform(LinkedList<ActionFeature> features) throws IllegalArgumentException {
        String varRequest = "information";
        String valueRequest = getActionFeatureValue("value", features);
        String typeRequest = "text";

        long timestamp = System.currentTimeMillis();

        if ((!varRequest.isEmpty()) && (!typeRequest.isEmpty()) && (!valueRequest.isEmpty())) {
            return sMSG_HEADER
                    + "INFORM"
                    + sMSG_SEPARATOR
                    + timestamp
                    + sMSG_SEPARATOR
                    + varRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + valueRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + typeRequest.replace("'", "");

        } else {
            throw new IllegalArgumentException("INFORM message malformed");
        }
    }

    /**
     * Given the features of an INFORM action, convert it into a string in the protocol format.
     *
     * @param features parameters to the command ([... x="hello world"...]).
     *                 this plugin takes arguments
     *                 - time
     *                 - var: variables
     *                 - value: value(s) for every variable.
     *                 - type: types for the inputs for every variable.
     * @return String that is sent to the js client to provide information in the INFORM box of the Web GUI.
     */
    private String encodeProceed(LinkedList<ActionFeature> features, String vm_uid) throws IllegalArgumentException {
        String varRequest = "information";
        String valueRequest = getActionFeatureValue("value", features);
        String typeRequest = "text";

        long timestamp = System.currentTimeMillis();

        if ((!varRequest.isEmpty()) && (!typeRequest.isEmpty()) && (!valueRequest.isEmpty())) {
            return sMSG_HEADER
                    + "PROCEED"
                    + sMSG_SEPARATOR
                    + timestamp
                    + sMSG_SEPARATOR
                    + varRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + valueRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + typeRequest.replace("'", "")
                    + sMSG_SEPARATOR
                    + vm_uid
                    ;

        } else {
            throw new IllegalArgumentException("INFORM message malformed");
        }
    }


    /**
     * Invoked when a websocket connection gets a message from the web app.
     */
    public void handleGUIMessage(String message) {
        if (message.startsWith(sMSG_HEADER)) {

            // parse message
            String[] msgParts = message.split(sMSG_SEPARATOR);

            if (msgParts.length > 1) {
                String msg = msgParts[1];

                // MESSAGE VAR: VSMMessage#VAR#<var>#<value>#<VSM_THREAD_ID>
                if (msg.equals("VAR")) {
                    String var = msgParts[2];
                    String value = msgParts[3];

                    // If we received the user choice (submit/cancel)
                    if (var.equals(sREQUEST_RESULT_VAR_DEFAULT)) {
                        // Reassign the variable name to the one defined in the properties
                        var = mRequestResultVar;
                        // reset the request cache
                        mLastRequestMessage = null;
                        // reset the inform cache
                        mLastInformMessage = null;

                        mLogger.message("Processing " + msgParts[3] + " message ...");
                        String vm_uid_frm_client = msgParts[4];
                        mLogger.message("Unlocking thread " + vm_uid_frm_client);

                        synchronized (mActivityWorkerMap) {
                            if (mActivityWorkerMap.containsKey(vm_uid_frm_client)) {
                                mLogger.message("Removing id from active activities ids ...");
                                mActivityWorkerMap.remove(vm_uid_frm_client);
                                // wake me up ...
                                mLogger.message("Unlocking activity manager ...");
                                mActivityWorkerMap.notifyAll();
                                mLogger.message("done.");
                            } else {
                                mLogger.failure("Activityworker for action with id " + vm_uid_frm_client +
                                        " has been stopped before ...");
                            }
                        }
                    }


                    if (mProject.hasVariable(var)) {
//                        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
                        mProject.setVariable(var, new StringValue(value));

                    } else {
                        mLogger.warning("Can't assign sceneflow variable " + var + " with value " + value + ": global project variable not defined");
                    }

                    // MESSAGE GO: VSMMessage#Go
                } else if (msg.equals("Go")) {
//                    mLogger.message("Assigning sceneflow variable " + mSceneflowGoVar + " with value " + message);
                    mProject.setVariable(mSceneflowGoVar, true);

                }
                // MESSAGE STATUS: VSMMessage#STATUS
                else if (msg.equals("STATUS")) {
                    String value = msgParts[2];
//                    mLogger.message("Assigning sceneflow variable " + sSTUDYMASTER_INFO_VAR + " with value " + value);

                    if (mProject.hasVariable(sSTUDYMASTER_INFO_VAR)) {
                        mProject.setVariable(sSTUDYMASTER_INFO_VAR, value);
                    }

                    try {
                        String PongMessageToClient = "VSMMessage#STATUS#alive#okay";
                        synchronized (this) {
                            mWebsockets.forEach(ws -> ws.send(PongMessageToClient));
                        }
                    } catch (Exception e) {
                        mLogger.failure("Unknown error: " + e);
                    }

                } else {
                    // MESSAGE UNKNOWN!!!
                    mLogger.warning("Unsupported message '" + msg + "' received.");
                }

            } else {
                mLogger.warning("Message malformed: no proper separation with '" + sMSG_SEPARATOR + "'");
            }

        } else {
            mLogger.warning("Message malformed: no header '" + sMSG_HEADER + "'");
        }
    }

    @Override
    public void update(EventObject event) {
        if (event instanceof VariableChangedEvent) {
            VariableChangedEvent ev = (VariableChangedEvent) event;
            ev.getVarValue();
            String info = event.toString();
            info = info.split("\\(")[1].split(",")[0] + "#" + info.split("#c#")[1].split("\\)")[0];
            String varListUpdatesMessage = "VSMMessage#UPDATE#" + info.replace("'", "");

            try {
                mWebsockets.forEach(websocks -> websocks.send(varListUpdatesMessage));
            } catch (Exception e) {
                mLogger.failure(e.toString());
            }
        }
    }
}
