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
 * @author Patrick Gebhard, Lenny Händler, Sarah Hoffmann
 * This plugin uses the Javalin web server to create a remote "Studymaster" connection over websocket
 * and serve a html/javascript interface for control. The interface source is located in the resources.
 */
public class WebStudyMasterExecutor extends ActivityExecutor {
    static final String sMSG_SEPARATOR = "#";
    static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;
    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    Receiver mMessagereceiver;
    private String mSceneflowVar;
    private Javalin app;

    /**
     * Default constructor, pass values to superclass.
     * @param config Plugin configuration, as it can be set via the extensions settings, or manually via xml.
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
            final String name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if (name.equalsIgnoreCase("stop")) {
                app.stop();
            } else {
                String sendData = encodeRequest(activity, features);
                synchronized (this) {
                    websockets.forEach(ws -> ws.send(sendData));
                }
            }
        }
    }

    /**
     *
     * @param activity the invoked command ([<command> ...]). Accepted: REQUEST
     * @param features parameters to the command ([... x="hello world"...]).
     *                 this plugin takes arguments
     *                 - time
     *                 - var: variables
     *                 - values: value(s) for every variable.
     *                 - type: types for the inputs for every variable.
     * @return String that is sent to the js client, the format is the same as in the Studymaster plugin
     */
    @NotNull
    private String encodeRequest(AbstractActivity activity, LinkedList<ActionFeature> features) {
        var mMessage = activity.getName();
        var mMessageTimeInfo = getActionFeatureValue("time", features);
        var mMessageRequestVar = getActionFeatureValue("var", features);
        var mMessageRequestValues = getActionFeatureValue("values", features);
        var mMessageRequestType = getActionFeatureValue("type", features);

        long timestamp = System.currentTimeMillis();

        if (!mMessage.equalsIgnoreCase("REQUEST")) {
            return message(mMessage, mMessageTimeInfo, timestamp);
        } else if (mMessage.equalsIgnoreCase("REQUEST")
                && (!mMessageRequestVar.isEmpty()) && (!mMessageRequestType.isEmpty()) && (!mMessageRequestValues.isEmpty())) {
            return varRequestWithValues(mMessage, mMessageRequestVar, mMessageRequestValues, mMessageRequestType, timestamp);
        } else {
            return (sMSG_HEADER + "None" + sMSG_SEPARATOR + timestamp);
        }
    }

    /**
     * Format a message to the client
     * @param mMessage Message to be displayed
     * @param mMessageTimeInfo action parameter "time"
     * @param timestamp Time the message was sent
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

    @NotNull
    private String varRequestWithValues(String mMessage, String mMessageRequestVar, String mMessageRequestValues, String mMessageRequestType, long timestamp) {
        return (
                sMSG_HEADER
                        + mMessage
                        + sMSG_SEPARATOR
                        + timestamp
                        + sMSG_SEPARATOR
                        + mMessageRequestVar.replace("'", "")
                        + sMSG_SEPARATOR
                        + mMessageRequestValues.replace("'", "")
                        + sMSG_SEPARATOR
                        + mMessageRequestType.replace("'", "")
        );
    }

    @Override
    public void launch() {
        mLogger.message("Loading StudyMaster message sender and receiver ...");
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));

        mSceneflowVar = mConfig.getProperty("variable");

        mMessagereceiver = new Receiver(this);

        app = Javalin.create(config -> {
            config.addStaticFiles("/react-studymaster/build");
            config.enforceSsl = true;
        }).start(port);
        app.ws("/ws", ws -> {
            ws.onConnect(this::addWs);
            ws.onMessage(ctx -> mMessagereceiver.handleMessage(ctx.message()));
            ws.onClose(this::removeWs);
            ws.onError(ctx -> mLogger.failure("Errored: " + ctx.error()));
        });
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        mLogger.message("closed socket");
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        mLogger.message("Connected");
        this.websockets.add(ws);
    }

    @Override
    public void unload() {
        websockets.clear();
        app.stop();
    }

    public void setSceneFlowVariable(String message) {
        mLogger.message("Assigning sceneflow variable " + mSceneflowVar + " with value " + message);
        mProject.setVariable(mSceneflowVar, new StringValue(message));
    }

    public void setSceneFlowVariable(String var, String value) {
        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
        if (mProject.hasVariable(var)) {
            mProject.setVariable(var, new StringValue(value));
        }
    }
}
