/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamelWs;

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
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;

import java.util.*;

/**
 * @author Patrick Gebhard
 */
public class charamelWsExecutor extends ActivityExecutor {
    static final String sMSG_SEPARATOR = "#";
    static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;
    // The singelton logger instance
    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    private String mSceneflowVar;
    private Javalin app;
    static long sUtteranceId = 0;

    public charamelWsExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "${'" + id + "'}";
    }

    public synchronized Long getVMUtteranceId() {return ++sUtteranceId;}

    @Override
    public void execute(AbstractActivity activity) {

        final String activity_actor = activity.getActor();

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("${'").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("${'");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                // prepare for Vuppetmaster
                String vmuid = activity_actor + "_utterance_" + getVMUtteranceId();
                String cmd = "${'" + vmuid + "':'start'}$" + sa.getText() + "${'" + vmuid + "':'stop'}$";

                mLogger.message("Utterance with CMD Markers: " + cmd);

                // Make text activity blocking
                activity.setType(AbstractActivity.Type.blocking);

                // Send command object
                synchronized (mActivityWorkerMap) {
                    broadcast(Strings.speakCommand(mProject.getAgentConfig(activity_actor).getProperty("voice"), cmd));

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
                    }
                }
            }
        } else {
            final String name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if (name.equalsIgnoreCase("test")) {

                mLogger.message("Testing ...");

                broadcast(Strings.testMsg);
            } else if (name.equalsIgnoreCase("stop")) {
                app.stop();
            } else {
                var mMessage = activity.getName();
                var mMessageTimeInfo = getActionFeatureValue("time", features);
                var mMessageRequestVar = getActionFeatureValue("var", features);
                var mMessageRequestValues = getActionFeatureValue("values", features);

                long timestamp = System.currentTimeMillis();

                String sendData = (sMSG_HEADER + "None" + sMSG_SEPARATOR + timestamp);

                if (!mMessage.equalsIgnoreCase("REQUEST")) {
                    sendData = (sMSG_HEADER
                            + mMessage
                            + sMSG_SEPARATOR
                            + timestamp
                            + ((!mMessageTimeInfo.isEmpty()) ? sMSG_SEPARATOR + mMessageTimeInfo : ""));
                } else if (mMessage.equalsIgnoreCase("REQUEST")
                        && (!mMessageRequestVar.isEmpty())
                        && (!mMessageRequestValues.isEmpty())) {
                    sendData = (sMSG_HEADER
                            + mMessage
                            + sMSG_SEPARATOR
                            + timestamp
                            + sMSG_SEPARATOR
                            + mMessageRequestVar
                            + sMSG_SEPARATOR
                            + mMessageRequestValues.replace("'", ""));
                }
                synchronized (this) {
                    String finalSendData = sendData;
                    websockets.forEach(ws -> ws.send(finalSendData));
                }
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading Charamel VuppetMaster Executor (WS) ...");
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));

        app = Javalin.create(config -> config.enforceSsl = true).start(port);
        app.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                this.addWs(ctx);
                mLogger.message("Connected to Charamel VuppetMaster");
                ctx.send(Strings.launchString);
            });
            ws.onMessage(ctx -> {
                mLogger.message("Got a message from Charamel VuppetMaster...");
                handleMessage(ctx);
            });
            ws.onClose(ctx -> {
                this.removeWs(ctx);
                mLogger.message("Closed");
            });
            ws.onError(ctx -> mLogger.failure("Error handling ws message exchnage"));
        });
    }

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
        mLogger.message("Processing Charamel VuppetMaster message: >" + message + "<");

        // clean message
        message = message.replace("{", "");
        message = message.replace("}", "");
        message = message.replace("'", "");
        message = message.replace("\"", "");

        // split header and content
        if (message.contains(":")) {
            mLogger.message("Message is related to an ongoing action");
            String[] parts = message.split(":");
            String header = parts[0];
            String content = parts[1];

//            // can be used for avatar speech activity processing in sceneflow.
//            String[] headerParts = header.split("_");
//            String actor = headerParts[0];
//            String action = headerParts[1];
//            String cnt = headerParts[2];

            // check if there the activity manager waits for an action to be finished
            if (content.equalsIgnoreCase("stop")) {
                synchronized (mActivityWorkerMap) {
                    if (mActivityWorkerMap.containsKey(header)) {
                        mActivityWorkerMap.remove(header);
                    } else {
                        mLogger.failure("Activityworker for action with id " + header + " has been stopped before ...");
                    }
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            }

        } else {
            mLogger.message("Message is a timemark");
        }
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

    private synchronized void broadcast(String msg) {
        for (WsContext ws : websockets) {
            ws.send(msg);
        }
    }

    @Override
    public void unload() {
        websockets.clear();
        app.stop();
    }

    boolean hasProjectVar(String var) {
        return mProject.hasVariable(var);
    }

    void setSceneFlowVariable(String message) {
        mLogger.message("Assigning sceneflow variable " + mSceneflowVar + " with value " + message);
        mProject.setVariable(mSceneflowVar, new StringValue(message));
    }

    void setSceneFlowVariable(String var, String value) {
        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
        if (mProject.hasVariable(var)) {
            mProject.setVariable(var, new StringValue(value));
        }
    }

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("");
    }
}
