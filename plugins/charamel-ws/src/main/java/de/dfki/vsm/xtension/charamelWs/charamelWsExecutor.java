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
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

/**
 * @author Patrick Gebhard
 */
public class charamelWsExecutor extends ActivityExecutor {
    static final String sMSG_SEPARATOR = "#";
    static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    private String mSceneflowVar;
    private Javalin app;

    public charamelWsExecutor(PluginConfig config, RunTimeProject project) {
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
                var mMessage = activity.getName();
                var mMessageTimeInfo = getActionFeatureValue("time", features);
                var mMessageRequestVar = getActionFeatureValue("var", features);
                var mMessageRequestValues = getActionFeatureValue("values", features);

                long timestamp = System.currentTimeMillis();

                String sendData = (sMSG_HEADER + "None" + sMSG_SEPARATOR + timestamp);

                if (!mMessage.equalsIgnoreCase("REQUEST")) {
                    sendData = (sMSG_HEADER + mMessage + sMSG_SEPARATOR + timestamp + ((!mMessageTimeInfo.isEmpty()) ? sMSG_SEPARATOR + mMessageTimeInfo : ""));
                } else if (mMessage.equalsIgnoreCase("REQUEST") && (!mMessageRequestVar.isEmpty()) && (!mMessageRequestValues.isEmpty())) {
                    sendData = (sMSG_HEADER + mMessage + sMSG_SEPARATOR + timestamp + sMSG_SEPARATOR + mMessageRequestVar + sMSG_SEPARATOR + mMessageRequestValues.replace("'", ""));
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
        mLogger.message("Loading StudyMaster message sender and receiver ...");
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));


        app = Javalin.create(config -> {
            config.enforceSsl = true;
        }).start(port);
        app.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                this.addWs(ctx);
                System.out.println("Connected");
                ctx.send("{\n" +
                        "  \"type\": \"animation\",\n" +
                        "  \"name\": \"\",\n" +
                        "  \"uuid\": \"58a11974-146c-4e3a-ab47-b180922cdec9\",\n" +
                        "  \"timeline\": [\n" +
                        "    {\n" +
                        "      \"type\": \"timeline-element\",\n" +
                        "      \"subtype\": \"tts\",\n" +
                        "      \"uuid\": \"606fee1b-ad84-406d-9667-bd5822291166\",\n" +
                        "      \"name\": \"hello\",\n" +
                        "      \"track\": \"uuid_tts\",\n" +
                        "      \"timestamp\": 200,\n" +
                        "      \"duration\": 5000,\n" +
                        "      \"data\": {\n" +
                        "        \"text\": \"Hello,my name is Gloria\"\n" +
                        "      }\n" +
                        "    },\n" +
                        "    {\n" +
                        "      \"type\": \"timeline-element\",\n" +
                        "      \"subtype\": \"motion\",\n" +
                        "      \"uuid\": \"d291dacd-d53d-4968-b425-cfaa9e44a6cc\",\n" +
                        "      \"name\": \"greet01.glb\",\n" +
                        "      \"track\": \"motions1_uuid\",\n" +
                        "      \"timestamp\": 0,\n" +
                        "      \"duration\": 2400,\n" +
                        "      \"data\": {\n" +
                        "        \"attack\": 500,\n" +
                        "        \"decay\": 500,\n" +
                        "        \"speed\": 1,\n" +
                        "        \"path\": \"humanoid/interaction/greet/greet01.glb\"\n" +
                        "      }\n" +
                        "    },\n" +
                        "    {\n" +
                        "      \"type\": \"timeline-element\",\n" +
                        "      \"subtype\": \"morph\",\n" +
                        "      \"uuid\": \"10536d63-fc88-4183-aed5-735ea9170da2\",\n" +
                        "      \"name\": \"emot_happy\",\n" +
                        "      \"track\": \"morph_uuid\",\n" +
                        "      \"timestamp\": 600,\n" +
                        "      \"duration\": 2000,\n" +
                        "      \"data\": {\n" +
                        "        \"morph\": \"uuid_emot_happy\",\n" +
                        "        \"attack\": 500,\n" +
                        "        \"decay\": 500\n" +
                        "      }\n" +
                        "    }\n" +
                        "  ]\n" +
                        "}");
            });
            ws.onMessage(ctx -> {
                System.out.println(ctx.message());
            });
            ws.onClose(ctx -> {
                this.removeWs(ctx);
                System.out.println("Closed");
            });
            ws.onError(ctx -> System.out.println("Errored"));
        });
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

    @Override
    public void unload() {
        websockets.clear();
        app.stop();
    }

    public boolean hasProjectVar(String var) {
        return mProject.hasVariable(var);
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

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("");
    }
}
