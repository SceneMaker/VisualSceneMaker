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
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.charamelWs.Commands.*;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author Lenny Händler, Patrick Gebhard
 */
public class charamelWsExecutor extends ActivityExecutor {
    static long sUtteranceId = 0;
    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    // The system logger
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    private Javalin mJavaLinInstance;
    private String mPathToCertificate = "";
    private String mVSMCharacterSpeakingVar = "";

    // PG: 18.11.2020 global Sceneflow variable for the whole turn information.
    private final String mVSMCharacterTurnVar = "turn_utterance";
    // PG: 25.05.2020 global sceneflow variable for the currently performed (animation) action
    private final String mVSMCharacterGestureVar = "avatar_animation";
    // PG: 28.07.2020 global Sceneflow variable for the whole turn information.
    private final String mVSMCharacterSystemVar = "avatarsystem_status";

    public charamelWsExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "${'" + id + "'}$";
    }

    public synchronized Long getVMUtteranceId() {
        return ++sUtteranceId;
    }

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
                broadcast(new TimeLine(
                        new SpeakCommand(cmd, mProject.getAgentConfig(activity_actor).getProperty("voice"), activity_actor)
                ));
                mLogger.message("Speech command with CMD markers send ...");


                // PG: 18.11.2020 let vsm model know what the current character is speaking - this is dirty. it should actually be done with messages
                if (mProject.hasVariable(mVSMCharacterTurnVar)) {
                    // Whole Turn: mProject.setVariable(mVSMCharacterTurnVar, sa.getSceneTurn().getCleanText());
                    mProject.setVariable(mVSMCharacterTurnVar, text);
                }

                // let vsm model know that character has started speaking wrt to a scene - this is dirty. it should actually be done with messages
                if ((sa.getTurnNumber() == 1) && (sa.getUtteranceNumber() == 1)) { //do this only for the first turn and first utterance, within the scene
                    if (mProject.hasVariable(mVSMCharacterSpeakingVar)) {
                        mProject.setVariable(mVSMCharacterSpeakingVar, new BooleanValue(true));
                    }
                }

                synchronized (mActivityWorkerMap) {
                    if (!websockets.isEmpty()) { //only enable blocking method if at least one connection exists.
                        // TODO: make sure it is a valid connection with a valid VuppetMaster client

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

                // let vsm model know that character is speaking - this is dirty. it should actually be done with messages
                mLogger.warning("Current turn number is " + sa.getTurnNumber() + " / " + sa.getTotalTurns());
                mLogger.warning("Current utterance number in turn is " + sa.getUtteranceNumber() + " / " + sa.getTotalUtterances());
                if ((sa.getTurnNumber() == sa.getTotalTurns()) && (sa.getUtteranceNumber() == sa.getTotalUtterances())) { // do this only after the last turn
                    if (mProject.hasVariable(mVSMCharacterSpeakingVar)) {
                        mProject.setVariable(mVSMCharacterSpeakingVar, new BooleanValue(false));
                    }
                }
            }
        } else {
            final String name = activity.getName();

            if (name.equalsIgnoreCase("test")) {
                mLogger.message("Testing ...");
                //broadcast(Strings.testMsg);
            } else if (name.equalsIgnoreCase("stop")) {
                mJavaLinInstance.stop();
            }

            parseAction(name, activity.getFeatures());

            // 25.25. pG: Assume action is an animation action - store information for  processing in global vsm model var
            if (mProject.hasVariable(mVSMCharacterGestureVar)) {
                mProject.setVariable(mVSMCharacterGestureVar, new StringValue(name));
            }
        }
    }

    private void parseAction(String name, LinkedList<ActionFeature> f) {
        switch (name) {
            case "angry": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_angry", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "bored": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_bored", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "crazy": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_crazy", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "demanding": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_demanding", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "disappointed": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_disappointed", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "disgust": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_disgust", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "happy": {
                String intensityStr = getActionFeatureValue("intensity", f);
                String attackStr = getActionFeatureValue("attack", f);
                EmotionManualConfigCommand ec = new EmotionManualConfigCommand("emot_happy", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr), (attackStr.isEmpty()) ? 200 : Integer.parseInt(attackStr));
                broadcast(ec);
                break;
            }
            case "pensively": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_pensively", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "sad": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_sad", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "smile": {
                String intensityStr = getActionFeatureValue("intensity", f);
                String attackStr = getActionFeatureValue("attack", f);
                EmotionManualConfigCommand ec = new EmotionManualConfigCommand("emot_smile", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr), (attackStr.isEmpty()) ? 200 : Integer.parseInt(attackStr));
                broadcast(ec);
                break;
            }
            case "surprised": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("emot_surprised", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "blink": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("mimic_blink", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "blow": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("mimic_blow", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "wink": {
                String intensityStr = getActionFeatureValue("intensity", f);
                EmotionCommand hc = new EmotionCommand("mimic_winkl", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
                broadcast(hc);
                break;
            }
            case "camera": {
                String posStr = getActionFeatureValue("position", f);
                CameraCommand.CameraPos pos = CameraCommand.CameraPos.valueOf(posStr);
                broadcast(new CameraCommand(pos));
                break;
            }
            case "lookat": {
                String xString = getActionFeatureValue("x", f);
                String yString = getActionFeatureValue("y", f);
                double xPos = Double.parseDouble(xString);
                double yPos = Double.parseDouble(yString);
                broadcast(new LookCommand(xPos, yPos));
                break;
            }
            case "headtilt": {
                String xString = getActionFeatureValue("xrot", f);
                String yString = getActionFeatureValue("yrot", f);
                String zString = getActionFeatureValue("zrot", f);
                double xRot = Double.parseDouble(xString);
                double yRot = Double.parseDouble(yString);
                double zRot = Double.parseDouble(zString);
                broadcast(new HeadTilt(xRot, yRot, zRot));
                break;
            }
            case "armscrossed": {
                broadcast(new TimeLine(new ArmscrossedCommand()));
                break;
            }
            case "armbewegungen": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                broadcast(new TimeLine(new ArmbewegungenCommand(direction)));
                break;
            }
            case "background": {
                String url = getActionFeatureValue("url", f);
                broadcast(new BackgroundCommand(url));
                break;
            }
            case "converse": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                new ConverseCommand(direction);
                break;
            }
            case "countleft": {
                String numberString = getActionFeatureValue("number", f);
                int number = (numberString.isEmpty()) ? 1 : Integer.parseInt(numberString);
                number = (number > 5) ? 5 : number;
                broadcast(new TimeLine(new CountLeftCommand(number)));
                break;
            }
            case "emphasis": {
                broadcast(new TimeLine(new EmphasisCommand()));
                break;
            }
            case "explain": {
                String numberString = getActionFeatureValue("number", f);
                int number = (numberString.isEmpty()) ? 1 : Integer.parseInt(numberString);
                number = (number > 4) ? 4 : number;
                broadcast(new TimeLine(new ExplainCommand(number)));
                break;
            }
            case "foldhands": {
                broadcast(new TimeLine(new FoldhandsCommand()));
                break;
            }
            case "hairback": {
                broadcast(new TimeLine(new HairbackCommand()));
                break;
            }
            case "handontable": {
                broadcast(new TimeLine(new HandontableCommand()));
                break;
            }
            case "handscircle": {
                broadcast(new TimeLine(new HandscircleCommand()));
                break;
            }
            case "handstogether": {
                broadcast(new TimeLine(new HandstogetherCommand()));
                break;
            }
            case "headshake": {
                broadcast(new TimeLine(new HeadShakeCommand()));
                break;
            }
            case "interogative": {
                broadcast(new TimeLine(new InterogativeCommand()));
                break;

            }
            case "introduce": {
                String numberString = getActionFeatureValue("number", f);
                int number = (numberString.isEmpty()) ? 1 : Integer.parseInt(numberString);
                broadcast(new TimeLine(new IntroduceCommand(number)));
                break;
            }
            case "leftfist": {
                broadcast(new TimeLine(new LeftfistCommand()));
                break;
            }
            case "legscrossed": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                broadcast(new TimeLine(new LegcrossedCommand(direction)));
                break;
            }
            case "lookatdirection": {
                String clockXString = getActionFeatureValue("clockX", f);
                String clockYString = getActionFeatureValue("clockY", f);
                int clockX = (clockXString.isEmpty()) ? 12 : Integer.parseInt(clockXString);
                int clockY = (clockYString.isEmpty()) ? 12 : Integer.parseInt(clockYString);
                clockX = (clockX > 12) ? 12 : ((clockX < 1) ? 1 : clockX);
                clockY = (clockY > 12) ? 12 : ((clockY < 1) ? 1 : clockY);
                broadcast(new TimeLine(new LookAtDirectionCommand(clockX, clockY)));
                break;
            }
            case "lookleft": {
                String steppingString = getActionFeatureValue("stepping", f);
                int stepping = (steppingString.isEmpty()) ? 20 : Integer.parseInt(steppingString);
                broadcast(new TimeLine(new LookLeftCommand(stepping)));
                break;
            }
            case "lookright": {
                String steppingString = getActionFeatureValue("stepping", f);
                int stepping = (steppingString.isEmpty()) ? 20 : Integer.parseInt(steppingString);
                broadcast(new TimeLine(new LookRightCommand(stepping)));
                break;
            }
            case "luemmeln": {
                String variantString = getActionFeatureValue("variant", f);
                int variant = (variantString.isEmpty()) ? 1 : Integer.parseInt(variantString);
                variant = (variant > 6) ? 6 : ((variant < 1) ? 1 : variant);
                broadcast(new TimeLine(new LuemmelnCommand(variant)));
                break;
            }
            case "nod": {
                broadcast(new TimeLine(new NodCommand()));
                break;
            }
            case "openarm": {
                broadcast(new TimeLine(new OpenArmCommand()));
                break;
            }
            case "openfists": {
                broadcast(new TimeLine(new OpenfistsCommand()));
                break;
            }
            case "pointopenpalm": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                broadcast(new TimeLine(new PointOpenPalmCommand(direction)));
                break;
            }
            case "pointovershoulder": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                broadcast(new TimeLine(new PointovershoulderCommand(direction)));
                break;
            }
            case "pointdownleft": {
                broadcast(new TimeLine(new PointDownLeft()));
                break;
            }
            case "pointdownright": {
                broadcast(new TimeLine(new PointDownRight()));
                break;
            }
            case "protectassertive": {
                broadcast(new TimeLine(new ProtectAssertiveCommand()));
                break;
            }
            case "protectdefensive": {
                broadcast(new TimeLine(new ProtectDefensiveCommand()));
                break;
            }
            case "shakehead": {
                broadcast(new TimeLine(new ShakeheadCommand()));
                break;
            }
            case "showpalm": {
                broadcast(new TimeLine(new ShowPalmCommand()));
                break;
            }
            case "showpalmdirection": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                broadcast(new TimeLine(new ShowPalmDirectionCommand(direction)));
                break;
            }
            case "sitbrave": {
                broadcast(new TimeLine(new SitBraveCommand()));
                break;
            }
            case "sit": {
                broadcast(new TimeLine(new SitCommand()));
                break;
            }
            case "sitnodd": {
                broadcast(new TimeLine(new SitNoddCommand()));
                break;
            }
            case "sittalk": {
                String stepString = getActionFeatureValue("step", f);
                int step = (stepString.isEmpty()) ? 1 : Integer.parseInt(stepString);
                step = (step > 12) ? 12 : ((step < 1) ? 1 : step);
                broadcast(new TimeLine(new SitTalkCommand(step)));
                break;
            }
            case "supportive": {
                broadcast(new TimeLine(new SupportiveCommand()));
                break;
            }
            case "think": {
                broadcast(new TimeLine(new ThinkingCommand()));
                break;
            }
            case "ups": {
                broadcast(new TimeLine(new UpsCommand()));
                break;
            }
            case "wave": {
                broadcast(new TimeLine(new WaveCommand()));
                break;
            }
            case "sequence": {
                String sequenceName = getActionFeatureValue("name", f);
                broadcast(new SequenceCommand(sequenceName));
                break;
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading Charamel VuppetMaster Executor (WebSocket) ...");
        final int wss_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("wss_port")));
        final int ws_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("ws_port")));
        mJavaLinInstance = createServer(wss_port, ws_port);

        setupWS();
    }

    private void setupWS() {
        final String sceneflowVar = mConfig.getProperty("sceneflowVar");
        mVSMCharacterSpeakingVar = mConfig.getProperty("characterSpeaking");

        mJavaLinInstance.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                this.addWs(ctx);
                mLogger.message("Connected to Charamel VuppetMaster");

                // let sceneflow know that a client has connected
                if (mProject.hasVariable(sceneflowVar)) {
                    mProject.setVariable(sceneflowVar, true);
                }
            });
            ws.onMessage(this::handleMessage);
            ws.onClose(ctx -> {
                this.removeWs(ctx);

                mLogger.message("Closed");
                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }

                mLogger.message("Trying to reconnect");
                setupWS();
            });
            ws.onError(ctx -> {
                mLogger.failure("Error handling ws message exchange");
            });
        });

    }


    private Javalin createServer(int wss_port, int ws_port) {
        mPathToCertificate = mConfig.getProperty("certificate");
        Javalin app;
        if (mPathToCertificate != null) {
            app = Javalin.create(config -> {
                config.server(() -> {
                    Server server = new Server();
                    ServerConnector sslConnector = null;
                    sslConnector = new ServerConnector(server, getSslContextFactory());
                    sslConnector.setPort(wss_port);
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    server.setConnectors(new Connector[]{sslConnector, connector});
                    return server;
                });
            }).start();
        } else {
            app = Javalin.create(config -> config.enforceSsl = true).start(ws_port);
        }
        return app;
    }

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
        mLogger.message("Processing Charamel VuppetMaster message: >" + message + "<");

        // status messages always contains a ":"
        if (message.contains(":")) { // status message
            processStatusMessage(message);
        } else { // time mark message
            processTimeMarkMessage(message);
        }
    }

    private void processTimeMarkMessage(String message) {
        mLogger.message("Message is a time mark action");

        //clean message
        message = message.replace("\"", "");
        message = "$" + message + "$"; // bracketing "$" are not send back from VuppetMaster

        //execute scheduled action
        mLogger.message("Tell VSM activity scheduler to handle action represented by time marker >" + message + "<");

        if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(message)) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(message);
        } else {
            mLogger.failure("Marker has already been processed!");
        }
    }

    private void processStatusMessage(String message) {
        mLogger.message("Message is related to an ongoing action");

        // clean message
        message = cleanMessage(message);

        // split header and content
        String[] parts = message.split(":");
        String header = parts[0];
        String content = parts[1];

        mLogger.message("Message header is >" + header + "<, content is >" + content + "<");

        // PG 28.07.2021: Added system information if user has allowed audio output on the webpage.
        if (header.contains("fixAudioContext")) {
            //PG 28.07.2021: Inform model about the audio output availability.
            String audioavailable = (content.contains("true")) ? "audio_available" : "audio_not_available";
            if (mProject.hasVariable(mVSMCharacterSystemVar)) {
                mProject.setVariable(mVSMCharacterSystemVar, audioavailable);
            }
        }

        // check if there the activity manager waits for an action to be finished
        if (content.equalsIgnoreCase("stop")) {

            mLogger.message("Processing stop message ...");

            synchronized (mActivityWorkerMap) {
                if (mActivityWorkerMap.containsKey(header)) {
                    mLogger.message("Removing id from active activities ids ...");
                    mActivityWorkerMap.remove(header);
                    // wake me up ...
                    mLogger.message("Unlocking activity manager ...");
                    mActivityWorkerMap.notifyAll();
                    mLogger.message("done.");
                } else {
                    mLogger.failure("Activityworker for action with id " + header + " has been stopped before ...");
                }
            }
        }
    }

    @NotNull
    private static String cleanMessage(String message) {
        message = message.replace("{", "");
        message = message.replace("}", "");
        message = message.replace("'", "");
        message = message.replace("\"", "");
        return message;
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

    private synchronized void broadcast(Broadcastable msg) {
        for (WsContext ws : websockets) {
            ws.send(msg.toJson());
        }
    }

    @Override
    public void unload() {
        websockets.clear();
        mJavaLinInstance.stop();
    }

    private SslContextFactory getSslContextFactory() {
        SslContextFactory sslContextFactory = new SslContextFactory.Server();
        sslContextFactory.setKeyStorePath(this.getClass().getResource(mPathToCertificate).toExternalForm()); //default "/my-release-key.keystore"
        sslContextFactory.setKeyStorePassword("123456");
        return sslContextFactory;
    }

    // get the value of a feature (added PG) - quick and dirty
    protected static String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("").replace("'", "");
    }
}
