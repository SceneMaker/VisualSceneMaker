package de.dfki.vsm.xtension.charamelWs;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.charamelWs.Commands.*;
import de.dfki.vsm.xtension.util.communication.ReceiveSenderPort;
import de.dfki.vsm.xtension.util.plugin.AgentPlugin;
import de.dfki.vsm.xtension.util.runtime.DrivenRuntime;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

import static de.dfki.vsm.runtime.activity.executor.ActivityExecutor.getActionFeatureValue;

public class Charamel implements AgentPlugin {
    static long sUtteranceId = 0;
    private final ReceiveSenderPort<String, String> receiveSenderPort;
    private final LOGConsoleLogger mLogger;
    private final DrivenRuntime runtime;
    private final String mVSMCharacterTurnVar = "turn_utterance";
    // PG: 25.05.2020 global sceneflow variable for the currently performed (animation) action
    private final String mVSMCharacterGestureVar = "avatar_animation";
    // PG: 28.07.2020 global Sceneflow variable for the whole turn information.
    private final String mVSMCharacterSystemVar = "avatarsystem_status";
    private Map<String, AgentPlugin> actors;
    private String mVSMCharacterSpeakingVar = "";


    public Charamel(ReceiveSenderPort<String, String> receiveSenderPort, String VSMCharacterSpeakingVar, LOGConsoleLogger mLogger, DrivenRuntime runtime) {
        this.mLogger = mLogger;
        this.mVSMCharacterSpeakingVar = VSMCharacterSpeakingVar;
        this.receiveSenderPort = receiveSenderPort;
        this.runtime = runtime;
        receiveSenderPort.registerMessageHandler(this::handleMessage);
        receiveSenderPort.registerMessageHandler(this::handleMessage);
    }

    @NotNull
    static String cleanMessage(String message) {
        message = message.replace("{", "");
        message = message.replace("}", "");
        message = message.replace("'", "");
        message = message.replace("\"", "");
        return message;
    }

    public Broadcastable parseAction(String name, List<ActionFeature> f) {
        switch (name) {
            case "angry": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_angry", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "bored": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_bored", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "crazy": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_crazy", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "demanding": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_demanding", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "disappointed": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_disappointed", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "disgust": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return new EmotionCommand("emot_disgust", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr));
            }
            case "happy": {
                String manualOperationStr = getActionFeatureValue("manual", f);
                manualOperationStr = (manualOperationStr.isEmpty()) ? "false" : manualOperationStr;
                boolean manual = Boolean.parseBoolean(manualOperationStr);

                String intensityStr = getActionFeatureValue("intensity", f);
                String attackStr = getActionFeatureValue("attack", f);

                float intensity = (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr);
                int attack = (attackStr.isEmpty()) ? 200 : Integer.parseInt(attackStr);
                if (manual) {
                    return (new EmotionManualConfigCommand("emot_happy", intensity, attack));
                } else {
                    String holdStr = getActionFeatureValue("hold", f);
                    return (new EmotionCommand("emot_happy", intensity, attack, (holdStr.isEmpty()) ? 1000 : Integer.parseInt(holdStr)));
                }

            }
            case "pensively": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("emot_pensively", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "sad": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("emot_sad", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "smile": {
                String manualOperationStr = getActionFeatureValue("manual", f);
                manualOperationStr = (manualOperationStr.isEmpty()) ? "false" : manualOperationStr;
                boolean manual = Boolean.parseBoolean(manualOperationStr);

                String intensityStr = getActionFeatureValue("intensity", f);
                String attackStr = getActionFeatureValue("attack", f);

                float intensity = (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr);
                int attack = (attackStr.isEmpty()) ? 200 : Integer.parseInt(attackStr);
                if (manual) {
                    return (new EmotionManualConfigCommand("emot_smile", intensity, attack));
                } else {
                    String holdStr = getActionFeatureValue("hold", f);
                    return (new EmotionCommand("emot_smile", intensity, attack, (holdStr.isEmpty()) ? 1000 : Integer.parseInt(holdStr)));
                }
            }
            case "surprised": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("emot_surprised", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "blink": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("mimic_blink", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "blow": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("mimic_blow", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "wink": {
                String intensityStr = getActionFeatureValue("intensity", f);
                return (new EmotionCommand("mimic_winkl", (intensityStr.isEmpty()) ? 0.7f : Float.parseFloat(intensityStr)));
            }
            case "camera": {
                String posStr = getActionFeatureValue("position", f);
                CameraCommand.CameraPos pos = CameraCommand.CameraPos.valueOf(posStr);
                return (new CameraCommand(pos));
            }
            case "lookat": {
                String xString = getActionFeatureValue("x", f);
                String yString = getActionFeatureValue("y", f);
                double xPos = Double.parseDouble(xString);
                double yPos = Double.parseDouble(yString);
                return (new LookCommand(xPos, yPos));
            }
            case "headtilt": {
                String xString = getActionFeatureValue("xrot", f);
                String yString = getActionFeatureValue("yrot", f);
                String zString = getActionFeatureValue("zrot", f);
                double xRot = Double.parseDouble(xString);
                double yRot = Double.parseDouble(yString);
                double zRot = Double.parseDouble(zString);
                return (new HeadTilt(xRot, yRot, zRot));
            }
            case "armscrossed": {
                return (new TimeLine(new ArmscrossedCommand()));
            }
            case "armbewegungen": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                return (new TimeLine(new ArmbewegungenCommand(direction)));
            }
            case "background": {
                String url = getActionFeatureValue("url", f);
                return (new BackgroundCommand(url));
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
                number = Math.min(number, 5);
                return (new TimeLine(new CountLeftCommand(number)));
            }
            case "emphasis": {
                return (new TimeLine(new EmphasisCommand()));
            }
            case "explain": {
                String numberString = getActionFeatureValue("number", f);
                int number = (numberString.isEmpty()) ? 1 : Integer.parseInt(numberString);
                number = Math.min(number, 4);
                return (new TimeLine(new ExplainCommand(number)));
            }
            case "foldhands": {
                return (new TimeLine(new FoldhandsCommand()));
            }
            case "hairback": {
                return (new TimeLine(new HairbackCommand()));
            }
            case "handontable": {
                return (new TimeLine(new HandontableCommand()));
            }
            case "handscircle": {
                return (new TimeLine(new HandscircleCommand()));
            }
            case "handstogether": {
                return (new TimeLine(new HandstogetherCommand()));
            }
            case "headshake": {
                return (new TimeLine(new HeadShakeCommand()));
            }
            case "interogative": {
                return (new TimeLine(new InterogativeCommand()));
            }
            case "introduce": {
                String numberString = getActionFeatureValue("number", f);
                int number = (numberString.isEmpty()) ? 1 : Integer.parseInt(numberString);
                return (new TimeLine(new IntroduceCommand(number)));
            }
            case "leftfist": {
                return (new TimeLine(new LeftfistCommand()));
            }
            case "legscrossed": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                return (new TimeLine(new LegcrossedCommand(direction)));
            }
            case "lookatdirection": {
                String clockXString = getActionFeatureValue("clockX", f);
                String clockYString = getActionFeatureValue("clockY", f);
                int clockX = (clockXString.isEmpty()) ? 12 : Integer.parseInt(clockXString);
                int clockY = (clockYString.isEmpty()) ? 12 : Integer.parseInt(clockYString);
                clockX = (clockX > 12) ? 12 : (Math.max(clockX, 1));
                clockY = (clockY > 12) ? 12 : (Math.max(clockY, 1));
                return (new TimeLine(new LookAtDirectionCommand(clockX, clockY)));
            }
            case "lookleft": {
                String steppingString = getActionFeatureValue("stepping", f);
                int stepping = (steppingString.isEmpty()) ? 20 : Integer.parseInt(steppingString);
                return (new TimeLine(new LookLeftCommand(stepping)));
            }
            case "lookright": {
                String steppingString = getActionFeatureValue("stepping", f);
                int stepping = (steppingString.isEmpty()) ? 20 : Integer.parseInt(steppingString);
                return (new TimeLine(new LookRightCommand(stepping)));
            }
            case "luemmeln": {
                String variantString = getActionFeatureValue("variant", f);
                int variant = (variantString.isEmpty()) ? 1 : Integer.parseInt(variantString);
                variant = (variant > 6) ? 6 : (Math.max(variant, 1));
                return (new TimeLine(new LuemmelnCommand(variant)));
            }
            case "nod": {
                return (new TimeLine(new NodCommand()));
            }
            case "openarm": {
                return (new TimeLine(new OpenArmCommand()));
            }
            case "openfists": {
                return (new TimeLine(new OpenfistsCommand()));
            }
            case "pointopenpalm": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                return (new TimeLine(new PointOpenPalmCommand(direction)));
            }
            case "pointovershoulder": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                return (new TimeLine(new PointovershoulderCommand(direction)));
            }
            case "pointdownleft": {
                return (new TimeLine(new PointDownLeft()));
            }
            case "pointdownright": {
                return (new TimeLine(new PointDownRight()));
            }
            case "protectassertive": {
                return (new TimeLine(new ProtectAssertiveCommand()));
            }
            case "protectdefensive": {
                return (new TimeLine(new ProtectDefensiveCommand()));
            }
            case "shakehead": {
                return (new TimeLine(new ShakeheadCommand()));
            }
            case "showpalm": {
                return (new TimeLine(new ShowPalmCommand()));
            }
            case "showpalmdirection": {
                String directionString = getActionFeatureValue("direction", f);
                Direction direction = (directionString.isEmpty()) ? Direction.RIGHT : Direction.valueOf(directionString);
                return (new TimeLine(new ShowPalmDirectionCommand(direction)));
            }
            case "sitbrave": {
                return (new TimeLine(new SitBraveCommand()));
            }
            case "sit": {
                return (new TimeLine(new SitCommand()));
            }
            case "sitnodd": {
                return (new TimeLine(new SitNoddCommand()));
            }
            case "sittalk": {
                String stepString = getActionFeatureValue("step", f);
                int step = (stepString.isEmpty()) ? 1 : Integer.parseInt(stepString);
                step = (step > 12) ? 12 : (Math.max(step, 1));
                return (new TimeLine(new SitTalkCommand(step)));
            }
            case "supportive": {
                return (new TimeLine(new SupportiveCommand()));
            }
            case "think": {
                return (new TimeLine(new ThinkingCommand()));
            }
            case "ups": {
                return (new TimeLine(new UpsCommand()));
            }
            case "wave": {
                return (new TimeLine(new WaveCommand()));
            }
        }
        return null;
    }

    public synchronized Long getVMUtteranceId() {
        return ++sUtteranceId;
    }

    private void broadcast(Broadcastable broadcastable) {
        receiveSenderPort.sendMessage(broadcastable.toJson());
    }

    @Override
    public void execute(ActionActivity activity) {
        String name = activity.getName();
        if (name.equalsIgnoreCase("stop")) {
            stop();
        }
        Broadcastable action = parseAction(name, activity.getFeatures());
        // 25.25. pG: Assume action is an animation action - store information for  processing in global vsm model var
        if (runtime.hasVar(mVSMCharacterGestureVar)) {
            runtime.setVar(mVSMCharacterGestureVar, new StringValue(name));
        }
        broadcast(action);
    }

    public void execute(SpeechActivity sa) {
        // prepare for Vuppetmaster
        String activity_actor = sa.getActor();

        String vmuid = activity_actor + "_utterance_" + getVMUtteranceId();
        String cmd = "${'" + vmuid + "':'start'}$" + sa.getText() + "${'" + vmuid + "':'stop'}$";

        mLogger.message("Utterance with CMD Markers: " + cmd);

        AgentPlugin agent = actors.get(activity_actor);
        agent.execute(sa);

        AgentConfig agentConfig = runtime.getAgentConfig(activity_actor);
        String voice = agentConfig.getProperty("voice");

        SpeakCommand speakCommand = new SpeakCommand(cmd, voice, activity_actor);

        broadcast(new TimeLine(
                speakCommand
        ));
        mLogger.message("Speech command with CMD markers send ...");


        // PG: 18.11.2020 let vsm model know what the current character is speaking - this is dirty. it should actually be done with messages
        if (runtime.hasVar(mVSMCharacterTurnVar)) {
            // Whole Turn: mProject.setVariable(mVSMCharacterTurnVar, sa.getSceneTurn().getCleanText());
            runtime.setVar(mVSMCharacterTurnVar, sa.getTextOnly("${'").trim());
        }

        // let vsm model know that character has started speaking wrt to a scene - this is dirty. it should actually be done with messages
        if ((sa.getTurnNumber() == 1) && (sa.getUtteranceNumber() == 1)) { //do this only for the first turn and first utterance, within the scene
            if (runtime.hasVar(mVSMCharacterSpeakingVar)) {
                runtime.setVar(mVSMCharacterSpeakingVar, new BooleanValue(true));
            }
        }

        if (sa.getType() == AbstractActivity.Type.blocking) { // Wait only if activity is blocking
            // wait until we got feedback
            if (!receiveSenderPort.canSend()) {
                mLogger.warning("Blocking action command was send to nowhere. Executor will not wait. ");
            } else {
                runtime.wait(vmuid);
            }
        } else {
            mLogger.message("ActivityWorker does not feedback on action with id " + vmuid + " since action is non-blocking ...");
        }
        // let vsm model know that character is speaking - this is dirty. it should actually be done with messages
        mLogger.warning("Current turn number is " + sa.getTurnNumber() + " / " + sa.getTotalTurns());
        mLogger.warning("Current utterance number in turn is " + sa.getUtteranceNumber() + " / " + sa.getTotalUtterances());
        if ((sa.getTurnNumber() == sa.getTotalTurns()) && (sa.getUtteranceNumber() == sa.getTotalUtterances())) { // do this only after the last turn
            if (runtime.hasVar(mVSMCharacterSpeakingVar)) {
                runtime.setVar(mVSMCharacterSpeakingVar, new BooleanValue(false));
            }
        }
    }

    void processTimeMarkMessage(String message) {
        mLogger.message("Message is a time mark action");

        //clean message
        message = message.replace("\"", "");
        message = "$" + message + "$"; // bracketing "$" are not send back from VuppetMaster

        runtime.handleMarker(message);
        //execute scheduled action
        mLogger.message("Tell VSM activity scheduler to handle action represented by time marker >" + message + "<");
    }

    void processStatusMessage(String message) {
        mLogger.message("Message is related to an ongoing action");

        // clean message
        message = Charamel.cleanMessage(message);

        // split header and content
        String[] parts = message.split(":");
        String header = parts[0];
        String content = parts[1];

        mLogger.message("Message header is >" + header + "<, content is >" + content + "<");

        // PG 28.07.2021: Added system information if user has allowed audio output on the webpage.
        if (header.contains("fixAudioContext")) {
            //PG 28.07.2021: Inform model about the audio output availability.
            String audioavailable = (content.contains("true")) ? "audio_available" : "audio_not_available";
            if (runtime.hasVar(mVSMCharacterSystemVar)) {
                runtime.setVar(mVSMCharacterSystemVar, audioavailable);
            }
        }

        // check if there the activity manager waits for an action to be finished
        if (content.equalsIgnoreCase("stop")) {
            runtime.stopWaiting(header);
        }
    }

    synchronized void handleMessage(String message) {

        // status messages always contains a ":"
        if (message.contains(":")) { // status message
            processStatusMessage(message);
        } else { // time mark message
            processTimeMarkMessage(message);
        }
    }

    @Override
    public void start() {

    }

    public String marker(long id) {
        return "${'" + id + "'}$";
    }

    @Override
    public void stop() {
        receiveSenderPort.stop();
    }

}
