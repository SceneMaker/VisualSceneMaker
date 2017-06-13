package de.dfki.vsm.xtension.tricatworld;

import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.tricatworld.util.property.EmpatProjectProperty;
import de.dfki.vsm.xtension.tricatworld.xml.command.TriCatWorldCommand;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.TriCatWorldCmdObject;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.TriCatWorldActObject;
import de.dfki.vsm.xtension.tricatworld.xml.feedback.TriCatWorldFeedback;
import de.dfki.vsm.xtension.tricatworld.xml.util.ActionLoader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class TriCatWorldExecutor extends ActivityExecutor implements ExportableProperties {

    // The tworld listener
    private TriCatWorldListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, TriCatWorldHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    // The Charamel Action loader 
    private final ActionLoader mActionLoader = ActionLoader.getInstance();
    // The word mapping properties
    private final Properties mWordMapping = new Properties();
    // The flag if we use the JPL
    private final boolean mUseJPL;
    // The flag for executables
    private final boolean mUseExe;
    private ExportableProperties exportableProperties = new EmpatProjectProperty();

    // Construct the executor
    public TriCatWorldExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Get the JPL flag value
        mUseJPL = Boolean.parseBoolean(
                mConfig.getProperty("usejpl"));
        // Get the executable flag value
        mUseExe = Boolean.parseBoolean(
                mConfig.getProperty("useexe"));
    }

    // Launch the executor 
    @Override
    public final void launch() {
        // Get the plugin configuration
        final String tworlddir = mConfig.getProperty("tworlddir");
        final String tworldexe = mConfig.getProperty("tworldexe");
        final String tworldcmd = mConfig.getProperty("tworldcmd");
        final String cactordir = mConfig.getProperty("cactordir");
        final String cactorexe = mConfig.getProperty("cactorexe");
        final String cactorcmd = mConfig.getProperty("cactorcmd");
        // Get the working directory
        final String workindir = new File(".").getAbsolutePath();
        //
        if (mUseExe) {
            // Check the executable files
            if (!exists(cactordir)) {
                dialog("Missing '" + cactordir + "' in '" + workindir + "'");
            }
            if (!exists(tworlddir)) {
                dialog("Missing '" + tworlddir + "' in '" + workindir + "'");
            }
            // Create the plugin's processes
            try {
                mProcessMap.put(cactorexe, Runtime.getRuntime().exec(
                        "cmd /c start /min " + cactorexe + " " + cactorcmd, null, new File(cactordir)));
                mProcessMap.put(tworldexe, Runtime.getRuntime().exec(
                        "cmd /c start " + tworldexe + " " + tworldcmd, null, new File(tworlddir)));
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
        }
        // Create the connection
        mListener = new TriCatWorldListener(8000, this);
        // Start the connection
        mListener.start();
        // Wait for the TWorld
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for TWorld");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure(exc.toString());
            }
        }
        // Broadcast start signal
        broadcast("Start");
    }

    // Unload the executor 
    @Override
    public final void unload() {
        // Abort the client threads
        for (final TriCatWorldHandler client : mClientMap.values()) {
            client.abort();
            // Join the client thread
            try {
                client.join();
            } catch (final InterruptedException exc) {
                mLogger.failure(exc.toString());
                // Print some information 
                mLogger.message("Joining client thread");
            }
        }
        // Clear the map of clients 
        mClientMap.clear();
        // Abort the server thread
        try {
            mListener.abort();
            // Join the client thread
            mListener.join();
            // Print some information 
            mLogger.message("Joining server thread");
        } catch (final InterruptedException exc) {
            mLogger.failure(exc.toString());
        }

        if (mUseExe) {
            // Wait for pawned processes
            for (final Entry<String, Process> entry : mProcessMap.entrySet()) {
                // Get the process entry
                final String name = entry.getKey();
                final Process process = entry.getValue();
                try {
                    // Kill the processes
                    final Process killer = Runtime.getRuntime().exec("taskkill /F /IM " + name);
                    // Wait for the killer
                    killer.waitFor();
                    // Print some information 
                    mLogger.message("Joining killer " + name + "");
                    // Wait for the process
                    process.waitFor();
                    // Print some information 
                    mLogger.message("Joining process " + name + "");
                } catch (final IOException | InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            // Clear the map of processes 
            mProcessMap.clear();
        }
    }

    @Override
    public final synchronized String marker(final long id) {
        // TWorld style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        // Get action information
        final Type activity_type = activity.getType();
        final String activity_text = activity.getText();
        final String activity_name = activity.getName();
        //final String activity_mode = activity.getMode();
        final String activity_actor = activity.getActor();
        final List activity_features = activity.getFeatures();
        // Initialize the command        
        TriCatWorldActObject triCatWorldAct = null;
        // set all activities blocking
        activity.setType(activity_type.blocking);

        // Check the activity type
        if (activity instanceof SpeechActivity) {
            final SpeechActivity speech_activity = (SpeechActivity) activity;
            final String speech_text = speech_activity.getTextOnly("$(").trim();
            final List<String> time_marks = speech_activity.getTimeMarks("$(");

            if (speech_text.isEmpty()) {
                // If speech_text is empty we assume that the activity has 
                // empty speech text but has marker activities registered
                for (final String tm : time_marks) {
                    //mLogger.message("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                    return;
                }
            } else {
                // load wordmapping database
                try {
                    String wmf = mProject.getProjectPath() + File.separator + mProject.getAgentConfig(activity_actor).getProperty("wordmapping");
                    wmf = wmf.replace("\\", "/");
                    mWordMapping.load(new FileReader(new File(wmf)));
                } catch (IOException ex) {
                    mLogger.failure("Wordmapping file (" + mProject.getAgentConfig(activity_actor).getProperty("wordmapping") + ") not found!");
                }
                // do the pronounciation mapping
                speech_activity.doPronounciationMapping(mWordMapping);
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
                // build action
                triCatWorldAct = mActionLoader.loadCharamelAnimation("Speak", speech_activity.getBlocks(), speech_activity.getPunct(), aid);

            }
        } else {
            System.err.println("Activity Name: '" + activity_name + "'");
            // Get the unique actor id
            final String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
            // Check the activity name
            if (activity_name.equalsIgnoreCase("StopSpeaking")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("Reject")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Challenge")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("ShowPalms")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("OpenArms")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookLeft")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookRight")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("No")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongNo")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointLeft")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointRight")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentLeft")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentRight")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Welcome")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Yes")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongYes")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Angry")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Demanding")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Disgust")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Neutral")) {
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, "1.0", aid);
            } else if (activity_name.equalsIgnoreCase("Sad")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Smile")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Happy")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                triCatWorldAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("CancelMoveTo")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Play")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Stop")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Release")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("ReleaseLookAt")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("DisableNPCMeeting")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("EnableNPCMeeting")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("DisableAmbientSound")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("EnableAmbientSound")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("AmbientLight")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("value"));
            } else if (activity_name.equalsIgnoreCase("AmbientSound")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("value"));
            } else if (activity_name.equalsIgnoreCase("Load")) {
                String url = "file:///" + mProject.getProjectPath()
                        + File.separator + mProject.getAgentConfig(
                                activity_actor).getProperty(activity.get("value"));
                url = url.replace("\\", "/");
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, url);
            } else if (activity_name.equalsIgnoreCase("Rotate")) {
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("hdegree"), activity.get("vdegree"));
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                } else {
                    triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("degree"));
                }
            } else if (activity_name.equalsIgnoreCase("LookAt")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("viewtarget"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("MoveTo")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("locname"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Say")) {
//                String url = "file:///" + mProject.getProjectPath()
//                        + File.separator + mProject.getAgentConfig(
//                                activity_actor).getProperty(activity.get("value"));
//                url = url.replace("\\", "/");
                String url = activity.get("value");
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, url, activity.get("2d"));
            } else if (activity_name.equalsIgnoreCase("PlayStream")) { // added pg 23.3.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action PlayStream not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("file"), activity.get("start"), activity.get("end"));
            } else if (activity_name.equalsIgnoreCase("PlayTopic")) { // added pg 27.4.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action PlayTopic not processed - agent(name) is not debriefing");
                    return;
                }
                String playquestion = (activity.get("playquestion") != null) ? activity.get("playquestion") : "0";
                String playanswer = (activity.get("playanswer") != null) ? activity.get("playanswer") : "0";
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("topicid"), playquestion, playanswer);
            } else if (activity_name.equalsIgnoreCase("LoadDebriefing")) { // added pg 27.4.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action LoadDebriefing not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("eventlog"), activity.get("screenvideo"), activity.get("cameravideo"));
            } else if (activity_name.equalsIgnoreCase("HideBriefingInfo")) { // added pg 4.5.2017
                // Ugly: activity_actor has to be: debriefing
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action HideBriefingInfo not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("UnloadDebriefing")) { // added pg 4.5.2017
                // Ugly: activity_actor has to be: debriefing
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action UnloadDebriefing not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("ShowNote")) { // added pg 27.4.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action ShowNote not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("text"));
            } else if (activity_name.equalsIgnoreCase("PinNote")) { // added pg 4.5.2017
                mLogger.message("\033[1;35mProcessing Activity:\n" + activity_name + "\033[0m");
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action PinNote not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("DiscardNote")) { // added pg 4.5.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action ShowNote not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("DiscardAllNotes")) { // added pg 4.5.2017
                // Ugly: activity_actor has to be: DebriefingScreen
                if (!activity_actor.equalsIgnoreCase("debriefing")) {
                    mLogger.warning("Action ShowNote not processed - agent(name) is not debriefing");
                    return;
                }
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("PlayAudio")) {
                String url = "file:///" + mProject.getProjectPath()
                        + File.separator + mProject.getAgentConfig(
                                activity_actor).getProperty(activity.get("value"));
                url = url.replace("\\", "/");
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, url);
            } else if (activity_name.equalsIgnoreCase("Color")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("r"), activity.get("g"), activity.get("b"));
            } else if (activity_name.equalsIgnoreCase("SitDown")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("chairname"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Stop")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Warp")) {
                String target = activity.get("viewtarget");
                if (target != null) {
                    triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("location"), target);
                } else {
                    triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("location"));
                }
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("WorldPosition")) {
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("x"), activity.get("y"), activity.get("z"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("MoveToWorldPosition")) { // added pg 5.5.2017
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("x"), activity.get("y"), activity.get("z"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Camera") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("x"), activity.get("y"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("FocalLength") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("value"), activity.get("time"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            }  else if (activity_name.equalsIgnoreCase("FieldOfView") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("value"), activity.get("time"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            }  else if (activity_name.equalsIgnoreCase("ResetFieldOfView") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("value"), activity.get("time"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("DefaultFocalLength") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("viewtarget"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("CameraOffset") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name,
                        activity.get("x"), activity.get("y"), activity.get("z"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Scale") && activity_actor.equalsIgnoreCase("player")) { // this is action player only activity_name
                triCatWorldAct = mActionLoader.loadTWorldAnimation(activity_name, activity.get("value"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    triCatWorldAct.resetActionCmd(activity_actor + "_" + triCatWorldAct.getActionCmd());
                }
            } else {
                // Unknown activity_name
            }
        }

        // Create command object
        final TriCatWorldCommand triCatWorldCmd = new TriCatWorldCommand();
        triCatWorldCmd.addObject(new TriCatWorldCmdObject(activity_actor, triCatWorldAct));
        // Write the commmand to XML        
        final String message = XMLUtilities.xmlStringToPrettyXMLString(triCatWorldCmd.toString());
        // Print debug message
        mLogger.message("\033[0;35mExecuting command " + activity_name + " on actor " + activity_actor + ":\n" + message + "\033[0m");
        // Send command object
        synchronized (mActivityWorkerMap) {
            broadcast(message);

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(triCatWorldAct.getId(), cAW);

            if (activity.getType() == activity_type.blocking) { // Wait only if activity is blocking
                // wait until we got feedback
                //mLogger.message("ActivityWorker " + tworld_cmd_action.getId() + " waiting ...");

                while (mActivityWorkerMap.containsValue(cAW)) {
                    try {
                        mActivityWorkerMap.wait();
                    } catch (InterruptedException exc) {
                        mLogger.failure(exc.toString());
                    }
                }

                StringBuilder sb = new StringBuilder();

                if (mActivityWorkerMap.size() > 1) {
                    for (String aw : mActivityWorkerMap.keySet()) {
                        sb.append(aw).append(", ");
                    }

                    sb.delete(sb.length() - 2, sb.length());
                    //mLogger.message("ActivityWorker " + tworld_cmd_action.getId() + " done, ActivityWorker (" + sb.toString() + ") stil active ...");
                } else {
                    //mLogger.message("ActivityWorker " + tworld_cmd_action.getId() + " done ...");
                }
            }
        }
        // Return when terminated
    }
    // Accept some connection

    public final void accept(final Socket socket) {
        // Make new client thread 
        final TriCatWorldHandler client = new TriCatWorldHandler(socket, this);
        // Add the client to list
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        // Print some information
        //mLogger.message("Accepting " + client.getName() + "");
    }

    // Handle some message
    public final void handle(final String input, final TriCatWorldHandler client) {
        // Sanitize the message
        final String message = XMLUtilities.xmlStringToPrettyXMLString(
                input.replaceAll("..xml\\s+version........", ""));
        // Print some information
        mLogger.message("\033[1;35mHandling new message:\n" + message + "\033[0m");
        // Check and notify the relevant threads
        synchronized (mActivityWorkerMap) {
            final TriCatWorldFeedback triCatFeedBack = new TriCatWorldFeedback();
            try {
                XMLUtilities.parseFromXMLStream(triCatFeedBack,
                        new ByteArrayInputStream(message.getBytes("UTF-8")));
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
                return;
            }

            // Handle action feedback
            if (triCatFeedBack.hasActionFeedback()) {
                // added pg 24.3.2017 - process multiple actions in feedback 
                for (de.dfki.vsm.xtension.tricatworld.xml.feedback.action.Action action : triCatFeedBack.mFeedbackActions) {
                    // handling every /*ambient_setup*/ feedback
                    if (action.mActionFeedback.mName.equalsIgnoreCase("action_finished")) {
                        // check if the acitivy action feedback was speech feedback
                        if (action.mActionFeedback.hasCaiEvent()) {
                            if (action.mActionFeedback.mCaiEvent.hasTTSStatus()) {
                                if (action.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("start")) {
                                    // TODO - get id - for now there is none                          
                                    mLogger.message("\033[1;34mAgent starts speaking" + "\033[0m");
                                    // Set character voice activity variable
                                    if (mUseJPL) {
                                        JPLEngine.query("now(Time), "
                                                + "jdd(["
                                                + "type:" + "event" + ","
                                                + "name:" + "agent" + ","
                                                + "mode:" + "voice" + ","
                                                + "data:" + "start" + ","
                                                + "time:" + "Time" + ","
                                                + "from:" + "0" + ","
                                                + "life:" + "0" + ","
                                                + "conf:" + "1.0"
                                                + "]).");
                                    } else {
                                        mProject.setVariable("AgentIsSpeaking", true);
                                    }
                                }
                                if (action.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("text_maker")) {
                                    //mLogger.message("Handling Charamel Marker " + tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mMarker);
                                    mProject.getRunTimePlayer().getActivityScheduler().handle(action.mActionFeedback.mCaiEvent.mTts.mMarker);
                                }
                                if (action.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("end")) {
                                    // TODO - get id - for now there is none
                                    // Set character voice activity variable
                                    mLogger.message("\033[1;34mAgent finishes speaking" + "\033[0m");
                                    if (mUseJPL) {
                                        JPLEngine.query("now(Time), "
                                                + "jdd(["
                                                + "type:" + "event" + ","
                                                + "name:" + "agent" + ","
                                                + "mode:" + "voice" + ","
                                                + "data:" + "stop" + ","
                                                + "time:" + "Time" + ","
                                                + "from:" + "0" + ","
                                                + "life:" + "0" + ","
                                                + "conf:" + "1.0"
                                                + "]).");
                                    } else {
                                        mProject.setVariable("AgentIsSpeaking", false);
                                    }
                                    // wake me up ..
                                    mActivityWorkerMap.notifyAll();
                                }
                                // TODO marker!
                            }
                        } else {
                            // there is no cai_event - hence no tts notification.
                            // since it is an action_finished message, remove the activity in any case
                            if (mActivityWorkerMap.containsKey(action.mId)) {
                                mActivityWorkerMap.remove(action.mId);
                            } else {
                                mLogger.failure("Activityworker for " + action.mId + " has been stopped before ...");
                            }
                            // wake me up ..
                            mActivityWorkerMap.notifyAll();
                        }
                    }
                }
            }

            // Handle action feedback
            if (triCatFeedBack.hasObjectFeedback()) {
                // added pg 24.3.2017 - process multiple objects in feedback 
                for (de.dfki.vsm.xtension.tricatworld.xml.feedback.object.Object object : triCatFeedBack.mFeedbackObjects) {
                    HashMap<String, AbstractValue> values = new HashMap<>();
                    values.put("type", new StringValue(object.mObjectFeedback.mName));
                    values.put("elicitor", new StringValue(object.mObjectFeedback.mTriggerObject));
                    values.put("name", new StringValue(object.mName));

                    try {
                        //RunTimeInstance runTime = RunTimeInstance.getInstance();
                        StructValue struct = new StructValue(values);
                        //runTime.setVariable(mProject, "feedback", struct);
                        mProject.setVariable("feedback", struct);//GM
                    } catch (Exception e) {
                        // System.out.println("not running");
                    }
                }
            }
        }
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final TriCatWorldHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

    // Show action waiting dialog
    private void dialog(final String message) {
        WaitingDialog InfoDialog = new WaitingDialog(message);
        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
    }

    // Check if action file exists
    private boolean exists(final String path) {
        final File file = new File(path);
        if (file.exists() && file.isDirectory()) {
            return true;
        }
        return false;
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
