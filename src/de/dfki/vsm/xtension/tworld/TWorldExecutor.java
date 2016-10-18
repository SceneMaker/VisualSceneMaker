package de.dfki.vsm.xtension.tworld;

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
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.tworld.xml.command.TWorldCommand;
import de.dfki.vsm.xtension.tworld.xml.command.object.Object;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.Action;
import de.dfki.vsm.xtension.tworld.xml.feedback.TWorldFeedback;
import de.dfki.vsm.xtension.tworld.xml.util.ActionLoader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class TWorldExecutor extends ActivityExecutor {

    // The tworld listener
    private TWorldListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, TWorldHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    // The Charamel Action loader 
    private final ActionLoader mActionLoader = ActionLoader.getInstance();
    // The word mapping properties
    private final Properties mWordMapping = new Properties();
    // The flag if we user the JPL
    private final boolean mUseJPL;

    // Construct the executor
    public TWorldExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Get the JPL flag value
        mUseJPL = Boolean.parseBoolean(
                mConfig.getProperty("usejpl"));
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
        // Create the connection
        mListener = new TWorldListener(8000, this);
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
        for (final TWorldHandler client : mClientMap.values()) {
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
        final String activity_mode = activity.getMode();
        final String activity_actor = activity.getActor();
        final List activity_features = activity.getFeatures();
        // Initialize the command
        TWorldCommand tworld_final_cmd = null;
        Action tworld_cmd_action = null;
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
                    mLogger.warning("Directly executing activity at timemark " + tm);
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
                tworld_cmd_action = mActionLoader.loadCharamelAnimation("Speak", speech_activity.getBlocks(), speech_activity.getPunct(), aid);
            }
        } else {
            // Get the unique actor id
            final String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
            // Check the activity name
            if (activity_name.equalsIgnoreCase("StopSpeaking")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("Reject")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Challenge")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("ShowPalms")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("OpenArms")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookLeft")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookRight")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("No")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongNo")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointLeft")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointRight")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentLeft")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentRight")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Welcome")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Yes")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongYes")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Angry")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Demanding")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Disgust")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Neutral")) {
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, "1.0", aid);
            } else if (activity_name.equalsIgnoreCase("Sad")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Smile")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("Happy")) {
                String intensity = activity.getValueOf("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                tworld_cmd_action = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("CancelMoveTo")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Play")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Stop")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Release")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("ReleaseLookAt")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("AmbientLight")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("value"));
            } else if (activity_name.equalsIgnoreCase("AmbientSound")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("value"));
            } else if (activity_name.equalsIgnoreCase("Load")) {
                String url = "file:///" + mProject.getProjectPath()
                        + File.separator + mProject.getAgentConfig(
                                activity_actor).getProperty(activity.getValueOf("value"));
                url = url.replace("\\", "/");
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, url);
            } else if (activity_name.equalsIgnoreCase("LookAt")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("viewtarget"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("MoveTo")) {
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Say")) {
                String url = "file:///" + mProject.getProjectPath()
                        + File.separator + mProject.getAgentConfig(
                                activity_actor).getProperty(activity.getValueOf("value"));
                url = url.replace("\\", "/");
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, url);
            } else if (activity_name.equalsIgnoreCase("PlayAudio")) {
                String url = "file:///" + mProject.getProjectPath()
                        + File.separator + mProject.getAgentConfig(
                                activity_actor).getProperty(activity.getValueOf("value"));
                url = url.replace("\\", "/");
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, url);
            } else if (activity_name.equalsIgnoreCase("Color")) {
                tworld_cmd_action = mActionLoader.loadAnimation(activity_name,
                        activity.getValueOf("r"), activity.getValueOf("g"), activity.getValueOf("b"));
            } else if (activity_name.equalsIgnoreCase("SitDown")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("chairname"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Stop")) {
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name);
            } else if (activity_name.equalsIgnoreCase("Warp")) {
                String target = activity.getValueOf("viewtarget");
                if (target != null) {
                    tworld_cmd_action = mActionLoader.loadAnimation(activity_name, activity.getValueOf("location"), target);
                } else {
                    tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("location"));
                }
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("WorldPosition")) {
                tworld_cmd_action = mActionLoader.loadAnimation(activity_name,
                        activity.getValueOf("x"), activity.getValueOf("y"), activity.getValueOf("z"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Camera") && activity_actor.equalsIgnoreCase("player")) { // this is a player only activity_name
                tworld_cmd_action = mActionLoader.loadAnimation(activity_name,
                        activity.getValueOf("x"), activity.getValueOf("y"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("FocalLength") && activity_actor.equalsIgnoreCase("player")) { // this is a player only activity_name
                tworld_cmd_action = mActionLoader.loadAnimation(activity_name,
                        activity.getValueOf("value"), activity.getValueOf("time"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("DefaultFocalLength") && activity_actor.equalsIgnoreCase("player")) { // this is a player only activity_name
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("viewtarget"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("CameraOffset") && activity_actor.equalsIgnoreCase("player")) { // this is a player only activity_name
                tworld_cmd_action = mActionLoader.loadAnimation(activity_name,
                        activity.getValueOf("x"), activity.getValueOf("y"), activity.getValueOf("z"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else if (activity_name.equalsIgnoreCase("Scale") && activity_actor.equalsIgnoreCase("player")) { // this is a player only activity_name
                tworld_cmd_action = mActionLoader.loadTWorldAnimation(activity_name, activity.getValueOf("value"));
                if (activity_actor.equalsIgnoreCase("player")) {
                    tworld_cmd_action.resetActionCmd(activity_actor + "_" + tworld_cmd_action.getActionCmd());
                }
            } else {
                // Unknown activity_name
            }
        }
        //mLogger.message("Building command " + activity_name + " for actor " + activity_actor);
        // Finalize build activity_name
        final Object tworld_cmd_object = new Object(activity_actor, tworld_cmd_action);
        tworld_final_cmd = new TWorldCommand();
        tworld_final_cmd.addObject(tworld_cmd_object);
        // Write the commmand to XML
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        final IOSIndentWriter iosw = new IOSIndentWriter(out);
        XMLUtilities.writeToXMLWriter(tworld_final_cmd, iosw);
        // GM_: Why is this necessary
        // Fuck German Umlaute and Encoding
        String message = out.toString().
                replace("ö", "oe").
                replace("ä", "ae").
                replace("ü", "ue").
                replace("Ö", "Oe").
                replace("Ä", "Ae").
                replace("Ü", "Ue").
                replace("ß", "ss").
                replace("\n", " ").
                replace("   ", " ").
                replace("  ", " ");

        mLogger.message("Executing command " + activity_name + " on actor " + activity_actor + ":\n" + prettyPrint(message));

        // Send activity_name to tworld 
        synchronized (mActivityWorkerMap) {
            broadcast(message);

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(tworld_cmd_action.getId(), cAW);

            if (activity.getType() == activity_type.blocking) { // Wait only if activity is blocking
                // wait until we got feedback
                mLogger.success("ActivityWorker " + tworld_cmd_action.getId() + " waiting ...");

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
                    mLogger.success("ActivityWorker " + tworld_cmd_action.getId() + " done, ActivityWorker (" + sb.toString() + ") stil active ...");
                } else {
                    mLogger.success("ActivityWorker " + tworld_cmd_action.getId() + " done ...");
                }
            }
        }
        // Return when terminated
    }

    // Accept some connection
    public final void accept(final Socket socket) {
        // Make new client thread 
        final TWorldHandler client = new TWorldHandler(socket, this);
        // Add the client to list
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        // Print some information
        mLogger.success("Accepting " + client.getName() + "");
    }

    // Handle some message
    public final void handle(
            final String input,
            final TWorldHandler client) {
        // Sanitize the message
        final String message = input.replaceAll("..xml\\s+version........", "");
        // Print some information
        mLogger.message("Handling new message:\n" + prettyPrint(message) + "");
        // Notify the relevant threads
        synchronized (mActivityWorkerMap) {
            final TWorldFeedback tworld_final_feedback = new TWorldFeedback();
            try {
                final InputStream stream = new ByteArrayInputStream(message.getBytes("UTF-8"));
                XMLUtilities.parseFromXMLStream(tworld_final_feedback, stream);
            } catch (final UnsupportedEncodingException exc) {
                mLogger.failure(exc.toString());
                return;
            }
            //
            if (tworld_final_feedback.hasActionFeedback()) {
                final String actionType = tworld_final_feedback.mFeedbackAction.mName;
                String id = tworld_final_feedback.mFeedbackAction.mId;
                String actionStatusType = tworld_final_feedback.mFeedbackAction.mActionFeedback.mName;
                String actionStatusValue = tworld_final_feedback.mFeedbackAction.mActionFeedback.mValue;

                // handling every /*ambient_setup*/ feedback
                if (actionStatusType.equalsIgnoreCase("action_finished")) {
                    // check if the acitivy action feedback was speech feedback
                    if (tworld_final_feedback.mFeedbackAction.mActionFeedback.hasCaiEvent()) {
                        if (tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.hasTTSStatus()) {
                            if (tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("start")) {
                                // TODO - get id - for now there is none
                                // Set character voice activity variable                               
                                //mLogger.warning("Agent starts speaking");
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
                                    // Set speaking variable
                                    mProject.setVariable("AgentIsSpeaking", true);
                                }
                            }
                            if (tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("text_maker")) {
                                mLogger.success("Handling Charamel Marker " + tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mMarker);
                                mProject.getRunTimePlayer().getActivityScheduler().handle(tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mMarker);
                            }
                            if (tworld_final_feedback.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("end")) {
                                // TODO - get id - for now there is none
                                // Set character voice activity variable
                                //mProject.setVariable("susanne_voice_activity", new StringValue(""));
                                //mProject.setVariable("tom_voice_activity", new StringValue(""));
                                //mLogger.warning("Agent finishes speaking");
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
                                    // Set speaking variable
                                    mProject.setVariable("AgentIsSpeaking", false);
                                }
//                                // remove the activity
//                                if (mActivityWorkerMap.containsKey(id)) {
//                                    mActivityWorkerMap.remove(id);
//                                } else {
//                                    mLogger.warning("Activityworker for " + id + " has been stopped before (TTS end speak notification is received after cmd end notification)");
//                                }
                                // wake me up ..
                                mActivityWorkerMap.notifyAll();
                            }

                            // TODO marker!
                        }
                    } else { // there is no cai_event - hence no tts notification.
                        // remove the activity in any case
                        if (mActivityWorkerMap.containsKey(id)) {
                            mActivityWorkerMap.remove(id);
                        } else {
                            mLogger.warning("Activityworker for " + id + " has been stopped before (cmd end notification is received after TTS end speak notification)");
                        }
                        // wake me up ..
                        mActivityWorkerMap.notifyAll();
                    }
                }
            }

            if (tworld_final_feedback.hasObjectFeedback()) {
                HashMap<String, AbstractValue> values = new HashMap<>();
                values.put("type", new StringValue(tworld_final_feedback.mFeedbackObject.mObjectFeedback.mName));
                values.put("elicitor", new StringValue(tworld_final_feedback.mFeedbackObject.mObjectFeedback.mTriggerObject));
                values.put("name", new StringValue(tworld_final_feedback.mFeedbackObject.mName));

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

    // Broadcast some message
    private void broadcast(final String message) {
        for (final TWorldHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

    // Show a waiting dialog
    private void dialog(final String message) {
        WaitingDialog InfoDialog = new WaitingDialog(message);
        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
    }

    // Check if a file exists
    private boolean exists(final String path) {
        final File file = new File(path);
        if (file.exists() && file.isDirectory()) {
            return true;
        }
        return false;
    }

    // Pretty print XM event
    public final String prettyPrint(final String string) {
        try {
            // Get the string input stream
            final ByteArrayInputStream stream = new ByteArrayInputStream(
                    string.getBytes("UTF-8"));
            // Construct the XML pipeline
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Transformer transformer
                    = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            // Parse the XML document
            final StreamResult result
                    = new StreamResult(new StringWriter());
            final Document document = parser.parse(stream);
            final DOMSource source = new DOMSource(document);
            // Transform the document
            transformer.transform(source, result);
            // Return the representation
            return result.getWriter().toString();
        } catch (final ParserConfigurationException | IllegalArgumentException | SAXException | IOException | TransformerException exc) {
            mLogger.failure(exc.toString());
            // Return failure if the parsing failed
            return null;
        }
    }
}
