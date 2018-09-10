package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
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
import de.dfki.vsm.xtension.charamel.util.property.CharamelProjectProperty;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;
import de.dfki.vsm.xtension.charamel.xml.feedback.CharamelFeedback;
import de.dfki.vsm.xtension.charamel.xml.util.CharamelActionLoader;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class CharamelExecutor extends ActivityExecutor implements ExportableProperties {

    // The tworld listener
    private CharamelListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, CharamelHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    // The Charamel Action loader 
    private final CharamelActionLoader mActionLoader = CharamelActionLoader.getInstance();
    // The word mapping properties
    private final Properties mWordMapping = new Properties();
    // The flag if we use the JPL
    private final boolean mUseJPL;
    // The flag for executables
    private final boolean mUseExe;
    private final ExportableProperties exportableProperties = new CharamelProjectProperty();
    // The TCP socket connection 
    private Socket mSocket;

    // Construct the executor
    public CharamelExecutor(final PluginConfig config, final RunTimeProject project) {
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
            // Create the plugin's processes
            try {
                mProcessMap.put(cactorexe, Runtime.getRuntime().exec(
                        "cmd /c start /min " + cactorexe + " " + cactorcmd, null, new File(cactordir)));
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Create the connection
        mLogger.message("Connecting to Charamel server ...");

        while (mSocket == null) {
            try {
                // Create the socket
                mSocket = new Socket("localhost", 4000);
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }

            mLogger.message("Wait a bit ...");

            try {
                // Wait a bit ...
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure(exc.toString());
            }
        }

        connectToCharamel(mSocket);
    }

    // Unload the executor 
    @Override
    public final void unload() {
        try {
            mSocket.close();
        } catch (IOException e) {
            mLogger.failure(e.toString());
        }
        
        // Abort the client threads
        for (final CharamelHandler client : mClientMap.values()) {
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
//        try {
//            mListener.abort();
//            // Join the client thread
//            mListener.join();
//            // Print some information 
//            mLogger.message("Joining server thread");
//        } catch (final InterruptedException exc) {
//            mLogger.failure(exc.toString());
//        }

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
        // Bracket style bookmarks
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
        CharamelActObject charamelAct = null;
        // set all activities blocking
        activity.setType(Type.blocking);

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
                charamelAct = mActionLoader.buildCharamelAnimation("Speak", speech_activity.getBlocks(), speech_activity.getPunct(), aid);
            }
        } else {
            System.err.println("Activity Name: '" + activity_name + "'");
            // Get the unique actor id
            final String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
            // Check the activity name
            if (activity_name.equalsIgnoreCase("SittingStart")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("SittingSop")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("StopSpeaking")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("Reject")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Challenge")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("SittingStart")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("SittingStop")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("ShowPalms")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("OpenArms")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookLeft")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("LookRight")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("No")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongNo")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointLeft")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("PointRight")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentLeft")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("PresentRight")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Welcome")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Yes")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("StrongYes")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Angry")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Demanding")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Disgust")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Neutral")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, "1.0", aid);
            } else if (activity_name.equalsIgnoreCase("Sad")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Smile")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("Happy")) {
                String intensity = activity.get("intensity");
                intensity = (intensity == null) ? "1.0" : intensity;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, intensity, aid);
                activity.setType(Type.parallel);
            } else if (activity_name.equalsIgnoreCase("InitJointAnimation")) {
                String joint = activity.get("joint");
                joint = (joint == null) ? "" : joint;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, joint, aid);
                //activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("ConfigureJointAnimation")) {
                String joint = activity.get("joint");
                joint = (joint == null) ? "" : joint;
                String factor = activity.get("factor");
                factor = (factor == null) ? "0.0" : factor;
                String interpolation = activity.get("interpolation");
                interpolation = (interpolation == null) ? "" : interpolation;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, joint, factor, interpolation, aid);
                //activity.setType(activity_type.parallel);
            } else if (activity_name.equalsIgnoreCase("OrientJoint")) {
                String joint = activity.get("joint");
                joint = (joint == null) ? "" : joint;
                String interpolation = activity.get("interpolation");
                String xdegree = activity.get("x");
                xdegree = (xdegree == null) ? "0.0" : xdegree;
                String ydegree = activity.get("y");
                ydegree = (ydegree == null) ? "0.0" : ydegree;
                String zdegree = activity.get("z");
                zdegree = (zdegree == null) ? "0.0" : zdegree;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, joint, interpolation, xdegree, ydegree, zdegree, aid);
                activity.setType(Type.parallel);
            } else {
                // Unknown activity_name
            }
        }

        // Create command object
//        final CharamelCommand triCatWorldCmd = new CharamelCommand();
//        triCatWorldCmd.addObject(new CharamelCmdObject(activity_actor, charamelAct));
        // Write the commmand to XML        
        final String message = XMLUtilities.xmlStringToPrettyXMLString(charamelAct.toString());
        // Print debug message
        mLogger.message("\033[0;35mExecuting command " + activity_name + " on actor " + activity_actor + ":\n" + message + "\033[0m");
        // Send command object
        synchronized (mActivityWorkerMap) {
            broadcast(message);

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(charamelAct.getId(), cAW);

            if (activity.getType() == Type.blocking) { // Wait only if activity is blocking
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

    // Start some connection
    public final void connectToCharamel(final Socket socket) {
        // Make new client thread 
        final CharamelHandler client = new CharamelHandler(socket, this);
        // Add the client to list
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        // Print some information
        mLogger.warning("Handling connection " + client.getName() + "");
    }

    // Handle some message
    public final void handle(final String input, final CharamelHandler client) {
        // Sanitize the message
        final String message = XMLUtilities.xmlStringToPrettyXMLString(
                input.replaceAll("..xml\\s+version........", ""));
        // Print some information
        mLogger.message("\033[1;35mHandling new message:\n" + message + "\033[0m");
        // Check and notify the relevant threads
        synchronized (mActivityWorkerMap) {
            final CharamelFeedback charamelFeedback = new CharamelFeedback();
            try {
                XMLUtilities.parseFromXMLStream(charamelFeedback,
                        new ByteArrayInputStream(message.getBytes("UTF-8")));
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
                return;
            }

            // Handle action feedback
            if (charamelFeedback.hasActionFeedback()) {
                // added pg 24.3.2017 - process multiple actions in feedback 
                for (de.dfki.vsm.xtension.charamel.xml.feedback.action.Action action : charamelFeedback.mFeedbackActions) {
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
            if (charamelFeedback.hasObjectFeedback()) {
                // added pg 24.3.2017 - process multiple objects in feedback 
                for (de.dfki.vsm.xtension.charamel.xml.feedback.object.Object object : charamelFeedback.mFeedbackObjects) {
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
        for (final CharamelHandler client : mClientMap.values()) {
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
        return file.exists() && file.isDirectory();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
