package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.WordMapping;
import de.dfki.vsm.xtension.charamel.util.property.CharamelProjectProperty;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;
import de.dfki.vsm.xtension.charamel.xml.feedback.action.*;
import de.dfki.vsm.xtension.charamel.xml.util.CharamelActionLoader;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class CharamelExecutor extends ActivityExecutor implements ExportableProperties {

    // The tworld listener
    private CharamelListener mListener;
    // The map of processes
    private final Map<String, Process> mProcessMap = new HashMap<>();
    // The client thread list
    private final Map<String, CharamelHandler> mClientMap = new HashMap<>();
    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    // The Charamel Action loader 
    private final CharamelActionLoader mActionLoader = CharamelActionLoader.getInstance();
    // The word mapping properties
    private WordMapping mWordMapping = new WordMapping();
    // The flag if we use the JPL
    private final boolean mUseJPL;
    // The flag for executables
    private final boolean mUseExe;
    private final ExportableProperties exportableProperties = new CharamelProjectProperty();
    // The TCP socket connection 
    private Socket mSocket;

    /**
     * Construct the executor
     */
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

    /**
     * Launch the executor
     */
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

    /**
     * Unload the executor
     */
    @Override
    public final void unload() {
        try {
            mSocket.close();
            mSocket = null;
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

        if (mUseExe) {
            // Wait for pawned processes
            for (final Entry<String, Process> entry : mProcessMap.entrySet()) {
                killProcess(entry);
            }
            // Clear the map of processes 
            mProcessMap.clear();
        }
    }

    private void killProcess(Entry<String, Process> entry) {
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
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                // load wordmapping database
                mWordMapping.load(activity_actor, mProject);
                // do the pronounciation mapping
                speech_activity.doPronounciationMapping(mWordMapping);
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
                // build action
                charamelAct = mActionLoader.buildCharamelAnimation("Speak", speech_activity.getBlocks(), speech_activity.getPunct(), aid);
            }
        } else {
            mLogger.message("Activity Name: '" + activity_name + "'");
            // Get the unique actor id
            final String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
            // Check the activity name
            if (activity_name.equalsIgnoreCase("SetCamera")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("HideAvatar")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("GetCamera")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid);
            } else if (activity_name.equalsIgnoreCase("SittingStart")) {
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
            }else if (activity_name.equalsIgnoreCase("RawXML")) {
                String XML = activity.get("XML");
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, aid,XML);
                activity.setType(Type.parallel);
            }
            else if (activity_name.equalsIgnoreCase("Neutral")) {
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
            }
            else if (activity_name.equalsIgnoreCase("ArmsCrossed")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name,aid);
                activity.setType(Type.parallel);
            }            
            else if (activity_name.equalsIgnoreCase("HandToFace")) {
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name,aid);
                activity.setType(Type.parallel);
            else if (activity_name.equalsIgnoreCase("InitJointAnimation")) {
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
            } else if (activity_name.equalsIgnoreCase("MoveCamera")) {
                String posX = activity.get("posX");
                posX = (posX == null) ? "0.0" : posX;
                String posY = activity.get("posY");
                posY = (posY == null) ? "0.0" : posY;
                String posZ = activity.get("posZ");
                posZ = (posZ == null) ? "0.0" : posZ;
                String lookX = activity.get("lookX");
                lookX = (lookX == null) ? "0.0" : lookX;
                String lookY = activity.get("lookY");
                lookY = (lookY == null) ? "0.0" : lookY;
                String lookZ = activity.get("lookZ");
                lookZ = (lookZ == null) ? "0.0" : lookZ;
                String upX = activity.get("upX");
                upX = (upX == null) ? "0.0" : upX;
                String upY = activity.get("upY");
                upY = (upY == null) ? "0.0" : upY;
                String upZ = activity.get("upZ");
                upZ = (upZ == null) ? "0.0" : upZ;
                charamelAct = mActionLoader.loadCharamelAnimation(activity_name, posX, posY, posZ, lookX, lookY, lookZ, upX, upY, upZ, aid);
                activity.setType(Type.parallel);
            } else {
                mLogger.warning("unknowns charamel activity");
                // Unknown activity_name
            }
        }

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
        mLogger.message("\033 Executor recieverd input:\n" + input + "\033[0m");
        final String message = XMLUtilities.xmlStringToPrettyXMLString(
                input.replaceAll("..xml\\s+version........", ""));
        // Print some information
        mLogger.message("\033[1;35mHandling new message:\n" + message + "\033[0m");
        // Check and notify the relevant threads
        // still necessary? synchronized (mActivityWorkerMap) {
        Feedback charamelFeedback = new Feedback();
        try {
            XMLUtilities.parseFromXMLStream(charamelFeedback,
                    new ByteArrayInputStream(message.getBytes(StandardCharsets.UTF_8)));
            mLogger.message("parsing done");
        } catch (final Exception exc) {
            mLogger.failure("Error: " + exc.toString());
            exc.printStackTrace();
            return;
        }

        // Handle action feedback
        handle(charamelFeedback);
        mLogger.message("handling done");

    }

    /**
     * Broadcast some message
     */
    private void broadcast(final String message) {
        mClientMap.values().stream().forEach((client) -> {
            client.send(message);
        });
    }

    /**
     * Show action waiting dialog
     */
    private void dialog(final String message) {
        WaitingDialog InfoDialog = new WaitingDialog(message);
        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
    }

    /**
     * Check if action file exists
     */
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

    public void handle(Feedback feedback) {
        handleChildren(feedback);
    }

    public void handle(Tts tts) {
        synchronized (mActivityWorkerMap) {
            switch (tts.mStatus) {
                case "start":
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
                    break;

                case "text_maker":
                    mLogger.message("tts text_maker");
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tts.mMarker);
                    break;

                case "text_marker":
                    mLogger.message("tts text_marker");
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tts.mMarker);
                    break;

                case "end":
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
                    break;
            }

            handleChildren(tts);
        }
    }

    public void handle(Action action) {
        handleChildren(action);
    }

    public void handle(CaiEvent caiEvent) {

        handleChildren(caiEvent);
    }

    private void handleChildren(CharaXMLElement charaXMLElement) {
        charaXMLElement.getChildren().stream().forEach((child) -> {
            child.handle(this);
        });
    }

    public void handle(CaiCommand caiCmd) {
        handleChildren(caiCmd);
    }

    public void handle(CaiResponse caiRsp) {
        List<CharaXMLElement> children = caiRsp.getChildren();
        String status = "";
        String actionID = "";
        for (CharaXMLElement child : children) {
            if (child instanceof Status) status = child.getText();
            if (child instanceof CaiCommand) actionID = ((CaiCommand) child).getMId();
        }
        mLogger.message("found status,actionID: " + status + ", " + actionID);
        if ((status.equalsIgnoreCase("success") || status.equalsIgnoreCase("failure")) && !actionID.equals("")) {
            synchronized (mActivityWorkerMap) {
                if (mActivityWorkerMap.containsKey(actionID)) {
                    mActivityWorkerMap.remove(actionID);
                } else {
                    mLogger.failure("Activityworker for " + actionID + " has been stopped before ...");
                }
                // wake me up ..
                mActivityWorkerMap.notifyAll();
            }
        }
        handleChildren(caiRsp);
    }

    public void handle(Status status) {
        handleChildren(status);
    }
}
