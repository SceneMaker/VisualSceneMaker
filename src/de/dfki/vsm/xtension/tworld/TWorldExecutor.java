package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
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
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class TWorldExecutor extends ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The tworld listener
    private TWorldListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, TWorldHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    // The word mapping properties
    Properties mWordMapping = new Properties();

    // Construct the executor
    public TWorldExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
    }

    // Launch the executor 
    @Override
    public void launch() {
        // Get the plugin configuration
        final String tworlddir = mConfig.getProperty("tworlddir");
        final String tworldexe = mConfig.getProperty("tworldexe");
        final String tworldcmd = mConfig.getProperty("tworldcmd");
        final String cactordir = mConfig.getProperty("cactordir");
        final String cactorexe = mConfig.getProperty("cactorexe");
        final String cactorcmd = mConfig.getProperty("cactorcmd");

        // Create the plugin's processes
        boolean isCactODirPresent = isPathExisting(cactordir);
        boolean isTWorldPresent = isPathExisting(tworlddir);

        if (!isCactODirPresent || !isTWorldPresent) {
            String missing = (!isCactODirPresent) ? cactordir : tworlddir;
            String message = "Missing installation folder " + missing;
            WaitingDialog InfoDialog = new WaitingDialog(message);
            InfoDialog.setModal(true);
            InfoDialog.setVisible(true);
            return;
        }
        try {
            mProcessMap.put(cactorexe, Runtime.getRuntime().exec(
                    "cmd /c start /min " + cactorexe + " " + cactorcmd, null, new File(cactordir)));
            mProcessMap.put(tworldexe, Runtime.getRuntime().exec(
                    "cmd /c start " + tworldexe + " " + tworldcmd, null, new File(tworlddir)));
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
        // Create the connection
        mListener = new TWorldListener(8000, this);
        // Start the connection
        mListener.start();
        //
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for TWorld");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {

            }
        }
        broadcast("Start");
    }

    private boolean isPathExisting(String path) {

        File f = new File(path);
        if (f.exists() && f.isDirectory()) {
            return true;
        }
        return false;
    }

    // Unload the executor 
    @Override
    public void unload() {
        // Abort the client threads
        for (final TWorldHandler client : mClientMap.values()) {
            client.abort();
            // Join the client thread
            try {
                client.join();
            } catch (final Exception exc) {
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
        } catch (final Exception exc) {
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
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Clear the map of processes 
        mProcessMap.clear();
    }

    @Override
    public synchronized final String marker(final long id) {
        // TWorld style bookmarks
        return "$(" + id + ")";
    }

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        // Get action information
        final String cmd = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatureList();

        // initialize build command
        TWorldCommand mTWC;
        Action twcoa = null;

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            //mLogger.success("text is " + text);
            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                    return;
                }
            } else {
                // load wordmapping database
                try {
                    String wmf = mProject.getProjectPath() + File.separator + mProject.getAgentConfig(activity.getActor()).getProperty("wordmapping");
                    wmf = wmf.replace("\\", "/");
                    mWordMapping.load(new FileReader(new File(wmf)));
                } catch (IOException ex) {
                    mLogger.failure("Wordmapping file (" + mProject.getAgentConfig(activity.getActor()).getProperty("wordmapping") + ") not found!");
                }

                // do the pronounciation mapping
                sa.doPronounciationMapping(mWordMapping);

                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Speak", sa.getBlocks(), sa.getPunctuation(), aid);

                // wait a little bit ...
                try {
                    Thread.sleep(350);
                } catch (final InterruptedException exc) {

                }
            }
        } else {
            if (cmd.equalsIgnoreCase("StopSpeaking")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("StopSpeaking", aid);
            }

            if (cmd.equalsIgnoreCase("Angry")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Angry", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Demanding")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Demanding", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Disgust")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Disgust", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Neutral")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Neutral", "1.0", aid);
            }

            if (cmd.equalsIgnoreCase("Sad")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Sad", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Smile")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Smile", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Happy")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                String intensity = getActionFeatureValue("intensity", features);
                intensity = (intensity == "") ? "1.0" : intensity;
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Happy", intensity, aid);
            }

            if (cmd.equalsIgnoreCase("Reject")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Reject", aid);
            }

            if (cmd.equalsIgnoreCase("Challenge")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Challenge", aid);
            }

            if (cmd.equalsIgnoreCase("ShowPalms")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("ShowPalms", aid);
            }

            if (cmd.equalsIgnoreCase("OpenArms")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("OpenArms", aid);
            }

            if (cmd.equalsIgnoreCase("LookLeft")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("LookLeft", aid);
            }

            if (cmd.equalsIgnoreCase("LookRight")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("LookRight", aid);
            }

            if (cmd.equalsIgnoreCase("No")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("No", aid);
            }

            if (cmd.equalsIgnoreCase("StrongNo")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("StrongNo", aid);
            }

            if (cmd.equalsIgnoreCase("PointLeft")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("PointLeft", aid);
            }

            if (cmd.equalsIgnoreCase("PointRight")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("PointRight", aid);
            }

            if (cmd.equalsIgnoreCase("PresentLeft")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("PresentLeft", aid);
            }

            if (cmd.equalsIgnoreCase("PresentRight")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("PresentRight", aid);
            }

            if (cmd.equalsIgnoreCase("Welcome")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Welcome", aid);
            }

            if (cmd.equalsIgnoreCase("Yes")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("Yes", aid);
            }

            if (cmd.equalsIgnoreCase("StrongYes")) {
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
                // build action
                twcoa = ActionLoader.getInstance().loadCharamelAnimation("StrongYes", aid);
            }

            if (cmd.equalsIgnoreCase("AmbientLight")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("value", features));
            }

            if (cmd.equalsIgnoreCase("AmbientSound")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("value", features));
            }

            if (cmd.equalsIgnoreCase("CancelMoveTo")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Load")) {
                String url = "file:///" + mProject.getProjectPath() + File.separator + mProject.getAgentConfig(activity.getActor()).getProperty(getActionFeatureValue("value", features));
                url = url.replace("\\", "/");
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, url);
            }

            if (cmd.equalsIgnoreCase("LookAt")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("viewtarget", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("MoveTo")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("location", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("Play")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Stop")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Release")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("ReleaseLookAt")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Say")) {
                String url = "file:///" + mProject.getProjectPath() + File.separator + mProject.getAgentConfig(activity.getActor()).getProperty(getActionFeatureValue("value", features));
                url = url.replace("\\", "/");
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, url);
            }

            if (cmd.equalsIgnoreCase("PlayAudio")) {
                String url = "file:///" + mProject.getProjectPath() + File.separator + mProject.getAgentConfig(activity.getActor()).getProperty(getActionFeatureValue("value", features));
                url = url.replace("\\", "/");
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, url);
            }

            if (cmd.equalsIgnoreCase("Color")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("r", features), getActionFeatureValue("g", features), getActionFeatureValue("b", features));
            }

            if (cmd.equalsIgnoreCase("SitDown")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("chairname", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("Stop")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Warp")) {
                String target = getActionFeatureValue("viewtarget", features);
                if (target != null) {
                    twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("location", features), target);
                } else {
                    twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("location", features));
                }
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("WorldPosition")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("x", features), getActionFeatureValue("y", features), getActionFeatureValue("z", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("Camera") && activity.getActor().equalsIgnoreCase("player")) { // this is a player only command
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("x", features), getActionFeatureValue("y", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("FocalLength") && activity.getActor().equalsIgnoreCase("player")) { // this is a player only command
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("value", features), getActionFeatureValue("time", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("DefaultFocalLength") && activity.getActor().equalsIgnoreCase("player")) { // this is a player only command
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("time", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("CameraOffset") && activity.getActor().equalsIgnoreCase("player")) { // this is a player only command
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("x", features), getActionFeatureValue("y", features), getActionFeatureValue("z", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }

            if (cmd.equalsIgnoreCase("Scale") && activity.getActor().equalsIgnoreCase("player")) { // this is a player only command
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("value", features));
                // reset the command name to include the actor which is required on tworld side - TODO get rid of this in Tworld side
                if (activity.getActor().equalsIgnoreCase("player")) {
                    twcoa.resetActionCmd(activity.getActor() + "_" + twcoa.getActionCmd());
                }
            }
        }

        mLogger.message("Building command " + cmd + " for Actor " + activity.getActor());
        // finalize build command
        Object twco = new Object(activity.getActor(), twcoa);
        mTWC = new TWorldCommand();
        mTWC.addObject(twco);

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(mTWC, iosw);

        mLogger.message("Execute Actor " + activity.getActor() + ", command " + cmd);

        String message = "";
        // log TWorld command
        //try {
        // Fuck German Umlaute and Encoding
        message = out.toString().replace("ö", "oe").replace("ä", "ae").replace("ü", "ue").replace("Ö", "Oe").replace("Ä", "Ae").replace("Ü", "Ue").replace("ß", "ss").replace("\n", " ").replace("   ", " ").replace("  ", " ");
        //message = new String(out.toString().getBytes("ISO-8859-1"), "UTF-8");
        mLogger.message(message);
        //} catch (UnsupportedEncodingException ex) {
        //    mLogger.failure(ex.getMessage());
        //}

        // send command to platform 
        synchronized (mActivityWorkerMap) {
            broadcast(message);

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(twcoa.getId(), cAW);

            // wait until we got feedback
            mLogger.warning("ActivityWorker " + twcoa.getId() + " waiting ....");

            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }

            mLogger.warning("ActivityWorker " + twcoa.getId() + " done ....");
        }
        // Return when terminated
    }

    // Accept some socket
    public void accept(final Socket socket) {
        // Make new client thread 
        final TWorldHandler client = new TWorldHandler(socket, this);
        // Add the client to list
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        //
        mLogger.warning("Accepting " + client.getName() + "");
    }

    // Handle some message
    public void handle(final String message, final TWorldHandler client) {
        // sanitize message
        String clean = message.replaceAll("..xml\\s+version........", "");
        mLogger.warning("Handling " + clean + "");

        synchronized (mActivityWorkerMap) {
            TWorldFeedback twf = new TWorldFeedback();
            InputStream stream;
            try {
                stream = new ByteArrayInputStream(clean.getBytes("UTF-8"));
                XMLUtilities.parseFromXMLStream(twf, stream);
            } catch (UnsupportedEncodingException ex) {
                mLogger.failure("Feedback is not in TWorldFeedback format, will not be handled!");
                return;
            }

            if (twf.hasActionFeedback()) {
                String actionType = twf.mFeedbackAction.mName;
                String id = twf.mFeedbackAction.mId;
                String actionStatusType = twf.mFeedbackAction.mActionFeedback.mName;
                String actionStatusValue = twf.mFeedbackAction.mActionFeedback.mValue;

                //mLogger.message("Action type " + actionType + ", id " + id + ", status " + actionStatusType + ", value " + actionStatusValue);
                // handling caixml feedback
//                if (actionType.equalsIgnoreCase("caixml") && actionStatusType.equalsIgnoreCase("action_finished")) {
//                    if (mActivityWorkerMap.containsKey(id)) {
//                        mActivityWorkerMap.remove(id);
//                    }
//                    // wake me up ..
//                    mActivityWorkerMap.notifyAll();
//                }
                // handling every /*ambient_setup*/ feedback
                if (/*actionType.equalsIgnoreCase("ambient_setup") && */actionStatusType.equalsIgnoreCase("action_finished")) {
                    // check if the acitivy action feedback was speech feedback
                    if (twf.mFeedbackAction.mActionFeedback.hasCaiEvent()) {
                        if (twf.mFeedbackAction.mActionFeedback.mCaiEvent.hasTTSStatus()) {
                            if (twf.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("start")) {
                                // TODO - get id - for now there is none
                                // Set character voice activity variable
                                mProject.setVariable("susanne_voice_activity", new StringValue("1"));
                                mProject.setVariable("tom_voice_activity", new StringValue("1"));
                            }
                            if (twf.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("text_maker")) {
                                mLogger.success("Handling Charamel Marker " + twf.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mMarker);
                                mProject.getRunTimePlayer().getActivityScheduler().handle(twf.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mMarker);
                            }
                            if (twf.mFeedbackAction.mActionFeedback.mCaiEvent.mTts.mStatus.equalsIgnoreCase("end")) {
                                // TODO - get id - for now there is none
                                // Set character voice activity variable
                                mProject.setVariable("susanne_voice_activity", new StringValue(""));
                                mProject.setVariable("tom_voice_activity", new StringValue(""));

                                // remove the activity
                                if (mActivityWorkerMap.containsKey(id)) {
                                    mActivityWorkerMap.remove(id);
                                }
                                // wake me up ..
                                mActivityWorkerMap.notifyAll();
                            }

                            // TODO marker!
                        }
                    } else {
                        // remove the activity in any case
                        if (mActivityWorkerMap.containsKey(id)) {
                            mActivityWorkerMap.remove(id);
                        }
                        // wake me up ..
                        mActivityWorkerMap.notifyAll();
                    }
                }
            }

            if (twf.hasObjectFeedback()) {
                HashMap<String, AbstractValue> values = new HashMap<>();
                values.put("type", new StringValue(twf.mFeedbackObject.mObjectFeedback.mName));
                values.put("elicitor", new StringValue(twf.mFeedbackObject.mObjectFeedback.mTriggerObject));
                values.put("name", new StringValue(twf.mFeedbackObject.mName));

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
}
