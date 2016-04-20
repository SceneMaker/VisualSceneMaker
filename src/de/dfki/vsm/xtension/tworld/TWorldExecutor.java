package de.dfki.vsm.xtension.tworld;

import de.dfki.stickman.StickmanStage;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
//import de.dfki.vsm.runtime.RunTimeInstance;
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
import de.dfki.vsm.xtension.tworld.xml.command.object.action.charamel.Speak;
import de.dfki.vsm.xtension.tworld.xml.feedback.TWorldFeedback;
import de.dfki.vsm.xtension.tworld.xml.util.ActionLoader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

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
        try {
            mProcessMap.put(cactorexe, Runtime.getRuntime().exec(
                    "cmd /c start " + cactorexe + " " + cactorcmd, null, new File(cactordir)));
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

            // get the charamel avatar id
            String aid = mProject.getAgentConfig(activity.getActor()).getProperty("aid");
            // build action
            twcoa = ActionLoader.getInstance().loadCharamelAnimation("Speak", sa.getBlocks(), sa.getPunctuation(), aid);
        } else {
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
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("url", features));
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

            if (cmd.equalsIgnoreCase("Relase")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("RelaseLookAt")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd);
            }

            if (cmd.equalsIgnoreCase("Say")) {
                twcoa = ActionLoader.getInstance().loadAnimation(cmd, getActionFeatureValue("url", features));
            }

            if (cmd.equalsIgnoreCase("SetColor")) {
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
        }

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
                    if (mActivityWorkerMap.containsKey(id)) {
                        mActivityWorkerMap.remove(id);
                    }
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
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
