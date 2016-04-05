package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityScheduler;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.feedback.MarkerFeedback;
import de.dfki.vsm.runtime.activity.manager.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.tworld.command.TWorldCommand;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObject;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObjectAction;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObjectAmbientSetupAction;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObjectMoveToLoactionAction;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObjectSetSoundAmbientAction;
import de.dfki.vsm.xtension.tworld.command.TWorldCommandObjectCharamelSpeakAction;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
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
    // The execution id
    private int sId = 0;
    // my current activity
    private AbstractActivity mActivity = null;

    // Construct the executor
    public TWorldExecutor(final PluginConfig config, final RunTimeProject project) {
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
    public final String marker(final long id) {
        // TWorld style bookmarks
        //return "\\mrk=" + id + "\\";
        return "$(" + id + ")";
    }

    private final String getExecutionId() {
        return "" + sId++;
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
    public final void execute(
            final AbstractActivity activity,
            final ActivityScheduler scheduler) {
        // Compile the activity
//        final String command = activity.toString();
//        // Print some information
//        //System.err.println("Command '" + command + "'");
//        //

        // get action information
        final String actor = activity.getActor();
        final String name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatureList();

        mLogger.message("Execute Actor " + actor + ", command " + name);

        // initialize build command
        TWorldCommand mTWC;
        TWorldCommandObjectAction twcoa = null;

        if (name.equalsIgnoreCase("MoveTo")) {
            twcoa = new TWorldCommandObjectMoveToLoactionAction(getActionFeatureValue("location", features));
        }

        if (name.equalsIgnoreCase("SetAmbient")) {
            twcoa = new TWorldCommandObjectAmbientSetupAction(getActionFeatureValue("value", features));
        }

        if (name.equalsIgnoreCase("SetSound")) {
            twcoa = new TWorldCommandObjectSetSoundAmbientAction(getActionFeatureValue("value", features));
        }

        if (name.equalsIgnoreCase("Speak")) {
            if (activity instanceof SpeechActivity) {
                SpeechActivity sa = (SpeechActivity) activity;
                twcoa = new TWorldCommandObjectCharamelSpeakAction(sa.getBlocks(), sa.getPunctuation());
            }
        }

        // set the command id
        String executionId = getExecutionId();
        twcoa.setId(executionId);

        // finalize build command
        TWorldCommandObject twco = new TWorldCommandObject(actor, twcoa);
        mTWC = new TWorldCommand();
        mTWC.addObject(twco);

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(mTWC, iosw);

        String message = "";
        // log TWorld command
        try {
            message = new String(out.toByteArray(), "UTF-8").replace("\n", " ");
            mLogger.message(new String(out.toByteArray(), "UTF-8"));
        } catch (UnsupportedEncodingException ex) {
            mLogger.failure(ex.getMessage());
        }

//        final String message = ""
//                + "<TWorldCommand>\n"
//                + "<object name=\"Susanne\">\n"
//                + "<action name=\"caixml\" id=\"734\">\n"
//                + "<!-- Charamel Command -->\n"
//                + "<cai_request version='1.0'>\n"
//                + "<cai_command id=\"2\">\n"
//                + "RenderXML\n"
//                + "<animation_track>\n"
//                + "<pause>\n"
//                + "</pause>\n"
//                + "<motion \n"
//                + "speed='1.0' \n"
//                + "attack='1000' \n"
//                + "decay='1000' \n"
//                + "start='0' \n"
//                + "duration='2000'>\n"
//                + "walk/turns/turn_90r\n"
//                + "</motion>\n"
//                + "</animation_track>\n"
//                + "</cai_command>\n"
//                + "</cai_request>\n"
//                + "</action>\n"
//                + "</object>\n"
//                + "</TWorldCommand>";
        // send command to platform 
        synchronized (mActivityWorkerMap) {
            // Remember activity
            mActivity = activity;
            broadcast(message);

            // organize wait for feedback
            String id = getExecutionId();
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(id, cAW);

            // wait until we got feedback
            mLogger.warning("ActivityWorker " + id + " waiting ....");

            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }

            mLogger.warning("ActivityWorker " + id + " done ....");
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
        mLogger.warning("Handling " + message + "");

        synchronized (mActivityWorkerMap) {
            if (message.contains("action id=")) {

                int start = message.lastIndexOf("#") + 1;
                String animId = message.substring(start);

                if (mActivityWorkerMap.containsKey(animId)) {
                    mActivityWorkerMap.remove(animId);
                }
                // wake me up ..
                mActivityWorkerMap.notifyAll();

            }

            if (message.contains("$(")) {

                // wake me up ..
                mActivityWorkerMap.notifyAll();
                // play the acitiviy
                mProject.getScenePlayer().getActivityManager().handle(new MarkerFeedback(mActivity, message));

            }
        }
//        synchronized (mActivityWorkerMap) {
//            if (message.contains("734")) {
//                mActivityWorkerMap.remove("734");
//                mActivityWorkerMap.notifyAll();
//            }
//        }
    }

    // Handle some message
    public void handle(final String message, final SSIRunTimePlugin plugin) {
        mLogger.warning("Handling " + message + "");
        //
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final TWorldHandler client : mClientMap.values()) {
            client.send(message);
        }
    }
}
