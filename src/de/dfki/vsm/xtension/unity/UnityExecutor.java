package de.dfki.vsm.xtension.unity;

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

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class UnityExecutor extends ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The tworld listener
    private UnityListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, UnityHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    // The unity cmd id
    private static int cmdId = 0;

    // Construct the executor
    public UnityExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
    }

    // Launch the executor 
    @Override
    public void launch() {
        // Get the plugin configuration
//        final String tworlddir = mConfig.getProperty("tworlddir");
//        final String tworldexe = mConfig.getProperty("tworldexe");
//        final String tworldcmd = mConfig.getProperty("tworldcmd");
//        final String cactordir = mConfig.getProperty("cactordir");
//        final String cactorexe = mConfig.getProperty("cactorexe");
//        final String cactorcmd = mConfig.getProperty("cactorcmd");

//        // Create the plugin's processes
//        try {
//            mProcessMap.put(cactorexe, Runtime.getRuntime().exec(
//                    "cmd /c start /min " + cactorexe + " " + cactorcmd, null, new File(cactordir)));
//            mProcessMap.put(tworldexe, Runtime.getRuntime().exec(
//                    "cmd /c start " + tworldexe + " " + tworldcmd, null, new File(tworlddir)));
//        } catch (final Exception exc) {
//            mLogger.failure(exc.toString());
//        }
        // Create the connection
        mListener = new UnityListener(8000, this);
        // Start the connection
        mListener.start();
        //
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for Unity");
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
        for (final UnityHandler client : mClientMap.values()) {
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
        final LinkedList<ActionFeature> features = activity.getFeatures();

        // initialize build command
        String unityCommand = "";

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
                unityCommand = cmdId + ", " + activity.getActor() + ", " + sa.toString() + sa.getPunct();

            }
        } else {
            unityCommand = cmdId + ", " + activity.getActor() + ", " + activity.getName();

        }

        mLogger.message("Execute Actor " + activity.getActor() + ", command " + unityCommand);

        // send command to platform 
        synchronized (mActivityWorkerMap) {
            broadcast(unityCommand);

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(cmdId + "", cAW);

            // wait until we got feedback
            mLogger.warning("ActivityWorker " + cmdId + " waiting ....");

            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }

            mLogger.warning("ActivityWorker " + cmdId + " done ....");
        }
        // Return when terminated
    }

    // Accept some socket
    public void accept(final Socket socket) {
        // Make new client thread 
        final UnityHandler client = new UnityHandler(socket, this);
        // Add the client to list
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        //
        mLogger.warning("Accepting " + client.getName() + "");
    }

    // Handle some message
    public void handle(final String message, final UnityHandler client) {
        // sanitize message

        mLogger.warning("Handling " + message + "");

        synchronized (mActivityWorkerMap) {
            if (message.contains("#TRIGGER#")) {
                int start = message.lastIndexOf("#") + 1;
                String trigger = message.substring(start);
               
                mProject.setVariable("unitytrigger", new StringValue(trigger));
            }
            
            if (message.contains("#CMD#end#")) {
                int start = message.lastIndexOf("#") + 1;
                String animId = message.substring(start);

                if (mActivityWorkerMap.containsKey(animId)) {
                    mActivityWorkerMap.remove(animId);
                }
                // wake me up ..
                mActivityWorkerMap.notifyAll();
            } else if (message.contains("$")) {
                // wake me up ..
                mActivityWorkerMap.notifyAll();
                mProject.getRunTimePlayer().getActivityScheduler().handle(message);
            }
        }
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final UnityHandler client : mClientMap.values()) {
            client.send(message);
        }
    }
}
