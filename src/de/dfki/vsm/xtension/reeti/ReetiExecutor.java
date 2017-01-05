package de.dfki.vsm.xtension.reeti;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.hcm.robots.messaging.CommandMessage;
import de.hcm.robots.messaging.StatusMessage;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ReetiExecutor extends ActivityExecutor {

    // The unique command id counter
    private volatile long mCmdId = 0;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The map of activity workers
    private final HashMap<String, ActivityWorker> mWorkerMap
            = new HashMap();
    // The reeti connection handler
    private ReetiHandler mHandler;
    
    // record the curernt positon of the motor (neckRotat)
    private static int iPosition;

    // Construct the executor
    public ReetiExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
    }

    // Get a new unique id
    private synchronized long newCmdId() {
        return ++mCmdId;
    }

    // Launch the executor 
    @Override
    public void launch() {
        // Get the plugin configuration
        final String lhost = mConfig.getProperty("lhost");
        final String rhost = mConfig.getProperty("rhost");
        final int lport = Integer.parseInt(mConfig.getProperty("lport"));
        final int rport = Integer.parseInt(mConfig.getProperty("rport"));
        // Start the connection handler
        mHandler = new ReetiHandler(
                this, lhost, lport, rhost, rport);
        mHandler.start();
    }

    // Unload the executor 
    @Override
    public void unload() {
        // Abort the connection handler
        mHandler.abort();
        try {
            // Joint the onnection handler
            mHandler.join();
        } catch (final InterruptedException exc) {
            mLogger.failure(exc.toString());
        }
    }

    @Override
    public final String marker(final long id) {
        // Loquendo style bookmarks
        return "\\book=" + id + "";
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        // Get the current worker
        final ActivityWorker worker = (ActivityWorker) Thread.currentThread();
        // Get activity information
        final String name = activity.getName();
        final String mode = activity.getMode();
        final String actor = activity.getActor();
        final String type = activity.getType().name();
        final String text = activity.getText();
        final LinkedList<ActionFeature> features = activity.getFeatures();
        // Create new command id
        final String cmid = activity + ":" + String.valueOf(newCmdId());
        // Create the new command 
        CommandMessage command = null;
        
        if (activity instanceof SpeechActivity) {
            String activityText = ((SpeechActivity)activity).getTextOnly("\\book=").trim();
            if (activityText.isEmpty()) {
                    LinkedList<String> timemarks = ((SpeechActivity)activity).getTimeMarks("\\book=");
                    for (String tm : timemarks) {
                        mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                    }
            }else{
                     // Create the speech command
                command = new CommandMessage(cmid, "speech");
                // Append the tts text param
                command.addParameter("text", "\\voice=" + "Cereproc" + " " + "\\language=" + "en" + " " + text);
//                command.addParameter("text", "\\voice=" + "Kate" + " " + "\\language=" + "en" + " " + text);
            }
        } else if (activity instanceof ActionActivity) {
            // Create the action command
            command = new CommandMessage(cmid, name);
            // Append the action features
            for (final ActionFeature feature : features) {
                // if there are some variables in the features, change them to the values we want.
                if(ReetiCommandUtility.checkCommandValue(feature.getVal())){
                    command.addParameter(feature.getKey(), ReetiCommandUtility.updateCommandValue(feature.getVal()));
                }else{
                    command.addParameter(feature.getKey(), feature.getVal().replaceAll("'", ""));
                }
            }
        } else {
            // Print some error message
        }

        if (command != null) {
            // Print some information
            mLogger.warning("Activity worker " + worker + " executing behavior activity " + activity + ":\n"
                    + "Activity features:\n"
                    + "    actor: " + actor + "\n"
                    + "    mode: " + mode + "\n"
                    + "    name: " + name + "\n"
                    + "    type: " + type + "\n"
                    + "    text: " + text + "\n"
                    + "Constructing command with id " + cmid + " and content \n" + command.toString() + "\n");

            // Send command to platform 
            synchronized (mWorkerMap) {
                // Try to send the command
                if (mHandler.sendString(command.toString())) {
                    // Then let the worker sleep ...
                    mWorkerMap.put(cmid, worker);
                    // ... until the command is done
                    while (mWorkerMap.containsValue(worker)) {
                        try {
                            mWorkerMap.wait();
                        } catch (final InterruptedException exc) {
                            mLogger.failure(exc.toString());
                        }
                    }
                }
            }
        }
    }

    // Handle a notification message
    public void handle(final String message) {
        // Parse a status message
        final StatusMessage status = new StatusMessage(message);
        final String uid = status.getTaskID();
        final String typ = status.getStatus();
        // Print some debug information
        mLogger.message("Receiving status message with id " + uid + " and status " + typ);

        synchronized (mWorkerMap) {
            // Check the type of the message
            if (typ.equals("finished")) {
                // Remove the worker thread
                final Thread worker = mWorkerMap.remove(uid);
                // Debug information
                mLogger.message("Worker '" + worker + "' finished execution of command with id '" + uid + "'");
                // Notify waiting workers
                mWorkerMap.notifyAll();
            } else if (typ.equals("request")) {
                // record the current motor position
                String sPosition = status.getStatusDetails().get("neckRotat");
                iPosition = Integer.parseInt(sPosition);
                // Remove the worker thread
                final Thread worker = mWorkerMap.remove(uid);
                // Debug information
                mLogger.message("Worker '" + worker + "' request execution of command with id '" + uid + "'");
                // Notify waiting workers
                mWorkerMap.notifyAll();
            }else if (status.equals("rejected")) {
                // Remove the worker thread
                final Thread worker = mWorkerMap.remove(uid);
                // Debug information
                mLogger.message("Worker '" + worker + "' rejected execution of command with id '" + uid + "'");
                // Notify waiting workers
                mWorkerMap.notifyAll();
            } else if (typ.equals("bookmark")) {
                // Get the marker    
                final String id = status.getStatusDetails().get("id");
                // Get the worker thread
                final Thread worker = mWorkerMap.get(uid);
                // Debug information
                mLogger.message("Worker '" + worker + "' arrived bookmark with id " + id + " during execution of command with id '" + uid + "'");
                //
                final String marker = marker(Long.parseLong(id.replaceAll("[.!?,;:]", "")));
                // Handle the marker
                mScheduler.handle(marker);
            } else {
                //
            }
        }
    }
    
    public static int getCurrentPosition(){
        return iPosition;
    }
}
