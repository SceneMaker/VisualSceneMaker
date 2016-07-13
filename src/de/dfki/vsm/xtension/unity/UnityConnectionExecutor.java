package de.dfki.vsm.xtension.unity;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.unity.commands.Command;
import de.dfki.vsm.xtension.unity.commands.RaiseHandCommand;
import de.dfki.vsm.xtension.unity.commands.SpeechCommand;
import java.io.File;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * @author J.-L. Himbert <s9jehimb@stud.uni-saarland.de>
 */
public final class UnityConnectionExecutor extends ActivityExecutor {
    public final String MarkerStartSign = "$(";
    public final String MarkerEndSign = ")";

    private final LOGConsoleLogger _logger;
    private UnityConnectionListener _listener;
    private final HashMap<String, Process> _processes;
    private final HashMap<String, UnityConnectionHandler> _clients;
    private final HashMap<String, ActivityWorker> _activityWorkers;
    private static int _cmdId = 0;

    public UnityConnectionExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);

        _logger = LOGConsoleLogger.getInstance();
        _processes = new HashMap();
        _clients = new HashMap();
        _activityWorkers = new HashMap();
    }

    @Override
    public void launch() {
        
        // Get the plugin configuration
//        final String applicationPath = mConfig.getProperty("path");
//        final String applicationName = mConfig.getProperty("exe");
//        int applicationPort;
//        try {
//            applicationPort = Integer.parseInt(mConfig.getProperty("port"));
//        } catch (NumberFormatException ex) {
//            _logger.warning("Using default port 3000 because of exception:" + 
//                            ex.toString());
//            applicationPort = 3000;
//        }
//
//        // TODO: Comment out!
//        // Create the plugin's processes
//        try {
//            _logger.message("Starting a new instance of the Unity3D application.");
////            _processes.put(applicationName, 
////                           Runtime.getRuntime().exec("cmd /c start /min " + applicationName, null, new File(applicationPath)));
//        } catch (final Exception ex) {
//            _logger.failure(ex.toString());
//        }
//
//        // Create and start the connection
//        _listener = new UnityConnectionListener(applicationPort, this);
//        _listener.start();
//
//        // Wait for new clients
//        while (_clients.isEmpty()) {
//            _logger.message("Waiting for new connection from the Unity3D plugin.");
//            try {
//                Thread.sleep(1000);
//            } catch (final InterruptedException ex) { 
//                _logger.failure(ex.toString());
//            }
//        }

        // Maybe we send a ready command here that is acknowledged by the unity plugin...
        // broadcast("Start");

    }

    @Override
    public void unload() {
        // Unload all clients
//        for (final UnityConnectionHandler client : _clients.values()) {
//            client.abort();
//
//            try {
//                _logger.message("Joining client thread");
//                client.join();
//            } catch (final Exception ex) {
//                _logger.failure(ex.toString());
//            }
//        }
//        _clients.clear();
//        
//        // Unload the server
//        try {
//            _listener.abort();
//            
//            _logger.message("Joining server thread");
//            _listener.join();
//        } catch (final Exception ex) {
//            _logger.failure(ex.toString());
//        }
//
//        // Wait for pawned processes
//        for (final Entry<String, Process> entry : _processes.entrySet()) {
//            final String name = entry.getKey();
//            final Process process = entry.getValue();
//            try {
//                // Kill the processes
//                final Process killer = Runtime.getRuntime().exec("taskkill /F /IM " + name);
//                _logger.message("Joining killer " + name + "");
//                killer.waitFor();
//
//                _logger.message("Joining process " + name + "");
//                process.waitFor();
//            } catch (final Exception ex) {
//                _logger.failure(ex.toString());
//            }
//        }
//
//        // Clear the map of processes 
//        _processes.clear();
    }

    @Override
    public synchronized final String marker(final long id) {
        return MarkerStartSign + id + MarkerEndSign;
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        if (activity == null) {
            _logger.message("The activity parameter was null. Skipped.");
            return;
        }
        
        Command command = null;
        
        String actor = activity.getActor();
        
        if (activity instanceof SpeechActivity) {
            SpeechActivity speech = (SpeechActivity)activity;

            // If text is empty - assume activity has marker activities registered
            if (speech.getTextOnly(MarkerStartSign).trim().isEmpty()) {
                for (String timemark : speech.getTimeMarks(MarkerStartSign)) {
                    _logger.warning("Directly executing activity at timemark " + timemark);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(timemark);
                    return;
                }
            } else {
                command = new SpeechCommand(_cmdId++, speech);
            }
        } else  {
            ActionActivity action = (ActionActivity)activity;
            String cmd = action.getName();
            
            
            if (cmd.equalsIgnoreCase("Options")){
                _logger.message("We have Options! actor=" + actor + ", cmd=" + cmd);
                
                // There have to be two features: display and values
                final LinkedList<ActionFeature> features = activity.getFeatureList();
                
                String displayStr = getActionFeatureValue("display", features);
                String valueStr = getActionFeatureValue("value", features);

                _logger.message("display=" + displayStr + ", value=" + valueStr);

                
                return;
            }
            
        }
        
//        if (activity.getName().equalsIgnoreCase("raisehand")) {
//            command = new RaiseHandCommand(_cmdId++, activity.getActor());
//        } else {
//            _logger.message("Unknown command found: " + activity.getName());
//            return;
//        }
        
        _logger.message("Execute Actor " + activity.getActor() + ", command " + command.toString());
        synchronized (_activityWorkers) {
            broadcast(command);

            // organize wait for feedback
            ActivityWorker activityWorker = (ActivityWorker)Thread.currentThread();
            _activityWorkers.put(Integer.toString(command.getCommandId()), activityWorker);

            // wait until we got feedback
            _logger.warning("ActivityWorker " + command.getCommandId() + " waiting...");

            while (_activityWorkers.containsValue(activityWorker)) {
                try {
                    _activityWorkers.wait();
                } catch (InterruptedException ex) {
                    _logger.failure(ex.toString());
                }
            }

            _logger.success("ActivityWorker " + command.getCommandId() + " done.");
        }
    }

    // Accept some socket
    public void accept(final Socket socket) {
        final UnityConnectionHandler client = new UnityConnectionHandler(socket, this);
        _logger.message("Accepting new client: " + client.getName());
        _clients.put(client.getName(), client);
        client.start();
    }
    
    // Handle some message
    public void handle(final Command command, final UnityConnectionHandler client) {
        _logger.message("Handling command: " + command.toString());

        synchronized (_activityWorkers) {
            if (command instanceof SpeechCommand) {
                SpeechCommand speech = (SpeechCommand)command;
                // Do whatever needs to be done!
            }
            else if (command instanceof RaiseHandCommand) {
                RaiseHandCommand raiseHand = (RaiseHandCommand)command;
                // Do whatever needs to be done!
                // example:
                mProject.setVariable("name", "value");
            }
            else {
                _logger.warning("Command unhandled: " + command.toString());
            }
            
            /* 
             * Maybe, we just send a message command back and do it like below?
             */
            /* 
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
            */
        }
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

    // Broadcast some message
    private void broadcast(final Command command) {
        _clients.values().stream().forEach((client) -> { client.send(command); });
    }
    
}
