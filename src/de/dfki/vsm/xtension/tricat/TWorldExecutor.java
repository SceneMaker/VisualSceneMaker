package de.dfki.vsm.xtension.tricat;

import de.dfki.vsm.model.project.DeviceConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import java.io.File;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 */
public final class TWorldExecutor extends ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The executor's name
    private final String mName;
    // The runtime project
    //private final RunTimeProject mProject;
    //
    private TWorldListener mServer;
    // The mao of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, TWorldHandler> mClientMap = new HashMap();

    // Construct the executor
    public TWorldExecutor(final String name, final RunTimeProject project) {
        // Initialize the plugin
        super(project);
        // Initialize the name
        mName = name;
        // Initialize the project
        //mProject = project;
    }

    // Launch the executor 
    @Override
    public void launch() {
        try {
            mProcessMap.put("EmpatTest.exe", Runtime.getRuntime().exec(
                    "cmd /c start \"" + "\" EmpatTest.exe", null,
                    new File("D:\\EmpaT\\software\\EmpaT\\CharActorServer")));

            mProcessMap.put("EmpaT.exe", Runtime.getRuntime().exec(
                    "cmd /c start \"" + "\" EmpaT.exe "
                    + "-SceneMakerIP 127.0.0.1 -SceneMakerPort 8000 "
                    + "-CharActorIP 127.0.0.1 -CharActorPort 4000", null,
                    new File("D:\\EmpaT\\software\\EmpaT")));
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }

        // Parse the configuration
        final DeviceConfig config = mProject.getDeviceConfig(mName);
        // Create the connection
        mServer = new TWorldListener(8000, this);
        // Start the connection
        mServer.start();
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
            mServer.abort();
            // Join the client thread
            mServer.join();
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
        return "\\mrk=" + id + "\\";
    }

    @Override
    public final void execute(
            final AbstractActivity activity,
            final ActivityManager scheduler) {
        // Compile the activity
        final String command = activity.toString();
        // Print some information
        //System.err.println("Command '" + command + "'");
        //
        final String message = ""
                + "<TWorldCommand>\n"
                + "<object name=\"Susanne\">\n"
                + "<action name=\"caixml\" id=\"734\">\n"
                + "<!-- Charamel Command -->\n"
                + "<cai_request version='1.0'>\n"
                + "<cai_command id=\"2\">\n"
                + "RenderXML\n"
                + "<animation_track>\n"
                + "<pause>\n"
                + "</pause>\n"
                + "<motion \n"
                + "speed='1.0' \n"
                + "attack='1000' \n"
                + "decay='1000' \n"
                + "start='0' \n"
                + "duration='2000'>\n"
                + "walk/turns/turn_90r\n"
                + "</motion>\n"
                + "</animation_track>\n"
                + "</cai_command>\n"
                + "</cai_request>\n"
                + "</action>\n"
                + "</object>\n"
                + "</TWorldCommand>";
        broadcast(message);

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
