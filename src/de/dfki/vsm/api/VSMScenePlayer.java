package de.dfki.vsm.api;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.runtime.interpreter.Environment;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLUtility;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public abstract class VSMScenePlayer implements RunTimePlayer {

    // The VSM runtime environment
    protected final RunTimeInstance mRunTime = RunTimeInstance.getInstance();
    // The defaut system logger
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Waiting Tasks
    protected final HashMap<String, Thread> mThreadQueue = new HashMap<>();
    // The Agent Clients
    protected HashMap<String, VSMAgentClient> mAgentMap = new HashMap<>();

    // The Player Name
    protected String mPlayerName;
    // The ScenePlayer Config
    protected PlayerConfig mPlayerConfig;
    // The SceneMaker Project
    protected RunTimeProject mProject;
    // The SceneMaker Project
    protected SceneFlow mSceneFlow;

    // The System Timer Thead
    protected volatile long mStartupTime;
    protected volatile long mCurrentTime;
    protected VSMSystemTimer mSystemTimer;

    // Construct the scene player
    protected VSMScenePlayer() {
        // Print some information
        mLogger.message("Creating VSM Scene Player");
    }

    // Launch the scene player
    @Override
    public boolean launch(final RunTimeProject project) {
        // Initialize project data
        mProject = project;
        // Initialize the sceneflow
        mSceneFlow = mProject.getSceneFlow();
        // Initialize player config
        mPlayerName = mProject.getPlayerName(this);
        mPlayerConfig = project.getPlayerConfig(mPlayerName);

        // Initialize all the clients
        final int count = Integer.parseInt(
                mPlayerConfig.getProperty("vsm.agent.number"));
        // Construct individual clients
        for (int i = 0; i < count; i++) {
            final String name = mPlayerConfig.getProperty("vsm.agent." + i + ".name");
            final String uaid = mPlayerConfig.getProperty("vsm.agent." + i + ".uaid");
            final String type = mPlayerConfig.getProperty("vsm.agent." + i + ".type");
            final String rhost = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.host");
            final String rport = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.port");
            final String rflag = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.flag");
            final String lhost = mPlayerConfig.getProperty("vsm.agent." + i + ".local.host");
            final String lport = mPlayerConfig.getProperty("vsm.agent." + i + ".local.port");
            // Print some information
            mLogger.message(""
                    + "Agent #" + i + " Name        : '" + name + "'" + "\r\n"
                    + "Agent #" + i + " Uaid        : '" + uaid + "'" + "\r\n"
                    + "Agent #" + i + " Type        : '" + type + "'" + "\r\n"
                    + "Agent #" + i + " Remote Host : '" + rhost + "'" + "\r\n"
                    + "Agent #" + i + " Remote Port : '" + rport + "'" + "\r\n"
                    + "Agent #" + i + " Remote Flag : '" + rflag + "'" + "\r\n"
                    + "Agent #" + i + " Local Host  : '" + lhost + "'" + "\r\n"
                    + "Agent #" + i + " Local Port  : '" + lport);

            // Create a client of that type
            if (type.equals("tcp")) {
                final VSMAgentClient client = new VSMTCPSockClient(
                        this, name, uaid, rhost,
                        Integer.parseInt(rport));
                // Add the client to map
                mAgentMap.put(name, client);
                // Now start the client
                client.start();
            } else if (type.equals("udp")) {
                final VSMAgentClient client = new VSMUDPSockClient(
                        this, name, uaid, lhost,
                        Integer.parseInt(lport),
                        rhost, Integer.parseInt(rport),
                        Boolean.parseBoolean(rflag));
                // Add the client to map
                mAgentMap.put(name, client);
                // Now start the client
                client.start();
            } else {
                // Print some information
                mLogger.failure("Unknown protocol '" + type + "'");
            }
        }

        // Initialize prolog data
        final String swilhost = mPlayerConfig.getProperty("swi.handler.local.host");
        final String swilport = mPlayerConfig.getProperty("swi.handler.local.port");
        final String swirhost = mPlayerConfig.getProperty("swi.handler.remote.host");
        final String swirport = mPlayerConfig.getProperty("swi.handler.remote.port");
        final String swirconn = mPlayerConfig.getProperty("swi.handler.remote.flag");
        final String swilbase = mPlayerConfig.getProperty("swi.handler.local.base");

        // Print some information
        mLogger.message("" + "SWI Query Handler Local Host  : '" + swilhost + "'" + "\r\n"
                + "SWI Query Handler Remote Host : '" + swirhost + "'" + "\r\n"
                + "SWI Query Handler Local Port  : '" + swilport + "'" + "\r\n"
                + "SWI Query Handler Remote Port : '" + swirport + "'" + "\r\n"
                + "SWI Query Handler Remote Flag : '" + swirconn + "'" + "\r\n"
                + "SWI Query Handler Base Files  : '" + swilbase + "'");

        // Initialize the JPL engine
        JPLEngine.init();
        // And load the prolog sources
        JPLEngine.load(swilbase + "/*.pl");

        // Initialize the system timer
        mSystemTimer = new VSMSystemTimer(this, 10);
        // And start the system timer
        mSystemTimer.start();
        // Print some information
        mLogger.message("Launching VSM Scene Player");
        // Return true at success
        return true;
    }

    // Unload the scene player
    @Override
    public boolean unload() {

        // Shutdown all agent clients
        for (final VSMAgentClient client : mAgentMap.values()) {

            // Shutdown Agent Client
            client.abort();
            // Join With Agent Client
            try {
                client.join();
                // Print some information
                mLogger.message("Joining VSM Agent Client");
            } catch (Exception exc) {
                // Print some information
                mLogger.warning(exc.toString());
            }
        }

        // Shutdown system timer
        mSystemTimer.abort();
        // Join with system timer
        try {
            mSystemTimer.join();
            // Print Debug Information
            mLogger.message("Joining System Timer");
        } catch (Exception exc) {
            // Print some information
            mLogger.warning(exc.toString());
        }

        // Clear The Task Map
        mThreadQueue.clear();
        // Clear The Agents Map
        mAgentMap.clear();
        // Print Debug Information
        mLogger.message("Unloading VSM Scene Player");
        // Return true at success
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setStartupTime(final long value) {
        mStartupTime = value;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final long getStartupTime() {
        return mStartupTime;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setCurrentTime(final long value) {
        mCurrentTime = value;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final long getCurrentTime() {
        return mCurrentTime;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final HashMap<String, Thread> getWaitingThreadQueue() {
        return mThreadQueue;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final VSMAgentClient getAgentClient(final String name) {
        return mAgentMap.get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // TODO: Does This Method Need To be Synchronized
    // What Are The Advantaged If It Is Synchronized
    public final /*synchronized*/ boolean query(final String querystr) {

        // Print Debug Information
        //System.err.println("Executing Query '" + querystr + "' In JPL Engine '" + JPLEngine.string() + "'");
        // Make The Query To The KB
        JPLResult result = JPLEngine.query(querystr);

        // Check The Query Results
        if (result.size() == 1) {

            // Get The First And Single Substitution
            HashMap<String, String> subst = result.getFirst();

            // Try To Set The Variables Locally
            // Because A Local Thread Is Trying
            try {

                // Compute The Current Running Process
                Process thread = (Process) Thread.currentThread();

                // Get The Environment Of The Process
                Environment environment = thread.getEnvironment();

                // Set The Variables In The Environment
                for (Map.Entry<String, String> entry : subst.entrySet()) {
                    try {

                        // Print Some Information
                        //System.err.println(entry.getKey() + "->" + entry.getValue());
                        // Here we write a variable without having the interpreter lock!!!!!!! ?????????
                        //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        //try {
                        // Initialize The Lock
                        RunTimeInstance.getInstance().getLock(mProject).lock();
                        //
                        environment.write(entry.getKey(), new StringValue(JPLUtility.convert(entry.getValue())));

                    } catch (Exception exc) {

                        // Print Debug Information
                        mLogger.failure(exc.toString());
                    } finally {
                        RunTimeInstance.getInstance().getLock(mProject).unlock();
                    }
                }
            } catch (Exception exc) {

                // Try To Set The Variables Globally
                // Because An Extern Thread Is Trying
                for (Map.Entry<String, String> entry : subst.entrySet()) {
                    RunTimeInstance.getInstance().setVariable(mProject, entry.getKey(), entry.getValue());
                }
            }

            return true;
        } else {
            return false;
        }
    }

    // Handle a client connection
    protected abstract void handle(final VSMAgentClient client);
}
