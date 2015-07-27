package de.dfki.vsm.api;

import de.dfki.vsm.model.config.ConfigElement;
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
import de.dfki.vsm.util.log.LOGNovaFileLogger;
import de.dfki.vsm.util.log.LOGSSISockLogger;
import java.io.File;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Not me
 */
public abstract class VSMScenePlayer implements RunTimePlayer {

    // The VSM Runtime Environment
    protected final RunTimeInstance mVSM3RunTime = RunTimeInstance.getInstance();

    // The System File Logger
    protected final LOGDefaultLogger mVSM3Log = LOGDefaultLogger.getInstance();

    // A Nova Logger Instance
    protected final LOGNovaFileLogger mNovaLog = LOGNovaFileLogger.getInstance();

    // A Nova Logger Instance
    protected final LOGSSISockLogger mSockLog = LOGSSISockLogger.getInstance("127.0.0.1", 4000);

    // The Waiting Tasks
    protected final HashMap<String, Thread> mWaitingThreadQueue = new HashMap<>();

    // The Agent Clients
    protected HashMap<String, VSMAgentClient> mAgentClientMap = new HashMap<>();

    // The Player Name
    protected String mPlayerName;
    // The ScenePlayer Config
    protected PlayerConfig mPlayerConfig;
    // The SceneMaker Project
    protected RunTimeProject mProjectData;
    // The SceneMaker Project
    protected SceneFlow mSceneFlow;

    // The System Timer Thead
    protected volatile long mStartupTime;
    protected volatile long mCurrentTime;
    protected VSMSystemTimer mSystemTimer;

    // The Query Handler
    protected VSMQueryHandler mQueryHandler;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected VSMScenePlayer() {

        // Print Debug Information
        mVSM3Log.message("Creating VSM Abstract Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public boolean launch(final RunTimeProject project) {
        // Init SceneMaker 3 Project
        mProjectData = project;
        mSceneFlow = mProjectData.getSceneFlow();
        mPlayerName = mProjectData.getPlayerName(this);
        mPlayerConfig = project.getPlayerConfig(mPlayerName);
        // Print Debug Information
        mVSM3Log.message("Launching VSM Scene Player");

        // Initialize The Properties
        final String numagent = mPlayerConfig.getProperty("vsm.agent.number");
        
        for (int i = 0; i < Integer.parseInt(numagent); i++) {

            // Get Agent's Initial Data
            final String name = mPlayerConfig.getProperty("vsm.agent." + i + ".name");
            final String uaid = mPlayerConfig.getProperty("vsm.agent." + i + ".uaid");
            final String type = mPlayerConfig.getProperty("vsm.agent." + i + ".type");
            final String rhost = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.host");
            final String rport = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.port");
            final String rflag = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.flag");
            final String lhost = mPlayerConfig.getProperty("vsm.agent." + i + ".local.host");
            final String lport = mPlayerConfig.getProperty("vsm.agent." + i + ".local.port");

            // Print Out The Properties
            mVSM3Log.message("" + "Agent #" + i + " Name        : '" + name + "'" + "\r\n" + "Agent #" + i
                    + " Uaid        : '" + uaid + "'" + "\r\n" + "Agent #" + i + " Type        : '" + type
                    + "'" + "\r\n" + "Agent #" + i + " Remote Host : '" + rhost + "'" + "\r\n" + "Agent #" + i
                    + " Remote Port : '" + rport + "'" + "\r\n" + "Agent #" + i + " Remote Flag : '" + rflag
                    + "'" + "\r\n" + "Agent #" + i + " Local Host  : '" + lhost + "'" + "\r\n" + "Agent #" + i
                    + " Local Port  : '" + lport);

            // Create A Client For This Agent
            // Check The Type Of This Agent
            if (type.equals("tcp")) {
                final VSMAgentClient client = new VSMTCPSockClient(this, name, uaid, rhost, Integer.parseInt(rport));

                // Add The Client To Map
                mAgentClientMap.put(name, client);

                // Now Start The Client
                client.start();
            } else if (type.equals("udp")) {
                final VSMAgentClient client = new VSMUDPSockClient(this, name, uaid, lhost, Integer.parseInt(lport),
                        rhost, Integer.parseInt(rport), Boolean.parseBoolean(rflag));

                // Add The Client To Map
                mAgentClientMap.put(name, client);

                // Now Start The Client
                client.start();
            } else {

                // Unknown Protocol
            }
        }

        // Initialize The Properties
        final String swilhost = mPlayerConfig.getProperty("swi.handler.local.host");
        final String swilport = mPlayerConfig.getProperty("swi.handler.local.port");
        final String swirhost = mPlayerConfig.getProperty("swi.handler.remote.host");
        final String swirport = mPlayerConfig.getProperty("swi.handler.remote.port");
        final String swirconn = mPlayerConfig.getProperty("swi.handler.remote.flag");
        final String swilbase = mPlayerConfig.getProperty("swi.handler.local.base");

        // Print Out The Properties
        mVSM3Log.message("" + "SWI Query Handler Local Host  : '" + swilhost + "'" + "\r\n"
                + "SWI Query Handler Remote Host : '" + swirhost + "'" + "\r\n"
                + "SWI Query Handler Local Port  : '" + swilport + "'" + "\r\n"
                + "SWI Query Handler Remote Port : '" + swirport + "'" + "\r\n"
                + "SWI Query Handler Remote Flag : '" + swirconn + "'" + "\r\n"
                + "SWI Query Handler Base Files  : '" + swilbase + "'");

        // Initialize the JPL Engine
        JPLEngine.init();
        
        File file = new File(swilbase);
        
        mVSM3Log.message(file.getAbsolutePath());

        // Load The Prolog Program
        JPLEngine.load(swilbase + "/*.pl");

        // Create The Query Handler
        mQueryHandler = new VSMQueryHandler(this);

        // Initialize The System Timer
        mSystemTimer = new VSMSystemTimer(this, 10);

        // Initialize Query Handler
        mQueryHandler.init(swilhost, Integer.parseInt(swilport), swirhost, Integer.parseInt(swirport),
                Boolean.parseBoolean(swirconn));

        // Now Start The System Timer
        mSystemTimer.start();

        // Now Start The Query Handler
        mQueryHandler.start();

        // Return true at success
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public boolean unload() {

        // Shutdown Agent Clients
        for (VSMAgentClient client : mAgentClientMap.values()) {

            // Shutdown Agent Client
            client.abort();

            // Join With Agent Client
            try {

                // Join With Agent Client
                client.join();

                // Print Debug Information
                mVSM3Log.message("Joining Generic VSM Agent Client");
            } catch (Exception exc) {

                // Print Debug Information
                mVSM3Log.warning(exc.toString());
            }
        }

        // Shutdown Other Threads
        // mQueryHandler.abort();
        mSystemTimer.abort();

        // Join With All Threads
        try {

            // Join With The Query Handler
            // mQueryHandler.join();
            // Print Debug Information
            mVSM3Log.message("Awaiting System Timer");

            // Join With The System Timer
            mSystemTimer.join();

            // Print Debug Information
            mVSM3Log.message("Joining System Timer");
        } catch (Exception exc) {

            // Print Debug Information
            mVSM3Log.warning(exc.toString());
        }

        // Clear The Task Map
        mWaitingThreadQueue.clear();

        // Clear The Agents Map
        mAgentClientMap.clear();

        // Print Debug Information
        mVSM3Log.message("Unloading Generic VSM Scene Player");

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
        return mWaitingThreadQueue;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final VSMAgentClient getAgentClient(final String name) {
        return mAgentClientMap.get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // TODO: Does This Method Need To be Synchronized
    // What Are The Advantaged If It Is Synchronized
    public final /* synchronized */ boolean query(final String querystr) {

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
                        environment.write(entry.getKey(), new StringValue(JPLUtility.convert(entry.getValue())));

                        // Print Some Information
                        // System.err.println(entry.getKey() + "->" + entry.getValue());
                    } catch (Exception exc) {

                        // Print Debug Information
                        mVSM3Log.failure(exc.toString());
                    }
                }
            } catch (Exception exc) {

                // Try To Set The Variables Globally
                // Because An Extern Thread Is Trying
                for (Map.Entry<String, String> entry : subst.entrySet()) {
                    RunTimeInstance.getInstance().setVariable(mProjectData, entry.getKey(), entry.getValue());
                }
            }
            
            return true;
        } else {
            return false;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setVariable(final String name, final String value) {
        if (mVSM3RunTime.hasVariable(mProjectData, name)) {

            // Debug Some Information
            mVSM3Log.message("Finding Variable '" + name + "' To Value '" + value + "'");

            // Set The Variable Now
            mVSM3RunTime.setVariable(mProjectData, name, value);

            // Debug Some Information
            mVSM3Log.message("Setting Variable '" + name + "' To Value '" + value + "'");
        } else {

            // Debug Some Information
            mVSM3Log.message("SceneMaker Variable '" + name + "' Not Available");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setVariable(final String name, final boolean value) {
        if (mVSM3RunTime.hasVariable(mProjectData, name)) {

            // Debug Some Information
            mVSM3Log.message("Finding Variable '" + name + "' To Value '" + value + "'");

            // Set The Variable Now
            mVSM3RunTime.setVariable(mProjectData, name, value);

            // Debug Some Information
            mVSM3Log.message("Setting Variable '" + name + "' To Value '" + value + "'");
        } else {

            // Debug Some Information
            mVSM3Log.message("SceneMaker Variable '" + name + "' Not Available");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setVariable(final String name, final int value) {
        if (mVSM3RunTime.hasVariable(mProjectData, name)) {

            // Debug Some Information
            mVSM3Log.message("Finding Variable '" + name + "' To Value '" + value + "'");

            // Set The Variable Now
            mVSM3RunTime.setVariable(mProjectData, name, value);

            // Debug Some Information
            mVSM3Log.message("Setting Variable '" + name + "' To Value '" + value + "'");
        } else {

            // Debug Some Information
            mVSM3Log.message("SceneMaker Variable '" + name + "' Not Available");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected abstract void handle(final VSMAgentClient client);
}
