package de.dfki.vsm.api;

import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.configs.PlayerConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.runtime.Environment;
import de.dfki.vsm.runtime.RunTime;
import de.dfki.vsm.runtime.player.ScenePlayer;
import de.dfki.vsm.runtime.player.ScenePlayer.Task;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.log.LOGNovaFileLogger;
import de.dfki.vsm.util.log.LOGSSISockLogger;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public abstract class VSMScenePlayer implements ScenePlayer {

    // The VSM Runtime Environment
    protected final RunTime mVSM3RunTime
            = RunTime.getInstance();
    // The System File Logger
    protected final LOGDefaultLogger mVSM3Log
            = LOGDefaultLogger.getInstance();
    // A Nova Logger Instance
    protected final LOGNovaFileLogger mNovaLog
            = LOGNovaFileLogger.getInstance();
    // A Nova Logger Instance
    protected final LOGSSISockLogger mSockLog
            = LOGSSISockLogger.getInstance("127.0.0.1", 4000);
    // The ScenePlayer Config
    protected final PlayerConfig mPlayerConfig;
    // The SceneMaker Project
    protected final ProjectData mProjectData;
    // The SceneMaker Project
    protected final SceneFlow mSceneFlow;
    // The System Timer Thead  
    protected volatile long mStartupTime;
    protected volatile long mCurrentTime;
    protected VSMSystemTimer mSystemTimer;
    // The Query Handler
    protected VSMQueryHandler mQueryHandler;
    // The Event Handler
    protected SSIEventHandler mEventHandler;
    // The Event Handler
    protected MSSEventHandler mTouchHandler;
    // The Waiting Tasks 
    protected final HashMap<String, Task> mPlayerTaskQueue = new HashMap<>();
    // The Agent Clients    
    protected HashMap<String, VSMAgentClient> mAgentClientMap = new HashMap<>();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected VSMScenePlayer(final ProjectData project) {
        // Init SceneMaker 3 Project 
        mProjectData = project;
        // Init The SceneMaker Sceneflow
        mSceneFlow = project.getSceneFlow();
        // Init Scene Player Config
        mPlayerConfig = project.getScenePlayerProperties();
        // Print Debug Information
        mVSM3Log.message("Creating Generic Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void launch() {
        // Print Debug Information
        mVSM3Log.message("Launching Generic Player");
        // Initialize The Properties
        final String numagent = mPlayerConfig.property("agents.count");
        for (int i = 1; i <= Integer.parseInt(numagent); i++) {
            // Get Agent's Name, Host And Port
            final String name = mPlayerConfig.property("agent." + i + ".name");
            final String uaid = mPlayerConfig.property("agent." + i + ".uaid");
            final String host = mPlayerConfig.property("agent." + i + ".host");
            final String port = mPlayerConfig.property("agent." + i + ".port");

            // Print Out The Properties
            mVSM3Log.message(""  
                    + "Agent-Name:" + name + "\r\n"
                    + "Agent-Uaid:" + uaid + "\r\n"
                    + "Agent-Host:" + host + "\r\n"
                    + "Agent-Port:" + port);
            // Create A Client For This Agent
            VSMAgentClient client = new VSMAgentClient(this, name, uaid, host, Integer.parseInt(port));
            // Add The Client To Map
            mAgentClientMap.put(name, client);
            // Now Start The Client
            client.start();
            // Print Debug Information
            mVSM3Log.message("Starting Agent '" + name + "' With Id '" + uaid + "' On '" + host + ":" + port + "'");
        }
        // Initialize The Properties
        final String msslhost = mPlayerConfig.property("msslhost");
        final String msslport = mPlayerConfig.property("msslport");
        final String mssrhost = mPlayerConfig.property("mssrhost");
        final String mssrport = mPlayerConfig.property("mssrport");
        final String mssrconn = mPlayerConfig.property("mssrconn");
        final String swilhost = mPlayerConfig.property("swilhost");
        final String swilport = mPlayerConfig.property("swilport");
        final String swirhost = mPlayerConfig.property("swirhost");
        final String swirport = mPlayerConfig.property("swirport");
        final String swirconn = mPlayerConfig.property("swirconn");
        final String swilbase = mPlayerConfig.property("swilbase");
        final String ssilhost = mPlayerConfig.property("ssilhost");
        final String ssilport = mPlayerConfig.property("ssilport");
        final String ssirhost = mPlayerConfig.property("ssirhost");
        final String ssirport = mPlayerConfig.property("ssirport");
        final String ssirconn = mPlayerConfig.property("ssirconn");
        final String nlufbase = mPlayerConfig.property("nlufbase");
        // Print Out The Properties
        mVSM3Log.message(""
                + "SWI-Local-Host :" + swilhost + "\r\n"
                + "SWI-Remote-Host :" + swirhost + "\r\n"
                + "SWI-Local-Port :" + swilport + "\r\n"
                + "SWI-Remote-Port :" + swirport + "\r\n"
                + "SWI-Remote-Flag :" + swirconn + "\r\n"
                + "SWI-Base-Files :" + swilbase + "\r\n"
                + "MSS-Local-Host :" + msslhost + "\r\n"
                + "MSS-Remote-Host :" + mssrhost + "\r\n"
                + "MSS-Local-Port :" + msslport + "\r\n"
                + "MSS-Remote-Port :" + mssrport + "\r\n"
                + "MSS-Remote-Flag :" + mssrconn + "\r\n"
                + "NLU-Base-Files :" + nlufbase + "\r\n"
                + "SSI-Local-Host :" + ssilhost + "\r\n"
                + "SSI-Remote-Host :" + ssirhost + "\r\n"
                + "SSI-Local-Port :" + ssilport + "\r\n"
                + "SSI-Remote-Port :" + ssirport + "\r\n"
                + "SSI-Remote-Flag :" + ssirconn + "");
        // Initialize the JPL Engine
        JPLEngine.init();
        // Load The Prolog Program
        JPLEngine.load(swilbase + "/*.pl");
        // Create The Query Handler
        mQueryHandler = new VSMQueryHandler(this);
        // Initialize The System Timer
        mSystemTimer = new VSMSystemTimer(this, 10);
        // Init SSI Speech Server
        mEventHandler = new SSIEventHandler(this);
        // Init Touch Event Server
        mTouchHandler = new MSSEventHandler(this);
        //Initialize Query Handler
        mQueryHandler.init(swilhost, Integer.parseInt(swilport),
                swirhost, Integer.parseInt(swirport),
                Boolean.parseBoolean(swirconn));
        // Initialize The Event Handler
        mEventHandler.init(
                ssilhost, Integer.parseInt(ssilport),
                ssirhost, Integer.parseInt(ssirport),
                Boolean.parseBoolean(ssirconn));
        // Initialize The Event Handler
        mTouchHandler.init(
                msslhost, Integer.parseInt(msslport),
                mssrhost, Integer.parseInt(mssrport),
                Boolean.parseBoolean(mssrconn));
        // Now Start The System Timer
        mSystemTimer.start();
        // Now Start The Query Handler
        mQueryHandler.start();
        // Now Start The Event Handler
        mEventHandler.start();
        // Now Start The Event Handler
        mTouchHandler.start();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void unload() {
        // Shutdown Agent Clients
        for (VSMAgentClient client : mAgentClientMap.values()) {
            // Shutdown Agent Client
            client.abort();
            // Join With Agent Client
            try {
                // Join With Agent Client
                client.join();
                // Print Debug Information
                mVSM3Log.message("Joining Agent Client");
            } catch (Exception exc) {
                // Print Debug Information
                mVSM3Log.warning(exc.toString());
            }
        }
        // Shutdown Other Threads
        mEventHandler.abort();
        mTouchHandler.abort();
        mQueryHandler.abort();
        mSystemTimer.abort();
        // Join With All Threads
        try {
            // Join With The Event Handler
            mEventHandler.join();
            // Print Debug Information
            mVSM3Log.message("Joining Event Handler");
            // Join With The Event Handler
            mTouchHandler.join();
            // Print Debug Information
            mVSM3Log.message("Joining Touch Handler");
            // Join With The Query Handler
            mQueryHandler.join();
            // Print Debug Information
            mVSM3Log.message("Joining Query Handler");
            // Join With The System Timer
            mSystemTimer.join();
            // Print Debug Information
            mVSM3Log.message("Joining System Timer");
        } catch (Exception exc) {
            // Print Debug Information
            mVSM3Log.warning(exc.toString());
        }
        // Clear The Task Map
        mPlayerTaskQueue.clear();
        // Clear The Agents Map
        mAgentClientMap.clear();
        // Print Debug Information
        mVSM3Log.message("Unloading Generic Player");
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
    public final HashMap<String, Task> getPlayerTaskQueue() {
        return mPlayerTaskQueue;
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
    public final synchronized boolean query(final String querystr) {
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
                        environment.write(entry.getKey(),
                                new StringValue(entry.getValue()));
                        // Print Some Information
                        //System.err.println(entry.getKey() + "->" + entry.getValue());
                    } catch (Exception exc) {
                        // Print Debug Information
                        mVSM3Log.failure(exc.toString());
                    }
                }
            } catch (Exception exc) {
                // Try To Set The Variables Globally
                // Because An Extern Thread Is Trying
                for (Map.Entry<String, String> entry : subst.entrySet()) {
                    RunTime.getInstance().setVariable(
                            mProjectData.getSceneFlow(),
                            entry.getKey(),
                            entry.getValue());
                }
            }
            return true;
        } else {
            return false;
        }
    }

    /*
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void print(final String printstr) {
        // Logg Some Message With Logger
        mVSM3Log.message(printstr);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void nova(final String type, final String value) {
        // Get The System Time
        final long time = getCurrentTime();
        // Get The Current Time
        final long date = System.currentTimeMillis();
        // Log Some Message To NOVA
        try {
            // Try To get The VSM Process
            final Process thread = ((Process) Thread.currentThread());
            final String node = thread.getName();
            // Log Some VSM Process Data
            final String logmsg = ""
                    + "<log "
                    + "type=\"" + type + "\" "
                    + "node=\"" + node + "\" "
                    + "date=\"" + date + "\" "
                    + "time=\"" + time + "\">"
                    + value
                    + "</log>";
            mNovaLog.message(logmsg);
            mSockLog.message(logmsg);
        } catch (Exception exc) {
            // Debug Some Inforamtion
            mVSM3Log.warning(exc.toString());
            try {
                // Try To get The VSM Process
                final Task thread = ((Task) Thread.currentThread());
                final String task = thread.getName();
                // Log Some VSM Process Data
                final String logmsg = ""
                        + "<log "
                        + "type=\"" + type + "\" "
                        + "task=\"" + task + "\" "
                        + "date=\"" + date + "\" "
                        + "time=\"" + time + "\">"
                        + value
                        + "</log>";
                mNovaLog.message(logmsg);
                mSockLog.message(logmsg);
            } catch (Exception ecx) {
                // Debug Some Inforamtion
                mVSM3Log.warning(ecx.toString());
                try {
                    // Try To get The VSM Process
                    final Thread thread = Thread.currentThread();
                    final String name = thread.getName();
                    // Log Some VSM Process Data
                    final String logmsg = ""
                            + "<log "
                            + "type=\"" + type + "\" "
                            + "name=\"" + name + "\" "
                            + "date=\"" + date + "\" "
                            + "time=\"" + time + "\">"
                            + value
                            + "</log>";
                    mNovaLog.message(logmsg);
                    mSockLog.message(logmsg);
                } catch (Exception xec) {
                    // Debug Some Inforamtion
                    mVSM3Log.failure(xec.toString());
                }
            }
        }
    }
    */

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected abstract void handle(final VSMAgentClient client);
}
