package de.dfki.vsm.xapistuff;

//package de.dfki.vsm.api;
//
//import de.dfki.vsm.model.project.PlayerConfig;
//import de.dfki.vsm.runtime.project.RunTimeProject;
//import de.dfki.vsm.model.sceneflow.SceneFlow;
//import de.dfki.vsm.runtime.interpreter.Environment;
//import de.dfki.vsm.runtime.interpreter.Process;
//import de.dfki.vsm.runtime.RunTimeInstance;
//import de.dfki.vsm.runtime.player.RunTimePlayer;
//import de.dfki.vsm.runtime.values.StringValue;
//import de.dfki.vsm.util.jpl.JPLEngine;
//import de.dfki.vsm.util.jpl.JPLResult;
//import de.dfki.vsm.util.jpl.JPLUtility;
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//import java.util.HashMap;
//import java.util.Map;
//
///**
// * @author Gregor Mehlmann
// */
//public abstract class VSMScenePlayer implements RunTimePlayer {
//
//    // The runtime environment
//    protected final RunTimeInstance mRunTime
//            = RunTimeInstance.getInstance();
//    // The defaut system logger
//    protected final LOGDefaultLogger mLogger
//            = LOGDefaultLogger.getInstance();
//    // The quue of waiting tasks
//    protected final HashMap<String, Thread> mThreadQueue
//            = new HashMap<>();
//    // The map of agent clients 
//    protected HashMap<String, VSMAgentClient> mAgentMap
//            = new HashMap<>();
//    // The scene player name
//    protected String mPlayerName;
//    // The scene player config
//    protected PlayerConfig mPlayerConfig;
//    // The runtime project data
//    protected RunTimeProject mProject;
//    // The sceneflow of the project
//    protected SceneFlow mSceneFlow;
//    // The launch startup time
//    protected volatile long mStartupTime;
//    // The current player time
//    protected volatile long mCurrentTime;
//    // The system timer thread
//    protected VSMSystemTimer mSystemTimer;
//
//    // Construct the scene player
//    protected VSMScenePlayer() {
//        // Print some information
//        mLogger.message("Creating VSM Scene Player");
//    }
//
//    // Launch the scene player
//    @Override
//    public boolean launch(final RunTimeProject project) {
//        // Initialize project data
//        mProject = project;
//        // Initialize the sceneflow
//        mSceneFlow = mProject.getSceneFlow();
//        // Initialize player name
//        mPlayerName = mProject.getPlayerName(this);
//        // Initialize player config
//        mPlayerConfig = project.getPlayerConfig(mPlayerName);
//
//        // Initialize all the clients
//        final int count = Integer.parseInt(
//                mPlayerConfig.getProperty("vsm.agent.number"));
//        // Construct individual clients
//        for (int i = 0; i < count; i++) {
//            final String name = mPlayerConfig.getProperty("vsm.agent." + i + ".name");
//            final String uaid = mPlayerConfig.getProperty("vsm.agent." + i + ".uaid");
//            final String type = mPlayerConfig.getProperty("vsm.agent." + i + ".type");
//            final String rhost = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.host");
//            final String rport = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.port");
//            final String rflag = mPlayerConfig.getProperty("vsm.agent." + i + ".remote.flag");
//            final String lhost = mPlayerConfig.getProperty("vsm.agent." + i + ".local.host");
//            final String lport = mPlayerConfig.getProperty("vsm.agent." + i + ".local.port");
//            // Print some information
//            mLogger.message(""
//                    + "Agent " + i + " Name        : '" + name + "'" + "\r\n"
//                    + "Agent " + i + " Uaid        : '" + uaid + "'" + "\r\n"
//                    + "Agent " + i + " Type        : '" + type + "'" + "\r\n"
//                    + "Agent " + i + " Remote Host : '" + rhost + "'" + "\r\n"
//                    + "Agent " + i + " Remote Port : '" + rport + "'" + "\r\n"
//                    + "Agent " + i + " Remote Flag : '" + rflag + "'" + "\r\n"
//                    + "Agent " + i + " Local Host  : '" + lhost + "'" + "\r\n"
//                    + "Agent " + i + " Local Port  : '" + lport);
//            // Create a client of that type
//            if (type.equals("tcp")) {
//                final VSMAgentClient client = new VSMTCPSockClient(
//                        this, name, uaid, rhost,
//                        Integer.parseInt(rport));
//                // Add the client to map
//                mAgentMap.put(name, client);
//                // Now start the client
//                client.start();
//            } else if (type.equals("udp")) {
//                final VSMAgentClient client = new VSMUDPSockClient(
//                        this, name, uaid,
//                        lhost, Integer.parseInt(lport),
//                        rhost, Integer.parseInt(rport),
//                        Boolean.parseBoolean(rflag));
//                // Add the client to map
//                mAgentMap.put(name, client);
//                // Now start the client
//                client.start();
//            } else {
//                // Print some information
//                mLogger.failure("Unknown protocol '" + type + "'");
//            }
//        }
//
//        // Initialize prolog data
//        final String swilhost = mPlayerConfig.getProperty("swi.handler.local.host");
//        final String swilport = mPlayerConfig.getProperty("swi.handler.local.port");
//        final String swirhost = mPlayerConfig.getProperty("swi.handler.remote.host");
//        final String swirport = mPlayerConfig.getProperty("swi.handler.remote.port");
//        final String swirconn = mPlayerConfig.getProperty("swi.handler.remote.flag");
//        final String swilbase = mPlayerConfig.getProperty("swi.handler.local.base");
//        // Print some information
//        mLogger.message(""
//                + "SWI Query Handler Local Host  : '" + swilhost + "'" + "\r\n"
//                + "SWI Query Handler Remote Host : '" + swirhost + "'" + "\r\n"
//                + "SWI Query Handler Local Port  : '" + swilport + "'" + "\r\n"
//                + "SWI Query Handler Remote Port : '" + swirport + "'" + "\r\n"
//                + "SWI Query Handler Remote Flag : '" + swirconn + "'" + "\r\n"
//                + "SWI Query Handler Base Files  : '" + swilbase + "'");
//
//        // Initialize the JPL engine
//        JPLEngine.init();
//        // And load the prolog sources
//        JPLEngine.load(swilbase + "/*.pl");
//
//        // Initialize the system timer
//        mSystemTimer = new VSMSystemTimer(this, 10);
//        // And start the system timer
//        mSystemTimer.start();
//        // Print some information
//        mLogger.message("Launching VSM Scene Player");
//        // Return true at success
//        return true;
//    }
//
//    // Unload the scene player
//    @Override
//    public boolean unload() {
//        // Shutdown all agent clients
//        for (final VSMAgentClient client : mAgentMap.values()) {
//            // Shutdown an agent client
//            client.abort();
//            // Join with the agent client
//            try {
//                client.join();
//                // Print some information
//                mLogger.message("Joining VSM Agent Client '" + client.getAgentName() + "'");
//            } catch (Exception exc) {
//                // Print some information
//                mLogger.warning(exc.toString());
//            }
//        }
//
//        // Shutdown the system timer
//        mSystemTimer.abort();
//        // Join with the system timer
//        try {
//            mSystemTimer.join();
//            // Print some information
//            mLogger.message("Joining VSM System Timer");
//        } catch (Exception exc) {
//            // Print some information
//            mLogger.warning(exc.toString());
//        }
//
//        // Clear map of waiting tasks
//        mThreadQueue.clear();
//        // Clear map of agent clients
//        mAgentMap.clear();
//        // Print some information
//        mLogger.message("Unloading VSM Scene Player");
//        // Return true at success
//        return true;
//    }
//
//    // Initialize the startup time
//    public final void setStartupTime(final long value) {
//        mStartupTime = value;
//    }
//
//    // Get the player startup time
//    public final long getStartupTime() {
//        return mStartupTime;
//    }
//
//    // Set the current player time
//    public final void setCurrentTime(final long value) {
//        mCurrentTime = value;
//    }
//
//    // Get the current player time
//    public final long getCurrentTime() {
//        return mCurrentTime;
//    }
//
//    // Execute a given JPL query
//    public final /*synchronized*/ boolean query(final String querystr) {
//        // TODO: Does This Method Need To be Synchronized?
//        // What Are The Advantaged If It Is Synchronized?
//
//        // Make The Query To The KB
//        JPLResult result = JPLEngine.query(querystr);
//
//        // Check The Query Results
//        if (result.size() == 1) {
//
//            // Get The First And Single Substitution
//            HashMap<String, String> subst = result.getFirst();
//
//            // Try To Set The Variables Locally
//            // Because A Local Thread Is Trying
//            try {
//
//                // Compute The Current Running Process
//                Process thread = (Process) Thread.currentThread();
//
//                // Get The Environment Of The Process
//                Environment environment = thread.getEnvironment();
//
//                // Set The Variables In The Environment
//                for (Map.Entry<String, String> entry : subst.entrySet()) {
//                    try {
//
//                        // Print Some Information
//                        //System.err.println(entry.getKey() + "->" + entry.getValue());
//                        // Here we write a variable without having the interpreter lock!!!!!!! ?????????
//                        //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//                        //try {
//                        // Initialize The Lock
//                        mRunTime.getLock(mProject).lock();
//                        
//                        
//                        // This call returns nothing if the variable exists and and throws an exeption
//                        environment.write(entry.getKey(), new StringValue(JPLUtility.convert(entry.getValue())));
//
//                    } catch (Exception exc) {
//
//                        // Print Debug Information
//                        mLogger.failure(exc.toString());
//                    } finally {
//                        RunTimeInstance.getInstance().getLock(mProject).unlock();
//                    }
//                }
//            } catch (Exception exc) {
//
//                // Try To Set The Variables Globally
//                // Because An Extern Thread Is Trying
//                for (Map.Entry<String, String> entry : subst.entrySet()) {
//                    RunTimeInstance.getInstance().setVariable(mProject, entry.getKey(), entry.getValue());
//                }
//            }
//
//            return true;
//        } else {
//            return false;
//        }
//    }
//
//    // Get the runtime project data
//    public final RunTimeProject getProject() {
//        return mProject;
//    }
//
//    // Get the runtime project data
//    public final RunTimeInstance getRunTime() {
//        return mRunTime;
//    }
//
//    // Handle a client connection
//    protected abstract void handle(final VSMAgentClient client);
//}
