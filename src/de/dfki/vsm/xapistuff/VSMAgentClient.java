package de.dfki.vsm.xapistuff;

//package de.dfki.vsm.api;
//
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//
///**
// * @author Gregor Mehlmann
// */
//public abstract class VSMAgentClient extends Thread {
//
//    // The singelton logger instance
//    protected final LOGDefaultLogger mLogger
//            = LOGDefaultLogger.getInstance();
//    // The thread terminatioin flag
//    protected volatile boolean mDone = false;
//    // The respective player instance
//    protected final VSMScenePlayer mPlayer;
//    // The agent client's features
//    protected final String mAgentName;
//    protected final String mAgentUaid;
//    protected final String mRemoteHost;
//    protected final int mRemotePort;
//   
//    // Construct an agent client
//    public VSMAgentClient(
//            final VSMScenePlayer player,
//            final String name, final String uaid,
//            final String host, final int port) {
//        // Initialize the player
//        mPlayer = player;
//        // Initialize the fields
//        mAgentName = name;
//        mAgentUaid = uaid;
//        mRemoteHost = host;
//        mRemotePort = port;
//        // Debug Some Information
//        mLogger.message("Creating VSM Agent Client '"
//                + name + "' With Id '" + uaid + "' To '" + host + ":" + port + "'");
//    }
//
//    // Get the agent client name
//    public final String getAgentName() {
//        return mAgentName;
//    }
//
//    // Get the agent client id
//    public final String getAgentUaid() {
//        return mAgentUaid;
//    }
//
//    // Get the agent client host
//    public final String getRemoteHost() {
//        return mRemoteHost;
//    }
//
//    // Get the agent client port
//    public final int getRemotePort() {
//        return mRemotePort;
//    }
//
//    // Run the client connection
//    @Override
//    public abstract void run();
//
//    // Abort the client connection
//    public abstract void abort();
//
//    // Send some bytes via client
//    public abstract boolean sendBytes(final byte[] bytes);
//
//    // Send some string via client
//    public abstract boolean sendString(final String string);
//
//    // Receive some bytes via client
//    public abstract byte[] recvBytes(final int size);
//
//    // Send some string via client
//    public abstract String recvString();
//}
