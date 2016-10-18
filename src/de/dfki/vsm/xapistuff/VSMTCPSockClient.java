package de.dfki.vsm.xapistuff;

//package de.dfki.vsm.api;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.util.bin.BINUtilities;
//
////~--- JDK imports ------------------------------------------------------------
//
//import java.io.BufferedReader;
//import java.io.BufferedWriter;
//import java.io.DataInputStream;
//import java.io.DataOutputStream;
//import java.io.IOException;
//import java.io.InputStreamReader;
//import java.io.OutputStreamWriter;
//
//import java.net.Socket;
//
///**
// * @author Gregor Mehlmann
// */
//public final class VSMTCPSockClient extends VSMAgentClient {
//
//    // The Client Socket
//    private Socket mSocket;
//
//    // The Data Streams
//    private DataInputStream  mInput;
//    private DataOutputStream mOutput;
//
//    // The Reader&Writer
//    private BufferedReader mReader;
//    private BufferedWriter mWriter;
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public VSMTCPSockClient(final VSMScenePlayer player, final String name, final String uaid, final String rhost,
//                            final int rport) {
//
//        // Initialize The Client
//        super(player, name, uaid, rhost, rport);
//
//        // Debug Some Information
//        mLogger.message("Creating TCP Agent Client For '" + name + "' With Id '" + uaid + "' On '" + rhost + ":"
//                        + rport + "'");
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final void start() {
//        try {
//
//            // Create The New Socket
//            mSocket = new Socket(mRemoteHost, mRemotePort);
//
//            // Init In&Output Stream
//            mInput  = new DataInputStream(mSocket.getInputStream());
//            mOutput = new DataOutputStream(mSocket.getOutputStream());
//
//            // Establish IO Channels
//            mReader = new BufferedReader(new InputStreamReader(mSocket.getInputStream(), "UTF-8"));
//            mWriter = new BufferedWriter(new OutputStreamWriter(mSocket.getOutputStream(), "UTF-8"));
//
//            // Debug Some Information
//            mLogger.message("Constructing TCP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
//                            + mRemoteHost + ":" + mRemotePort + "'");
//        } catch (final IOException exc) {
//
//            // Debug Some Information
//            mLogger.failure(exc.toString());
//        }
//
//        // Start The Client Thread
//        super.start();
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final void abort() {
//
//        // Set Termination Flag
//        mDone = true;
//
//        // Close The Socket Now
//        if ((mSocket != null) &&!mSocket.isClosed()) {
//            try {
//
//                // Close The Socket
//                mSocket.close();
//            } catch (final IOException exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        }
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final void run() {
//
//        // Debug Some Information
//        mLogger.message("Starting TCP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
//                        + mRemoteHost + ":" + mRemotePort + "'");
//
//        // Execute While Not Done
//        try {
//            while (!mDone) {
//
//                // Constantly Handle Data
//                mPlayer.handle(this);
//            }
//        } catch (final Exception exc) {
//
//            // Debug Some Information
//            mLogger.failure(exc.toString());
//        }
//
//        // Debug Some Information
//        mLogger.message("Stopping TCP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
//                        + mRemoteHost + ":" + mRemotePort + "'");
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final boolean sendBytes(final byte[] bytes) {
//
//        // Try To Send The Message
//        if (mOutput != null) {
//            try {
//
//                // Write Out The Message
//                mOutput.write(bytes);
//
//                // Flush Out The Stream
//                mOutput.flush();
//
//                // Debug Some Information
//                mLogger.message("Sending Message '" + BINUtilities.BytesToHexString(bytes)
//                                + "' Over TCP Agent Client '" + mAgentName + "' With Id '" + mAgentUaid + "'");
//
//                // Return At Success
//                return true;
//            } catch (Exception exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        } else {
//
//            // Debug Some Information
//            mLogger.warning("Cannot Send Over TCP Agent Client '" + mAgentName + "' With Id '" + mAgentUaid + "'");
//        }
//
//        // Return At Failure
//        return false;
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final boolean sendString(final String string) {
//
//        // Try To Send The Message
//        if (mWriter != null) {
//            try {
//
//                // Write Out The Message
//                mWriter.write(string);
//                mWriter.newLine();
//                mWriter.flush();
//
//                // Debug Some Information
//                mLogger.message("Sending Message '" + string + "' Over TCP Agent Client '" + mAgentName + "' With Id '"
//                                + mAgentUaid + "'");
//
//                // Return At Success
//                return true;
//            } catch (Exception exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        } else {
//
//            // Debug Some Information
//            mLogger.warning("Cannot Send Over TCP Agent Client '" + mAgentName + "' With Id '" + mAgentUaid + "'");
//        }
//
//        // Return At Failure
//        return false;
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final byte[] recvBytes(final int size) {
//
//        // Try To Read The Line
//        if (mInput != null) {
//            try {
//
//                // Read In The Answer
//                byte[] bytes = new byte[size];
//
//                mInput.readFully(bytes);
//
//                // Debug Some Information
//                mLogger.message("Reading Message'" + BINUtilities.BytesToHexString(bytes) + "' From TCP Agent Client '"
//                                + mAgentName + "' With Id '" + mAgentUaid + "'");
//
//                // Return The Notification
//                return bytes;
//            } catch (Exception exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        } else {
//
//            // Debug Some Information
//            mLogger.warning("Cannot Read From TCP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "'");
//        }
//
//        // Otherwise Return Null
//        return null;
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public final String recvString() {
//
//        // Check The Reader State
//        if (mReader != null) {
//            try {
//
//                // Read In The Answer
//                final String line = mReader.readLine();
//
//                // Debug Some Information
//                mLogger.message("Reading Message'" + line + "'");
//
//                // Return The Notification
//                return line;
//            } catch (Exception exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        }
//
//        // Otherwise Return Null
//        return null;
//    }
//}
