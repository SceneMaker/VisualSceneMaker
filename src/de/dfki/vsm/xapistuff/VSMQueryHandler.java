package de.dfki.vsm.xapistuff;

//package de.dfki.vsm.api;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.util.jpl.JPLEngine;
//import de.dfki.vsm.util.jpl.JPLResult;
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//
////~--- JDK imports ------------------------------------------------------------
//
//import java.io.IOException;
//
//import java.net.DatagramPacket;
//import java.net.DatagramSocket;
//import java.net.InetSocketAddress;
//import java.net.SocketAddress;
//
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//
///**
// * @author Gregor Mehlmann
// */
//public class VSMQueryHandler extends Thread {
//
//    // The Datagram Packet For Messages
//    private final byte[]         mBuffer = new byte[1024];    // 2^14
//    private final DatagramPacket mPacket = new DatagramPacket(mBuffer, mBuffer.length);
//
//    // Get The System logger
//    public final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
//
//    // The Message Pattern
//    private final Pattern mPattern = Pattern.compile("<query type=\"(.*?)\">(.*?)</query>");
//
//    // The Datagram Socket To Receive The TOP
//    // Touch Events And The According Buffer
//    private DatagramSocket mSocket;
//
//    // The Local Socket Handler Address
//    private SocketAddress mLAddr;
//
//    // The Remote Socket Handler Address
//    private SocketAddress mRAddr;
//
//    // The Scene Player
//    private final VSMScenePlayer mPlayer;
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public VSMQueryHandler(final VSMScenePlayer player) {
//
//        // Initialize The Player
//        mPlayer = player;
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public final void init(final String lhost, final int lport, final String rhost, final int rport,
//                           final boolean rconn) {
//        try {
//
//            // Create The New Socket
//            // Create The Addresses
//            mLAddr = new InetSocketAddress(lhost, lport);
//
//            // Create The UDP Socket
//            mSocket = new DatagramSocket(mLAddr);
//
//            // Debug Some Information
//            mLogger.message("Initializing SWI Query Handler");
//
//            // Connect The UDP Socket
//            if (rconn) {
//
//                // Create The Addresses
//                mRAddr = new InetSocketAddress(rhost, rport);
//
//                // Connect The UDP Socket
//                mSocket.connect(mRAddr);
//
//                // Debug Some Information
//                mLogger.message("Connecting SWI Query Handler");
//            }
//
//            // Print Debug Information
//            mLogger.message("Constructing SWI Query Handler");
//        } catch (Exception exc) {
//
//            // Debug Some Information
//            mLogger.warning(exc.toString());
//        }
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public final void abort() {
//
//        // Close The Datagram Socket
//        if (mSocket != null) {
//            try {
//
//                // Close The Datagram Socket
//                if (!mSocket.isClosed()) {
//                    mSocket.close();
//
//                    // Debug Some Information
//                    mLogger.message("Aborting SWI Query Handler");
//                }
//            } catch (Exception exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        } else {
//
//            // Debug Some Information
//            mLogger.message("Cannot Abort SWI Query Handler");
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
//        mLogger.message("Starting SWI Query Handler");
//
//        while ((mSocket != null) &&!mSocket.isClosed()) {
//            try {
//
//                // Debug Some Information
//                mLogger.message("SWI Query Handler Trying To Receive");
//
//                // Receive The Data Packet
//                mSocket.receive(mPacket);
//
//                // Debug Some Information
//                mLogger.message("SWI Query Handler Received Some Data");
//
//                // Get The String Data
//                final String received = new String(mPacket.getData(), 0, mPacket.getLength(), "UTF-8");
//
//                // Print Some Information
//                mLogger.message("SWI Query Handler Receiving:\r\n" + received);
//
//                // Parse The Received
//                final Matcher matcher = mPattern.matcher(received);
//
//                //
//                if (matcher.matches()) {
//
//                    // Get The Prolog Query
//                    final String type  = matcher.group(1);
//                    final String query = matcher.group(2);
//
//                    // Print Some Information
//                    mLogger.message("SWI Query Handler Executing:\r\n" + query);
//
//                    // Get The Prolog Query
//                    if (type.equalsIgnoreCase("vsm")) {
//
//                        // Execute The Query In SceneMaker
//                        mPlayer.query(query);
//
//                        // Create The Answer Packet
//                        // final DatagramPacket answer = new DatagramPacket(
//                        // received.toString().getBytes(),
//                        // received.toString().length(),
//                        // mPacket.getSocketAddress());
//                        // Send The Answer Packet
//                        // mSocket.send(answer);
//                    } else if (type.equalsIgnoreCase("jpl")) {
//
//                        // Execute The Query Directly
//                        JPLResult result = JPLEngine.query(query);
//
//                        // Print Some Information
//                        mLogger.message("SWI Query Handler Resulting:\r\n" + result.toString());
//
//                        // Create The Answer Packet
//                        // final DatagramPacket answer = new DatagramPacket(
//                        // result.toString().getBytes(),
//                        // result.toString().length(),
//                        // mPacket.getSocketAddress());
//                        // Send The Answer Packet
//                        // mSocket.send(answer);
//                    }
//                } else {
//
//                    // Create The Answer Packet
//                    // final DatagramPacket answer = new DatagramPacket(
//                    // received.toString().getBytes(),
//                    // received.toString().length(),
//                    // mPacket.getSocketAddress());
//                    // Send The Answer Packet
//                    // mSocket.send(answer);
//                }
//            } catch (IOException exc) {
//
//                // Debug Some Information
//                mLogger.warning(exc.toString());
//            }
//        }
//
//        // Debug Some Information
//        mLogger.message("Stopping SWI Query Handler");
//    }
//}
