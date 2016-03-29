package de.dfki.vsm.xapistuff;

//package de.dfki.vsm.api;
//
//import java.net.DatagramPacket;
//import java.net.DatagramSocket;
//import java.net.InetSocketAddress;
//import java.net.SocketAddress;
//import java.net.SocketException;
//import java.util.Arrays;
//
///**
// * @author Gregor
// */
//public final class VSMUDPSockClient extends VSMAgentClient {
//
//    // The datagram socket
//    private DatagramSocket mSocket;
//    // The local address
//    private SocketAddress mLocalAddr;
//    // The local host
//    private final String mLocalHost;
//    // The loacl port
//    private final int mLocalPort;
//    // The remote address
//    private SocketAddress mRemoteAddr;
//    // The remote flag
//    private final boolean mRemoteFlag;
//
//    // Construct the UDP client
//    public VSMUDPSockClient(
//            final VSMScenePlayer player,
//            final String name, final String uaid,
//            final String lhost, final int lport,
//            final String rhost, final int rport,
//            final boolean rflag) {
//        // Initialize the VSM client
//        super(player, name, uaid, rhost, rport);
//        // Initialize the UDP client
//        mLocalHost = lhost;
//        mLocalPort = lport;
//        mRemoteFlag = rflag;
//        // Print some information
//        mLogger.message("Creating UDP Agent Client" + " "
//                + "'" + mAgentName + "' With Id '" + mAgentUaid + "'"
//                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
//                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
//    }
//
//    // Start the UDP client thread
//    @Override
//    public final void start() {
//        try {
//            // Create local addresses
//            mLocalAddr = new InetSocketAddress(mLocalHost, mLocalPort);
//            // Create the UDP socket
//            mSocket = new DatagramSocket(mLocalAddr);
//            // Connect the UDP socket
//            if (mRemoteFlag) {
//                // Create remote address
//                mRemoteAddr = new InetSocketAddress(mRemoteHost, mRemotePort);
//                // Connect the UDP socket
//                mSocket.connect(mRemoteAddr);
//                // Print some information
//                mLogger.message("Connecting UDP Agent Client" + " "
//                        + "'" + mAgentName + "' With Id '" + mAgentUaid + "'"
//                        + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
//                        + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
//            }
//        } catch (final SocketException exc) {
//            // Print some information
//            mLogger.failure(exc.toString());
//        }
//        // Start the client thread 
//        super.start();
//    }
//
//    // Abort the client connection
//    @Override
//    public final void abort() {
//        // Set Termination Flag
//        mDone = true;
//        // Close The Socket Now
//        if ((mSocket != null) && !mSocket.isClosed()) {
//            mSocket.close();
//        }
//    }
//
//    // Execute the client connection
//    @Override
//    public final void run() {
//        // Print some information
//        mLogger.message("Starting UDP Agent Client" + " "
//                + "'" + mAgentName + "' With Id '" + mAgentUaid + "'"
//                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
//                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
//        // Execute while not done
//        try {
//            while (!mDone) {
//                // Constantly handle data
//                mPlayer.handle(this);
//            }
//        } catch (final Exception exc) {
//            // Print some information
//            mLogger.failure(exc.toString());
//        }
//        // Print some information
//        mLogger.message("Stopping UDP Agent Client" + " "
//                + "'" + mAgentName + "' With Id '" + mAgentUaid + "'"
//                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
//                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
//    }
//
//    // Send some bytes via the client
//    @Override
//    public final boolean sendBytes(final byte[] bytes) {
//        // Return false at failure
//        return false;
//    }
//
//    // Send some string via the socket
//    @Override
//    public final boolean sendString(final String string) {
//        try {
//            // Create the byte buffer
//            final byte[] buffer = string.getBytes("UTF-8");
//            // Create the UDP packet
//            final DatagramPacket packet
//                    = new DatagramPacket(buffer, buffer.length);
//            // And send the UDP packet
//            mSocket.send(packet);
//               // Return true at success
//            return true;
//        } catch (final Exception exc) {
//            // Print some information
//            mLogger.failure(exc.toString());
//            // Return false at failure 
//            return false;
//        }
//
//    }
//
//    // Receive some bytes via the socket
//    @Override
//    public final byte[] recvBytes(final int size) {
//        try {
//            // Construct a byte array
//            final byte[] buffer = new byte[size];
//            // Construct an UDP packet
//            final DatagramPacket packet
//                    = new DatagramPacket(buffer, buffer.length);
//            // Receive the UDP packet
//            mSocket.receive(packet);
//            // Return the buffer now
//            return Arrays.copyOf(buffer, packet.getLength());
//        } catch (final Exception exc) {
//            // Print some information
//            mLogger.warning(exc.toString());
//            // Return null at failure 
//            return null;
//        }
//    }
//
//    // Receive some string via the socket
//    @Override
//    public final String recvString() {
//        try {
//            // Receive a byte buffer
//            final byte[] buffer = recvBytes(4096);
//            // Check the buffer content
//            if (buffer != null) {
//                // Construct a message
//                final String message
//                        = new String(buffer, 0, buffer.length, "UTF-8");
//                // And return message
//                return message;
//            }
//        } catch (final Exception exc) {
//            // Print some information
//            mLogger.failure(exc.toString());
//        }
//        // Return null at failure 
//        return null;
//    }
//}
