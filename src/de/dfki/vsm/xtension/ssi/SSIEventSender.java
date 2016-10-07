package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.Arrays;

/**
 * @author Gregor Mehlmann
 */
final class SSIEventSender extends Thread {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The thread termination flag
    private boolean mDone = false;
    // The datagram connection 
    private DatagramSocket mSocket;
    // The local socket adress 
    private final int mLPort;
    private final String mLHost;
    private final SocketAddress mLAddr;
    // The remote socket adress 
    private final int mRPort;
    private final String mRHost;
    private final SocketAddress mRAddr;

    // Construct the proxy server
    public SSIEventSender(
            final String lHost, final int lPort,
            final String rHost, final int rPort) {
        // Initialize the local data
        mLHost = lHost;
        mLPort = lPort;
        // Initialize the remote data
        mRHost = rHost;
        mRPort = rPort;
        // Initialize the address data
        mLAddr = new InetSocketAddress(mLHost, mLPort);
        mRAddr = new InetSocketAddress(mRHost, mRPort);
        // Print some information
        mLogger.message("Creating SSI event handler local address " + mLAddr);
        mLogger.message("Creating SSI event handler remote address " + mRAddr);
    }

    // Execute the server thread
    @Override
    public final void start() {
        try {
            // Create the server socket
            mSocket = new DatagramSocket(mLAddr);
            // Connect the server socket
            mSocket.connect(mRAddr);
            // Print some information
            mLogger.message("Connecting SSI event handler:\n"
                    + " Local Address " + mLHost + ":" + mLPort + "\n"
                    + " Remote Address " + mRHost + ":" + mRPort);
            // Start the server thread
            super.start();
        } catch (final SocketException exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Abort the server thread
    public final void abort() {
        // Set the termination flag
        mDone = true;
        // Eventually close the socket
        if (mSocket != null && !mSocket.isClosed()) {
            mSocket.close();
        }
        // Interrupt if sleeping
        interrupt();
    }

    // Execute the server thread
    @Override
    public final void run() {
        // Receive while not done
        while (!mDone) {
            // Receive a new message
            final String message = recvString();
            // Chek the new message
            if (message != null) {
                // Do nothing here
            }
        }
    }

    // Receive a message
    public final String recv() {
        if (mSocket != null) {
            // Create the datagram packet
            final byte[] buffer = new byte[4096];
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // Try to receive new data
            try {
                // Receive the datagram packet
                mSocket.receive(packet);
                // Get the datagram's string 
                final String message = new String(
                        packet.getData(), 0,
                        packet.getLength(), "UTF-8");
                // Debug some information
                //mLogger.warning("SSI event sender receiving '" + message + "'");
                // Return received data
                return message;
            } catch (final IOException exc) {
                // Debug some information
                mLogger.warning(exc.toString());
                // Otherwise return null
                return null;
            }
        } else {
            // Otherwise return null
            return null;
        }
    }

    // Send a message 
    public final boolean sendString(final String string) {
        try {
            // Create the byte buffer
            final byte[] buffer = string.getBytes("UTF-8");
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // And send the UDP packet
            mSocket.send(packet);
            // Print some information
            //mLogger.message("SSI event handler sending '" + string + "'");
            // Return true at success
            return true;
        } catch (final IOException exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return false at failure 
            return false;
        }
    }

    // Receive a byte array
    private byte[] recvBytes() {
        try {
            // Construct a byte array
            final byte[] buffer = new byte[4096];
            // Construct an UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // Receive the UDP packet
            mSocket.receive(packet);
            // Return the buffer now
            return Arrays.copyOf(buffer, packet.getLength());
        } catch (final IOException exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return null at failure 
            return null;
        }
    }

    // Receive a string 
    private String recvString() {
        try {
            // Receive a byte buffer
            final byte[] buffer = recvBytes();
            // Check the buffer content
            if (buffer != null) {
                // Construct a message
                final String message
                        = new String(buffer, 0, buffer.length, "UTF-8");
                // And return message
                return message;
            }
        } catch (final UnsupportedEncodingException exc) {
            // Print some information
            mLogger.failure(exc.toString());
        }
        // Return null at failure 
        return null;
    }
}
