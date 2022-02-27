package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * @author Gregor Mehlmann
 */
final class SSIEventReceiver extends Thread {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The local socket adress 
    private final int mLPort;
    private final String mLHost;
    private final SocketAddress mLAddr;
    // The reference to the player
    private final SSIEventHandler mHandler;
    // The thread termination flag
    private boolean mDone = false;
    // The datagram connection 
    private DatagramSocket mSocket;
    // time measure
    //private long mTimeCounter = System.nanoTime();

    // Construct the proxy server
    public SSIEventReceiver(
            final SSIEventHandler handler,
            final String lHost, final int lPort) {
        // Initialize the handler
        mHandler = handler;
        // Initialize the local data
        mLHost = lHost;
        mLPort = lPort;
        // Initialize the address data
        mLAddr = new InetSocketAddress(mLHost, mLPort);
        // Print some information
        mLogger.message("Creating SSI event handler local address " + mLAddr);
    }

    // Execute the server thread
    @Override
    public final void start() {
        try {
            // Create the server socket
            mSocket = new DatagramSocket(mLAddr);
            // Start the server thread
            super.start();
            mLogger.message("Starting the server");
        } catch (final SocketException exc) {
            mLogger.failure(exc.toString());
        }

        // initialze time measurement
        //mTimeCounter = System.nanoTime();
    }

    // Abort the server thread
    public final void abort() {
        // Set the termination flag
        mDone = true;
        // Eventually close the socket
        if (mSocket != null && !mSocket.isClosed()) {
            mSocket.close();
            mLogger.message("Getting aborted");
        }
        // Interrupt if sleeping
        interrupt();
    }

    // Execute the server thread
    @Override
    public final void run() {
        // Receive while not done ...
        mLogger.message("In the main run!");
        //mLogger.message(mDone);
        while (!mDone) {
            // Receive a new message
            mLogger.message("in the while loop");
            final String message = recvString();
            // Useful for check which sender/event: mLogger.message("Received message: "  + message);
            mLogger.message("Printing the received msg"+message);

            //mLogger.failure(message);
            // Check message content
            if (message != null) {
                // Start time measure
                //mTimeCounter = System.nanoTime();
                final ByteArrayInputStream stream = new ByteArrayInputStream(
                        message.getBytes(StandardCharsets.UTF_8));
                // Create an sequence object
                final SSIEventArray sequence = new SSIEventArray();
                if (XMLUtilities.parseFromXMLStream(sequence, stream)) {
                    // Delegate sequence handling
                    mHandler.handle(sequence);
                } else {
                    mLogger.message("Cannot parse the SSI events ...");
                }
            }
        }
    }

    // Send a message via the server
    public final boolean sendString(final String string) {
        try {
            // Create the byte buffer
            final byte[] buffer = string.getBytes(StandardCharsets.UTF_8);
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // And send the UDP packet
            mSocket.send(packet);
            // Return true at success
            return true;
        } catch (final IOException exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return false at failure 
            return false;
        }
    }

    // Receive a sized byte array
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

    // Receive a string from socket
    private String recvString() {
        // Receive a byte buffer
        final byte[] buffer = recvBytes();
        mLogger.message("recv strung");
        // Check the buffer content
        if (buffer != null) {
            mLogger.message("recv strung2");
            // Construct a message
            final String message
                    = new String(buffer, 0, buffer.length, StandardCharsets.UTF_8);
            // And return message
            return message;
        }
        // Return null at failure
        return null;
    }
}
