package de.dfki.vsm.xtension.reeti;

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
public final class ReetiHandler extends Thread {

    // The logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The termiation flag
    protected volatile boolean mDone = false;
    // The reeti executor 
    protected final ReetiExecutor mExecutor;
    // The datagram socket
    private DatagramSocket mSocket;
    // The local address
    private SocketAddress mLocalAddr;
    private final String mLocalHost;
    private final int mLocalPort;
    // The remote address
    private SocketAddress mRemoteAddr;
    private final String mRemoteHost;
    private final int mRemotePort;

    // Construct the UDP client
    public ReetiHandler(
            final ReetiExecutor executor,
            final String lhost, final int lport,
            final String rhost, final int rport) {
        // Initialize the executor
        mExecutor = executor;
        // Initialize the connection
        mLocalHost = lhost;
        mLocalPort = lport;
        mRemoteHost = rhost;
        mRemotePort = rport;
        // Print some information
        mLogger.message("Creating Reeti Connection" + " "
                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
    }

    // Start the handler thread
    @Override
    public final void start() {
        try {
            // Create local address
            mLocalAddr = new InetSocketAddress(mLocalHost, mLocalPort);
            // Create the UDP socket
            mSocket = new DatagramSocket(mLocalAddr);
            // Create remote address
            mRemoteAddr = new InetSocketAddress(mRemoteHost, mRemotePort);
            // Connect the UDP socket
            mSocket.connect(mRemoteAddr);
            // Print some information
            mLogger.message("Connecting Reeti Handler" + " "
                    + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
                    + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");

        } catch (final SocketException exc) {
            // Print some information
            mLogger.failure(exc.toString());
        }
        // Start the handler thread 
        super.start();
    }

    // Abort the client connection
    public final void abort() {
        // Set termination flag
        mDone = true;
        // Close the socket now
        if ((mSocket != null) && !mSocket.isClosed()) {
            mSocket.close();
        }
    }

    // Execute the client connection
    @Override
    public final void run() {
        // Print some information
        mLogger.message("Starting Reeti Handler" + " "
                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
        // Execute while not done
        try {
            while (!mDone) {
                // Receive a new message
                final String message = recvString();
                if (message != null) {
                    // Handle the message
                    mExecutor.handle(message);
                }
            }
        } catch (final Exception exc) {
            // Print some information
            mLogger.failure(exc.toString());
        }
        // Print some information
        mLogger.message("Stopping Reeti Handler" + " "
                + " " + "To '" + mRemoteHost + ":" + mRemotePort + "'"
                + " " + "On '" + mLocalHost + ":" + mLocalPort + "'");
    }

    // Send some string via the socket
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
            mLogger.message("Reeti Handler Sending '" + string + "'");
            // Return true at success
            return true;
        } catch (final IOException exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return false at failure 
            return false;
        }

    }

    // Receive some bytes via the socket
    public final byte[] recvBytes(final int size) {
        try {
            // Construct a byte array
            final byte[] buffer = new byte[size];
            // Construct an UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // Receive the UDP packet
            mSocket.receive(packet);
            // Return the buffer now
            return Arrays.copyOf(buffer, packet.getLength());
        } catch (final IOException exc) {
            // Print some information
            mLogger.warning(exc.toString());
            // Return null at failure 
            return null;
        }
    }

    // Receive some string via the socket
    public final String recvString() {
        try {
            // Receive a byte buffer
            final byte[] buffer = recvBytes(4096);
            // Check the buffer content
            if (buffer != null) {
                // Construct a message
                final String message
                        = new String(buffer, 0, buffer.length, "UTF-8");
                // Print some information
                mLogger.message("Reeti Handler Receiving '" + message + "'");
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
