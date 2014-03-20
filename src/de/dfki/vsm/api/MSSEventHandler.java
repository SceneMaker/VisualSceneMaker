package de.dfki.vsm.api;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

/**
 * @author Gregor Mehlmann
 */
public class MSSEventHandler extends Thread {

    // The Datagram Socket To Receive All The 
    // Touch Events And The According Buffer
    private DatagramSocket mSocket;
    // The Local Socket Handler Address
    private SocketAddress mLAddr;
    // The Remote Socket Handler Address
    private SocketAddress mRAddr;
    // The Datagram Packet For Messages
    private final byte[] mBuffer = new byte[1024];
    private final DatagramPacket mPacket
            = new DatagramPacket(mBuffer, mBuffer.length);
    // The System Logger To Log Some Debug
    // Information To Console And Logfile
    private LOGDefaultLogger mLogger;
    // The NAO Robot Player
    private VSMScenePlayer mPlayer;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public MSSEventHandler(final VSMScenePlayer player) {
        mPlayer = player;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public void init(
            final String lhost,
            final int lport,
            final String rhost,
            final int rport,
            final boolean rconn) {

        try {
            // Initialize The Logger
            mLogger = LOGDefaultLogger.getInstance();
            // Create The New Socket
            // Create The Addresses
            mLAddr = new InetSocketAddress(lhost, lport);
            // Create The UDP Socket
            mSocket = new DatagramSocket(mLAddr);
            // Connect The UDP Socket
            if (rconn) {
                // Create The Addresses
                mRAddr = new InetSocketAddress(rhost, rport);
                // Connect The UDP Socket
                mSocket.connect(mRAddr);
                // Debug Some Information
                mLogger.message("Connecting Touch Handler");
            }
            // Print Debug Information
            mLogger.message("Creating Touch Handler");
        } catch (Exception exc) {
            // Debug Some Information
            mLogger.warning(exc.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public void abort() {
        // Close The Datagram Socket
        if (mSocket != null) {
            try {
                // Close The Datagram Socket
                if (!mSocket.isClosed()) {
                    mSocket.close();
                    // Debug Some Information
                    mLogger.message("Aborting Touch Handler");
                }
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void run() {
        // Debug Some Information
        mLogger.message("Starting Touch Handler");
        while (mSocket != null
                && !mSocket.isClosed()) {
            try {
                // Receive The Data Packet
                mSocket.receive(mPacket);
                // Get The String Data
                final String received = new String(
                        mPacket.getData(), 0,
                        mPacket.getLength(), "UTF-8");
                // Get The System time
                final long time = mPlayer.getCurrentTime();
                // Create The New Fact
                final String fact = "[" + "\r\n"
                        + "  " + "type:" + "event" + "," + "\r\n"
                        + "  " + "name:" + "undef" + "," + "\r\n"
                        + "  " + "sent:" + "table" + "," + "\r\n"
                        + "  " + "recv:" + "vsmv3" + "," + "\r\n"                        
                        + "  " + "mode:" + "touch" + "," + "\r\n"
                        + "  " + "dist:" + "0" + "," + "\r\n"
                        + "  " + "life:" + "0" + "," + "\r\n"
                        + "  " + "time:" + time + "," + "\r\n"
                        + "  " + "conf:" + "1.0" + "," + "\r\n"
                        + "  " + "data:\r\n" + received + "\r\n"
                        + "]";
                // Print Some Information
                mLogger.message(fact);
                // Assert The New Fact
                mPlayer.query("jdd(" + fact + ").");

            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        }
        // Debug Some Information
        mLogger.message("Stopping Touch Handler");
    }
}
