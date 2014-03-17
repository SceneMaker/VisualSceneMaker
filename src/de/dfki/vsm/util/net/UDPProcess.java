package de.dfki.vsm.util.net;

import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

/**
 * @author Gregor Mehlmann
 */
public abstract class UDPProcess extends NETProcess {

    // The UDP Datagram Socket
    protected DatagramSocket mSocket;
    // The Local Port Of The Handler
    private final Integer mLPort;
    // The Local Host Of The Handler
    private final String mLHost;
    // The Remote Port Of The Handler
    private final Integer mRPort;
    // The Remote Host Of The Handler
    private final String mRHost;
    // The Local  Handler Address
    private final SocketAddress mLAddr;
    // The Remote  Handler Address 
    private final SocketAddress mRAddr;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected UDPProcess(
            final Integer localPort,
            final String localHost) {
        // Initialize The Local Connection Fields
        mLPort = localPort;
        mLHost = localHost;
        // Initialize The Connection Fields Addresses
        mLAddr = new InetSocketAddress(mLHost, mLPort);
        // Initialize The Remote Connection Fields
        mRPort = null;
        mRHost = null;
        mRAddr = null;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected UDPProcess(
            final Integer localPort,
            final String localHost,
            final Integer remotePort,
            final String remoteHost) {
        // Initialize The Local Connection Fields
        mLPort = localPort;
        mLHost = localHost;
        // Initialize The Remote Connection Fields
        mRPort = remotePort;
        mRHost = remoteHost;
        // Initialize The Connection Fields Addresses
        mLAddr = new InetSocketAddress(mLHost, mLPort);
        mRAddr = new InetSocketAddress(mRHost, mRPort);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {
        // Debug Some Information
        mLogger.message("Starting UDP Process '" + this + "'");
        // Try To Bind The UDP Server Socket
        try {
            // Bind The Local Datagram Socket
            mSocket = new DatagramSocket(mLAddr);
            // Debug Some Information
            mLogger.message("Binding UDP Process '" + this
                    + "' To Local Address'" + mLAddr + "'");
            // Connect To The Remote Address
            if (mRAddr != null) {
                mSocket.connect(mRAddr);
                // Debug Some Information
                mLogger.message("Connecting UDP Process '" + this
                        + "' To Remote Address'" + mRAddr + "'");
            }
            // Execute The Handler
            execute();
        } catch (Exception exc) {
            // Debug Some Information
            mLogger.warning("Catching UDP Process '" + this
                    + "' With '" + exc.toString() + "'");
        }
        // Debug Some Information
        mLogger.message("Stopping UDP Process '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void abort() {
        // Debug Some Information
        mLogger.warning("Canceling UDP Process '" + this + "'");
        // Check The Socket Object
        if (mSocket != null && !mSocket.isClosed()) {
            // Close The Socket
            mSocket.close();
        }
    }
}
