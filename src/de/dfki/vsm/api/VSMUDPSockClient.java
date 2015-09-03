package de.dfki.vsm.api;

//~--- JDK imports ------------------------------------------------------------
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;

/**
 * @author Not me
 */
public final class VSMUDPSockClient extends VSMAgentClient {

    // The Datagram Socket
    private DatagramSocket mSocket;

    // The Local Address
    private SocketAddress mLocalAddr;

    // The Loacl Host
    private final String mLocalHost;

    // The Loacl Port
    private final int mLocalPort;

    // The Remote Address
    private SocketAddress mRemoteAddr;

    // he Remote Flag
    private final boolean mRemoteFlag;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VSMUDPSockClient(
            final VSMScenePlayer player,
            final String name, final String uaid,
            final String lhost, final int lport,
            final String rhost, final int rport,
            final boolean rflag) {

        // Initialize The Client
        super(player, name, uaid, rhost, rport);

        // Initialize UDP Client
        mLocalHost = lhost;
        mLocalPort = lport;
        mRemoteFlag = rflag;

        // Debug Some Information
        mVSM3Log.message("Creating UDP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
                + mRemoteHost + ":" + mRemotePort + "' From '" + mLocalHost + ":" + mLocalPort + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void start() {
        try {

            // Create The Addresses
            mLocalAddr = new InetSocketAddress(mLocalHost, mLocalPort);

            // Create The UDP Socket
            mSocket = new DatagramSocket(mLocalAddr);            
           
            // Connect The UDP Socket
            if (mRemoteFlag) {

                // Create The Addresses
                mRemoteAddr = new InetSocketAddress(mRemoteHost, mRemotePort);

                // Connect The UDP Socket
                mSocket.connect(mRemoteAddr);

                // Debug Some Information
                mVSM3Log.message("Connecting UDP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid
                        + "' On '" + mRemoteHost + ":" + mRemotePort + "' From '" + mLocalHost + ":"
                        + mLocalPort + "'");
            }

            // Debug Some Information
            mVSM3Log.message("Starting UDP Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
                    + mRemoteHost + ":" + mRemotePort + "' From '" + mLocalHost + ":" + mLocalPort + "'");
        } catch (final SocketException exc) {

            // Debug Some Information
            mVSM3Log.failure(exc.toString());
        }

        // Start The Client Thread
        super.start();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void abort() {

        // Set Termination Flag
        mDone = true;

        // Close The Socket Now
        if ((mSocket != null) && !mSocket.isClosed()) {
            mSocket.close();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {

        // Debug Some Information
        mVSM3Log.message("Starting VSM Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
                + mRemoteHost + ":" + mRemotePort + "'");

        // Execute While Not Done
        try {
            while (!mDone) {

                // Constantly Handle Data
                mPlayer.handle(this);
            }
        } catch (final Exception exc) {

            // Debug Some Information
            mVSM3Log.failure(exc.toString());
        }

        // Debug Some Information
        mVSM3Log.message("Stopping VSM Agent Client For '" + mAgentName + "' With Id '" + mAgentUaid + "' On '"
                + mRemoteHost + ":" + mRemotePort + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final boolean sendBytes(final byte[] bytes) {

        // Return At Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final boolean sendString(final String string) {
        try {
            //
            mVSM3Log.message("Sending '" + string + "'");
            
            //
            final byte[] buffer = string.getBytes("UTF-8");
            //
            final DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
            //
            mSocket.send(packet);
            //
            return true;

        } catch (final Exception exc) {
            mVSM3Log.failure(exc.toString());
        }
        // Return At Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final byte[] recvBytes(final int size) {
        if ((mSocket != null) && !mSocket.isClosed()) {
            try {

                // Create The Datagram Packet
                final byte[] buffer = new byte[size];
                
                final DatagramPacket mPacket = new DatagramPacket(buffer, buffer.length);

                // Receive The Data Packet
                mSocket.receive(mPacket);

                // Debug Some Information
                mVSM3Log.message("VSM Agent Client Receiving Message:");

                // Return The Message
                return buffer;
            } catch (Exception exc) {

                // Debug Some Information
                mVSM3Log.warning(exc.toString());

                // Debug Some Information
                mVSM3Log.warning("VSM Agent Client Has Bad State");
            }
        }

        // Otherwise Return Null
        return null;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String recvString() {

        // Receive The Byte Packet
        final byte[] buffer = recvBytes(1024);

        // Try To Read Byte PAcket
        if (buffer != null) {
            try {
                final String message = new String(buffer, 0, buffer.length, "UTF-8");

                //
                return message;
            } catch (final Exception exc) {

                // Debug Some Information
                mVSM3Log.warning(exc.toString());

                // Debug Some Information
                mVSM3Log.warning("VSM Agent Client Has Bad State");
            }
        }

        // Otherwise Return Null
        return null;
    }
}
