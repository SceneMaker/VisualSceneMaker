package de.dfki.vsm.util.net;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public abstract class TCPProcess extends NETProcess {

    // The Connection Socket
    protected Socket mSocket;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public TCPProcess(final Socket socket) {

        // Create The TCP Handler Instance
        mSocket = socket;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public TCPProcess(final String host, final Integer port) {

        // Create The TCP Handler Instance
        try {

            // Initialize The Socket
            mSocket = new Socket(host, port);
        } catch (IOException exc) {

            // Log Some Debug Information
            mLogger.failure(exc.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {

        // Debug Some Information
        mLogger.message("Starting TCP Process '" + this + "'");

        try {

            // Check The Socket Object
            if (mSocket != null) {

                // Check The Socket State
                if (mSocket.isConnected()) {

                    // Execute The TCP Connection Handler
                    execute();
                }
            }
        } catch (Exception exc) {

            // Debug Some Information
            mLogger.warning("Catching TCP Process '" + this + "' With '" + exc.toString() + "'");
        }

        // Debug Some Information
        mLogger.message("Stopping TCP Process '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void abort() {

        // Check The Socket Object
        if ((mSocket != null) &&!mSocket.isClosed()) {

            // Check The Socket State
            try {

                // Close The Server Socket
                mSocket.close();
            } catch (IOException exc) {

                // Log Some Debug Information
                mLogger.failure(exc.toString());
            }
        }
    }
}
