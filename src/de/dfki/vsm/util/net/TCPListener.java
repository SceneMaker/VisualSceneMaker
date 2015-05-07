package de.dfki.vsm.util.net;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.net.ServerSocket;
import java.net.Socket;

import java.util.HashSet;

/**
 * @author Gregor Mehlmann
 */
public abstract class TCPListener extends NETProcess {

    // The Boolean Termination Flag
    private boolean mDone = false;

    // The Local Listener Port
    private final Integer mPort;

    // The TCP Listener Socket
    private ServerSocket mSocket;

    // The List Of TCP Handlers
    protected final HashSet<TCPProcess> mTeam;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public TCPListener(final Integer port) {

        // Initialize The Listener Port
        mPort = port;

        // Initialize The Handler Team
        mTeam = new HashSet<>();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {

        // Debug Some Information
        mLogger.message("Starting TCP Listener '" + this + "'");

        // Bind TCP Server Socket
        try {

            // Initialize Server Socket
            mSocket = new ServerSocket(mPort);
        } catch (IOException exc) {

            // Debug Some Information
            mLogger.warning("Catching TCP Listener '" + this + "' With '" + exc.toString() + "'");

            // Return At Some Failure
            return;
        }

        // Execute The Listener
        execute();

        // Debug Some Information
        mLogger.message("Stopping TCP Listener '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected void execute() {

        // Continue Accepting Connections
        while (!mDone) {
            try {

                // Accept An Incoming Connection
                final Socket socket = mSocket.accept();

                // Create A New Handler For This
                final TCPProcess process = accept(socket);

                // Add The New Socket Handler To List
                mTeam.add(process);

                // Handle The New Socket Connection
                process.start();
            } catch (IOException exc) {

                // Debug Some Information
                mLogger.warning("Catching TCP Listener '" + this + "' With '" + exc.toString() + "'");
            }
        }

        // Abort All Team Members
        for (TCPProcess process : mTeam) {
            process.abort();
        }

        // Join With All Team Members
        for (TCPProcess process : mTeam) {
            try {
                process.join();
            } catch (Exception exc) {

                // Debug Some Information
                mLogger.failure("Catching TCP Listener '" + this + "' With '" + exc.toString() + "'");
            }
        }

        mTeam.clear();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void abort() {

        // Request The Termination
        mDone = true;

        // Check The Socket Object
        if ((mSocket != null) && mSocket.isBound()) {

            // Close The Server Socket
            try {
                mSocket.close();
            } catch (IOException exc) {
                mLogger.failure(exc.toString());
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected abstract TCPProcess accept(final Socket socket);
}
