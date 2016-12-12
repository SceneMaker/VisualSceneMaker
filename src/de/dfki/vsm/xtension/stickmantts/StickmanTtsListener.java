package de.dfki.vsm.xtension.stickmantts;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * @author Alvaro Cepero
 */
public class StickmanTtsListener extends Thread {

    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The executor instance
    private final StickmanTtsExecutor mExecutor;
    // The local socket port 
    private final int mPort;
    // The TCP socket connection 
    private ServerSocket mSocket;
    // The thread termination flag
    private boolean mDone = false;


    public StickmanTtsListener(final int port, final StickmanTtsExecutor executor) {
        // Initialize the port
        mPort = port;
        // Initialize the executor
        mExecutor = executor;

    }

    // Parse specific elements
    @Override
    public final void start() {
        try {
            // Create the server socket
            mSocket = new ServerSocket(mPort);
            // Start the server thread
            super.start();
        } catch (final IOException exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Abort the server thread
    public final void abort() {
        // Set the termination flag
        mDone = true;
        // Eventually close the socket
        if (mSocket != null && !mSocket.isClosed()) {
            try {
                mSocket.close();
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
        }
        // Interrupt if sleeping
        interrupt();
    }

    // Execute the server thread
    @Override
    public final void run() {
        // Accept and manage clients
        while (!mDone) {
            try {
                // Accept a client socket
                final Socket socket = mSocket.accept();
                // Accept the new socket
                mExecutor.accept(socket);
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
        }
    }
}
