package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public class CharamelListener extends Thread {

    // The logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The executor instance
    private final CharamelExecutor mExecutor;
    // The local socket port 
    private final int mPort;
    // The TCP socket connection 
    private Socket mSocket;
    // The thread termination flag
    private boolean mDone = false;

    public CharamelListener(final int port, final CharamelExecutor executor) {
        // Initialize the port
        mPort = port;
        // Initialize the executor
        mExecutor = executor;
    }

    /**
     * Parse specific elements
     */
    @Override
    public final void start() {
        try {
            // Create the server socket
            mSocket = new Socket("localhost", mPort);
        } catch (final IOException exc) {
            mLogger.failure(exc.toString());
        }
    }

    /**
     * Abort the server thread
     */
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

    /**
     * Execute the server thread
     */
    @Override
    public final void run() {

        mLogger.message("Ready for accepting socket connections ...");
        mExecutor.connectToCharamel(mSocket);
    }
}