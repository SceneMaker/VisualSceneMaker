package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public class CharamelHandler extends Thread {

    // The logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The executor instance
    private final CharamelExecutor mExecutor;
    // The client socket
    private final Socket mSocket;
    // The socket streams
    private DataInputStream mInStream;
    private DataOutputStream mOutStream;
    // The termination flag
    private boolean mDone = false;

    // Create the client thread
    public CharamelHandler(final Socket socket, final CharamelExecutor executor) {
        // Initialize the socket
        mSocket = socket;
        // Initialize the executor
        mExecutor = executor;
    }

    // Start the client thread
    @Override
    public void start() {
        
        try {
            mOutStream = new DataOutputStream(mSocket.getOutputStream());
            mOutStream.flush();
            mInStream = new DataInputStream(mSocket.getInputStream());
        } catch (final IOException exc) {
            mLogger.failure(exc.toString());
        }

//        try {
//            // Get the socket streams
//            mInStream = new BufferedReader(
//                    new InputStreamReader(
//                            mSocket.getInputStream(), "UTF-8"));
//            mOutStream = new BufferedWriter(
//                    new OutputStreamWriter(
//                            mSocket.getOutputStream(), "UTF-8"));
//        } catch (final IOException exc) {
//            mLogger.failure(exc.toString());
//        }
        // Start the thread
        super.start();
    }

    // Abort the client thread
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

    // Receive some message
    public final String recv() {
        try {
            // wait and get response
                byte[] respArr;
                synchronized (mSocket.getInputStream()) {
                    byte[] header = new byte[12];
                    mInStream.readFully(header);
                    int msgSize = BINUtilities.BytesLEToInt(header);
                    // read the message
                    respArr = new byte[msgSize];
                    mInStream.readFully(respArr);
                }
                final String message = new String(respArr, "UTF-8").trim();
            
            // Receive The Next Line
            //final String message = mInStream.readLine();
            // Debug Some Information
            mLogger.success("Received '" + message + "'");
            // Return Received Data
            return message;
        } catch (final IOException exc) {
            // Debug Some Information
            mLogger.warning(exc.toString());
            // Otherwise Return Null
            return null;
        }
    }

    // Send some message 
    public final boolean send(final String string) {
        try {
            // Construct The Message
            final byte[] bytes = string.getBytes("UTF-8");
            // And Then Send Message 
            mOutStream.write(BINUtilities.IntToBytesLE(100));
            mOutStream.write(BINUtilities.IntToBytesLE(0));
            mOutStream.write(BINUtilities.IntToBytesLE(bytes.length));
            mOutStream.write(bytes);

            // Print some information
            mLogger.success("Sended '" + string + "'");
            // Return true at success
            return true;
        } catch (final IOException exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return false at failure 
            return false;
        }
    }

    // Execute the client thread
    @Override
    public final void run() {
        while (!mDone) {
            // Receive a new message
            final String message = recv();
            if (message != null) {
                // Handle the message
                mExecutor.handle(message, this);
            }
        }
    }
}
