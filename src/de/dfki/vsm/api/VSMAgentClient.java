package de.dfki.vsm.api;

import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public class VSMAgentClient extends Thread {

    // The Client Socket
    protected Socket mSocket;
    // The Data Streams
    protected DataInputStream mInput;
    protected DataOutputStream mOutput;
    // The Reader&Writer
    protected BufferedReader mReader;
    protected BufferedWriter mWriter;
    // The Scene Player
    protected final VSMScenePlayer mPlayer;
    // The Agents Features
    protected final String mName;
    protected final String mHost;
    protected final String mUaid;
    protected final int mPort;
    // Termination Flag
    protected boolean mDone = false;
    // The System logger
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VSMAgentClient(
            final VSMScenePlayer player,
            final String name,
            final String uaid,
            final String host,
            final int port) {
        // Initialize The Player
        mPlayer = player;
        // Initialize The Fields
        mName = name;
        mUaid = uaid;
        mHost = host;
        mPort = port;
        // Debug Some Information
        mLogger.message("Creating Agent Client");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getUaid() {
        return mUaid;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getHost() {
        return mHost;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int getPort() {
        return mPort;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void start() {
        try {
            // Create The New Socket
            mSocket = new Socket(mHost, mPort);
            // Init In&Output Stream
            mInput = new DataInputStream(mSocket.getInputStream());
            mOutput = new DataOutputStream(mSocket.getOutputStream());
            // Establish IO Channels
            mReader = new BufferedReader(
                    new InputStreamReader(mSocket.getInputStream(), "UTF-8"));
            mWriter = new BufferedWriter(
                    new OutputStreamWriter(mSocket.getOutputStream(), "UTF-8"));
            // Debug Some Information
            mLogger.message("Starting Agent Client");
        } catch (Exception exc) {
            // Debug Some Information
            mLogger.warning(exc.toString());
        }
        // Start The Client Thread 
        super.start();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void abort() {
        // Set Termination Flag
        mDone = true;
        // Interrupt Thread State
        interrupt();
        // Close The Socket Now
        if (mSocket != null) {
            try {
                if (!mSocket.isClosed()) {
                    // Close The Socket
                    mSocket.close();
                    // Debug Some Information
                    mLogger.message("Aborting Agent Client Now");
                } else {
                    // Debug Some Information
                    mLogger.warning("Cannot Abort Agent Client");
                }
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        } else {
            // Debug Some Information
            mLogger.warning("Cannot Abort Agent Client");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final boolean send(final byte[] bytes) {
        // Try To Send The Message
        if (mOutput != null) {
            try {
                // Write Out The Message
                mOutput.write(bytes);
                // Flush Out The Stream                
                mOutput.flush();
                // Debug Some Information
                mLogger.message("Sending Message '" + BINUtilities.BytesToHexString(bytes) + "'");
                // Return At Success
                return true;
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        } else {
            // Debug Some Information
            mLogger.warning("Cannot Send Over Agent Client");
        }
        // Return At Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void send(final String string) {
        // Try To Send The Message
        if (mWriter != null) {
            try {
                // Write Out The Message
                mWriter.write(string);
                mWriter.newLine();
                mWriter.flush();
                // Debug Some Information
                mLogger.message("Sending Message '" + string + "'");
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        } else {
            // Debug Some Information
            mLogger.warning("Cannot Send Over Agent Client");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final byte[] recv(final int size) {
        // Try To Read The Line
        if (mInput != null) {
            try {
                // Read In The Answer
                byte[] bytes = new byte[size];
                mInput.readFully(bytes);
                // Debug Some Information
                mLogger.message("Reading Message'" + BINUtilities.BytesToHexString(bytes) + "'");
                // Return The Notification
                return bytes;
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        } else {
            // Debug Some Information
            mLogger.warning("Cannot Read From Agent Client");
        }
        // Otherwise Return Null
        return null;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String recv() {
        // Try To Read The Line
        if (mReader != null) {
            try {
                // Read In The Answer
                final String line = mReader.readLine();
                // Debug Some Information
                mLogger.message("Reading Message'" + line + "'");
                // Return The Notification
                return line;
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        } else {
            // Debug Some Information
            mLogger.warning("Cannot Read From Agent Client");
        }
        // Otherwise Return Null
        return null;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void run() {
        // Debug Some Information
        mLogger.message("Executing Agent Client");
        // Constantly Read Data
        while (!mDone) {
            mPlayer.handle(this);
        }
        // Debug Some Information
        mLogger.message("Stopping Agent Client");
    }

}
