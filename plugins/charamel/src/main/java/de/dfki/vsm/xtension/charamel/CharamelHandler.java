package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

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
    private Socket mSocket;
    // The socket streams
    private InputStream mInStream;
    private DataOutputStream mOutStream;
    private DataInputStream dInStream;
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
            mInStream = mSocket.getInputStream();
            dInStream = new DataInputStream(mInStream);
            //new BufferedReader(new InputStreamReader(mSocket.getInputStream(), "UTF-8"));
        } catch (final IOException exc) {
            mLogger.failure(exc.toString());
        }

        super.start();
    }

    // Abort the client thread
    public final void abort() {

        // Set the termination flag
        mDone = true;
        // Eventually close the socket
        if (mSocket != null) {
            if (!mSocket.isClosed()) {
                try {
                    mSocket.close();
                } catch (final IOException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            mSocket = null;
        }

        // Interrupt if sleeping
        interrupt();
    }

    // Receive some message
    public final String recv() {
        try {
            int msgTag, statusTag, msgLen;
            mLogger.warning("Before Msg Tag");
            msgTag = nextLeInt();
            mLogger.warning("Before Status Tag");
            statusTag = nextLeInt();
            mLogger.warning("Before Len Tag");
            msgLen = nextLeInt();
            mLogger.message("new Msg (tag: " + msgTag + " status: " + statusTag + " Length: " + msgLen + ")");
            byte[] msg = new byte[msgLen];
            dInStream.readFully(msg);
            return new String(msg, "UTF-8");
        } catch (IOException ex) {
            mLogger.warning(ex.toString());
            return null;
        }
    }

    Integer nextLeInt() throws IOException {
        byte[] b = new byte[4];
        dInStream.readFully(b);
        StringBuilder ib = new StringBuilder();

        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        return bb.getInt();
    }


    // Send some message
    public final boolean send(final String string) {
        try {
            // Construct The Message
            final byte[] bytes = string.getBytes(StandardCharsets.UTF_8);
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
