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
    //private BufferedReader mInStream;
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
        if (mSocket != null){
            if( !mSocket.isClosed()) {
            try {
                mSocket.close();
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
            }
            mSocket= null;
        }
       

        // Interrupt if sleeping
        interrupt();
    }

    // Receive some message
    public final String recv() {
       // try {
            // wait and get response
//            byte[] respArr;
//            synchronized (mSocket.getInputStream()) {
//                byte[] header = new byte[12];
//                mInStream.readFully(header);
//
//                mLogger.message("Header bytes " + header);
//                final String headerStr = new String(header, "UTF-8").trim();
//                mLogger.message("Header  " + headerStr);
//
//                int msgSize = BINUtilities.BytesLEToInt(header) + 2;
//                mLogger.message("Message Size  " + msgSize);
//                // read the message
//                respArr = new byte[msgSize];
//      
//                mInStream.readFully(respArr);
//
//                for (int i = 0; i < msgSize; i++) {
//                    mLogger.message("[" + i + "]\t" + String.format("0x%02X", respArr[i]) + "\t" + (char)respArr[i]);
//                }
//
//            }
//            final String message = new String(respArr, "UTF-8").trim();
/*
            // Receive The Next Line
            final String rawMessage = mInStream.readLine();
            // Cut of header - special charamel treatment
            byte[] respArr = rawMessage.getBytes();
            byte[] cleanedMessageArr = Arrays.copyOfRange(respArr, 12, respArr.length);
            final String message = new String(cleanedMessageArr, "UTF-8").trim();
            
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
*/      try {
            int msgTag,statusTag,msgLen;
            msgTag = nextLeInt();
            statusTag = nextLeInt();
            msgLen = nextLeInt();
            mLogger.message("new Msg (tag: "+msgTag+" status: "+statusTag+" Length: "+msgLen+")");
            byte[] msg = new byte[msgLen];
            dInStream.readFully(msg);
            return new String(msg,"UTF-8");
            
        } catch (IOException ex) {
            mLogger.warning(ex.toString());
            return null;
        }
    }
    Integer nextLeInt() throws IOException{
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
