package de.dfki.vsm.xtension.baxter;

import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStart;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStop;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

/**
 * @author Alvaro Cepero
 */
public class BaxterHandler extends Thread implements EventListener {

    // The logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    private  BaxterExecutor mExecutor;
    private  Socket mSocket;
    private BufferedReader mInStream;
    private BufferedWriter mOutStream;
    // The termination flag
    private boolean mDone = false;
    private final EventDispatcher mEventDispatcher = EventDispatcher.getInstance();

    // Create the client thread
    public BaxterHandler(final Socket socket, final BaxterExecutor executor) {
        // Initialize the socket
        mSocket = socket;
        // Initialize the executor
        mExecutor = executor;
        mEventDispatcher.register(this);
    }

    public BufferedReader getmInStream(){
        return mInStream;
    }

    // Start the client thread
    @Override
    public void start() {
        try {
            // Get the socket streams
            mInStream = new BufferedReader(
                    new InputStreamReader(
                            mSocket.getInputStream(), "UTF-8"));
            mOutStream = new BufferedWriter(
                    new OutputStreamWriter(
                            mSocket.getOutputStream(), "UTF-8"));
        } catch (final IOException exc) {
            mLogger.failure(exc.toString());
        }
        // Start the thread
        super.start();
    }

    // Abort the client thread
    public final void abort() {
        mEventDispatcher.remove(this);
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
    public  String recv() {
        try {
            // Receive The Next Line
            String message = mInStream.readLine();
            // Debug Some Information
            mLogger.message("Receiving '" +     message + "'");
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
            // Send the next message
            mOutStream.write(string);
            mOutStream.newLine();
            mOutStream.flush();
            // Print some information
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

    @Override
    public void update(EventObject event) {
        if(event instanceof LineStart){
            //mExecutor.handle(message, this);
            String executionId = ((LineStart) event).getExecutionId();
            if(executionId.startsWith(BaxterExecutor.sExecutionId)){
                mExecutor.scheduleSpeech(executionId);
            }
        }
        if(event instanceof LineStop){
            String message = "#AUDIO#end#" + ((LineStop) event).getExecutionId();
            String executionId = ((LineStop) event).getExecutionId();
            if(executionId.startsWith(BaxterExecutor.sExecutionId)){
                mExecutor.handle(message, this);
            }
        }
    }
}
