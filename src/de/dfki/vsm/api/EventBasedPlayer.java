package de.dfki.vsm.api;

import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author Gregor Mehlmann
 */
public abstract class EventBasedPlayer extends RunTimePlayer {

    // The queue of waiting tasks
    private final HashMap<String, Task> mTasks
            = new HashMap<String, Task>();
    // The queue of available clients
    private final HashMap<String, Client> mClients
            = new HashMap<String, Client>();

    @Override
    public boolean launch(final RunTimeProject project) {
        // Mockup the data initialization
        // Create a single client
        final Client client = new Client("127.0.0.1", 1000, "127.0.0.1", 2000);
        // Create two actors now
        mClients.put("Reeti", client);
        mClients.put("Naoli", client);
        // Return true at success
        return true;
    }

    @Override
    public boolean unload() {
        // Aquire the task queue lock
        synchronized (mTasks) {
            // Clear the task queue
            mTasks.clear();
            // Notify all the tasks
            mTasks.notifyAll();
        }
        // Return true at success
        return true;
    }

    // Request an utterance from engine
    protected void request(final Task task, final String event) {
        // Aquire the task queue lock
        synchronized (mTasks) {

        }
    }

    // Handle an event from the engine
    protected void handle(final Client client, final String event) {
        // Parse a mark from event
        final BookMarkEvent mark = BookMarkEvent.parse(event);
        // Check if mark is valid
        if (mark != null) {
            // Get the fields of the mark
            final String _type = mark.getType();
            final String _task = mark.getTask();
            final String _text = mark.getText();
            // Check the type of the mark
            if (_type.equals("STOPPED")) {
                // Aquire the task queue lock
                synchronized (mTasks) {
                    // Remove the task from queue
                    if (mTasks.remove(_task) != null) {
                        // Notify all waiting tasks
                        mTasks.notifyAll();
                    }
                }
            } else if (_type.equals("REACHED")) {
                // Aquire the task queue lock
                synchronized (mTasks) {
                    // Get the task from the queue
                    final Task task = mTasks.get(_task);
                    // Remove the task from queue
                    final String act = task.remove(_text);

                }
            } else {
                // Unknown mark type
            }
        }
    }

    // Request utterance from the engine
    //protected void request(final String utterance) {
    // Create the main
    //}
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private static class BookMarkEvent {

        private static final Pattern sPattern = Pattern.compile(
                "<BookMarkEvent type=\"(.*?)\" task=\"(.*?)\">(.*?)</Mark>");

        // Create a mark from an event
        public static BookMarkEvent parse(final String event) {
            final Matcher matcher = sPattern.matcher(event);
            if (matcher.matches()) {
                final String type = matcher.group(1);
                final String task = matcher.group(2);
                final String text = matcher.group(3);
                //
                return new BookMarkEvent(type, task, text);
            }
            // Return null at failure
            return null;
        }

        // Create a mark from the fields
        public static BookMarkEvent create(
                final String type,
                final String task,
                final String text) {
            return new BookMarkEvent(type, task, text);
        }

        private final String mTask;
        private final String mType;
        private final String mText;

        private BookMarkEvent(
                final String type,
                final String task,
                final String text) {
            mType = type;
            mTask = task;
            mText = text;
        }

        public final String getType() {
            return mType;
        }

        public final String getTask() {
            return mTask;
        }

        public final String getText() {
            return mText;
        }

        @Override
        public final String toString() {
            return "<BookMarkEvent"
                    + " " + "type=\"" + mType + "\""
                    + " " + "task=\"" + mTask + "\">"
                    + mText + "</Mark>";
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected class Task extends RunTimePlayer.Task {

        // The map of actions 
        private HashMap<String, String> mActionMap = new HashMap();

        protected Task(final String name) {
            super(name);
        }

        public final boolean isEmpty() {
            return mActionMap.isEmpty();
        }

        public final String append(final String id, final String text) {
            return mActionMap.put(id, text);
        }

        public final String remove(final String id) {
            return mActionMap.remove(id);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected class Client extends Thread {

        // The singelton logger instance
        private final LOGDefaultLogger mLogger
                = LOGDefaultLogger.getInstance();
        // The thread termination flag
        private boolean mDone = false;
        // The datagram connection 
        private DatagramSocket mSocket;
        // The both socket objects 
        private final SocketAddress mLAddr;
        private final SocketAddress mRAddr;

        // Create the handler thread
        public Client(
                final String lHost,
                final Integer lPort,
                final String rHost,
                final Integer rPort) {
            // Initialize the address data
            mLAddr = new InetSocketAddress(lHost, lPort);
            mRAddr = new InetSocketAddress(rHost, rPort);
        }

        // Start the handler thread
        @Override
        public final void start() {
            try {
                // Create the server socket
                mSocket = new DatagramSocket(mLAddr);
                // Connect the server socket
                // mSocket.connect(mRAddr);
                // Print some information
                mLogger.message("Starting client " + mLAddr + " " + mRAddr);
                // Start the server thread
                super.start();
            } catch (final SocketException exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Abort the client thread
        public final void abort() {
            // Set the termination flag
            mDone = true;
            // Eventually close the socket
            if (mSocket != null && !mSocket.isClosed()) {
                mSocket.close();
            }
            // Interrupt it if sleeping
            interrupt();
        }

        // Execute the client thread
        @Override
        public final void run() {
            // Receive while not done ...
            while (!mDone) {
                // Receive a new message
                final String message = recv();
                // Check message content
                if (message != null) {
                    // Handle the message
                    handle(this, message);
                }
            }
        }

        // Send a message via the client
        public final boolean send(final String string) {
            try {
                // Create the byte buffer
                final byte[] buffer = string.getBytes("UTF-8");
                // Create the UDP packet
                final DatagramPacket packet
                        = new DatagramPacket(buffer, buffer.length);
                // And send the UDP packet
                mSocket.send(packet);
                // Return true at success
                return true;
            } catch (final IOException exc) {
                // Print some information
                mLogger.failure(exc.toString());
                // Return false at failure 
                return false;
            }
        }

        // Receive a string from socket
        private final String recv() {
            try {
                // Construct a byte array
                final byte[] buffer = new byte[16384];
                // Construct an UDP packet
                final DatagramPacket packet
                        = new DatagramPacket(buffer, buffer.length);
                // Receive the UDP packet
                mSocket.receive(packet);
                // Return string at success 
                return new String(buffer, 0, packet.getLength(), "UTF-8");
            } catch (final Exception exc) {
                // Print some information
                mLogger.failure(exc.toString());
                // Return null at failure 
                return null;
            }
        }
    }
}
