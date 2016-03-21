package de.dfki.vsm.xtension.tricat;

import de.dfki.vsm.model.project.DeviceConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.feedback.StatusFeedback;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.ABORTED;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.RUNNING;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.STARTED;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.STOPPED;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

/**
 * @author Gregor Mehlmann
 */
public final class TricatExecutor extends Thread implements ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The executor's name
    private final String mName;
    // The runtime project
    private final RunTimeProject mProject;
    // The UDP datagram socket 
    private DatagramSocket mSocket;
    // The remote socket adress 
    private SocketAddress mAddress;
    // The thread termination flag
    private boolean mDone = false;

    // Construct the executor
    public TricatExecutor(final String name, final RunTimeProject project) {
        // Initialize the name
        mName = name;
        // Initialize the project
        mProject = project;
        // Initialize the executor
        start();
    }

    // Parse specific elements
    @Override
    public void start() {
        try {
            // Parse the configuration
            final DeviceConfig config = mProject.getDeviceConfig(mName);
            // Set the socket address
            mAddress = new InetSocketAddress(
                    config.getProperty("host"),
                    Integer.parseInt(config.getProperty("port")));
            // Create the server socket
            mSocket = new DatagramSocket();
            // Connect the server socket
            mSocket.connect(mAddress);
            // Print some information
            mLogger.message("Connecting tricat executor to " + mAddress.toString());
            // Start the client thread
            super.start();
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Abort the server thread
    public final void abort() {
        // Set the termination flag
        mDone = true;
        // Eventually close the socket
        if (mSocket != null && !mSocket.isClosed()) {
            mSocket.close();
        }
        // Interrupt if sleeping
        interrupt();
    }

    // Receive some message
    public final String recv() {
        if (mSocket != null) {
            // Create The Datagram Packet
            final byte[] buffer = new byte[4096];
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            try {
                // Receive The Datagram Packet
                mSocket.receive(packet);
                // Get The Datagram's String 
                final String message = new String(
                        packet.getData(), 0,
                        packet.getLength(), "UTF-8");
                // Debug Some Information
                mLogger.warning("Receiving '" + message + "'");
                // Return Received Data
                return message;
            } catch (final Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        }
        // Otherwise Return Null
        return null;
    }

    // Send some message 
    public final boolean send(final String string) {
        try {
            // Create the byte buffer
            final byte[] buffer = string.getBytes("UTF-8");
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length);
            // And send the UDP packet
            mSocket.send(packet);
            // Print some information
            mLogger.message("Sending '" + string + "'");
            // Return true at success
            return true;
        } catch (final Exception exc) {
            // Print some information
            mLogger.failure(exc.toString());
            // Return false at failure 
            return false;
        }
    }

    // Launch the executor 
    @Override
    public void launch() {

    }

    // Unload the executor 
    @Override
    public void unload() {

    }

    // Execute the server thread
    @Override
    public final void run() {
        // Receive while not done ...
        while (!mDone) {
            // Receive a new message
            final String message = recv();
            if (message != null) {
                //
                //final long time = mPlayer.getCurrentTime();
                // Parse the new message
                //final SSIEventObject event = new SSIEventObject(time, message);
                // Print some information
                // mLogger.message("SSI event handler receiving '" + event.toString() + "'");
                // Delegate the handling            
                //mPlayer.handle(event);
            }
        }
    }

    @Override
    public final void execute(
            final AbstractActivity activity,
            final ActivityManager scheduler) {
        // Compile the activity
        final String command = activity.toString();
        // Print some information
        System.err.println("Command '" + command + "'");
        // Give some status feeback
        //scheduler.handle(new StatusFeedback(activity, STARTED));
        //
        try {
            for (int i = 0; i < 5; i++) {
                // Simulate the execution
                Thread.sleep(1000);
                // Give some status feeback
                //scheduler.handle(new StatusFeedback(activity, RUNNING));
            }
        } catch (final Exception exc) {
            // Print some information
            System.err.println("Interrupting command execution '" + command + "'");
            // Give some status feeback
            //scheduler.handle(new StatusFeedback(activity, ABORTED));
        }

        // Give some status feeback
        //scheduler.handle(new StatusFeedback(activity, STOPPED));

    }

    @Override
    public final String marker(final long id) {
        // Acapela style bookmarks
        return "\\mrk=" + id + "\\";
    }
}
