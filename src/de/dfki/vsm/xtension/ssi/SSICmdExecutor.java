package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.logger.SSILoggerMessage;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public final class SSICmdExecutor extends ActivityExecutor {

    // The SSI receiver data
    private final String mLogHost;
    private final String mLogPort;
    private final String mLogVar;
    //private SSIEventSender mSSILog;
    private final String[] mSSIPipe;

    // Construct executor
    public SSICmdExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        mLogVar = mConfig.getProperty("logvar");
        mLogHost = mConfig.getProperty("loghost");
        mLogPort = mConfig.getProperty("logport");
        mSSIPipe = mConfig.getProperty("pipes").split(",");
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        //mSSILog = new SSIEventSender("127.0.0.1", 8888,
        //        mLogHost, Integer.valueOf(mLogPort));
        //mSSILog.start();
    }

    @Override
    public void unload() {
        //mSSILog.abort();
    }

    // Execute activity
    @Override
    public void execute(final AbstractActivity activity) {

        // Get log message features
        final String name = activity.getName();
        if (name.equalsIgnoreCase("start")) {
            start(activity.get("pipe"));
        } else if (name.equalsIgnoreCase("stop")) {
            stop(activity.get("pipe"));
        } else if (name.equalsIgnoreCase("log")) {
            //final String systime = Long.toString(System.currentTimeMillis());
            final String sender = activity.get("sender");//, features, map);
            final String event = activity.get("event");//, features, map);
            final String state = activity.get("state");//, features, map);
            final String time = activity.get("time");//, features, map);
            final String content = activity.get("content");//, features, map);
            final String duration = activity.get("duration");//, features, map);
            // Create final event message
            final SSILoggerMessage message
                    = new SSILoggerMessage(name,
                            (sender == null ? "default" : sender),
                            (event == null ? "default" : event),
                            (state == null ? "completed" : state),
                            (time == null ? "0" : time),
                            (duration == null ? "0" : duration),
                            (content == null ? " " : content));
            mProject.setVariable(mLogVar, message.toString());
            mLogger.warning("Sending '" + message.toString() + "' to '" + mLogHost + ":" + mLogPort);
            // Send the event message
            //mSSILog.sendString(message.toString());
            log(message.toString());
        }
    }

    private void pipe(
            final String line,
            final String pipe) {
        // Get correct address
        for (final String entry : mSSIPipe) {
            final String[] array = entry.split(":");
            final String name = array[0];
            final String host = array[1];
            final String port = array[2];
            if (name.equalsIgnoreCase(pipe)) {
                try {
                    final DatagramSocket socket = new DatagramSocket();
                    // Receive the datagram packet
                    final byte[] buffer = line.getBytes("UTF-8");
                    // Create the UDP packet
                    final DatagramPacket packet
                            = new DatagramPacket(buffer, buffer.length,
                                    new InetSocketAddress(host, Integer.valueOf(port)));
                    // And send the UDP packet
                    socket.send(packet);
                    socket.close();
                } catch (final Exception exc) {
                    exc.printStackTrace();
                }
                return;
            }
        }
    }

    private void start(final String pipe) {
        pipe("SSI:STRT:RUN1\0", pipe);
    }

    private void stop(final String pipe) {
        pipe("SSI:STOP:RUN1\0", pipe);
    }

    private void log(
            final String line) {
        try {
            final DatagramSocket socket = new DatagramSocket();
            // Receive the datagram packet
            final byte[] buffer = line.getBytes("UTF-8");
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length,
                            new InetSocketAddress(mLogHost, Integer.valueOf(mLogPort)));
            // And send the UDP packet
            socket.send(packet);
            socket.close();
        } catch (final Exception exc) {
            exc.printStackTrace();
        }
    }
}
