package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.xtension.ssi.logger.SSILoggerMessage;
import de.dfki.vsm.xtension.ssi.util.property.SSIProjectProperty;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.HashMap;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public final class SSICmdExecutor extends ActivityExecutor implements ExportableProperties {

    // The SSI receiver data
    private final String mLogHost;
    private final String mLogPort;
    public final String mLogVar;
    private final boolean mBroadcasting; // 9.6.17 add by PG
    public final String mBroadCastPort;  // 9.6.17 add by PG
    private final boolean mListenToStudyMaster; // 9.6.17 add by PG
    private final String mStudyMasterPort;  // 9.6.17 add by PG
    private final String[] mSSIPipe;
    private StudyMasterReceiverThread mSMReceiver; // 9.6.17 add by PG
    private ExportableProperties exportableProperties = new SSIProjectProperty();

    // Construct executor
    public SSICmdExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        mLogVar = mConfig.getProperty("logvar");
        mLogHost = mConfig.getProperty("loghost");
        mLogPort = mConfig.getProperty("logport");
        mSSIPipe = mConfig.getProperty("pipes").split(",");
        mBroadcasting = Boolean.valueOf(mConfig.getProperty("broadcast")); // 9.6.17 add by PG
        mBroadCastPort = mConfig.getProperty("broadcastport"); // 9.6.17 add by PG
        mListenToStudyMaster = Boolean.valueOf(mConfig.getProperty("listentostudymaster")); // 9.6.17 add by PG
        mStudyMasterPort = mConfig.getProperty("studymasterport"); // 9.6.17 add by PG
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        if (mListenToStudyMaster) {
            mSMReceiver = new StudyMasterReceiverThread(this, Integer.parseInt(mStudyMasterPort));
            mSMReceiver.start();
        }
    }

    @Override
    public void unload() {
        if (mListenToStudyMaster) {
            mSMReceiver.stopServer();
        }
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
        } else {
            //final String systime = Long.toString(System.currentTimeMillis());
            final String sender = activity.get("sender");//, features, map);
            final String event = activity.get("event");//, features, map);
            final String state = activity.get("state");//, features, map);
            final String time = activity.get("time");//, features, map);
            final String duration = activity.get("duration");//, features, map);

            SSILoggerMessage message = null;

            if (name.equalsIgnoreCase("request")) {
                final String var = activity.get("var");//, features, map);
                final String values = activity.get("values");//, features, map);

                // Create final variable request value
                message = new SSILoggerMessage(name,
                        (sender == null ? "default" : sender),
                        (event == null ? "default" : event),
                        (state == null ? "completed" : state),
                        (time == null ? "0" : time),
                        (duration == null ? "0" : duration),
                        ((var == null) && (values == null) ? " " : var + "#" + values));
            } else if (name.equalsIgnoreCase("log")) {
                final String content = activity.get("content");//, features, map);

                // Create final event message
                message = new SSILoggerMessage(name,
                        (sender == null ? "default" : sender),
                        (event == null ? "default" : event),
                        (state == null ? "completed" : state),
                        (time == null ? "0" : time),
                        (duration == null ? "0" : duration),
                        (content == null ? " " : content));
            }

            if (message != null) {
                mProject.setVariable(mLogVar, message.toString());
                mLogger.warning("Sending '" + message.toString() + "' to '" + mLogHost + ":" + mLogPort);
                // Send the event message
                //mSSILog.sendString(message.toString());
                log(message.toString());

                // 9.6. added by PG
                if (mBroadcasting) {
                    mLogger.warning("Broadcasting '" + message.toString() + ":" + mBroadCastPort);
                    broadcast(message.toString());
                }
            }
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

    public void broadcast(final String message) {
        DatagramSocket c;

        try {
            //Open a random port to send the package
            c = new DatagramSocket();
            c.setBroadcast(true);

            long timestamp = System.currentTimeMillis();

            byte[] sendData = (message.toString()).getBytes("UTF8");

            String hosts = "";

            Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
            while (interfaces.hasMoreElements()) {
                NetworkInterface networkInterface = interfaces.nextElement();

                if (networkInterface.isLoopback() || !networkInterface.isUp()) {
                    continue; // Don't want to broadcast to the loopback interface
                }

                for (InterfaceAddress interfaceAddress : networkInterface.getInterfaceAddresses()) {
                    InetAddress broadcast = interfaceAddress.getBroadcast();
                    if (broadcast == null) {
                        continue;
                    }

                    // Send the broadcast package
                    boolean packetSend = false;
                    try {
                        DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, broadcast, Integer.valueOf(mBroadCastPort));
                        c.send(sendPacket);
                        hosts = hosts + broadcast.getHostAddress() + ", ";
                        mLogger.message(message + " sent to " + broadcast.getHostAddress() + " on interface " + networkInterface.getDisplayName());
                        packetSend = true;
                    } catch (Exception e) {
                        packetSend = false;
                    }

                    if (packetSend) {
                        mLogger.message("Message successfully send");
                    }
                }
            }
            
            c.close();
        } catch (IOException ex) {
            mLogger.message(ex.toString());
        }
    }

    public boolean hasProjectVar(String var) {
        return mProject.hasVariable(var);
    }

    public void setSceneFlowVariable(String var, String value) {
        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
        mProject.setVariable(var, new StringValue(value));
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return exportableProperties.getExportableAgentProperties();
    }
}
