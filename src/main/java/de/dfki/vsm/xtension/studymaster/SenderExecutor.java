/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.studymaster;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Patrick Gebhard
 */
public class SenderExecutor extends ActivityExecutor {

    private int mPort;
    private String mSceneflowVar;

    // The message, format "VSMMessage#<string without space>#<timestamp>"
    static final String sMSG_SEPARATOR = "#";
    static final String sMSG_HEADER = "VSMMessage" + sMSG_SEPARATOR;

    private String mMessage = "";
    private String mMessageTimeInfo = "";
    private String mMessageRequestVar;
    private String mMessageRequestValues;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public SenderExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final List<ActionFeature> features = activity.getFeatures();

            mMessage = activity.getName();

            mMessageTimeInfo = getActionFeatureValue("time", features);

            mMessageRequestVar = getActionFeatureValue("var", features);

            mMessageRequestValues = getActionFeatureValue("values", features);

            send();
        }
    }

    private void send() {
        DatagramSocket c;
        // Find the server using UDP broadcast
        try {
            //Open a random port to send the package
            c = new DatagramSocket();
            c.setBroadcast(true);

            long timestamp = System.currentTimeMillis();

            byte[] sendData = (sMSG_HEADER + "None" + sMSG_SEPARATOR + timestamp).getBytes(StandardCharsets.UTF_8);

            if (!mMessage.equalsIgnoreCase("REQUEST")) {
                sendData = (sMSG_HEADER + mMessage + sMSG_SEPARATOR + timestamp + ((!mMessageTimeInfo.isEmpty()) ? sMSG_SEPARATOR + mMessageTimeInfo : "")).getBytes(StandardCharsets.UTF_8);
            } else if (mMessage.equalsIgnoreCase("REQUEST") && (!mMessageRequestVar.isEmpty()) && (!mMessageRequestValues.isEmpty())) {
                sendData = (sMSG_HEADER + mMessage + sMSG_SEPARATOR + timestamp + sMSG_SEPARATOR + mMessageRequestVar + sMSG_SEPARATOR + mMessageRequestValues.replace("'", "")).getBytes(StandardCharsets.UTF_8);
            }

            // Broadcast the message over all the network interfaces
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
                        DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, broadcast, mPort);
                        c.send(sendPacket);
                        hosts = hosts + broadcast.getHostAddress() + ", ";
                        mLogger.message(mMessage + " sent to " + broadcast.getHostAddress() + " on interface " + networkInterface.getDisplayName());
                        packetSend = true;
                    } catch (Exception e) {
                        packetSend = false;
                    }

                    if (packetSend) {
                        mProject.setVariable(mSceneflowVar, new StringValue("Message successfully send"));
                    }
                }
            }

            //Close the port!
            c.close();
        } catch (IOException ex) {
            mLogger.message(ex.toString());
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading Message Sender ...");

        mPort = Integer.parseInt(mConfig.getProperty("port"));
        mSceneflowVar = mConfig.getProperty("variable");

        mMessage = "VSM UPD Sender Started";
        send();
    }

    @Override
    public void unload() {

    }

}