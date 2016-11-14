/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.remote.message.LogMessage;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class SSILogMessageService extends ActivityExecutor {

    // The Port from which the message is send
    private int mPort;

    // Message Receiver
    LogMessageReceiverThread mSSILogMessageReceiver;

    // The Sceneflow variable for feedback
    private String mSceneflowVar;

    // memory of ongoing (continued) log messages
    private final HashMap<String, LogMessage> mContinuedLogMessages = new HashMap<>();

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public SSILogMessageService(PluginConfig config, RunTimeProject project) {
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
            activity.setType(AbstractActivity.Type.parallel);

            final LinkedList<ActionFeature> features = activity.getFeatures();

            String activityName = activity.getName().toLowerCase();

            LogMessage.Class messageClass;
            
            try {
                messageClass = LogMessage.Class.valueOf(activity.getName().toUpperCase().trim());
            } catch (IllegalArgumentException iae) {
                mLogger.warning("Wrong log Class - log message will be ignored");
                return;
            }

            long duration = 1000; // set the default duration to 1000 (ms)
            try {
                duration = Long.parseLong(getActionFeatureValue("duration", features));
            } catch (NumberFormatException nfe) {
            }

            LogMessage logMessage = new LogMessage();

            logMessage.setTimeStamp(System.currentTimeMillis());
            logMessage.setDuration(duration);

            logMessage.setClass(messageClass);

            // set state, if unknown set state COMPLETED
            final String state = getActionFeatureValue("state", features).toUpperCase().trim().replace("'", "");
            try {
                logMessage.setState(LogMessage.State.valueOf(state));

                if (logMessage.mState.equals(LogMessage.State.COMPLETED)) {
                    // check if there is a matching continued log message
                    if (mContinuedLogMessages.containsKey(logMessage.mContent)) {
                        LogMessage lm = mContinuedLogMessages.get(logMessage.mContent);
                        long lmTimeStamp = lm.mTimeStamp;
                        // set duration
                        logMessage.setDuration(logMessage.mTimeStamp - lmTimeStamp);
                        //remove remembered log messages
                        mContinuedLogMessages.remove(lm);
                    }
                }

                // remember continued log message for automatic caluclation of duration 
                if (logMessage.mState.equals(LogMessage.State.CONTINUED)) {
                    mContinuedLogMessages.put(logMessage.mContent, logMessage);
                    logMessage.setDuration(0);
                }
            } catch (IllegalArgumentException iae) {
                logMessage.setState(LogMessage.State.COMPLETED);
            }

            switch (messageClass) {
                case ACT: // e.g. [<agent> ACT text='Inform'] 
                    logMessage.setContent(getActionFeatureValue("text", features));
                    send(logMessage.toString());
                    break;
                case MESSAGE: // e.g. [<agent> MESSAGE text='InterviewPrepared']
                    logMessage.setContent(getActionFeatureValue("text", features));
                    send(logMessage.toString());
                    break;
                case SCENE: // e.g. [<agent> SCENE text='Welcome'] or e.g. [<agent> STATE text='Welcome' state='COMPLETED' duration='5500']
                    logMessage.setContent(getActionFeatureValue("text", features));
                    send(logMessage.toString());
                    break;
                case STATE: // e.g. [<agent> STATE text='UserIsSpeaking'] or e.g. [<agent> STATE text='UserIsSpeaking' state='COMPLETED' duration='2300']
                    logMessage.setContent(getActionFeatureValue("text", features));
                    send(logMessage.toString());
                    break;
                case VARREQUEST: // e.g. [<agent> VARREQUEST var='class' values='biology,math,music']
                    logMessage.setContent(getActionFeatureValue("var", features).trim().replace("'", "") + ":" + getActionFeatureValue("values", features).trim().replace("'", ""));
                    logMessage.setDuration(-1);
                    logMessage.setState(LogMessage.State.CONTINUED);
                    send(logMessage.toString());
                    break;
                default:
                    break;
            }
        }
    }

    private void send(String message) {
        
        mLogger.message("Sending log message: " + message);
        
        DatagramSocket c;
        // Find the server using UDP broadcast
        try {
            //Open a random port to send the package
            c = new DatagramSocket();
            c.setBroadcast(true);

            byte[] sendData = (message).getBytes("UTF8");

//            //Try the 255.255.255.255 first
//            try {
//                DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName("255.255.255.255"), mPort);
//                c.send(sendPacket);
//               // mLogger.message(">>> Request packet sent to: 255.255.255.255 (DEFAULT)");
//            } catch (Exception e) {
//            }
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
                        mLogger.message("Message sent to " + broadcast.getHostAddress() + " on interface " + networkInterface.getDisplayName());
                        packetSend = true;
                    } catch (Exception e) {
                        packetSend = false;
                    }

                    if (packetSend) {
                        mProject.setVariable(mSceneflowVar, new StringValue("Message successfully send"));
                    }
                }
            }

            //Close the socket
            c.close();
        } catch (IOException ex) {
            mLogger.message(ex.toString());
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading SSILogMessageService");
        mPort = Integer.parseInt(mConfig.getProperty("port"));
        mSceneflowVar = mConfig.getProperty("sceneflow_variable");

        mSSILogMessageReceiver = new LogMessageReceiverThread(this, mPort);
        mSSILogMessageReceiver.start();
    }

    @Override
    public void unload() {
        mSSILogMessageReceiver.stopServer();
    }

    public boolean hasProjectVar(String var) {
        return mProject.hasVariable(var);
    }

    public void setSceneFlowVariable(String message) {
        mLogger.message("Assigning sceneflow variable " + mSceneflowVar + " with value " + message);
        mProject.setVariable(mSceneflowVar, new StringValue(message));
    }

    public void setSceneFlowVariable(String var, String value) {
        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
        mProject.setVariable(var, new StringValue(value));
    }

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
    }

}
