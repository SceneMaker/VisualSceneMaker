/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.ssi.logger.SSILoggerMessage;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StudyMasterReceiverThread extends Thread {

    private final int mPort;
    private final SSICmdExecutor mExecutor;
    private boolean mRunning = true;
    private DatagramSocket mSocket;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public StudyMasterReceiverThread(SSICmdExecutor executor, int port) {
        mExecutor = executor;
        mPort = port;
    }

    @Override
    public void run() {
        try {
            mSocket = new DatagramSocket(null);
            mSocket.setReuseAddress(true);
            mSocket.setBroadcast(true);
            mSocket.bind(new InetSocketAddress(mPort));

            while (mRunning) {
                mLogger.message("Ready to receive messages ...");

                //Receive a packet
                byte[] recvBuf = new byte[15000];
                DatagramPacket packet = new DatagramPacket(recvBuf, recvBuf.length);
                mSocket.receive(packet);

                // Read Packet
                byte[] data = Arrays.copyOf(packet.getData(), packet.getLength());

                String message = new String(packet.getData(), StandardCharsets.UTF_8).trim();
                mLogger.message("Message received " + message + " from " + packet.getAddress().getHostAddress());
                if (message.startsWith("VSM")) {
                    // parse message
                    String[] msgParts = message.split("#");

                    if (msgParts.length > 1) {
                        String msgHeader = msgParts[0];
                        String msg = msgParts[1];
                        String timestamp = "";
                        String timeinfo = "";

                        if (msg.equalsIgnoreCase("RUALIVE")) {
                            SSILoggerMessage amsg = new SSILoggerMessage("vsmalive", "SceneMaker", "event", "state", "completed", "0","0");
                            mLogger.warning("Broadcasting '" + amsg.toString() + ":" + mExecutor.mBroadCastPort);
                            mExecutor.broadcast(amsg.toString());
                        } else {
                            if ((msg.equalsIgnoreCase("START")) || (msg.equalsIgnoreCase("ASSIGN"))) {
                                String var = msgParts[2];
                                String value = msgParts[3];

                                // added backwards compability
                                if (var.equalsIgnoreCase("none") && value.equalsIgnoreCase("Start")) {
                                    if (mExecutor.hasProjectVar(mExecutor.mLogVar)) {
                                        mExecutor.setSceneFlowVariable(mExecutor.mLogVar, "Go");
                                    }
                                } else if (mExecutor.hasProjectVar(var)) {
                                    mExecutor.setSceneFlowVariable(var, value);
                                }
                            }

                            // added backwards compability                        
                            if ((msg.equalsIgnoreCase("MESSAGE"))) {
                                String value = msgParts[2];

                                if ((value.equalsIgnoreCase("Go"))) {
                                    if (mExecutor.hasProjectVar(mExecutor.mLogVar)) {
                                        mExecutor.setSceneFlowVariable(mExecutor.mLogVar, value);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (IOException ex) {
            mLogger.message("Exception while receiving data ... " + ex.getMessage());
        }
    }

    public void stopServer() {
        if (mSocket != null) {
            mSocket.close();
        }
        mRunning = false;
    }
}
