/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.studymaster;

import de.dfki.vsm.util.log.LOGConsoleLogger;
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
public class ReceiverThread extends Thread {

    private int mPort;

    private StudyMasterReceiverExecutor mExecutor;

    private boolean mRunning = true;

    private DatagramSocket mSocket;

    // last raw data
    private static byte[] lastReceivedData;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ReceiverThread(StudyMasterReceiverExecutor executor, int port) {
        mExecutor = executor;
        mPort = port;
    }

    @Override
    public void run() {
        try {

//            mSocket = new MulticastSocket(mPort);
//            mSocket.setReuseAddress(true);
//            mSocket.setBroadcast(true);
//            mSocket.joinGroup(InetAddress.getByName("230.0.0.1"));
            //Keep a mSocket open to listen to all the UDP trafic that is destined for this port
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

                // Check if this has been received
                if (data.equals(lastReceivedData)) {
                    mLogger.message("Ommitting message - was received alread");
                    break;
                }
                lastReceivedData = data;

                String message = new String(packet.getData(), StandardCharsets.UTF_8).trim();
                mLogger.message("Message received " + message + " from " + packet.getAddress().getHostAddress());
                if (message.startsWith(SenderExecutor.sMSG_HEADER)) {

                    // parse message
                    String[] msgParts = message.split(SenderExecutor.sMSG_SEPARATOR);

                    if (msgParts.length > 1) {
                        String msgHeader = msgParts[0];
                        String msg = msgParts[1];
                        String timestamp = "";
                        String timeinfo = "";

                        if (msg.equalsIgnoreCase("VAR")) {
                            String var = msgParts[2];
                            String value = msgParts[3];

                            mExecutor.setSceneFlowVariable(var, value);
                        } else {
                            if (msgParts.length > 2) {
                                timestamp = msgParts[2];
                            }

                            if (msgParts.length == 4) {
                                timeinfo = msgParts[3];
                            }

                            mExecutor.setSceneFlowVariable(msg);
                        }
                    }

                    //Send a response
//                    byte[] sendData = "VSMMessage#Received".getBytes();
//                    DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, packet.getAddress(), packet.getPort());
//                    mSocket.send(sendPacket);
//
//                    mLogger.message("Sent confirmation to " + sendPacket.getAddress().getHostAddress());
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