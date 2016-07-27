/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Arrays;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ReceiverThread extends Thread {

    private int mPort;

    private ReceiverExecutor mExecutor;

    private boolean mRunning = true;

    private DatagramSocket mSocket;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ReceiverThread(ReceiverExecutor executor, int port) {
        mExecutor = executor;
        mPort = port;
    }

    @Override
    public void run() {
        try {
            //Keep a mSocket open to listen to all the UDP trafic that is destined for this port
            mSocket = new DatagramSocket(mPort, InetAddress.getByName("0.0.0.0"));
            mSocket.setBroadcast(true);

            while (mRunning) {
                mLogger.message("Ready to receive messages ...");

                //Receive a packet
                byte[] recvBuf = new byte[15000];
                DatagramPacket packet = new DatagramPacket(recvBuf, recvBuf.length);
                mSocket.receive(packet);

                // Read Packet
                byte[] realData = Arrays.copyOf(packet.getData(), packet.getLength());;

                String message = new String(packet.getData(), "UTF-8").trim();
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
                            
                            if (mExecutor.hasProjectVar(var)) {
                                mExecutor.setSceneFlowVariable(var, value);
                            }
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
            mLogger.message("Exception while receiving data ...");
        }
    }

    public void stopServer() {
        mSocket.close();
        mRunning = false;
    }
}
