/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.studymasterWeb;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.net.DatagramSocket;

import static de.dfki.vsm.xtension.studymasterWeb.WebStudyMasterExecutor.sMSG_HEADER;
import static de.dfki.vsm.xtension.studymasterWeb.WebStudyMasterExecutor.sMSG_SEPARATOR;

/**
 * @author Patrick Gebhard
 */
public class Receiver {

    private final WebStudyMasterExecutor mExecutor;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private DatagramSocket mSocket;

    public Receiver(WebStudyMasterExecutor executor) {
        mExecutor = executor;
    }


    void handleMessage(String message) {
        if (message.startsWith(sMSG_HEADER)) {
            System.out.println("Message in studymaster: " + message);

            // parse message
            String[] msgParts = message.split(sMSG_SEPARATOR);

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
                    if (msgParts.length > 3) {
                        timestamp = msgParts[3];
                    }

                    if (msgParts.length == 5) {
                        timeinfo = msgParts[4];
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
}