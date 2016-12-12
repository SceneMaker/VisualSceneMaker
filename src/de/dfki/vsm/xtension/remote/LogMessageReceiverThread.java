/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.remote.message.LogMessage;
import static de.dfki.vsm.xtension.remote.message.LogMessage.sID;
import static de.dfki.vsm.xtension.remote.message.LogMessage.sSEPARATOR;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.util.Arrays;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class LogMessageReceiverThread extends Thread {

    private int mPort;

    private SSILogMessageService mExecutor;

    private boolean mRunning = true;

    private DatagramSocket mSocket;

    // last raw data
    private static byte[] lastReceivedData;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public LogMessageReceiverThread(SSILogMessageService executor, int port) {
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
                mLogger.message("Ready to receive SSI LogMessages ...");

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

                String message = new String(packet.getData(), "UTF-8").trim();
                mLogger.message("Message received " + message + " from " + packet.getAddress().getHostAddress());
                if (message.startsWith(sID)) {

                    // parse message
                    String[] msgParts = message.split(sSEPARATOR);

                    if (msgParts.length != 6) {
                        mLogger.failure("Wrong message format! Does not match SENDER#CLASS#CONTENT#TIMESTAMP#DURATION#STATE");
                        break;
                    }

                    final LogMessage m = new LogMessage();
                    final String sender = msgParts[0];

                    // class
                    final String type = msgParts[1];
                    try {
                        m.setClass(LogMessage.Class.valueOf(type.toUpperCase().trim()));
                    } catch (IllegalArgumentException iae) {
                        mLogger.failure("Wrong message class! " + type);
                        break;
                    }
                    
                    final String content = msgParts[2];
                    if (type.equalsIgnoreCase(LogMessage.Class.VARASSIGN.toString())) { // var assign
                        final String[] vvParts = content.split(":");
                        final String var = vvParts[0];
                        final String value = vvParts[1];

                        mExecutor.setSceneFlowVariable(var, value);
                    }
                }
            }
        } catch (IOException ex) {
            mLogger.failure("Exception while receiving data ... " + ex.getMessage());
        }
    }

    public void stopServer() {
        if (mSocket != null) {
            mSocket.close();
        }
        mRunning = false;
    }
}
