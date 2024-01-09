package de.dfki.vsm.xtension.SIAHomeConnection;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.IOException;
import java.net.*;


/**
 * @author Chirag Bhuvaneshwara
 */
public class SIAHomeConnectionListener extends Thread {

    private DatagramSocket get_socket;
    private int get_port;
    private final SIAHomeConnectionExecutor executor;
    private boolean keepServerAlive;
    private String ASR_message;
    private LOGDefaultLogger mLogger;


    public SIAHomeConnectionListener(int get_port, LOGDefaultLogger mLogger, SIAHomeConnectionExecutor executor, boolean keepServerAlive) {
        this.get_port = get_port;
        this.executor = executor;
        this.keepServerAlive = keepServerAlive;
        this.mLogger = mLogger;
    }

    @Override
    public final void start() {
        try {
            this.get_socket = new DatagramSocket(this.get_port);
        } catch (SocketException e) {
            mLogger.failure("Failed to create socket: " + e);
        }
        super.start();
    }

    public void killprocess(){
        get_socket.close();
        this.keepServerAlive = false;
    }

    public String getASR_message(){
        return this.ASR_message;
    }

    @Override
    public final void run() {
        try {
            System.out.println("-- Running Server at " + InetAddress.getLocalHost() + "--");
        } catch (UnknownHostException e) {
            mLogger.failure("Failed to run server: " + e);
        }
        String msg;

        while (this.keepServerAlive) {

            byte[] buf = new byte[256];
            DatagramPacket packet = new DatagramPacket(buf, buf.length);

            // blocks until a packet is received
            try {
                get_socket.receive(packet);

                msg = new String(packet.getData()).trim();

                System.out.println(
                        "Message from " + packet.getAddress().getHostAddress() + ": " + msg);
                System.out.println(
                        "Type of msg : " + (msg.getClass()));

                ASR_message = msg;

                executor.set_app_intent(msg);
                System.out.println(executor.get_app_intent());
            } catch (IOException e) {
                mLogger.failure("Error while receiving packet: " + e);
            }
        }
    }
}