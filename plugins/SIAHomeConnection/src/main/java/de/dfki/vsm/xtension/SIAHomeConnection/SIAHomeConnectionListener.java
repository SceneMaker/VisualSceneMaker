package de.dfki.vsm.xtension.SIAHomeConnection;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.IOException;
import java.net.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;



/**
 * @author Mina Ameli
 */
public class SIAHomeConnectionListener extends Thread {
    private DatagramSocket get_socket;
    private int get_port;
    private final SIAHomeConnectionExecutor executor;
    private boolean keepServerAlive;
    private String ASR_message;
    private LOGDefaultLogger mLogger;
    public SIAHomeConnectionListener(int get_port, SIAHomeConnectionExecutor executor, boolean keepServerAlive) {
        this.get_port = get_port;
        this.executor = executor;
        this.keepServerAlive = keepServerAlive;
        this.mLogger = mLogger;
    }

    @Override
    public final void start() {
        try {
            this.get_socket = new DatagramSocket(this.get_port);
        } catch (IOException e) {
            mLogger.failure("Failed to create server socket: " + e);
        }
        super.start();
    }

    public void killprocess(){
        get_socket.close();
        this.keepServerAlive = false;
    }

    @Override
    public final void run() {
        try {
            System.out.println("-- Running Server at " + InetAddress.getLocalHost() + " The port is: " + this.get_port);
        } catch (UnknownHostException e) {
            mLogger.failure("Failed to run server: " + e);
        }
        String msg;
        if (this.get_socket == null) {
            System.err.println("Error: The socket is unavailable.");
        }

        System.out.println("I am running.");

        while (this.keepServerAlive) {
            System.out.println("Receiving data.");
            byte[] buf = new byte[256];
            DatagramPacket packet = new DatagramPacket(buf, buf.length);

            // blocks until a packet is received
            try {
                get_socket.receive(packet);
                msg = new String(packet.getData()).trim();
                System.out.println(
                        "Message from " + packet.getAddress().getHostAddress() + ": " + msg);
            } catch (IOException e) {
                mLogger.failure("Error while receiving packet: " + e);
            }
            this.keepServerAlive = true;
        }
    }
}