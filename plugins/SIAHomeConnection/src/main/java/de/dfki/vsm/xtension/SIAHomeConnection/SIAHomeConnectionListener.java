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

    private ServerSocket serverSocket;
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

    /**
    @Override
    public final void start() {
        try {
            this.get_socket = new DatagramSocket(this.get_port);
        } catch (SocketException e) {
            mLogger.failure("Failed to create socket: " + e);
        }
        super.start();
    }*/

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

    public String getASR_message(){
        return this.ASR_message;
    }

    @Override
    public final void run() {
        try {
            System.out.println("-- Running Server at " + InetAddress.getLocalHost() + "*************" + " The port is: " + this.get_port);
        } catch (UnknownHostException e) {
            mLogger.failure("Failed to run server: " + e);
        }
        String msg;

        /**while (this.keepServerAlive) {

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
        }*/

        while (this.keepServerAlive) {
            try (Socket clientSocket = serverSocket.accept();
                 BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))) {
                String inputLine = in.readLine();
                if (inputLine != null) {
                    System.out.println("Received message: " + inputLine);
                    ASR_message = inputLine;
                    executor.set_app_intent(inputLine);
                }
            } catch (IOException e) {
                System.out.println("Error while receiving data: " + e);
            }
        }
    }
}