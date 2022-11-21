package de.dfki.vsm.xtension.DialogManager;

import java.io.IOException;
import java.net.*;


/**
 * @author Chirag Bhuvaneshwara
 */
public class DialogManagerListener extends Thread {

    private DatagramSocket udpSocket;
    private int port;
    private final DialogManagerExecutor executor;
    private boolean keepServerAlive;
    private String ASR_message;


    public DialogManagerListener(int port, DialogManagerExecutor executor, boolean keepServerAlive) {
        this.port = port;
        this.executor = executor;
        this.keepServerAlive = keepServerAlive;
    }

    @Override
    public final void start() {
        try {
            this.udpSocket = new DatagramSocket(this.port);
        } catch (SocketException e) {
            throw new RuntimeException(e);
        }
        super.start();
    }

    public void killprocess(){
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
            throw new RuntimeException(e);
        }
        String msg;

        while (this.keepServerAlive) {

            byte[] buf = new byte[256];
            DatagramPacket packet = new DatagramPacket(buf, buf.length);

            // blocks until a packet is received
            try {
                udpSocket.receive(packet);
            } catch (IOException e) {
                System.out.println("Problem");
                throw new RuntimeException(e);
            }
            msg = new String(packet.getData()).trim();

            System.out.println(
                    "Message from " + packet.getAddress().getHostAddress() + ": " + msg);

            ASR_message = msg;

            executor.set_transcript(msg);
            System.out.println(executor.get_transcript());

        }
    }
}