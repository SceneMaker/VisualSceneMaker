package de.dfki.vsm.xtension.DialogManager;

import java.io.IOException;
import java.net.*;

public class DialogManagerSender {

    private DatagramSocket post_socket;
    private InetAddress post_address;
    private int post_port;


    public DialogManagerSender(int post_port, String post_address) throws SocketException, UnknownHostException {
        this.post_address = InetAddress.getByName(post_address);
        this.post_port = post_port;
        this.post_socket = new DatagramSocket();
    }

    public void SendInstruction(final byte[] data) {
        new Thread(() -> {
            try {
                DatagramPacket packet = new DatagramPacket(data, data.length, post_address, post_port);
                post_socket.send(packet);
            } catch (Exception e) {
                e.printStackTrace();
            }

        }).start();
    }

    public void killprocess() {
        this.post_socket.close();
    }

}
