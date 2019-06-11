package de.dfki.vsm.xtension.odp;

import java.io.IOException;
import java.net.*;

public class UdpClient {
    private DatagramSocket socket;
    private InetAddress address;
    private String ipAddress;
    private int port;
    private byte[] buf;

    public UdpClient(String ipAddress, int port) throws SocketException, UnknownHostException {
        this.ipAddress = ipAddress;
        this.port = port;
        socket = new DatagramSocket();
        address = InetAddress.getByName(this.ipAddress);
    }

    void clientSend(String msg) throws IOException {
        System.out.println("send: " + msg);
        buf = msg.getBytes();
        DatagramPacket packet = new DatagramPacket(buf, buf.length, address, this.port);
        socket.send(packet);
        // packet = new DatagramPacket(buf, buf.length);
        // socket.receive(packet);
        // String received = new String(packet.getData(), 0,
        // packet.getLength());
    }

    public void close() {
        socket.close();
    }
}
