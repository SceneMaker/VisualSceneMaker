package de.dfki.vsm.xtension.remote.client.sender.clients;

import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 * Created by alvaro on 5/2/17.
 */
public class UDPClient implements Clientable{

    private final int port;
    private final String host;
    private DataSendable dataCreator;
    byte[] buf = new byte[256];
    private DatagramSocket socket;

    public UDPClient(String host, int port){
        this.port = port;
        this.host = host;
    }

    @Override
    public void setDataCreator(DataSendable dataCreator) {
        this.dataCreator = dataCreator;
    }

    @Override
    public void connect() throws IOException {
        socket = new DatagramSocket();
    }

    @Override
    public void send() throws IOException {
        InetAddress address = InetAddress.getByName(host);
        String dataToSend = dataCreator.buildDataToSent();
        buf = dataToSend.getBytes();
        DatagramPacket packet = new DatagramPacket(buf, buf.length, address, port);
        socket.send(packet);
    }

    @Override
    public boolean isConnected() {
        return socket.isConnected();
    }
}
