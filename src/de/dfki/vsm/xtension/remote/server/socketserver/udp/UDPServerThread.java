package de.dfki.vsm.xtension.remote.server.socketserver.udp;

import de.dfki.vsm.xtension.remote.server.receiver.Receiver;
import de.dfki.vsm.xtension.remote.server.socketserver.ServerLoop;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;

/**
 * Created by alvaro on 6/14/17.
 */
public class UDPServerThread extends ServerLoop{
    private final DatagramSocket sock;
    private final Receiver receiver;
    private byte[] buffer = new byte[65536];
    private final DatagramPacket incoming;

    UDPServerThread(DatagramSocket socket, Receiver receiver){
        this.sock = socket;
        incoming = new DatagramPacket(buffer, buffer.length);
        this.receiver = receiver;
    }


    protected void receive() throws IOException {
        sock.receive(incoming);
        byte[] data = incoming.getData();
        String line = new String(data, 0, incoming.getLength());
        System.out.println("UDP Message from Client  :  " + line);
        receiver.receive(line);
    }



    @Override
    protected void close() throws IOException {
        sock.close();
    }

    @Override
    protected void cleanup() {
        buffer = null;
    }
}
