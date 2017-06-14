package de.dfki.vsm.xtension.remote.server.socketserver.udp;

import de.dfki.vsm.xtension.remote.server.receiver.Receiver;
import de.dfki.vsm.xtension.remote.server.socketserver.Servable;

import java.io.IOException;
import java.net.DatagramSocket;

/**
 * Created by alvaro on 6/14/17.
 */
public class UDPServer implements Servable {
    private final int port;
    private final Receiver receiver;
    DatagramSocket socket = null;

    public UDPServer(Receiver receiver, int port){
        this.port  = port;
        this.receiver = receiver;
    }

    @Override
    public void close() throws IOException {
        socket.close();
    }

    @Override
    public void startServer() {
        try {
            socket = new DatagramSocket(port);
            startIncomingThread();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void startIncomingThread() throws IOException {
        UDPServerThread incomingThread = new UDPServerThread(socket, receiver);
        incomingThread.start();
    }


}
