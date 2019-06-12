package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.*;

public class UdpClient {
    private DatagramSocket socket;
    private InetAddress address;
    private String ipAddress;
    private int port;
    private byte[] buf;

    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();


    public UdpClient(String ipAddress, int port) throws SocketException, UnknownHostException {
        this.ipAddress = ipAddress;
        this.port = port;
        socket = new DatagramSocket();
        address = InetAddress.getByName(this.ipAddress);
    }

    void clientSend(String msg) throws IOException {
        mLogger.message("OPDExecutor: Send message to ODP: " + msg);
        buf = msg.getBytes();
        DatagramPacket packet = new DatagramPacket(buf, buf.length, address, port);
        socket.send(packet);
    }

    public void close() {
        socket.close();
    }
}
