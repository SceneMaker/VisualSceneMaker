package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;

public class UpdServer extends Thread {
    private DatagramSocket socket;
    private boolean running;
    private byte[] buf = new byte[1024];

    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();


    public UpdServer(int port) throws SocketException {
        socket = new DatagramSocket(port);
    }

    @Override
    public void run() {
        running = true;
        mLogger.message("ODPExecutor: Ready to receive messages ...");
        while (running) {
            DatagramPacket packet = new DatagramPacket(buf, buf.length);
            try {
                if (packet.getLength() > 0) {
                    socket.receive(packet);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }

            if (packet.getLength() > 0) {
                String message = new String(packet.getData(), 0, packet.getLength());

                mLogger.message("OPD UPD Message received: " + message);
            }
        }
    }

    public void close() {
        running = false;
        socket.close();
    }
}
