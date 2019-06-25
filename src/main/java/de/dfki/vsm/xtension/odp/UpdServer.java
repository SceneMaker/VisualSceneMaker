package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;

public class UpdServer extends Thread {
    private DatagramSocket socket;
    private boolean running;
    private byte[] buf = new byte[1024];

    private RunTimeProject mProject;
    private String mSceneFlowVar = "opdMsg";

    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();


    public UpdServer(int port, RunTimeProject project) throws SocketException {
        mProject = project;
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

                mProject.setVariable(mSceneFlowVar, new StringValue(message));

                mLogger.message("OPD UPD Message received: " + message);
            }
        }
    }

    public void close() {
        running = false;
        socket.close();
    }
}
