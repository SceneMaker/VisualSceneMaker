package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONObject;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;

public class UpdServer extends Thread {
    private DatagramSocket socket;
    private boolean running;
    private byte[] buf = new byte[1024];

    private RunTimeProject mProject;
    private String mSceneFlowTaskVar = "opdTask";
    private String mSceneFlowFuncVar = "opdFunc";
    private String mSceneFlowContVar = "opdCont";

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

                JSONObject jObj = new JSONObject(message);

                String task = (jObj.getString("task") != null) ? jObj.getString("task") : "";
                String function = (jObj.getString("function") != null) ? jObj.getString("function") : "";
                String content = (jObj.getString("content") != null) ? jObj.getString("content") : "";

                mProject.setVariable(mSceneFlowTaskVar, new StringValue(task));
                mProject.setVariable(mSceneFlowFuncVar, new StringValue(function));
                mProject.setVariable(mSceneFlowContVar, new StringValue(content));

                mLogger.message("OPD UPD Message received: " + jObj);
            }
        }
    }

    public void close() {
        running = false;
        socket.close();
    }
}
