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
    private final String mSceneFlowTaskVar = "odpTask";
    private final String mSceneFlowFuncVar = "odpFunc";
    private final String mSceneFlowContVar = "odpCont";
    private final String mSceneFlowNumbVar = "odpNum"; // stores the int value if odpCont is an int value
    private final String mSceneFlowActiVar = "odpAct";

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

                mLogger.message("ODP UPD Message received: " + message);

                JSONObject jObj = new JSONObject(message);

                // task
                String task = "";
                try {
                    task = jObj.getString("task");
                } catch (Exception e) {
                    task = "";
                }

                // function
                String function = "";
                try {
                    function = jObj.getString("function");
                } catch (Exception e) {
                    function = "";
                }

                // content
                String content = "";
                try {
                    content = jObj.getString("content");
                } catch (Exception e) {
                    content = "";
                }

                // int content
                int number = -1;
                try {
                    number = Integer.parseInt(content);
                } catch (NumberFormatException e) {
                    number = -1;
                }

                // action
                String action = "";
                try {
                    action = jObj.getString("action");
                } catch (Exception e) {
                    action = "";
                }

                // ContactName
                String ContactName = "";
                try {
                    ContactName = jObj.getString("ContactName");
                } catch (Exception e) {
                    ContactName = "";
                }

                // ContactNumber
                String ContactNumber = "";
                try {
                    ContactNumber = jObj.getString("ContactNumber");
                } catch (Exception e) {
                    ContactNumber = "";
                }

                mProject.setVariable(mSceneFlowTaskVar, new StringValue(task));
                mProject.setVariable(mSceneFlowFuncVar, new StringValue(function));
                mProject.setVariable(mSceneFlowContVar, new StringValue(content));
                mProject.setVariable(mSceneFlowNumbVar, number);
                mProject.setVariable(mSceneFlowActiVar, new StringValue(action));

                mLogger.message("Parsed ODP Message: " + jObj);
            }
        }
    }

    public void close() {
        running = false;
        socket.close();
    }
}
