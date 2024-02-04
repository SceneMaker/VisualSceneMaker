package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.DriveSimulator.gson.VehiclesData;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.VSMSocketHandler;
import com.google.gson.Gson;

public class ConstructionHandler implements VSMSocketHandler {

    private final RunTimeProject mProject;
    Gson gson = new Gson();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    public VehiclesHandler(RunTimeProject project, int port){
        SocketClient client = new SocketClient(this, port);
        client.start();
        this.mProject = project;
        mLogger.message("started Vehicles Handler");
    }

    @Override
    public boolean handle(String msg) {
        VehiclesData vData = gson.fromJson(msg, VehiclesData.class);
        double speed = vData.getEgoVehicle().getSpeed();
        mProject.setVariable("speed",(float)speed);
        return true;
    }

}
