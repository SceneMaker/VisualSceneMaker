package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.DriveSimulator.gson.EgoVehicle;
import de.dfki.vsm.xtension.DriveSimulator.gson.VehiclesData;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.VSMSocketHandler;
import com.google.gson.Gson;
import io.socket.emitter.Emitter;

public class VehiclesHandler implements Emitter.Listener {

    private final RunTimeProject mProject;
    Gson gson = new Gson();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();


    public VehiclesHandler(RunTimeProject project){
        this.mProject = project;
        mLogger.message("started Vehicles Handler");
    }

    @Override
    public void call(Object... args) {
        EgoVehicle ego = gson.fromJson((String)args[0], EgoVehicle.class);
        double speed = ego.getSpeed();
        mProject.setVariable("speed",(float)speed);
    }
}
