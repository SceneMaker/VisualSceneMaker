package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.VSMSocketHandler;

public class DriveSimulatorExecutor  extends ActivityExecutor implements VSMSocketHandler {

    VehiclesHandler vehiclesClient;
    ConstructionHandler constructionClient;
    SocketIOClient socket;

    public DriveSimulatorExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {

    }

    @Override
    public void launch() {
        socket = new SocketIOClient("localhost","5000");
        socket.addListener("VEHICLES", new VehiclesHandler(mProject));
        socket.addListener("distance",new ConstructionHandler(mProject));
    }

    @Override
    public void unload() {
        socket.abort();
    }

    @Override
    public boolean handle(String msg) {
        return false;
    }
}
