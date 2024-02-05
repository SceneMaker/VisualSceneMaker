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
        vehiclesClient = new VehiclesHandler(mProject, 9988);
        constructionClient = new ConstructionHandler(mProject, 9989);
    }

    @Override
    public void unload() {
        vehiclesClient.unload();
        constructionClient.unload();
    }

    @Override
    public boolean handle(String msg) {
        return false;
    }
}
