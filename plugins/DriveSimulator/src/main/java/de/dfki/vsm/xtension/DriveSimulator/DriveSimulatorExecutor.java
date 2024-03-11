package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.sockets.DataSocket;

public class DriveSimulatorExecutor  extends ActivityExecutor {

    SpeedHandler vehiclesClient;
    ConStartHandler constructionClient;
    DataSocket speedSocket, conStartSocket;

    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

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
        SpeedHandler sHandler = new SpeedHandler(mProject);
        speedSocket = new DataSocket(sHandler,40421);

        ConStartHandler csHandler = new ConStartHandler(mProject);
        conStartSocket = new DataSocket(csHandler,40422);

       // ConStartHandler cHandler = new ConStartHandler(mProject);
       // SocketClient socketC = new SocketClient(cHandler,4002);
        speedSocket.start();
        conStartSocket.start();
    }

    @Override
    public void unload() {
        speedSocket.abort();
    }

}
