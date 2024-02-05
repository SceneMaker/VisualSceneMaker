package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.DriveSimulator.gson.ConstructionDistance;
import de.dfki.vsm.xtension.DriveSimulator.gson.VehiclesData;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.VSMSocketHandler;
import com.google.gson.Gson;

import java.lang.reflect.Type;

public class ConstructionHandler implements VSMSocketHandler {

    private final RunTimeProject mProject;
    Gson gson = new Gson();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    SocketClient client;

    public ConstructionHandler(RunTimeProject project, int port){
        client = new SocketClient(this, port);
        client.start();
        this.mProject = project;
        mLogger.message("started Vehicles Handler");
    }

    @Override
    public boolean handle(String msg) {
        ConstructionDistance constructionDistance = gson.fromJson(msg, (Type) VehiclesData.class);
        double constructionStart = constructionDistance.getConstructionStart();
        mProject.setVariable("constructionStart",(float)constructionStart);
        double constructionEnd = constructionDistance.getConstructionEnd();
        mProject.setVariable("constructionEnd",(float)constructionEnd);
        return true;
    }

    public void unload(){
        client.abort();
    }

}
