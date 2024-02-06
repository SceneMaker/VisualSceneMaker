package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.DriveSimulator.gson.ConstructionDistance;
import de.dfki.vsm.xtension.DriveSimulator.gson.VehiclesData;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.VSMSocketHandler;
import com.google.gson.Gson;
import io.socket.emitter.Emitter;

import java.lang.reflect.Type;

public class ConstructionHandler implements Emitter.Listener {

    private final RunTimeProject mProject;
    Gson gson = new Gson();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();


    public ConstructionHandler(RunTimeProject project){
        this.mProject = project;
        mLogger.message("started Vehicles Handler");
    }

    @Override
    public void call(Object... args) {
        ConstructionDistance constructionDistance = gson.fromJson((String)args[0], ConstructionDistance.class);
        double constructionStart = constructionDistance.getConstructionStart();
        mProject.setVariable("constructionStart",(float)constructionStart);
        double constructionEnd = constructionDistance.getConstructionEnd();
        mProject.setVariable("constructionEnd",(float)constructionEnd);
    }
}
