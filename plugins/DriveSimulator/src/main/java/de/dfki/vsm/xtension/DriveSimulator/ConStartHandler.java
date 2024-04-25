package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import com.google.gson.Gson;
import de.dfki.vsm.xtension.sockets.VSMSocketDataHandler;

public class ConStartHandler implements VSMSocketDataHandler {

    private final RunTimeProject mProject;
    Gson gson = new Gson();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();


    public ConStartHandler(RunTimeProject project){
        this.mProject = project;
        mLogger.message("started Speed Handler");
    }
    @Override
    public boolean handle(String msg) {
        return false;
    }
    @Override
    public boolean handle(double d) {
        mProject.setVariable("constructionStart",(float) d);
        return true;
    }
}
