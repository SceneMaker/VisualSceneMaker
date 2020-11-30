package de.dfki.vsm.xtension.tricatworld;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

public class WordMapping extends Properties {
    public void load(String agent, RunTimeProject mProject) {
        try {
            String wmf = mProject.getProjectPath() + File.separator + mProject.getAgentConfig(agent).getProperty("wordmapping");
            wmf = wmf.replace("\\", "/");
            super.load(new FileReader(new File(wmf)));
        } catch (IOException ex) {
            LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
            mLogger.failure("Wordmapping file (" + mProject.getAgentConfig(agent).getProperty("wordmapping") + ") not found!");
        }
    }
}
