package de.dfki.vsm.xtension.smartjacket;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.IOException;
import java.net.Socket;

public class SmartjacketExecutor extends ActivityExecutor {

    Socket writeSocket;


    public SmartjacketExecutor(PluginConfig config, RunTimeProject project) {
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
        while (writeSocket == null) {
            try {
                // Create the socket
                writeSocket = new Socket("localhost", 4000);
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }

        }
    }


    @Override
    public void unload() {

    }
}
