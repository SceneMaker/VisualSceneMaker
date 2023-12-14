package de.dfki.vsm.xtension.smartjacket;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.sockets.SocketClient;
import de.dfki.vsm.xtension.sockets.SocketHandler;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

public class SmartjacketExecutor extends ActivityExecutor implements SocketHandler {

    SocketClient socket;
    int port = 7867;
    private DataOutputStream outStream;


    public SmartjacketExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity || activity == null) {
            return;
        }
        switch (activity.getName()) {
            case "vibrate":
                socket.send("GO@@");
        }

    }

    @Override
    public void launch() {
        socket = new SocketClient(this, port);
        socket.start();
        mLogger.message("SmartJacket plugin ready");
    }

    @Override
    public void unload() {
        socket.abort();
    }

    @Override
    public boolean handle(String msg) {
        mLogger.message("received: " + msg);
        return true;
    }
}
