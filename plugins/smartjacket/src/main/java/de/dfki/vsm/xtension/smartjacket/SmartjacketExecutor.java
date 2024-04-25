package de.dfki.vsm.xtension.smartjacket;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIEventSender;
import de.dfki.vsm.xtenstion.sockets.VSMSocketHandler;
import de.dfki.vsm.xtension.sockets.WebSocketClient;

import java.io.DataOutputStream;

public class SmartjacketExecutor extends ActivityExecutor implements VSMSocketHandler {

    WebSocketClient socket;

    private String uri="ws://127.0.0.1:7867/jacket";
    private DataOutputStream outStream;
private SSIEventSender ssiEventSender;

    public SmartjacketExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        ssiEventSender = new SSIEventSender("localhost",4101,"127.0.0.1",4102);
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
            case "logVibration":
                ssiEventSender.sendString("vibration@jacket");
        }

    }

    @Override
    public void launch() {
        socket = new WebSocketClient(this, uri);
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
