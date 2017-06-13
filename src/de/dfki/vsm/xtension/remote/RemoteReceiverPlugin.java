package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remote.server.factories.ParserFactory;
import de.dfki.vsm.xtension.remote.server.factories.VariableSetterParser;
import de.dfki.vsm.xtension.remote.server.receiver.DataReceiver;
import de.dfki.vsm.xtension.remote.server.socketserver.ServerController;


/**
 * Created by alvaro on 5/11/17.
 */
public class RemoteReceiverPlugin extends RunTimePlugin {

    private final String variableName;
    private final int port;
    private ServerController serverController;
    private DataReceiver receiver;

    public RemoteReceiverPlugin(PluginConfig config, RunTimeProject project) {
        super(config, project);
        this.variableName = config.getProperty("variableName");
        this.port = Integer.parseInt(config.getProperty("port"));
    }

    @Override
    public void launch() {
        ParserFactory variableSetterFactory = new VariableSetterParser(mProject, variableName);
        receiver = new DataReceiver(variableSetterFactory);
        initServer();
    }
    private void initServer() {
        Thread thread = new Thread(() -> serverController = new ServerController(receiver, port));
        thread.start();
    }

    @Override
    public void unload() {

    }
}
