package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remote.server.ServerThread;
import de.dfki.vsm.xtension.remote.server.factories.ParserFactory;
import de.dfki.vsm.xtension.remote.server.factories.VariableSetterParser;
import de.dfki.vsm.xtension.remote.server.receiver.DataReceiver;
import de.dfki.vsm.xtension.remote.server.socketserver.Servable;
import de.dfki.vsm.xtension.remote.server.socketserver.tcpip.TCPIPServerController;
import de.dfki.vsm.xtension.remote.server.socketserver.udp.UDPServer;

import java.io.IOException;


/**
 * Created by alvaro on 5/11/17.
 */
public class RemoteReceiverPlugin extends RunTimePlugin {

    private final String variableName;
    private final int port;
    private DataReceiver receiver;
    private ServerThread serverThread;

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
        serverThread = new ServerThread(mConfig, receiver, port);
        serverThread.start();
    }

    @Override
    public void unload() {
        serverThread.closeConnection();
    }


}
