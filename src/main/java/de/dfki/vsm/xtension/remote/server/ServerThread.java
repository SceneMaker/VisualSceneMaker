package de.dfki.vsm.xtension.remote.server;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.xtension.remote.server.receiver.Receiver;
import de.dfki.vsm.xtension.remote.server.socketserver.Servable;
import de.dfki.vsm.xtension.remote.server.socketserver.tcpip.TCPIPServerController;
import de.dfki.vsm.xtension.remote.server.socketserver.udp.UDPServer;

import java.io.IOException;

/**
 * Created by alvaro on 6/14/17.
 */
public class ServerThread extends Thread{
    private Servable serverController = null;
    public ServerThread(PluginConfig config, Receiver receiver, int port){
        serverController = createServer(config, receiver, port);
    }

    public void run(){
        serverController.startServer();
    }

    public void closeConnection(){
        try {
            serverController.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static Servable createServer(PluginConfig config, Receiver receiver, int port){
        String serverType = config.getProperty("connection_type", "tcp/ip");
        if(serverType.equals("udp")){
            return new UDPServer(receiver, port);
        }
        return new TCPIPServerController(receiver, port);
    }


}
