package de.dfki.vsm.xtension.remote.client.factories;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.clients.DummyClient;
import de.dfki.vsm.xtension.remote.client.sender.clients.TCPIPClient;
import de.dfki.vsm.xtension.remote.client.sender.clients.UDPClient;

/**
 * Created by alvaro on 5/2/17.
 */
public class ClientsFactory {
    private final PluginConfig config;
    private final String rHost;
    private final String type;
    private final int rPort;

    public ClientsFactory(PluginConfig mConfig) {
        this.config = mConfig;
        rHost = config.getProperty("rHost");
        type = config.getProperty("connection_type", "tcp/ip");
        String port = config.getProperty("rPort");
        if(port != null)
            rPort = Integer.parseInt(port);
        else
            rPort = 0;
    }

    public Clientable buildClient() {
        if(type.equals("tcp/ip")){
            return new TCPIPClient(rHost, rPort);
        }else if(type.equals("udp")){
            return new UDPClient(rHost, rPort);
        }

        return new DummyClient();
    }
}
