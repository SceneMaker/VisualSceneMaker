package de.dfki.vsm.xtension.remote.client.sender.clients;

import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.IOException;

/**
 * Created by alvaro on 5/2/17.
 */
public class UDPClient implements Clientable{

    private final int port;

    public UDPClient(int port){
        this.port = port;
    }

    @Override
    public void setDataCreator(DataSendable dataCreator) {

    }

    @Override
    public void connect() throws IOException {

    }

    @Override
    public void send() throws IOException {

    }

    @Override
    public boolean isConnected() {
        return false;
    }
}
