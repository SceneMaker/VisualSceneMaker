package de.dfki.vsm.xtension.remote.client.sender.clients;

import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.IOException;

/**
 * Created by alvaro on 5/2/17.
 */
public class DummyClient implements Clientable {
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
