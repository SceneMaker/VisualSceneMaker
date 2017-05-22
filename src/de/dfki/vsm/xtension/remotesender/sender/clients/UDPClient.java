package de.dfki.vsm.xtension.remotesender.sender.clients;

import de.dfki.vsm.xtension.remotesender.sender.Clientable;
import de.dfki.vsm.xtension.remotesender.sender.DataSendable;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by alvaro on 5/2/17.
 */
public class UDPClient implements Clientable{
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
    public InputStream getInputStream() throws IOException {
        return null;
    }
}
