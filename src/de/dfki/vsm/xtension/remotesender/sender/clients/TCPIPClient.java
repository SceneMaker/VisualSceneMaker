package de.dfki.vsm.xtension.remotesender.sender.clients;

import de.dfki.vsm.xtension.remotesender.sender.Clientable;
import de.dfki.vsm.xtension.remotesender.sender.DataSendable;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

/**
 * Created by alvaro on 5/2/17.
 */
public class TCPIPClient implements Clientable {
    private final String host;
    private final int port;
    private Socket client;
    private DataOutputStream os;
    private DataSendable dataCreator;

    public TCPIPClient(String host, int port, DataSendable dataCreator){
        this.host = host;
        this.port  = port;
        this.dataCreator = dataCreator;
    }

    public TCPIPClient(String host, int port){
        this.host = host;
        this.port  = port;

    }

    @Override
    public void setDataCreator(DataSendable dataCreator){
        this.dataCreator = dataCreator;
    }


    @Override
    public void connect() throws IOException {
        client = new Socket(host, port);
        os = new DataOutputStream(client.getOutputStream());
    }

    @Override
    public void send() throws IOException {
        String dataToSend = dataCreator.buildDataToSent();
        os.writeBytes(dataToSend);
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return client.getInputStream();
    }

    @Override
    public boolean isConnected() {
        if(client == null)
            return false;
        return client.isConnected();
    }
}
