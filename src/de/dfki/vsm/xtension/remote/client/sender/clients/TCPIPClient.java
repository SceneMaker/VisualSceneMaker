package de.dfki.vsm.xtension.remote.client.sender.clients;

import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.DataOutputStream;
import java.io.IOException;
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
        if(!isConnected()){
            System.out.println("Client not connected to server.. Not sending");
            return;
        }
        String dataToSend = dataCreator.buildDataToSent();
        os.writeBytes(dataToSend);


    }

    @Override
    public boolean isConnected() {
        return client != null && client.isConnected();
    }
}
