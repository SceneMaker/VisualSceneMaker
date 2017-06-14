package de.dfki.vsm.xtension.remote.server.socketserver;




import de.dfki.vsm.xtension.remote.server.receiver.Receiver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.LinkedList;

/**
 * Created by alvaro on 4/30/17.
 */
public class ServerController {
    public static final int MAX_CONNECTED_CLIENTS = 4;
    private final boolean allowMultipleClients;
    private Receiver receiver;
    private ServerSocket server = null;
    private final LinkedList<Socket> clients = new LinkedList<>();
    private int port;

    public ServerController(Receiver receiver, int port){
        this.allowMultipleClients = false;
        startServer(receiver, port);
    }

    public ServerController(Receiver receiver, boolean allowMultipleClients, int port){
        this.allowMultipleClients = allowMultipleClients;
        startServer(receiver, port);
    }

    private void startServer(Receiver receiver, int port) {
        this.receiver = receiver;
        this.port = port;
        listen();
        connect();
    }

    public void close() throws IOException {
        server.close();
        for (Socket client: clients ) {
            client.close();
        }
    }

    private void listen(){
        try {
            server = new ServerSocket(port);
            System.out.println("Listening on port: " + port);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void connect(){
        try {
            acceptConnection();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void acceptConnection() throws IOException {
        while (canAcceptMoreConnections()){
            Socket clientSocket = server.accept();
            System.out.println("Connection established");
            ServerThread serverThread = new ServerThread(clientSocket, receiver);
            clients.add(clientSocket);
            serverThread.start();
        }
    }

    private boolean canAcceptMoreConnections() {
        return (allowMultipleClients && MAX_CONNECTED_CLIENTS > clients.size()) || (noClientsConnected());
    }

    private boolean noClientsConnected() {
        return clients.size() == 0;
    }

}
