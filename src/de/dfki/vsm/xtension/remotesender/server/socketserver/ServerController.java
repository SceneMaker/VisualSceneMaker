package de.dfki.vsm.xtension.remotesender.server.socketserver;


import de.dfki.vsm.xtension.remotesender.server.receiver.Receiver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.LinkedList;

/**
 * Created by alvaro on 4/30/17.
 */
public class ServerController {
    public static final int SERVER_PORT = 8100;
    public static final int MAX_CONNECTED_CLIENTS = 4;
    private final boolean allowMultipleClients;
    private Receiver receiver;
    private ServerSocket server = null;
    private final LinkedList<Socket> clients = new LinkedList<>();

    public ServerController(Receiver receiver){
        this.allowMultipleClients = false;
        startServer(receiver);
    }

    public ServerController(Receiver receiver, boolean allowMultipleClients){
        startServer(receiver);
        this.allowMultipleClients = allowMultipleClients;
    }

    private void startServer(Receiver receiver) {
        this.receiver = receiver;
        listen();
        connect();
    }

    public void close() throws IOException {
        server.close();
    }

    private void listen(){
        try {
            server = new ServerSocket(SERVER_PORT);
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
