package de.dfki.vsm.xtenstion.sockets;

import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

import java.net.URISyntaxException;

public class SocketIOClient {

    public static void main(String[] args) {
        try {
            // Connect to the Socket.IO server
            Socket socket = IO.socket("http://your-socket-io-server-url");

            // Set up event listeners
            socket.on(Socket.EVENT_CONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Connected to Socket.IO server");
                }
            });

            socket.on(Socket.EVENT_DISCONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    System.out.println("Disconnected from Socket.IO server");
                }
            });

            socket.on("your-event-name", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    // Handle incoming data
                    String data = (String) args[0];
                    System.out.println("Received data: " + data);
                }
            });

            // Connect to the server
            socket.connect();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }
}