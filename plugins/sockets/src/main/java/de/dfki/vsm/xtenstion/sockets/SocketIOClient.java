package de.dfki.vsm.xtenstion.sockets;

import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

import java.net.URISyntaxException;

public class SocketIOClient extends AbsJavaSocket {
    Socket socket;

    public SocketIOClient(Emitter.Listener listener, int port) {
        super(executor, port);
    }

    public SocketIOClient(VSMSocketHandler executor, String host, int port) {
        super(executor, host, port);
    }

    @Override
    public void abort() {

    }

    @Override
    void connect() {
        try {
            // Connect to the Socket.IO server
            socket = IO.socket(host+":"+port);

            // Set up event listeners
            socket.on(Socket.EVENT_CONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    logger.message("Connected to Socket.IO server");
                }
            });

            socket.on(Socket.EVENT_DISCONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    logger.message("Disconnected from Socket.IO server");
                }
            });

            socket.on("your-event-name", new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    // Handle incoming data
                    String data = (String) args[0];
                    logger.message("Received data: " + data);
                }
            });

            // Connect to the server
            socket.connect();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }
}