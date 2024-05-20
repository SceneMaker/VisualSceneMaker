package de.dfki.vsm.xtension.DriveSimulator;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

import java.net.URISyntaxException;

public class SocketIOClient {

    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    Socket socket;


    boolean connected;

    public SocketIOClient(String host, String port) {
        try {
            socket = IO.socket(host+":"+port);

            socket.on(Socket.EVENT_CONNECT, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    connected=true;
                    mLogger.message("Connected to the server");
                }
            });

        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }

    public void addListener(String event, Emitter.Listener listener){
        socket.on(event, listener);
    }

    public void connect() {
        socket = socket.connect();
    }

    public void abort(){
        socket.close();
    }

    public boolean isConnected() {
        return connected;
    }

}
