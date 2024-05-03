package de.dfki.vsm.xtension.sockets;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import javax.websocket.*;
import java.io.IOException;
import java.net.URI;

@ClientEndpoint
public class WebSocketClient extends AbsJavaSocket {

    private Session session;
    private String uri;

    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();

    public WebSocketClient(VSMSocketHandler executor, String uri) {
        super(executor, uri);
        this.uri = uri;
    }

    @OnOpen
    public void onOpen(Session session) {
        mLogger.message("Connected to server");
        this.session = session;
    }

    @OnMessage
    public void onMessage(String message) {
        mLogger.message("Received message: " + message);
    }

    @OnClose
    public void onClose() {
        mLogger.message("Connection closed");
    }

    public boolean send(String message) {
        this.session.getAsyncRemote().sendText(message);
        mLogger.message("Sent message: " + message);
        return true;
    }


    @Override
    public void abort() {
        try {
            session.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void connect() {
        try {
            WebSocketContainer container = ContainerProvider.getWebSocketContainer();
            Session session = container.connectToServer(WebSocketClientEndpoint.class, URI.create(uri));
        } catch (Exception e) {
            mLogger.failure(e.toString());
        }
    }

    @Override
    public void start() {
        connect();
        super.start();
    }



    @ClientEndpoint
    public class WebSocketClientEndpoint {

        @OnOpen
        public void onOpen(Session session) {
            System.out.println("Connected to server.");
        }

        @OnMessage
        public void onMessage(String message) {
            System.out.println("Received message: " + message);
        }
    }
}