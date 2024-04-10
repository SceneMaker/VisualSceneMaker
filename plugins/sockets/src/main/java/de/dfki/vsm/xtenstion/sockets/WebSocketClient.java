package de.dfki.vsm.xtenstion.sockets;

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

    public WebSocketClient(VSMSocketHandler executor,String uri) {
        super(executor,uri);
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
    void connect() {
        WebSocketContainer container = ContainerProvider.getWebSocketContainer();
        try {
            Session session = container.connectToServer(WebSocketClient.class, URI.create(uri));
            onOpen(session);
        } catch (Exception e) {
            mLogger.failure(e.toString());
        }
    }
}