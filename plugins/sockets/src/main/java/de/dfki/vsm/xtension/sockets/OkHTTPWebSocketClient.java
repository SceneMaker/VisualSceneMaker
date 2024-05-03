package de.dfki.vsm.xtension.sockets;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.WebSocket;
import okhttp3.WebSocketListener;
import okio.ByteString;


public class OkHTTPWebSocketClient extends AbsJavaSocket {

    private String uri;
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    private WebSocket webSocket;


    public OkHTTPWebSocketClient(VSMSocketHandler executor, String uri) {
        super(executor, uri);
        this.uri = uri;
    }

    public void sendMessage(String message) {
        if (webSocket != null) {
            webSocket.send(message);
        }
    }

    @Override
    public void connect() {
        OkHttpClient client = new OkHttpClient();

        Request request = new Request.Builder().url(uri).build();
        WebSocketListener listener = new WebSocketListener() {
            @Override
            public void onOpen(WebSocket webSocket, Response response) {
                super.onOpen(webSocket, response);
                // WebSocket connection opened
                OkHTTPWebSocketClient.this.webSocket = webSocket;
            }

            @Override
            public void onMessage(WebSocket webSocket, String text) {
                super.onMessage(webSocket, text);
                executor.handle(text);
                // Received text message
            }

            @Override
            public void onMessage(WebSocket webSocket, ByteString bytes) {
                super.onMessage(webSocket, bytes);
                // Received binary message
            }

            @Override
            public void onClosed(WebSocket webSocket, int code, String reason) {
                super.onClosed(webSocket, code, reason);
                // WebSocket connection closed
            }

            @Override
            public void onFailure(WebSocket webSocket, Throwable t, Response response) {
                super.onFailure(webSocket, t, response);
                // WebSocket connection
            }
        };

        webSocket = client.newWebSocket(request, listener);
    }

    @Override
    public void start() {
        connect();
        super.start();
    }

    @Override
    public void abort() {
        webSocket.close(1000, "Normal closure");
    }

}