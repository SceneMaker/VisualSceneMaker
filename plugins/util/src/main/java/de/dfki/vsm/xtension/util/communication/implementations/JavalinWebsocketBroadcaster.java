package de.dfki.vsm.xtension.util.communication.implementations;

import de.dfki.vsm.xtension.util.communication.ReceiveSenderPort;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class JavalinWebsocketBroadcaster<J, T> implements ReceiveSenderPort<T, J> {
    private final Javalin javalin;
    private final List<Consumer<J>> handlers = new ArrayList<>();
    private final List<WsContext> websockets = new ArrayList<>();
    private final List<Runnable> disconnectHandlers = new ArrayList<>();

    public JavalinWebsocketBroadcaster(int port, String certificatePath, String path) {
        this.javalin = createServer(port, port, certificatePath);

        javalin.ws(path, ws -> {
            ws.onConnect(this::addWs);
            ws.onMessage(ctx -> {
                J message = (J) ctx.message();
                handlers.forEach(consumer -> consumer.accept(message));
            });
            ws.onClose(this::removeWs);
        });
    }

    @Override
    public void sendMessage(T message) {
        for (WsContext ws : websockets) {
            ws.send(message);
        }
    }

    @Override
    public void registerMessageHandler(Consumer<J> messageHandler) {
        this.handlers.add(messageHandler);
    }

    @Override
    public void registerDisconnectHandler(Runnable disconnectHandler) {
        this.disconnectHandlers.add(disconnectHandler);
    }

    @Override
    public boolean canSend() {
        return !websockets.isEmpty();
    }

    @Override
    public void stop() {
        javalin.stop();
        websockets.clear();
    }

    @Override
    public void start() {
        javalin.start();
    }

    private Javalin createServer(int wss_port, int ws_port, String certificatePath) {
        if (certificatePath != null) {
            return Javalin.create(config -> {
                config.server(() -> {
                    Server server = new Server();
                    ServerConnector sslConnector = new ServerConnector(server, getSslContextFactory(certificatePath));
                    sslConnector.setPort(wss_port);
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    server.setConnectors(new Connector[]{sslConnector, connector});
                    return server;
                });
            });
        } else {
            return Javalin.create(config -> config.enforceSsl = true).start(ws_port);
        }
    }

    private SslContextFactory getSslContextFactory(String certPath) {
        SslContextFactory sslContextFactory = new SslContextFactory.Server();
        sslContextFactory.setKeyStorePath(this.getClass().getResource(certPath).toExternalForm()); //default "/my-release-key.keystore"
        sslContextFactory.setKeyStorePassword("123456");
        return sslContextFactory;
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

}
