package de.dfki.vsm.xtension.SIAHomeConnection;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.eclipse.jetty.server.Server;

import java.io.IOException;
import java.net.*;


/**
 * @author Mina Ameli
 */
public class SIAHomeConnectionServerThread extends Thread {
    private SIAHomeConnectionJSONHandler handler;
    public Server server;
    private int port;

    SIAHomeConnectionServerThread(SIAHomeConnectionJSONHandler handler, int port) {
        this.handler = handler;
        this.port = port;
    }

    @Override
    public final void start() {
        server = new Server(this.port);
        server.setHandler(this.handler);
        super.start();
        try {
            server.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

}