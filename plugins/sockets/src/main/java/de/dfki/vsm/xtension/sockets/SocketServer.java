package de.dfki.vsm.xtension.sockets;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;


/**
 * @author Manuel Anglet
 */
public class SocketServer extends AbsJavaSocket {

    private ServerSocket server;
    private Socket client;

    public SocketServer(SocketHandler executor, int port) {
        super(executor, port);
    }

    public SocketServer(SocketHandler executor, String host, int port) {
        super(executor, host, port);
    }


    // Start the client thread

    public void connect() {
        try {
            client = server.accept();
            outWriter = new OutputStreamWriter(client.getOutputStream(), StandardCharsets.UTF_8);
            outWriter.flush();
            InputStreamReader inputStream = new InputStreamReader(client.getInputStream(), StandardCharsets.UTF_8);
            inReader = new BufferedReader(inputStream);
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }
    }

    // Abort the client thread
    public final void abort() {
        done = true;
        try {
            if (client != null && !client.isClosed()) {
                client.close();
            }
            if (server != null && !server.isClosed()) {
                server.close();
            }
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }

        server = null;
        client = null;

        // Interrupt if sleeping
        interrupt();
    }

    @Override
    public final void run() {
        try {
            server = new ServerSocket(port);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        while (!done) {
            while (client == null || !client.isConnected()) {
                connect();
            }
            final String message;
            try {
                message = inReader.readLine();
                if (message != null) {
                    executor.handle(message);
                }
            } catch (IOException e) {
                logger.failure(e.toString());
            }

        }
    }
}
