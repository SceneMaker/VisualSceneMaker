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
public class SocketServer extends Thread {

    // The logger instance
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    // The executor instance
    private final SocketHandler executor;
    private final String host;

    int port;

    private ServerSocket server;
    private Socket client;
    // The socket streams
    private OutputStreamWriter outWriter;
    private BufferedReader inReader;
    // The termination flag
    private boolean done = false;


    // Create the client thread
    public SocketServer(SocketHandler executor, int port) {
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = "localhost";
    }

    public SocketServer(SocketHandler executor, String host, int port) {
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = host;
    }

    // Start the client thread
    @Override
    public void start() {
        try {
            server = new ServerSocket(port);
            client = server.accept();
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }
        try {
            outWriter = new OutputStreamWriter(client.getOutputStream(), StandardCharsets.UTF_8);
            outWriter.flush();
            InputStreamReader inputStream = new InputStreamReader(client.getInputStream(), StandardCharsets.UTF_8);
            inReader = new BufferedReader(inputStream);
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }
        super.start();
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

    // Receive some message
    public final String recv() throws IOException {
        return inReader.readLine();
    }

    // Send some message
    public final boolean send(final String string) {
        try {
            outWriter.write(string);
            return true;
        } catch (IOException e) {
            logger.failure(e.toString());
            return false;
        }

    }


    @Override
    public final void run() {
        while (!done) {
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
