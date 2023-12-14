package de.dfki.vsm.xtension.socketmodule;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.*;
import java.net.Socket;
import java.nio.charset.StandardCharsets;


/**
 * @author Manuel Anglet
 */
public class SocketClient extends Thread {

    // The logger instance
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    // The executor instance
    private final SocketHandler executor;
    private final String host;

    int port;
    // The client socket
    private Socket socket;
    // The socket streams
    private OutputStreamWriter outWriter;
    private BufferedReader inReader;
    // The termination flag
    private boolean done = false;


    // Create the client thread
    public SocketClient(SocketHandler executor, int port) {
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = "localhost";
    }

    public SocketClient(SocketHandler executor, String host, int port) {
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = host;
    }
    // Start the client thread
    @Override
    public void start() {

        while (socket == null) {
            try {
                socket = new Socket(host, port);
            } catch (final IOException exc) {
                logger.failure(exc.toString());
            }

            logger.message("Wait a bit ...");
            if(socket == null){
                try {
                    Thread.sleep(1000);
                } catch (final InterruptedException exc) {
                    logger.failure(exc.toString());
                }
            }
        }

        try {
            outWriter = new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8);
            outWriter.flush();
            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream(),StandardCharsets.UTF_8);
            BufferedReader inReader = new BufferedReader(inputStream);
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }

        super.start();
    }

    // Abort the client thread
    public void abort() {
        done = true;
        if (socket != null) {
            if (!socket.isClosed()) {
                try {
                    socket.close();
                } catch (final IOException exc) {
                    logger.failure(exc.toString());
                }
            }
            socket = null;
        }
        interrupt();
    }


    // Send some message
    public boolean send(final String string) {
        try {
            outWriter.write(string);
            return true;
        } catch (IOException e) {
            logger.failure(e.toString());
            return false;
        }
    }


    @Override
    public void run() {
        while (!done) {
            try {
                String message = inReader.readLine();
                if (message!=null && !message.equals("")) {
                    executor.handle(message);
                }
            } catch (IOException e) {
                logger.failure(e.toString());
            }

        }
    }
}
