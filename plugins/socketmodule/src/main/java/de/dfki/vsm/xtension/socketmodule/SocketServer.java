package main.java.de.dfki.vsm.xtension.socketmodule;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
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
    // The client socket
    private Socket socket;
    // The socket streams
    private OutputStreamWriter outStream;
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

        while (socket == null) {
            try {
                socket = new Socket(host, port);
            } catch (final IOException exc) {
                logger.failure(exc.toString());
            }

            logger.message("Wait a bit ...");
            if(socket == null){
                try {
                    // Wait a bit ...
                    Thread.sleep(1000);
                } catch (final InterruptedException exc) {
                    logger.failure(exc.toString());
                }
            }
        }

        try {
            outStream = new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8);
            outStream.flush();
            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream(),StandardCharsets.UTF_8);
            BufferedReader inReader = new BufferedReader(inputStream)
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }

        super.start();
    }

    // Abort the client thread
    public final void abort() {
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
        // Interrupt if sleeping
        interrupt();
    }

    // Receive some message
    public final String recv() throws IOException {
        return inStream.read()
    }

    // Send some message
    public final boolean send(final String string) {
        Boolean sended = false;
        int tries = 0;
        try {
            outStream.write(string);
            return true;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        while(!sended && tries<20){
            tries ++;
            wait(500)
            try {
                outStream.write(string);
                sended=true;
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return sended;
    }


    @Override
    public final void run() {
        while (!done) {
            final String message = inReader.readLine();
            if (message != null) {
                executor.handle(message);
            }
        }
    }
}
