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
abstract class AbsJavaSocket extends Thread {

    // The logger instance
    final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    // The executor instance
    final SocketHandler executor;
    final String host;

    int port;

    // The socket streams
    OutputStreamWriter outWriter;
    BufferedReader inReader;
    // The termination flag
    boolean done = false;


    // Create the client thread
    public AbsJavaSocket(SocketHandler executor, int port) {
        super();
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = "localhost";
    }

    public AbsJavaSocket(SocketHandler executor, String host, int port) {
        super();
        // Initialize the socket
        this.executor = executor;
        this.port = port;
        this.host = host;
    }

    // Start the client thread
    @Override
    public void start() {
        super.start();
    }

    // Abort the client thread
    abstract public  void abort() ;

    abstract public  void connect() ;

    // Send some message
    public boolean send(final String message) {
        try {
            outWriter.write(message);
            return true;
        } catch (IOException e) {
            logger.failure(e.toString());
            return false;
        }
    }



}
