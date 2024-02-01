package de.dfki.vsm.xtension.sockets;

import java.io.*;
import java.net.Socket;
import java.nio.charset.StandardCharsets;


/**
 * @author Manuel Anglet
 */
public class SocketClient extends AbsJavaSocket {

    private Socket socket;

    public SocketClient(VSMSocketHandler executor, int port) {
        super(executor, port);
    }

    public SocketClient(VSMSocketHandler executor, String host, int port) {
        super(executor, host, port);
    }


    public void connect() {
        try {
            socket = new Socket(host, port);

            outWriter = new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8);
            outWriter.flush();
            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8);
            BufferedReader inReader = new BufferedReader(inputStream);
        } catch (final IOException exc) {
            logger.failure(exc.toString());
        }
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

    public void run() {

        while (!done) {
            while (socket == null || !socket.isConnected()) {
                connect();
            }
            try {
                String message = inReader.readLine();
                if (message != null && !message.equals("")) {
                    executor.handle(message);
                }
            } catch (IOException e) {
                logger.failure(e.toString());
            }

        }
    }

}
