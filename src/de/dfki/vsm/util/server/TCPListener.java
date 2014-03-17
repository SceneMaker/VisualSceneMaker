package de.dfki.vsm.util.server;

import de.dfki.vsm.util.service.Service;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public class TCPListener extends Listener {

    private final ServerSocket mListenSocket; // The socket to listen for connections

    public TCPListener(ThreadGroup group, int port, Service service, Server server) throws IOException {
        super(group, port, service, server);
        mListenSocket = new ServerSocket(port);
        mListenSocket.setSoTimeout(600000);
    }

    public void pleaseStop() {
        mStop = true;
        interrupt(); // Stop blocking in accept()
        try {
            mListenSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
            mLogger.message(e.getMessage());
        }
    }

    @Override
    public void run() {
        while (!mStop) {
            try {
                Socket client = mListenSocket.accept();
                mServer.addConnection(client, mService);
            } catch (InterruptedIOException e) {
                //e.printStackTrace();
                mLogger.message("ServiceServer:" + e.getMessage());
            } catch (IOException e) {
                //e.printStackTrace();
                mLogger.message("ServiceServer:" + e.getMessage());
            }
        }
    }
}
