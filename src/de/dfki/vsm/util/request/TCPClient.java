package de.dfki.vsm.util.request;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */
public class TCPClient extends Client {
    private final Socket mSocket;    // The socket to listen for connections

    public TCPClient(ThreadGroup group, String host, int port, Request request, Crowd crowd) throws IOException {
        super(group, host, port, request, crowd);
        mSocket = new Socket(host, port);
        mSocket.setSoTimeout(600000);
    }

    @Override
    public void pleaseStop() {
        mStop = true;
        interrupt();    // Stop blocking in accept()

        try {
            mSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
            mLogger.message(e.getMessage());
        }
    }

    @Override
    public void run() {
        while (!mStop) {
            try {
                InputStream  in  = mSocket.getInputStream();
                OutputStream out = mSocket.getOutputStream();

                mRequest.request(in, out);
            } catch (IOException e) {
                e.printStackTrace();
                mLogger.message(e.getMessage());
            }
        }
    }
}
