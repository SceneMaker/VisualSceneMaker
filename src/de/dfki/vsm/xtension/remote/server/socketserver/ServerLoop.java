package de.dfki.vsm.xtension.remote.server.socketserver;

import java.io.IOException;

/**
 * Created by alvaro on 6/14/17.
 */
public abstract class ServerLoop extends Thread {
    public void run(){
        while(true) {
            try {
                receive();
            } catch (IOException e) {
                e.printStackTrace();
            }finally {
                cleanupAfterError();
            }
        }
    }

    private void cleanupAfterError()  {
        cleanup();
        try {
            closeConnection();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    protected abstract void receive() throws IOException;
    public abstract void closeConnection() throws IOException;


    protected abstract void cleanup();
}
