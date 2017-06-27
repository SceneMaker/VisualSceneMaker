package de.dfki.vsm.xtension.remote.server.socketserver;

import java.io.IOException;

/**
 * Created by alvaro on 6/14/17.
 */
public abstract class ServerLoop extends Thread {
    private boolean isRunning = true;

    public void run(){
        while(isRunning) {
            try {
                receive();
            } catch (IOException e) {
                e.printStackTrace();
                cleanupAfterError();
            }
        }
        cleanupAfterError();
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
    public  void closeConnection() throws IOException{
        isRunning = false;
        close();
    }

    protected abstract void close() throws IOException;


    protected abstract void cleanup();
}
