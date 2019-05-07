package de.dfki.vsm.xtension.remote.server.socketserver;

import java.io.IOException;

/**
 * Created by alvaro on 6/14/17.
 */
public interface Servable {
    void close() throws IOException;
    void startServer();

    void startIncomingThread() throws IOException;
}
