package de.dfki.vsm.util.server;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.service.Service;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.net.Socket;

/**
 * @author Gregor Mehlmann
 */

/**
 * This class is a subclass of Thread that handles an individual connection
 * between a client and a Service provided by this server. Because each such
 * connection has a thread of its own, each Service can have multiple
 * connections pending at once. Despite all the other threads in use, this is
 * the key feature that makes this a multi-threaded server implementation.
 */
public class Connection extends Thread {
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final Socket           mClientSocket;    // The socket to talk to the client through
    private final Service          mService;         // The service being provided to that client
    private final Server           mServer;

    /**
     * This constructor just saves some state and calls the superclass
     * constructor to create a thread to handle the connection. Connection
     * objects are created by Listener threads. These threads are part of the
     * server's ThreadGroup, so all Connection threads are part of that group,
     * too.
     */
    public Connection(Socket client, Service service, Server server) {
        super("Server.Connection:" + client.getInetAddress().getHostAddress() + ":" + client.getPort());
        mClientSocket = client;
        mService      = service;
        mServer       = server;
    }

    public Socket getClientSocket() {
        return mClientSocket;
    }

    public Service getService() {
        return mService;
    }

    /**
     * This is the body of each and every Connection thread. All it does is pass
     * the client input and output streams to the serve() method of the
     * specified Service object. That method is responsible for reading from and
     * writing to those streams to provide the actual service. Recall that the
     * Service object has been passed from the Server.addService() method to a
     * Listener object to the addConnection() method to this Connection object,
     * and is now finally being used to provide the service. Note that just
     * before this thread exits it always calls the endConnection() method to
     * remove itself from the set of connections
     */
    @Override
    public void run() {
        try {
            InputStream  in  = mClientSocket.getInputStream();
            OutputStream out = mClientSocket.getOutputStream();

            mService.serve(in, out);
        } catch (IOException e) {
            e.printStackTrace();
            mLogger.message(e.getMessage());
        } finally {
            mServer.endConnection(this);
        }
    }
}
