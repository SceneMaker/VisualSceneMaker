package de.dfki.vsm.util.server;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.service.Service;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map.Entry;

public class Server {

    protected final HashMap<Integer, Listener> mServices;
    protected final HashSet<Connection> mConnections;
    protected int mMaxConnections;
    protected final ThreadGroup mThreadGroup;
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    //
    private static Server sSingeltonInstance = null;

    public static synchronized Server getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new Server(10);
        }
        return sSingeltonInstance;
    }

    private Server(int maxConnections) {
        mThreadGroup = new ThreadGroup(Server.class.getName());
        mMaxConnections = maxConnections;
        mServices = new HashMap<Integer, Listener>();
        mConnections = new HashSet(maxConnections);
    }

    public synchronized void addService(Service service, int port) throws IOException {
        Integer key = new Integer(port); // the hashtable key
        // Check whether a service is already on that port
        if (mServices.get(key) != null) {
            throw new IllegalArgumentException("Port " + port + " already in use.");
        }

        Listener listener = null;
        if (service.getProtocol() == Service.Protocol.TCP) {
            listener = new TCPListener(mThreadGroup, port, service, this);
        } else if (service.getProtocol() == Service.Protocol.UDP) {
            listener = new UDPReceiver(mThreadGroup, port, service, this);
        } else {
            // Error
        }
        // Store it in the hashtable
        mServices.put(key, listener);
        // Log it
        mLogger.message("Starting service " + service.getClass().getName() + " on port " + port);
        // Start the listener running.
        listener.start();
    }

    public synchronized void removeService(int port) {
        Integer key = new Integer(port);
        // Look up the Listener object for the port in the hashtable
        final Listener listener = mServices.get(key);
        if (listener == null) {
            return;
        }
        // Ask the listener to stop
        listener.pleaseStop();
        // Remove it from the hashtable
        mServices.remove(key);
        // And log it.
        mLogger.message("Stopping service " + listener.getService().getClass().getName() + " on port " + port);
    }

    protected synchronized void addConnection(Socket s, Service service) {
        // If the connection limit has been reached
        if (mConnections.size() >= mMaxConnections) {
            try {
                // Then tell the client it is being rejected.
                PrintWriter out = new PrintWriter(s.getOutputStream());
                out.print("Connection refused; " + "the server is busy; please try again later.\n");
                out.flush();
                // And close the connection to the rejected client.
                s.close();
                // And log it, of course
                mLogger.message("Connection refused to " + s.getInetAddress().getHostAddress() + ":" + s.getPort() + ": max connections reached.");
            } catch (IOException e) {
                mLogger.message(e.getMessage());
            }
        } else { // Otherwise, if the limit has not been reached
            // Create a Connection thread to handle this connection
            Connection c = new Connection(s, service, this);
            // Add it to the list of current connections
            mConnections.add(c);
            // Log this new connection
            mLogger.message("Connected to " + s.getInetAddress().getHostAddress() + ":" + s.getPort() + " on port " + s.getLocalPort() + " for service " + service.getClass().getName());
            // And start the Connection thread to provide the service
            c.start();
        }
    }

    protected synchronized void endConnection(Connection c) {
        mConnections.remove(c);
        mLogger.message("Connection to " + c.getClientSocket().getInetAddress().getHostAddress() + ":" + c.getClientSocket().getPort() + " closed.");
    }

    public synchronized void setMaxConnections(int max) {
        mMaxConnections = max;
    }

    public synchronized void displayStatus(PrintWriter out) {
        // Display a list of all Services that are being provided
        Iterator keys = mServices.keySet().iterator();
        while (keys.hasNext()) {
            Integer port = (Integer) keys.next();
            TCPListener listener = (TCPListener) mServices.get(port);
            out.print("SERVICE " + listener.getService().getClass().getName() + " ON PORT " + port + "\n");
        }

        // Display the current connection limit
        out.print("MAX CONNECTIONS: " + mMaxConnections + "\n");

        // Display a list of all current connections
        Iterator conns = mConnections.iterator();
        while (conns.hasNext()) {
            Connection c = (Connection) conns.next();
            out.print("CONNECTED TO "
                    + c.getClientSocket().getInetAddress().getHostAddress() + ":"
                    + c.getClientSocket().getPort() + " ON PORT "
                    + c.getClientSocket().getLocalPort() + " FOR SERVICE "
                    + c.getService().getClass().getName() + "\n");
        }
    }

    public synchronized void info() {
        String infoString
                = "\r\n___________________________________________________________________________________________________________________\r\n"
                + "                                                                                                                   \r\n"
                + "                                  SceneMaker Service Server                                                        \r\n"
                + "___________________________________________________________________________________________________________________\r\n"
                + "                                                                                                                   \r\n";
        for (Entry<Integer, Listener> entry : mServices.entrySet()) {
            infoString += "Service " + entry.getValue().getService().getClass().getName() + " is listening on port " + entry.getKey() + "\r\n";
        }
        infoString += "Maximal number of connections allowed :" + mMaxConnections + "\r\n";

        for (Connection conn : mConnections) {
            infoString += "Connected to "
                    + conn.getClientSocket().getInetAddress().getHostAddress() + ":"
                    + conn.getClientSocket().getPort() + " on port "
                    + conn.getClientSocket().getLocalPort() + " for service "
                    + conn.getService().getClass().getName() + "\r\n";
        }
        infoString += "___________________________________________________________________________________________________________________\r\n";
        mLogger.message(infoString);
    }
}
