package de.dfki.vsm.util.request;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.server.Connection;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 */
public class Crowd {

    protected final HashMap<String, Client> mRequests;
    protected final HashSet<Connection> mConnections;
    protected int mMaxConnections;
    protected final ThreadGroup mThreadGroup;
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    //
    private static Crowd sSingeltonInstance = null;

    public static synchronized Crowd getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new Crowd(10);
        }
        return sSingeltonInstance;
    }

    private Crowd(int maxConnections) {
        mThreadGroup = new ThreadGroup(Request.class.getName());
        mMaxConnections = maxConnections;
        mRequests = new HashMap<String, Client>();
        mConnections = new HashSet(maxConnections);
    }

    public synchronized void addRequest(Request request, String host, int port) throws IOException {
        String key = host + ":" + Integer.toString(port); // the hashtable key
        // Check whether a service is already on that port
        if (mRequests.get(key) != null) {
            throw new IllegalArgumentException("Client " + key + " already in use.");
        }

        Client client = null;
        if (request.getProtocol() == Request.Protocol.TCP) {
            client = new TCPClient(mThreadGroup, host, port, request, this);
        } else if (request.getProtocol() == Request.Protocol.UDP) {
            client = new UDPClient(mThreadGroup, host, port, request, this);
        } else {
            // Error
        }
        // Store it in the hashtable
        mRequests.put(key, client);
        // Log it
        mLogger.message("Starting request " + request.getClass().getName() + " to " + key);
        // Start the listener running.
        client.start();
    }

    public synchronized void removeRequest(String host, int port) {
        String key = host + ":" + Integer.toString(port); // the hashtable key
        // Look up the Listener object for the port in the hashtable
        final Client client = mRequests.get(key);
        if (client == null) {
            return;
        }
        // Ask the listener to stop
        client.pleaseStop();
        // Remove it from the hashtable
        mRequests.remove(key);
        // And log it.
        mLogger.message("Stopping request " + client.getClass().getName() + " to " + key);
    }

//    protected synchronized void addConnection(Socket s, Service service) {
//        // If the connection limit has been reached
//        if (mConnections.size() >= mMaxConnections) {
//            try {
//                // Then tell the client it is being rejected.
//                PrintWriter out = new PrintWriter(s.getOutputStream());
//                out.print("Connection refused; " + "the server is busy; please try again later.\n");
//                out.flush();
//                // And close the connection to the rejected client.
//                s.close();
//                // And log it, of course
//                mLogger.message("Connection refused to " + s.getInetAddress().getHostAddress() + ":" + s.getPort() + ": max connections reached.");
//            } catch (IOException e) {
//                mLogger.message(e.getMessage());
//            }
//        } else { // Otherwise, if the limit has not been reached
//            // Create a Connection thread to handle this connection
//            Connection c = new Connection(s, service, this);
//            // Add it to the list of current connections
//            mConnections.add(c);
//            // Log this new connection
//            mLogger.message("Connected to " + s.getInetAddress().getHostAddress() + ":" + s.getPort() + " on port " + s.getLocalPort() + " for service " + service.getClass().getName());
//            // And start the Connection thread to provide the service
//            c.start();
//        }
//    }
//    protected synchronized void endConnection(Connection c) {
//        mConnections.remove(c);
//        mLogger.message("Connection to " + c.getClientSocket().getInetAddress().getHostAddress() + ":" + c.getClientSocket().getPort() + " closed.");
//    }
    public synchronized void setMaxConnections(int max) {
        mMaxConnections = max;
    }

//    public synchronized void displayStatus(PrintWriter out) {
//        // Display a list of all Services that are being provided
//        Iterator keys = mServices.keySet().iterator();
//        while (keys.hasNext()) {
//            Integer port = (Integer) keys.next();
//            TCPListener listener = (TCPListener) mServices.get(port);
//            out.print("SERVICE " + listener.getService().getClass().getName() + " ON PORT " + port + "\n");
//        }
//
//        // Display the current connection limit
//        out.print("MAX CONNECTIONS: " + mMaxConnections + "\n");
//
//        // Display a list of all current connections
//        Iterator conns = mConnections.iterator();
//        while (conns.hasNext()) {
//            Connection c = (Connection) conns.next();
//            out.print("CONNECTED TO " +
//                    c.getClientSocket().getInetAddress().getHostAddress() + ":" +
//                    c.getClientSocket().getPort() + " ON PORT " +
//                    c.getClientSocket().getLocalPort() + " FOR SERVICE " +
//                    c.getService().getClass().getName() + "\n");
//        }
//    }
    public synchronized void info() {
        String infoString
                = "\r\n___________________________________________________________________________________________________________________\r\n"
                + "                                                                                                                   \r\n"
                + "                                  SceneMaker Request Crowd                                                        \r\n"
                + "___________________________________________________________________________________________________________________\r\n"
                + "                                                                                                                   \r\n";
        for (Entry<String, Client> entry : mRequests.entrySet()) {
            infoString += "Request " + entry.getValue().getRequest().getClass().getName() + " is connected on " + entry.getKey() + "\r\n";
        }
        infoString += "Maximal number of connections allowed :" + mMaxConnections + "\r\n";

//        for (Connection conn : mConnections) {
//            infoString += "Connected to " +
//                    conn.getClientSocket().getInetAddress().getHostAddress() + ":" +
//                    conn.getClientSocket().getPort() + " on port " +
//                    conn.getClientSocket().getLocalPort() + " for service " +
//                    conn.getService().getClass().getName() + "\r\n";
//        }
        infoString += "___________________________________________________________________________________________________________________\r\n";
        mLogger.message(infoString);
    }
}
