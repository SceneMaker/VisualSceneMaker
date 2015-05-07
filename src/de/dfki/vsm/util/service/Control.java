package de.dfki.vsm.util.service;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.server.Server;

//~--- JDK imports ------------------------------------------------------------

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;

import java.util.StringTokenizer;

/**
 * @author Gregor Mehlmann
 */

/**
 * This is a non-trivial service. It implements a command-based protocol that
 * gives password-protected runtime control over the operation of the server.
 * See the main() method of the Server class to see how this service is started.
 *
 * The recognized commands are: password: give password; authorization is
 * required for most commands add: dynamically add a named service on a
 * specified port remove: dynamically remove the service running on a specified
 * port max: change the current maximum connection limit. status: display
 * current services, connections, and connection limit help: display a help
 * message quit: disconnect
 *
 * This service displays a prompt, and sends all of its output to the user in
 * capital letters. Only one client is allowed to connect to this service at a
 * time.
 */
public class Control implements Service {
    boolean              mConnected = false;    // Whether a client is already connected
    private final Server mServer;               // The server we control
    private final String mPassword;             // The password we require

    /**
     * Create a new Control service. It will control the specified Server
     * object, and will require the specified password for authorization Note
     * that this Service does not have a no argument constructor, which means
     * that it cannot be dynamically instantiated and added as the other,
     * generic services above can be.
     */
    public Control(Server server, String password) {
        mServer   = server;
        mPassword = password;
    }

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * This is the serve method that provides the service. It reads a line the
     * client, and uses java.util.StringTokenizer to parse it into commands and
     * arguments. It does various things depending on the command.
     */
    public void serve(InputStream i, OutputStream o) throws IOException {

        // Setup the streams
        BufferedReader in  = new BufferedReader(new InputStreamReader(i));
        PrintWriter    out = new PrintWriter(o);
        String         line;    // For reading client input lines

        // Has the user has given the password yet?
        boolean authorized = false;

        // If there is already a client connected to this service, display
        // a message to this client and close the connection. We use a
        // synchronized block to prevent a race condition.
        synchronized (this) {
            if (mConnected) {
                out.print("ONLY ONE CONTROL CONNECTION ALLOWED.\n");
                out.close();

                return;
            } else {
                mConnected = true;
            }
        }

        // This is the main loop: read a command, parse it, and handle it
        for (;;) {                   // infinite loop
            out.print("> ");         // Display a prompt
            out.flush();             // Make it appear right away
            line = in.readLine();    // Get the user's input

            if (line == null) {
                break;               // Quit if we get EOF.
            }

            try {

                // Use a StringTokenizer to parse the user's command
                StringTokenizer t = new StringTokenizer(line);

                if (!t.hasMoreTokens()) {
                    continue;                               // if input was empty
                }                                           // Get first word of the input and convert to lower case

                String command = t.nextToken().toLowerCase();

                // Now compare to each of the possible commands, doing the
                // appropriate thing for each command
                if (command.equals("password")) {           // Password command
                    String p = t.nextToken();               // Get the next word

                    if (p.equals(mPassword)) {              // Is it the password?
                        out.print("OK\n");                  // Say so
                        authorized = true;                  // Grant authorization
                    } else {
                        out.print("INVALID PASSWORD\n");    // Otherwise fail
                    }
                } else if (command.equals("add")) {         // Add Service command

                    // Check whether password has been given
                    if (!authorized) {
                        out.print("PASSWORD REQUIRED\n");
                    } else {

                        // Get the name of the service and try to
                        // dynamically load and instantiate it.
                        // Exceptions will be handled below
                        String  serviceName  = t.nextToken();
                        Class   serviceClass = Class.forName(serviceName);
                        Service service;

                        try {
                            service = (Service) serviceClass.newInstance();
                        } catch (NoSuchMethodError e) {
                            throw new IllegalArgumentException("Service must have a " + "no-argument constructor");
                        }

                        int port = Integer.parseInt(t.nextToken());

                        // If no exceptions occurred, add the service
                        mServer.addService(service, port);
                        out.print("SERVICE ADDED\n");      // acknowledge
                    }
                } else if (command.equals("remove")) {     // Remove service
                    if (!authorized) {
                        out.print("PASSWORD REQUIRED\n");
                    } else {
                        int port = Integer.parseInt(t.nextToken());

                        mServer.removeService(port);       // remove the service
                        out.print("SERVICE REMOVED\n");    // acknowledge
                    }
                } else if (command.equals("max")) {        // Set connection limit
                    if (!authorized) {
                        out.print("PASSWORD REQUIRED\n");
                    } else {
                        int max = Integer.parseInt(t.nextToken());

                        mServer.setMaxConnections(max);
                        out.print("MAX CONNECTIONS CHANGED\n");
                    }
                } else if (command.equals("status")) {     // Status Display
                    if (!authorized) {
                        out.print("PASSWORD REQUIRED\n");
                    } else {
                        mServer.displayStatus(out);
                    }
                } else if (command.equals("help")) {       // Help command

                    // Display command syntax. Password not required
                    out.print("COMMANDS:\n" + "\tpassword <password>\n" + "\tadd <service> <port>\n"
                              + "\tremove <port>\n" + "\tmax <max-connections>\n" + "\tstatus\n" + "\thelp\n"
                              + "\tquit\n");
                } else if (command.equals("quit")) {
                    break;                                  // Quit command.
                } else {
                    out.print("UNRECOGNIZED COMMAND\n");    // Error
                }
            } catch (Exception e) {
                out.print("ERROR WHILE PARSING OR EXECUTING COMMAND:\n" + e + "\n");
            }
        }

        mConnected = false;
        out.close();
        in.close();
    }
}
