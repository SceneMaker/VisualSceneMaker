package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import java.net.ServerSocket;
import java.net.Socket;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class TestSM3Proxy {

    // The System Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Message Pattern
    private static Pattern sPattern =
        Pattern.compile(
            "<action name=\"(.*?)\" type=\"(.*?)\" task=\"(.*?)\" date=\"(.*?)\" time=\"(.*?)\">(.*?)</action>");

    // The Server Socket
    private static ServerSocket sServer;

    // The Client Socket
    private static Socket sSocket;

    // Start H3D Proxy
    public static void main(String args[]) {
        try {

            // Initialize The Server Socket
            sServer = new ServerSocket(Integer.parseInt(args[0]));

            // Print Some Information
            sLogger.message("Creating H3DProxy Listener");
        } catch (Exception exc) {

            // Print Some Information
            sLogger.warning("Catching H3DProxy Listener");

            // Debug Some Information
            sLogger.warning(exc.toString());
        }

        // Continue Accepting Connections
        while ((sServer != null) && sServer.isBound()) {
            try {

                // Print Some Information
                sLogger.message("Starting H3DProxy Listener");

                // Accept An Incoming Connection
                sSocket = sServer.accept();

                // Print Some Information
                sLogger.message("Accepting H3DProxy Connection");
            } catch (Exception exc) {

                // Print Some Information
                sLogger.warning("Catching H3DProxy Listener");

                // Debug Some Information
                sLogger.warning(exc.toString());
            }

            if ((sSocket != null) &&!sSocket.isClosed()) {

                // Print Some Information
                sLogger.message("Starting H3DProxy Connection");

                try {

                    // Establish IO Channels
                    final BufferedReader reader = new BufferedReader(new InputStreamReader(sSocket.getInputStream(),
                                                      "UTF-8"));
                    final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(sSocket.getOutputStream(),
                                                      "UTF-8"));

                    // Print Some Information
                    sLogger.message("Executing H3DProxy Connection");

                    // Handle The Connection
                    boolean done = false;

                    while (!done) {

                        // Read A New Line From Connection
                        final String line = reader.readLine();

                        // Check The Content Of The Line
                        if (line != null) {

                            // Print Some Information
                            sLogger.message("H3DProxy Connection Receiving '" + line + "'");

                            // Check The Message Format
                            final Matcher matcher = sPattern.matcher(line);

                            if (matcher.matches()) {

                                // Compute The Action Parameters
                                final String action = matcher.group(0);
                                final String name   = matcher.group(1);
                                final String type   = matcher.group(2);
                                final String task   = matcher.group(3);
                                final String date   = matcher.group(4);
                                final String time   = matcher.group(5);
                                final String text   = matcher.group(6);

                                // Print Some Information
                                sLogger.message("H3DProxy Connection Accepting '" + action + "'");

                                // Sleep For Some Time Interval
                                try {
                                    Thread.sleep(2500);
                                } catch (Exception exc) {

                                    // Print Some Information
                                    sLogger.warning("Catching H3DProxy Connection");

                                    // Debug Some Information
                                    sLogger.warning(exc.toString());
                                }

                                // Send The Notification
                                writer.write(line);
                                writer.newLine();
                                writer.flush();

                                // Print Some Information
                                sLogger.message("H3DProxy Connection Sending: '" + line + "'");
                            } else {

                                // Print Some Information
                                sLogger.failure("H3DProxy Connection Rejecting '" + line + "'");
                            }
                        } else {

                            // Print Some Debug Information
                            sLogger.warning("Aborting H3DProxy Connection");

                            // Set The Termination Flag
                            done = true;
                        }
                    }
                } catch (Exception exc) {

                    // Print Some Information
                    sLogger.warning("Catching H3DProxy Connection");

                    // Debug Some Information
                    sLogger.warning(exc.toString());
                }

                // Print Some Information
                sLogger.message("Stopping H3DProxy Connection");
            }
        }

        // Print Some Information
        sLogger.message("Stopping H3DProxy Listener");
    }
}
