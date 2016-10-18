package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import java.net.Socket;

import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class TestSM3Client {

    // The System Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Message Pattern
    private static Pattern sPattern =
        Pattern.compile(
            "<action name=\"(.*?)\" type=\"(.*?)\" task=\"(.*?)\" date=\"(.*?)\" time=\"(.*?)\">(.*?)</action>");

    // The Reader Scanner
    private static Scanner sScanner;

    // The Client Socket
    private static Socket sSocket;

    public static void main(String args[]) {
        try {

            // Initialize The Scanner
            sScanner = new Scanner(new FileInputStream(args[0]), "UTF-8");

            // Initialize The Socket
            sSocket = new Socket(args[1], Integer.parseInt(args[2]));

            // Print Some Information
            sLogger.message("Creating H3DClient Connector");
        } catch (Exception exc) {

            // Print Some Information
            sLogger.warning("Catching H3DClient Connector");

            // Debug Some Information
            sLogger.warning(exc.toString());
        }

        if ((sSocket != null) &&!sSocket.isClosed()) {

            // Print Some Information
            sLogger.message("Starting H3DClient Connector");

            try {

                // Establish IO Channels
                final BufferedReader reader = new BufferedReader(new InputStreamReader(sSocket.getInputStream(),
                                                  "UTF-8"));
                final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(sSocket.getOutputStream(),
                                                  "UTF-8"));

                // Print Some Information
                sLogger.message("Executing H3DClient Connector");

                // Handle The Connection
                boolean done = false;

                while (!done) {

                    // Read A Line From Scanner
                    final String line = sScanner.nextLine();

                    // Check The Line From Scanner
                    if (line != null) {
                        if (!line.startsWith("#")) {

                            // Send The Line To H3D Proxy
                            writer.write(line);
                            writer.newLine();
                            writer.flush();

                            // Print Some Information
                            sLogger.message("H3DClient Connector Sending: '" + line + "'");

                            // Await H3DProxy Notification
                            final String ackn = reader.readLine();

                            // Check The Content Of The Line
                            if (ackn != null) {

                                // Print Some Information
                                sLogger.message("H3DClient Connector Receiving '" + ackn + "'");

                                // Check The Message Format
                                final Matcher matcher = sPattern.matcher(ackn);

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
                                    sLogger.message("H3DClient Connector Accepting '" + action + "'");

                                    /*
                                     * // Sleep For Some Time Interval
                                     * try {
                                     * Thread.sleep(2500);
                                     * } catch (Exception exc) {
                                     * // Print Some Information
                                     * sLogger.warning("Catching H3DClient Connector");
                                     * // Debug Some Information
                                     * sLogger.warning(exc.toString());
                                     * }
                                     */
                                } else {

                                    // Print Some Information
                                    sLogger.failure("H3DClient Connector Rejecting '" + ackn + "'");
                                }
                            } else {

                                // Print Some Debug Information
                                sLogger.warning("Aborting H3DClient Connector");

                                // Set The Termination Flag
                                done = true;
                            }
                        }
                    } else {

                        // Print Some Debug Information
                        sLogger.warning("Aborting H3DClient Connector");

                        // Set The Termination Flag
                        done = true;
                    }
                }
            } catch (Exception exc) {

                // Print Some Information
                sLogger.warning("Catching H3DClient Connector");

                // Debug Some Information
                sLogger.warning(exc.toString());
            }

            // Close The Socket Connection
            if ((sSocket != null) &&!sSocket.isClosed()) {
                try {
                    sSocket.close();
                } catch (Exception exc) {

                    // Print Some Information
                    sLogger.warning("Catching H3DClient Connector");

                    // Debug Some Information
                    sLogger.warning(exc.toString());
                }
            }

            // Print Some Information
            sLogger.message("Stopping H3DClient Connector");
        }
    }
}
