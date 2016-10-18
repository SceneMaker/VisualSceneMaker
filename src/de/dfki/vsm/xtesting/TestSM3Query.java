package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.io.FileInputStream;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import java.util.Scanner;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class TestSM3Query {

    // The System Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Message Pattern
    private static Pattern sPattern = Pattern.compile("<query type=\"(.*?)\">(.*?)</query>");

    // The UDP Datagram Socket
    private static DatagramSocket sSocket;

    // The Local  Handler Address
    private static SocketAddress sLAddr;

    // The Remote  Handler Address
    private static SocketAddress sRAddr;

    // The Reader Scanner
    private static Scanner sScanner;

    public static void main(String args[]) {
        try {

            // Initialize The Scanner
            sScanner = new Scanner(new FileInputStream(args[0]), "UTF-8");

            // Initialize The Adresses
            sLAddr = new InetSocketAddress(args[1], Integer.parseInt(args[2]));
            sRAddr = new InetSocketAddress(args[3], Integer.parseInt(args[4]));

            // Bind The Local Datagram Socket
            sSocket = new DatagramSocket(sLAddr);

            // Debug Some Information
            sLogger.message("Binding SM3Query Connector To '" + sLAddr + "'");

            // Try Bind The Remote UDP Socket
            if (sRAddr != null) {
                sSocket.connect(sRAddr);

                // Debug Some Information
                sLogger.message("Connecting SM3Query Connector To '" + sRAddr + "'");
            }

            // Print Some Information
            sLogger.message("Creating SM3Query Connector");
        } catch (Exception exc) {

            // Print Some Information
            sLogger.warning("Catching SM3Query Connector");

            // Debug Some Information
            sLogger.warning(exc.toString());
        }

        if ((sSocket != null) &&!sSocket.isClosed()) {

            // Print Some Information
            sLogger.message("Starting SM3Query Connector");

            try {

                // Print Some Information
                sLogger.message("Executing SM3Query Connector");

                // Handle The Connection
                boolean done = false;

                while (!done) {

                    // Read A Line From Scanner
                    final String line = sScanner.nextLine();

                    // Check The Line From Scanner
                    if (line != null) {
                        if (!line.startsWith("#")) {

                            // Create The Datagram Packet
                            final DatagramPacket packet = new DatagramPacket(line.getBytes(), line.length());

                            // Process The Datagram Packet
                            sSocket.send(packet);

                            // Print Some Information
                            sLogger.message("SM3Query Connector Sending: '" + line + "'");

                            // Process The Datagram Packet
                            sSocket.receive(packet);

                            //
                            final String ackn = new String(packet.getData(), 0, packet.getLength());

                            // Print Some Information
                            sLogger.message("SM3Query Connector Receiving: '" + ackn + "'");
                        }
                    } else {

                        // Print Some Debug Information
                        sLogger.warning("Aborting SM3Query Connector");

                        // Set The Termination Flag
                        done = true;
                    }
                }
            } catch (Exception exc) {

                // Print Some Information
                sLogger.warning("Catching SM3Query Connector");

                // Debug Some Information
                sLogger.warning(exc.toString());
            }

            // Print Some Information
            sLogger.message("Stopping SM3Query Connector");
        }
    }
}
