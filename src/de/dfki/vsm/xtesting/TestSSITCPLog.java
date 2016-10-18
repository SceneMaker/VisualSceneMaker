package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

//~--- JDK imports ------------------------------------------------------------

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import java.net.ServerSocket;
import java.net.Socket;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

/**
 *
 * @author Gregor Mehlmann
 */
public class TestSSITCPLog {

    // The System Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Server Socket
    private static ServerSocket sServer;

    // The Client Socket
    private static Socket sSocket;

    // Start SSI Logger
    public static void main(String args[]) {
        try {

            // Initialize The Server Socket
            sServer = new ServerSocket(Integer.parseInt(args[0]));

            // Print Some Information
            sLogger.message("Creating SSI Logger Listener");
        } catch (Exception exc) {

            // Print Some Information
            sLogger.warning("Catching SSI Logger Listener");

            // Debug Some Information
            sLogger.warning(exc.toString());
        }

        // Continue Accepting Connections
        while ((sServer != null) && sServer.isBound()) {
            try {

                // Print Some Information
                sLogger.message("Starting SSI Logger Listener");

                // Accept An Incoming Connection
                sSocket = sServer.accept();

                // Print Some Information
                sLogger.message("Accepting SSI Logger Connection");
            } catch (Exception exc) {

                // Print Some Information
                sLogger.warning("Catching SSI Logger Listener");

                // Debug Some Information
                sLogger.warning(exc.toString());
            }

            if ((sSocket != null) &&!sSocket.isClosed()) {

                // Print Some Information
                sLogger.message("Starting SSI Logger Connection");

                try {

                    // Establish IO Channels
                    final BufferedReader reader = new BufferedReader(new InputStreamReader(sSocket.getInputStream(),
                                                      "UTF-8"));
                    final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(sSocket.getOutputStream(),
                                                      "UTF-8"));

                    // Print Some Information
                    sLogger.message("Executing SSI Logger Connection");

                    // Handle The Connection
                    boolean done = false;

                    while (!done) {

                        // Read A New Line From Connection
                        final String line = reader.readLine();

                        // Check The Content Of The Line
                        if (line != null) {

                            // Print Some Information
                            sLogger.message("SSI Logger Connection Receiving '" + line + "'");

                            // Translate The SSI Speech Recognition Result Into
                            // An Adequate Document Object Model Representation.
                            final ByteArrayInputStream   stream   = new ByteArrayInputStream(line.getBytes("UTF-8"));
                            final DocumentBuilderFactory factory  = DocumentBuilderFactory.newInstance();
                            final DocumentBuilder        builder  = factory.newDocumentBuilder();
                            final Document               document = builder.parse(stream);
                            final Element                root     = document.getDocumentElement();

                            // Compute The SSI Event Structures
                            if (root.getTagName().equals("events")) {

                                // Get The Original Utterance
                                final NodeList events = root.getElementsByTagName("event");

                                for (int j = 0; j < events.getLength(); j++) {
                                    final Element event = ((Element) events.item(j));

                                    // Compute The Attributes
                                    String content = event.getTextContent();
                                    String sender  = event.getAttribute("sender");
                                    String mode    = event.getAttribute("event");
                                    String type    = event.getAttribute("type");
                                    String state   = event.getAttribute("state");
                                    float  from    = Float.parseFloat(event.getAttribute("from"));
                                    float  dur     = Float.parseFloat(event.getAttribute("dur"));
                                    float  prob    = Float.parseFloat(event.getAttribute("prob"));
                                    int    glue    = Integer.parseInt(event.getAttribute("glue"));

                                    // The Sensor Specific Data
                                    // Print Some Information
                                    sLogger.message("SSI Logger Receiving Content '" + content + "'");
                                    sLogger.message("SSI Logger Receiving Sender '" + sender + "'");
                                    sLogger.message("SSI Logger Receiving Event  '" + mode + "'");
                                    sLogger.message("SSI Logger Receiving Type '" + type + "'");
                                    sLogger.message("SSI Logger Receiving State  '" + state + "'");
                                    sLogger.message("SSI Logger Receiving From '" + from + "'");
                                    sLogger.message("SSI Logger Receiving Dur '" + dur + "'");
                                    sLogger.message("SSI Logger Receiving Prob '" + prob + "'");
                                    sLogger.message("SSI Logger Receiving Glue '" + glue + "'");
                                }
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
