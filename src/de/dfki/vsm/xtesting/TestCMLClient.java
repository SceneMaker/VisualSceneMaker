package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.io.DataInputStream;
import java.io.DataOutputStream;

import java.net.Socket;

import java.util.Arrays;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class TestCMLClient {

    // The System Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Message Pattern
    // The Message Pattern
    private static final Pattern mPattern =
        Pattern.compile(
            "<\\?xml version='1\\.0'\\?>\\n<cai_response>\\n<cai_command id='(.*?)'>RenderXML</cai_command>\\n\\t<status>SUCCESS</status>\\n</cai_response>");

    // The Reader Scanner
    private static Scanner sScanner;

    // The Client Socket
    private static Socket sSocket;

    public static void main(String args[]) {

        // Print Some Information
        sLogger.message("Host: " + args[1]);
        sLogger.message("Port: " + args[2]);
        sLogger.message("Name: " + args[3]);
        sLogger.message("Text: " + args[4]);

        final String example =
            "<?xml version='1.0'?>\n<cai_response>\n<cai_command id='N35TestScene'>RenderXML</cai_command>\n\t<status>SUCCESS</status>\n</cai_response>";

        System.err.println(example);

        // Define Ack Pattern
        final Matcher matcher = mPattern.matcher(example);

        // Find Some Matches
        if (matcher.matches()) {

            // Compute The Action Parameters
            final String action = matcher.group(0);
            final String uident = matcher.group(1);

            System.err.println(action);
            System.err.println(uident);
        }

        try {

            // Initialize The Scanner
            // sScanner = new Scanner(new FileInputStream(args[0]), "UTF-8");
            // Initialize The Socket
            sSocket = new Socket(args[1], Integer.parseInt(args[2]));

            // Print Some Information
            sLogger.message("Creating CMLClient Connector");
        } catch (Exception exc) {

            // Print Some Information
            sLogger.warning("Catching CMLClient Connector");

            // Debug Some Information
            sLogger.warning(exc.toString());
        }

        if ((sSocket != null) &&!sSocket.isClosed()) {

            // Print Some Information
            sLogger.message("Starting CMLClient Connector");

            try {

                // Establish IO Channels
                final DataInputStream  reader = new DataInputStream(sSocket.getInputStream());
                final DataOutputStream writer = new DataOutputStream(sSocket.getOutputStream());

                // Print Some Information
                sLogger.message("Executing CMLClient Connector");

                // Handle The Connection
                final String name    = args[3];
                final String text    = args[4];
                boolean      done    = false;
                int          counter = 0;

                while (!done) {

                    /*
                     * // Increment Message ID Counter
                     * counter++;
                     * // Construct Command Message
                     * final String line = ""
                     * + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                     * + "<cai_request version=\"1.0\">"
                     * + "<cai_command id=\"" + counter + "\" aid=\"" + name + "\">"
                     * + "RenderXML"
                     * + "<animation_track>"
                     * + "<speak_text>" + text + "</speak_text>"
                     * + "</animation_track>"
                     * + "</cai_command>"
                     * + "</cai_request>";
                     * // Convert To Binary Format
                     * final byte[] array = line.getBytes("UTF-8");
                     * // Write The CAI Header
                     * writer.write(BINUtilities.IntToBytesLE(100));
                     * writer.write(BINUtilities.IntToBytesLE(0));
                     * writer.write(BINUtilities.IntToBytesLE(array.length));
                     * // Write The CAI Command
                     * writer.write(array);
                     * // Print Some Information
                     * sLogger.message("CMLClient Connector Sending: '" + line + "' With Length '" + array.length + "'");
                     */

                    // Await CMLClient Notification
                    final byte[] header = new byte[12];

                    reader.readFully(header);
                    sLogger.message("CMLClient Connector Gets Header '" + BINUtilities.BytesToHexString(header) + "'");

                    //
                    final byte[] msgtag = Arrays.copyOfRange(header, 0, 4);

                    sLogger.message("CMLClient Connector Gets MsgTag '" + BINUtilities.BytesToHexString(msgtag) + "'");

                    final byte[] status = Arrays.copyOfRange(header, 4, 8);

                    sLogger.message("CMLClient Connector Gets Status '" + BINUtilities.BytesToHexString(status) + "'");

                    final byte[] length = Arrays.copyOfRange(header, 8, 12);

                    sLogger.message("CMLClient Connector Gets Length '" + BINUtilities.BytesToHexString(length) + "'");

                    //
                    final int size = BINUtilities.BytesLEToInt(length);

                    sLogger.message("CMLClient Connector Awaiting '" + size + "' Bytes");

                    //
                    final byte[] ackn = new byte[size];

                    reader.readFully(ackn);

                    //
                    final String data = new String(ackn, "UTF-8");

                    sLogger.message("CMLClient Connector Receiving '" + data + "'");

                    //

                    /*
                     *
                     * // Print Some Information
                     * sLogger.message("CMLClient Connector Receiving Message Tag '" + BINUtilities.BytesLEToInt(msgtag) + "'");
                     * sLogger.message("CMLClient Connector Receiving Status Value '" + BINUtilities.BytesLEToInt(status) + "'");
                     * sLogger.message("CMLClient Connector Receiving Buffer Size '" + BINUtilities.BytesLEToInt(length) + "'");
                     */

                    // done = true;
                }
            } catch (Exception exc) {

                // Print Some Information
                sLogger.warning("Catching CMLClient Connector");

                // Debug Some Information
                sLogger.warning(exc.toString());
            }

            // Close The Socket Connection
            if ((sSocket != null) &&!sSocket.isClosed()) {
                try {
                    sSocket.close();
                } catch (Exception exc) {

                    // Print Some Information
                    sLogger.warning("Catching CMLClient Connector");

                    // Debug Some Information
                    sLogger.warning(exc.toString());
                }
            }

            // Print Some Information
            sLogger.message("Stopping CMLClient Connector");
        }
    }
}
