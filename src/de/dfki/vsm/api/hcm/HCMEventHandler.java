package de.dfki.vsm.api.hcm;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

//~--- JDK imports ------------------------------------------------------------

import java.io.ByteArrayInputStream;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import java.util.Locale;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

/**
 * @author Not me
 */
public class HCMEventHandler extends Thread {

    // The Own Thread Termination Flag
    private volatile boolean mDone = false;

    // The System Logger Instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Datagram Socket For Events
    private DatagramSocket mSocket;

    // The Local Socket Handler Address
    private SocketAddress mLAddr;

    // The Remote Socket Handler Address
    private SocketAddress mRAddr;

    // The Current Scene Player Instance
    private final HCMScenePlayer mPlayer;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public HCMEventHandler(final HCMScenePlayer player) {

        // Initialize The Scene Player
        mPlayer = player;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void init(final String lhost, final int lport, final String rhost, final int rport,
                           final boolean rconn) {

        // Print Debug Information
        mLogger.message("Constructing HCM Event Handler");

        try {

            // Create The Addresses
            mLAddr = new InetSocketAddress(lhost, lport);

            // Create The UDP Socket
            mSocket = new DatagramSocket(mLAddr);

            // Debug Some Information
            mLogger.message("Binding HCM Event Handler At '" + lhost + ":" + lport + "'");

            // Connect The UDP Socket
            if (rconn) {

                // Create The Addresses
                mRAddr = new InetSocketAddress(rhost, rport);

                // Connect The UDP Socket
                mSocket.connect(mRAddr);

                // Debug Some Information
                mLogger.message("Connecting HCM Event Handler To '" + rhost + ":" + rport + "'");
            }
        } catch (Exception exc) {

            // Debug Some Information
            mLogger.warning(exc.toString());

            // Debug Some Information
            mLogger.warning("HCM Event Handler Has Bad State");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void abort() {

        // Print Debug Information
        mLogger.message("Interrupting HCM Event Handler");

        // Set Termination Flag
        mDone = true;

        // Interrupt Thread State
        interrupt();

        // Close The Datagram Socket
        if (mSocket != null) {
            try {

                // Close The Datagram Socket
                if (!mSocket.isClosed()) {

                    // Close The Socket Now
                    mSocket.close();

                    // Debug Some Information
                    mLogger.message("Closing HCM Event Handler");
                } else {

                    // Debug Some Information
                    mLogger.warning("HCM Event Handler Is Closed");
                }
            } catch (Exception exc) {

                // Debug Some Information
                mLogger.warning(exc.toString());

                // Debug Some Information
                mLogger.warning("HCM Event Handler Has Bad State");
            }
        } else {

            // Debug Some Information
            mLogger.warning("HCM Event Handler Is Null");
        }

        // Debug Some Information
        mLogger.message("Aborting HCM Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void run() {

        // Debug Some Information
        mLogger.message("Starting HCM Event Handler");

        //
        while ((mSocket != null) &&!mSocket.isClosed()) {
            try {

                // Create The Datagram Packet
                final byte[]         buffer  = new byte[4096];
                final DatagramPacket mPacket = new DatagramPacket(buffer, buffer.length);

                // Receive The Data Packet
                mSocket.receive(mPacket);

                // Get The String Data
                final String message = new String(mPacket.getData(), 0, mPacket.getLength(), "UTF-8");

                // Debug Some Information
                mLogger.message("HCM Event Handler Receiving Message:");

                // Debug Some Information
                mLogger.message(message);

                // Handle The Message
                handle(message);
            } catch (Exception exc) {

                // Debug Some Information
                mLogger.warning(exc.toString());

                // Debug Some Information
                mLogger.warning("HCM Event Handler Has Bad State");
            }
        }

        // Debug Some Information
        mLogger.message("Stopping HCM Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void handle(final String message) throws Exception {

        // Debug Some Information
        mLogger.message("HCM Event Handler Input");

        // Parse And Handle The HCM Events
        final ByteArrayInputStream   stream   = new ByteArrayInputStream(message.getBytes("UTF-8"));
        final DocumentBuilderFactory factory  = DocumentBuilderFactory.newInstance();
        final DocumentBuilder        builder  = factory.newDocumentBuilder();
        final Document               document = builder.parse(stream);

        // Get The HCM Events Object
        final Element events = document.getDocumentElement();

        // Check If We Have HCM Events
        if (events.getTagName().equals("events")) {

            // Debug Some Information
            mLogger.message("HCM Event Handler Events");

            // Get The List Of Events
            final NodeList eventList = events.getElementsByTagName("event");

            //
            if (eventList.getLength() > 0) {

                // Process Each HCM Event
                for (int i = 0; i < eventList.getLength(); i++) {

                    // Get The Current HCM Event
                    final Element event = ((Element) eventList.item(i));

                    // Get The Event Attributes
                    final String sent = event.getAttribute("sender").toLowerCase();
                    final String mode = event.getAttribute("event").toLowerCase();
                    final String stat = event.getAttribute("state").toLowerCase();
                    final String dist = event.getAttribute("from").toLowerCase();
                    final String life = event.getAttribute("dur").toLowerCase();
                    final String conf = String.format(Locale.US, "%.6f",
                                                      Double.valueOf(event.getAttribute("prob").toLowerCase()));
                    final String type = event.getAttribute("type");
                    final String glue = event.getAttribute("glue");

                    // Parse The Data Structure
                    String data = "[]";

                    // Paryse The Text Content
                    if (type.equals("EMPTY")) {

                        // Do Nothing
                    } else if (type.equals("STRING")) {
                        data = event.getTextContent();
                    } else if (type.equals("NTUPLE")) {
                        data = "[";

                        // Get The List Of Tuples
                        final NodeList tupleList = events.getElementsByTagName("tuple");

                        for (int j = 0; j < tupleList.getLength(); j++) {

                            // Get The Tuple Element
                            final Element tuple = ((Element) tupleList.item(j));

                            // Get The Attributes
                            final String string = tuple.getAttribute("string").toLowerCase();
                            final String value  = String.format(Locale.US, "%.6f",
                                                      Double.valueOf(tuple.getAttribute("value").toLowerCase()));

                            // Write The Feature
                            data += string + ":" + value;

                            // Write Next Feature
                            if (j < tupleList.getLength() - 1) {
                                data += ",\r\n";
                            }
                        }

                        data += "]";
                    }

                    // Create Typed Feature Structure
                    try {

                        // Get The Current System Time
                        final long time = mPlayer.getCurrentTime();

                        // Create The New Prolog Fact
                        final String fact = "[" + "\r\n" + "  " + "type:" + "event" + "," + "\r\n" + "  " + "sent:"
                                            + "ssiv2" + "," + "\r\n" + "  " + "recv:" + "vsmv3" + "," + "\r\n" + "  "
                                            + "name:" + sent + "," + "\r\n" + "  " + "mode:" + mode + "," + "\r\n"
                                            + "  " + "dist:" + dist + "," + "\r\n" + "  " + "life:" + life + ","
                                            + "\r\n" + "  " + "time:" + time + "," + "\r\n" + "  " + "conf:" + conf
                                            + "," + "\r\n" + "  " + "stat:" + stat + "," + "\r\n" + "  " + "glue:"
                                            + glue + "," + "\r\n" + "  " + "data:\r\n" + data + "\r\n" + "]";

                        // Debug Some Information
                        mLogger.message("Adding Feature Structure To Prolog Fact Base: \r\n" + fact);

                        // Assert The New Fact
                        JPLEngine.query("jdd(" + fact + ").");
                    } catch (Exception exc) {
                        exc.printStackTrace();

                        // Debug Some Information
                        mLogger.warning(exc.toString());
                    }
                }
            } else {

                // Debug Some Information
                mLogger.warning("Empty HCM Event List");
            }
        } else {

            // Debug Some Information
            mLogger.warning("Unknown HCM Input Format");
        }
    }
}
