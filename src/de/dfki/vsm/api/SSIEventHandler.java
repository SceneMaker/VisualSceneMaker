package de.dfki.vsm.api;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.ByteArrayInputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Locale;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public class SSIEventHandler extends Thread {

    // The Datagram Socket To Receive The SSI 
    // Speech Events And The According Buffer
    protected DatagramSocket mSocket;
    // The Local Socket Handler Address
    protected SocketAddress mLAddr;
    // The Remote Socket Handler Address
    protected SocketAddress mRAddr;
    // The Datagram Packet For Messages
    protected final byte[] mBuffer = new byte[2048];
    protected final DatagramPacket mPacket
            = new DatagramPacket(mBuffer, mBuffer.length);
    // The Scene Player
    protected VSMScenePlayer mPlayer;
    // The System Logger
    protected LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SSIEventHandler(final VSMScenePlayer player) {
        mPlayer = player;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void init(
            final String lhost,
            final int lport,
            final String rhost,
            final int rport,
            final boolean rconn) {

        try {
            // Create The Addresses
            mLAddr = new InetSocketAddress(lhost, lport);
            // Create The UDP Socket
            mSocket = new DatagramSocket(mLAddr);
            // Connect The UDP Socket
            if (rconn) {
                // Create The Addresses
                mRAddr = new InetSocketAddress(rhost, rport);
                // Connect The UDP Socket
                mSocket.connect(mRAddr);
                // Debug Some Information
                mLogger.message("Connecting Event Handler");
            }
            // Print Debug Information
            mLogger.message("Creating Event Handler");
        } catch (Exception exc) {
            // Debug Some Information
            mLogger.warning(exc.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void abort() {
        // Close The Datagram Socket
        if (mSocket != null) {
            try {
                // Close The Datagram Socket
                if (!mSocket.isClosed()) {
                    mSocket.close();
                    // Debug Some Information
                    mLogger.message("Aborting Event Handler");
                }
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }
        }
        // Debug Some Information
        mLogger.message("Aborting Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void run() {
        // Debug Some Information
        mLogger.message("Starting Event Handler");
        while (mSocket != null
                && !mSocket.isClosed()) {
            try {
                // Receive The Data Packet
                mSocket.receive(mPacket);
                // Get The String Data
                final String received = new String(
                        mPacket.getData(), 0,
                        mPacket.getLength(), "UTF-8");
                // Print Some Message
                mLogger.message(received);
                // Handle The Message
                handle(received);
            } catch (Exception exc) {
                // Debug Some Information
                mLogger.warning(exc.toString());
            }

        }

        // Debug Some Information
        mLogger.message("Stopping Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected void handle(final String message) throws Exception {
        // Translate The SSI Speech Recognition Result Into
        // An Adequate Document Object Model Representation.
        final ByteArrayInputStream stream
                = new ByteArrayInputStream(message.getBytes("UTF-8"));
        final DocumentBuilderFactory factory
                = DocumentBuilderFactory.newInstance();
        final DocumentBuilder builder = factory.newDocumentBuilder();
        final Document document = builder.parse(stream);
        ////////////////////////////////////////////////////////////////
        final Element root = document.getDocumentElement();
        // Compute The SSI Event Structures
        if (root.getTagName().equals("events")) {
            // Get The Original Utterance
            final NodeList event_list = root.getElementsByTagName("event");
            for (int j = 0; j < event_list.getLength(); j++) {
                final Element event = ((Element) event_list.item(j));
                // Compute The Attributes
                String text = event.getTextContent().toLowerCase();
                String sent = event.getAttribute("sender").toLowerCase();
                String mode = event.getAttribute("event").toLowerCase();
                String dist = event.getAttribute("from").toLowerCase();
                String life = event.getAttribute("dur").toLowerCase();
                String conf = String.format(
                        Locale.US, "%.2f", Double.valueOf(
                                event.getAttribute("prob").toLowerCase()));
                // The Sensor Specific Data
                String data = null;
                // Check Different Event Types
                if (sent.equals("audio") && mode.equals("speech")) {
                    // Parse The Input With Spin
                    if (text != null && !text.isEmpty()) {
                        // TODO: SPIN NLU Engine Integration
                        //data = mEngine.parseToTFS(text, "  ");
                    }
                } else if (sent.equals("msspeech") && mode.equals("spe ech")) {
                    if (text != null && !text.isEmpty()) {
                        data = text;
                    }
                    // Assert The New Fact
                    //JPLEngine.query("rll(sent,msspeech).");
                } else if (sent.equals("artkp") && mode.equals("mfixp")) {
                    mLogger.message("Receiving ARTK+ Gaze Tracking Event");
                    if (text != null && !text.isEmpty()) {
                        data = text;
                    }
                    // Assert The New Fact
                    //JPLEngine.query("jll(sent, artkp).");
                }
                // Create The Typed Feature Structure
                if (text != null && !text.isEmpty()) {
                    if (data != null && !data.isEmpty()) {
                        try {
                            // Get The System time
                            final long time = mPlayer.getCurrentTime();
                            // Create The New Fact
                            final String fact = "[" + "\r\n"
                                    + "  " + "type:" + "event" + "," + "\r\n"
                                    + "  " + "name:" + "ssiv2" + "," + "\r\n"
                                    + "  " + "sent:" + sent + "," + "\r\n"
                                    + "  " + "recv:" + "vsmv3" + "," + "\r\n"                  
                                    + "  " + "mode:" + mode + "," + "\r\n"
                                    + "  " + "dist:" + dist + "," + "\r\n"
                                    + "  " + "life:" + life + "," + "\r\n"
                                    + "  " + "time:" + time + "," + "\r\n"
                                    + "  " + "conf:" + conf + "," + "\r\n"
                                    + "  " + "data:\r\n" + data + "\r\n"
                                    + "]";
                            // Print Some Information
                            mLogger.message(fact);
                            // Assert The New Fact
                            JPLEngine.query("jdd(" + fact + ").");
                        } catch (Exception exc) {
                            exc.printStackTrace();
                            // Debug Some Information
                            mLogger.warning(exc.toString());
                        }
                    }
                }
            }
        }
    }
}
