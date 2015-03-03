package de.dfki.vsm.api.hcm;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.ByteArrayInputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.ArrayList;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
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
    protected HCMScenePlayer mPlayer;
    // The System Logger
    protected LOGDefaultLogger mVSM3Log
            = LOGDefaultLogger.getInstance();
    
    
    private static final Pattern sSSI_PROLOG_PATTERN = Pattern.compile("\\[\\w+.*\\w+\\]");
    private static final Pattern sSSI_XML_PATTERN = Pattern.compile("\\<\\.+\\>");

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SSIEventHandler(final HCMScenePlayer player) {
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
                mVSM3Log.message("Connecting Event Handler");
            }
            // Print Debug Information
            mVSM3Log.message("Creating Event Handler");
        } catch (Exception exc) {
            // Debug Some Information
            mVSM3Log.warning("Could not create Event Handler: "+ exc.toString());
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
                    mVSM3Log.message("Aborting Event Handler");
                }
            } catch (Exception exc) {
                // Debug Some Information
                mVSM3Log.warning(exc.toString());
            }
        }
        // Debug Some Information
        mVSM3Log.message("Aborting Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void run() {
        // Debug Some Information
        mVSM3Log.message("Starting Event Handler");
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
                mVSM3Log.message(received);
                // Handle The Message
                handle(received);
            } catch (Exception exc) {
                // Debug Some Information
                mVSM3Log.warning(exc.toString());
            }

        }

        // Debug Some Information
        mVSM3Log.message("Stopping Event Handler");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    protected void handle(final String message) throws Exception {
        //first check: is it an XML string?
        
        Matcher xmlMatcher = sSSI_XML_PATTERN.matcher(message);
        if ( xmlMatcher.matches() )
        {
        // Translate The SSI Event Into
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
                // Get The Original Content
                final NodeList event_list = root.getElementsByTagName("event");
                for (int j = 0; j < event_list.getLength(); j++) {
                    final Element event = ((Element) event_list.item(j));

                    // Compute The Attributes --------------------------------------
                    String text = event.getTextContent().toLowerCase();
                    String sent = event.getAttribute("sender").toLowerCase();
                    String mode = event.getAttribute("event").toLowerCase();
                    String dist = event.getAttribute("from").toLowerCase();
                    String life = event.getAttribute("dur").toLowerCase();
                    String conf = String.format(
                            Locale.US, "%.2f", Double.valueOf(
                                    event.getAttribute("prob").toLowerCase()));

                    // The Sensor Specific Data ------------------------------------
                    String data = null;

                    // Check Different Event Types
                    // a) is the data given in Prolog format?
                    if (text != null) {
                        mVSM3Log.message("trying to parse PROLOG event...");
                        Matcher matcher = sSSI_PROLOG_PATTERN.matcher(text);
                        if (matcher.matches()) {
                            data = text;
                        }
                    }

                    // b) is the data given im XML format?
                    if (data == null) {
                        mVSM3Log.message("trying to parse XML event...");
                        Matcher matcher = sSSI_XML_PATTERN.matcher(text);
                        if (matcher.matches()) {
                            data = parseXMLEventData(event);
                        }
                    }

                    // c) is the data given as a plain string?
                    if (data == null) {
                        mVSM3Log.message("interpreting data as plain string event...");

                        //TODO: remove illegal characters?
                        data = "'" + event + "'";
                    }

                    // register the event in the fact base
                    if (data != null) {
                        registerPrologEvent(sent, mode, dist, life, conf, data);
                    } else {
                        mVSM3Log.failure("Can't parse SSI event data:\n\t" + text);
                    }

                }
            }

        } else {
            //handle plain string event
            // Compute The Attributes --------------------------------------
            String sent = "unknown";
            String mode = "unknown";
            String dist = "0";
            String life = "0";
            String conf = String.format(Locale.US, "%.2f", 1.0);

            String data;
            //check: is it a prolog string?
            Matcher prologMatch = sSSI_PROLOG_PATTERN.matcher(message);
            if (prologMatch.matches()) {
                data = message;
            } else {
                data = "'" + message + "'"; //TODO: check for illegal characters?
            }
            registerPrologEvent(sent, mode, dist, life, conf, data);
        }
    }
    
    //==========================================================================
    // event parsing
    //==========================================================================
    
    
    public String parseXMLEventData(Element event)
    {
        NodeList childNodes = event.getChildNodes();
        int count = childNodes.getLength();
        
        if(count == 0)
        {
            mVSM3Log.warning("no data node found in SSI event");
        }
        
        //parse the content
        ArrayList<String> entries = new ArrayList<>();
        for(int i=0; i<count; i++)
        {
            Node node = childNodes.item(i);
            if(node.getNodeType() == Node.ELEMENT_NODE)
            {
                Element element = (Element)node;
                String prologStr = convertToProlog(element);
                entries.add(prologStr);
            }
        }
        
        if(entries.isEmpty()) return null;
        else if(entries.size()==1) return entries.get(0);
        else{
            //assemble list
            StringBuilder result = new StringBuilder("[");
            result.append(entries.get(0));
            for(int i=1; i<entries.size(); i++)
            {
                result.append(", ");
                result.append(entries.get(i));
            }    
            result.append("]");
            
            return result.toString();
        }
        
    }
    
    public String convertToProlog(Element element)
    {
        StringBuilder result = new StringBuilder();
        //convert the attributes -----------------------------------------------
        NamedNodeMap attributes = element.getAttributes();
        int numAttr = attributes.getLength();
        
        //first item
        if(numAttr>0)
        {   
            Node attr = attributes.item(0);
            result.append(attr.getNodeName());
            result.append(":");
            //TODO: check if value contains whitespace -> surround with '
            result.append(attr.getNodeValue());
        
            //remaining items
            for(int i=1; i<numAttr; i++)
            {
                attr = attributes.item(i);
                result.append(", ");
                result.append(attr.getNodeName());
                result.append(":");
                //TODO: check if value contains whitespace -> surround with '
                result.append(attr.getNodeValue());    
            }
        }
        
        //result now contains one of the following:
        //a) nothing: ""
        //b) a attribute: "attrName:attrValue"
        //c) a list of attributes: "attr1:value1, attr2:value2, attr3:value3"
        
        //convert the children -------------------------------------------------
        NodeList childNodes = element.getChildNodes();
        int childNodeCount = childNodes.getLength();
        
        ArrayList<String> children = new ArrayList<>();
        for(int i=0; i<childNodeCount; i++)
        {
            Node node = childNodes.item(i);
            if(node.getNodeType() == Node.ELEMENT_NODE)
            {
                Element child = (Element)node;
                String prologStr = convertToProlog(child);
                children.add(prologStr);
            }
        }
        
        //add converted children
        int numChildren = children.size();
        if(numChildren>0)
        {
            //if result is not empty, add separator
            if (result.length()>0)
                result.append(", ");
            
            //append first child
            result.append(children.get(0));
            
            //append all remaining children
            for(int i=1; i<numChildren; i++)
            {
                result.append(", ");
                result.append(children.get(i));
            }
        }
        
        //result now contains 
        //a) nothing
        //b) one attribute or child
        //c) a list of attributes and/or children
        
        if(result.length()==0)
            return element.getNodeName();
        else
        {
            result.append("]");
            return element.getNodeName()+":["+result.toString();
        }
    }
    
    
    /**
     * Forwards the given event to the prolog fact base.
     * 
     * @param sent the sender of the event
     * @param mode the modality of the event
     * @param dist the difference between the event's time stamp and
     *      the actual start time
     * @param life the duration of the event
     * @param conf the confidence value of the event
     * @param data the data content of the event 
     */
    public void registerPrologEvent(String sent, String mode, String dist,
            String life, String conf, String data)
    {
        if (data == null || data.isEmpty())
            return;
        
        // Create The Typed Feature Structure
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
            mVSM3Log.message("received event: "+fact);
            // Assert The New Fact
            JPLEngine.query("jdd(" + fact + ").");
        } catch (Exception exc) {
            exc.printStackTrace();
            // Debug Some Information
            mVSM3Log.warning(exc.toString());
        }
    }
    
}
