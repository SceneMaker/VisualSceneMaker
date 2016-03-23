package de.dfki.vsm.xtension.tricat;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.ByteArrayInputStream;
import java.util.Locale;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public final class SSIEventObject {

    // The singelton logger 
    private final static LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();

    // The creation time
    private final long mTime;
    // The SSI event data
    private String mSender;
    private String mDatatype;
    private String mGlueFlag;
    private String mContent;
    private String mStatus;
    private String mEvent;
    private String mDistance;
    private String mDuration;
    private String mConfidence;

    // Construct an SSI event object
    public SSIEventObject(
            final long time,
            final String text) {
        // Initialize the creation time
        mTime = time;
        // Parse the data from the text
        parse(text);
    }

    public final String getMode() {
        return mSender;
    }

    public final String getSend() {
        return mEvent;
    }

    public final String getStat() {
        return mStatus;
    }

    public final String getData() {
        return mContent;
    }

    // Get XML string representation
    public final String toXML() {
        return "<?xml version=\"1.0\"?>"
                + "<events ssi-v=\"V2\">"
                + "<event "
                + "sender=\"" + mSender + "\" "
                + "event=\"" + mEvent + "\" "
                + "from=\"" + mDistance + "\" "
                + "dur=\"" + mDuration + "\" "
                + "prob=\"" + mConfidence + "\" "
                + "type=\"" + mDatatype + "\" "
                + "state=\"" + mStatus + "\" "
                + "glue=\"" + mGlueFlag + "\""
                + "/>"
                + "</events>";
    }

    // Get Prolog fact representation
    public final String toFSR() {
        return "[" + "\r\n"
                + "  " + "type:" + "event" + "," + "\r\n"
                + "  " + "send:" + mEvent + "," + "\r\n"
                + "  " + "mode:" + mSender + "," + "\r\n"
                + "  " + "dist:" + mDistance + "," + "\r\n"
                + "  " + "life:" + mDuration + "," + "\r\n"
                + "  " + "time:" + mTime + "," + "\r\n"
                + "  " + "conf:" + mConfidence + "," + "\r\n"
                + "  " + "stat:" + mStatus + "," + "\r\n"
                + "  " + "glue:" + mGlueFlag
                + (mContent != null ? ",\r\n  " + "data:\r\n  " + mContent : "")
                + "]";
    }

    @Override
    public String toString() {
        return toXML();
    }

    // Parse an action object
    private void parse(final String text) {
        try {
            // Parse the XML string
            final ByteArrayInputStream stream = new ByteArrayInputStream(text.getBytes("UTF-8"));
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final Document document = builder.parse(stream);
            // Get the root element
            final Element element = document.getDocumentElement();
            // Check if we have an SSI events object
            if (element.getTagName().equals("events")) {
                // Get the list of single SSI events
                final NodeList eventList = element.getElementsByTagName("event");
                if (eventList.getLength() > 0) {
                    // Process each individual SSI event
                    for (int i = 0; i < eventList.getLength(); i++) {
                        final Element event = ((Element) eventList.item(i));
                        // Get The Event Attributes
                        mSender = event.getAttribute("event").toLowerCase();
                        mEvent = event.getAttribute("sender").toLowerCase();
                        mStatus = event.getAttribute("state").toLowerCase();
                        mDistance = event.getAttribute("from").toLowerCase();
                        mDuration = event.getAttribute("dur").toLowerCase();
                        mConfidence = String.format(Locale.US, "%.6f",
                                Double.valueOf(event.getAttribute("prob").toLowerCase()));
                        mDatatype = event.getAttribute("type");
                        mGlueFlag = event.getAttribute("glue");

                        // Parse The Data Structure
                        mContent = null;
                        // Paryse The Text Content
                        if (mDatatype.equals("EMPTY")) {
                            // Do Nothing
                        } else if (mDatatype.equals("STRING")) {
                            mContent = event.getTextContent();
                        } else if (mDatatype.equals("NTUPLE")) {
                            mContent = "[";
                            // Get The List Of Tuples
                            final NodeList tupleList = element.getElementsByTagName("tuple");
                            for (int j = 0; j < tupleList.getLength(); j++) {
                                // Get The Tuple Element
                                final Element tuple = ((Element) tupleList.item(j));
                                // Get The Attributes
                                final String string = tuple.getAttribute("string").toLowerCase();
                                final String value = String.format(Locale.US, "%.6f",
                                        Double.valueOf(tuple.getAttribute("value").toLowerCase()));
                                // Write The Feature
                                mContent += string + ":" + value;
                                // Write Next Feature
                                if (j < tupleList.getLength() - 1) {
                                    mContent += ",\r\n";
                                }
                            }
                            mContent += "]";
                        }
                    }
                }
            }
        } catch (final Exception exc) {
            // Print some information
            mLogger.failure(exc.toString());
        }
    }
}
