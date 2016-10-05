package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;
import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Locale;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class SSIEventEntry extends SSIEventObject {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The event data
    private String mSender;
    private String mEvent;
    private String mFrom;
    private String mDur;
    private String mProb;
    private String mType;
    private String mState;
    private String mGlue;
    // The event data
    private SSIEventData mData;

    // Get the event sender
    public final String getSender() {
        return mSender;
    }

    // Get the event type
    public final String getEvent() {
        return mEvent;
    }

    // Get the event time
    public final String getFrom() {
        return mFrom;
    }

    // Get the event duration
    public final String getDur() {
        return mDur;
    }

    // Get the event probabbility
    public final String getProb() {
        return mProb;
    }

    // Get the event type
    public final String getType() {
        return mType;
    }

    // Get the event state
    public final String getState() {
        return mState;
    }

    // Get the event glue
    public final String getGlue() {
        return mGlue;
    }

    // Get the event data
    public final SSIEventData getData() {
        return mData;
    }

    // Write the event
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        writer.println("<event "
                + "sender=\"" + mSender + "\" "
                + "event=\"" + mEvent + "\" "
                + "from=\"" + mFrom + "\" "
                + "dur=\"" + mDur + "\" "
                + "prob=\"" + mProb + "\" "
                + "type=\"" + mType + "\" "
                + "state=\"" + mState + "\" "
                + "glue=\"" + mGlue + "\">");
        // Write the data
        if (mData != null) {
            writer.print(mData.toString());
        }
        writer.println("</event>");
    }

    // Parse the event
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Check the element name
        if (element.getTagName().equals("event")) {
            // Get the event attributes
            mSender = element.getAttribute("sender").toLowerCase();
            mEvent = element.getAttribute("event").toLowerCase();
            mFrom = element.getAttribute("from").toLowerCase();
            mDur = element.getAttribute("dur").toLowerCase();
            mProb = String.format(Locale.US, "%.6f",
                    Double.valueOf(element.getAttribute("prob").toLowerCase()));
            mType = element.getAttribute("type");
            mGlue = element.getAttribute("glue");
            mState = element.getAttribute("state").toLowerCase();
            // Parse the data structure
            if (mType.equalsIgnoreCase("empty")) {
                mData = null;
            } else if (mType.equalsIgnoreCase("string")) {
                mData = new SSIStringData(element.getTextContent().trim());
            } else if (mType.equalsIgnoreCase("ntuple")
                    || mType.equalsIgnoreCase("map")) {
                final SSITupleData data = new SSITupleData();
                try {
                    final byte[] xml = element.getTextContent().getBytes("UTF-8");
                    final ByteArrayInputStream stream = new ByteArrayInputStream(xml);
                    // Parse the data
                    XMLUtilities.parseFromXMLStream(data, stream);
                } catch (final DOMException | UnsupportedEncodingException exc) {
                    mLogger.failure(exc.toString());
                }
                // Set the new data
                mData = data;
            } else {
                // Do nothing here
            }
        }
    }
}