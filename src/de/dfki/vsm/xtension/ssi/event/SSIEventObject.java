package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Locale;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class SSIEventObject implements XMLParseable, XMLWriteable {

    // The event data
    private String mSender;
    private String mEvent;
    private String mFrom;
    private String mDur;
    private String mProb;
    private String mType;
    private String mState;
    private String mGlue;
    // The event content
    private SSIEventData mData;

    // Get the event data
    public SSIEventData getData() {
        return mData;
    }

    @Override
    public void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
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

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Check the element name
        if (element.getTagName().equals("event")) {
            // Get The Event Attributes
            mSender = element.getAttribute("event").toLowerCase();
            mEvent = element.getAttribute("sender").toLowerCase();
            mFrom = element.getAttribute("from").toLowerCase();
            mDur = element.getAttribute("dur").toLowerCase();
            mProb = String.format(Locale.US, "%.6f",
                    Double.valueOf(element.getAttribute("prob").toLowerCase()));
            mType = element.getAttribute("type");
            mGlue = element.getAttribute("glue");
            mState = element.getAttribute("state").toLowerCase();
            // Parse The Data Structure
            if (mType.equals("EMPTY")) {
                mData = null;
            } else if (mType.equals("STRING")) {
                mData = new SSIStringData(
                        element.getTextContent());
            } else if (mType.equals("NTUPLE")) {

                final SSITupleData content = new SSITupleData();
                try {
                    final byte[] xml = element.getTextContent().getBytes("UTF-8");
                    final ByteArrayInputStream stream = new ByteArrayInputStream(xml);
                    // Parse the data
                    XMLUtilities.parseFromXMLStream(content, stream);
                } catch (final Exception exc) {
                    exc.printStackTrace();
                }
                // Set the new data
                mData = content;

            } else {
                // Do nothing
            }
        }
    }

    // Get string representation
    @Override
    public final String toString() {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(stream);
        try {
            writeXML(writer);
        } catch (final XMLWriteError exc) {
            exc.printStackTrace();
        }
        writer.flush();
        writer.close();
        try {
            return stream.toString("UTF-8");
        } catch (final Exception exc) {
            return stream.toString();
        }
    }
}
