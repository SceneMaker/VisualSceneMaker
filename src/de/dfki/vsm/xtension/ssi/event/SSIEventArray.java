package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class SSIEventArray implements XMLWriteable, XMLParseable {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The event sequence version
    private String mVersion;
    // The event object sequence
    private final ArrayList<SSIEventObject> mList = new ArrayList();

    // Create a new event array
    public SSIEventArray() {
        // Do nothing here
    }

    // Get event object sequence
    public final ArrayList<SSIEventObject> getEventList() {
        return mList;
    }

    // Write the event sequence
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        writer.println("<events ssi-v=\"" + mVersion + "\">").push();
        for (SSIEventObject event : mList) {
            event.writeXML(writer);
        }
        writer.pop().println("</events>");
    }

    // Parse the event sequence
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Check the element name
        if (element.getTagName().equals("events")) {
            // Parse the version name
            mVersion = element.getAttribute("ssi-v");
            // Parse the event sequence
            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    // Check the element name
                    if (element.getTagName().equals("event")) {
                        // Construct an event
                        final SSIEventObject event = new SSIEventObject();
                        // Parse the new event
                        event.parseXML(element);
                        // Append the new event
                        mList.add(event);
                    }
                }
            });
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
            mLogger.failure(exc.toString());
        }
        writer.flush();
        writer.close();
        try {
            return stream.toString("UTF-8");
        } catch (final UnsupportedEncodingException exc) {
            mLogger.failure(exc.toString());
            return stream.toString();
        }
    }
}
