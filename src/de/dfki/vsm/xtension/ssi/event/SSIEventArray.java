package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class SSIEventArray extends SSIEventObject {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The event sequence version
    private String mVersion;
    // The event object sequence
    private final ArrayList<SSIEventEntry> mList = new ArrayList();

    // Create a new event array
    public SSIEventArray() {
        // Do nothing here
    }

    // Get event object sequence
    public final ArrayList<SSIEventEntry> getEventList() {
        return mList;
    }

    // Write the event sequence
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        writer.println("<events ssi-v=\"" + mVersion + "\">").push();
        for (SSIEventEntry event : mList) {
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
                        final SSIEventEntry event = new SSIEventEntry();
                        // Parse the new event
                        event.parseXML(element);
                        // Append the new event
                        mList.add(event);
                    }
                }
            });
        }
    }
}
