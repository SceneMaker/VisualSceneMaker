package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import java.util.TreeSet;
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

    // Create a new event array
    public SSIEventArray(final String version) {
        mVersion = version;
    }
    
    public final int size() {
        return mList.size();
    }
    
    public final void clear() {
        mList.clear();
    }

    // Get event object sequence
    public final SSIEventEntry get(final int index) {
        return mList.get(index);
    }

    // Get event object sequence
    public final ArrayList<SSIEventEntry> list() {
        return mList;
    }

    // Get ordered event sequence
    public final TreeSet<SSIEventEntry> getTreeSet() {
        final TreeSet<SSIEventEntry> set = new TreeSet();
        for (final SSIEventEntry entry : mList) {
            set.add(entry);
        }
        mLogger.warning("Tree set size is '" + set.size() + "' while list size is '" + mList.size() + "'");
        return set;
    }

    // Get event object sequence
    public final boolean add(final SSIEventEntry entry) {
        return mList.add(entry);
    }
    
    public static SSIEventArray merge(
            final SSIEventArray first,
            final SSIEventArray second) {
        int index = 0;
        final SSIEventArray merged = new SSIEventArray("vsm");
        for (final SSIEventEntry secondEvent : first.list()) {
            while (index < second.size()) {
                final SSIEventEntry firstEvent = second.get(index);
                if (Integer.parseInt(firstEvent.getFrom())
                        < Integer.parseInt(secondEvent.getFrom())) {
                    merged.add(firstEvent);
                    index++;
                } else {
                    break;
                }
            }
            merged.add(secondEvent);
        }
        return merged;
    }

    // Write the event sequence
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        writer.println("<events ssi-v=\"" + mVersion + "\">").push();
        for (final SSIEventEntry event : mList) {
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
            XMLParseAction.processChildNodes(element, "event", new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    // Construct an event
                    final SSIEventEntry event = new SSIEventEntry();
                    // Parse the new event
                    event.parseXML(element);
                    // Append the new event
                    mList.add(event);
                }
            });
        }
    }
}
