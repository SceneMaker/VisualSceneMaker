package de.dfki.vsm.xtension.ssj.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.TreeSet;

/**
 * @author Gregor Mehlmann
 */
public final class SSJEventArray extends SSJEventObject
{
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The event sequence version
    private String mVersion;
    // The event object sequence
    protected final ArrayList<SSJEventEntry> mList = new ArrayList<>();

    // Create a new event array
    public SSJEventArray()
    {
        // Do nothing here
    }

    // Create a new event array
    public SSJEventArray(final String version)
    {
        mVersion = version;
    }

    public final int size()
    {
        return mList.size();
    }

    public final void clear()
    {
        mList.clear();
    }

    // Get event object sequence
    public final SSJEventEntry get(final int index)
    {
        return mList.get(index);
    }

    // Get event object sequence
    public final ArrayList<SSJEventEntry> list()
    {
        return mList;
    }

    // Get ordered event sequence
    public final TreeSet<SSJEventEntry> getTreeSet()
    {
        final TreeSet<SSJEventEntry> set = new TreeSet(mList);
        mLogger.warning("Tree set size is '" + set.size() + "' while list size is '" + mList.size() + "'");
        return set;
    }

    // Get event object sequence
    public final boolean add(final SSJEventEntry entry)
    {
        return mList.add(entry);
    }

    public static SSJEventArray merge(final SSJEventArray first, final SSJEventArray second)
    {
        int index = 0;
        final SSJEventArray merged = new SSJEventArray("vsm");
        for (final SSJEventEntry secondEvent : first.list())
        {
            while (index < second.size())
            {
                final SSJEventEntry firstEvent = second.get(index);
                if (Integer.parseInt(firstEvent.getFrom()) < Integer.parseInt(secondEvent.getFrom()))
                {
                    merged.add(firstEvent);
                    index++;
                }
                else
                {
                    break;
                }
            }
            merged.add(secondEvent);
        }
        return merged;
    }

    // Write the event sequence
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError
    {
        writer.println("<events ssi-v=\"" + mVersion + "\">").push();
        for (final SSJEventEntry event : mList)
        {
            event.writeXML(writer);
        }
        writer.pop().println("</events>");
    }

    // Parse the event sequence
    @Override
    public final void parseXML(final Element element) throws XMLParseError
    {
        // Check the element name
        if (element.getTagName().equals("events"))
        {
            // Parse the version name
            mVersion = element.getAttribute("ssi-v");
            // Parse the event sequence
            XMLParseAction.processChildNodes(element, "event", new XMLParseAction()
            {
                @Override
                public void run(final Element element) throws XMLParseError
                {
                    // Construct an event
                    final SSJEventEntry event = new SSJEventEntry();
                    // Parse the new event
                    event.parseXML(element);
                    // Append the new event
                    mList.add(event);
                }
            });
        }
    }
}
