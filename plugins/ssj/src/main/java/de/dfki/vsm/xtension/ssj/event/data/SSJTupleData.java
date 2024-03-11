package de.dfki.vsm.xtension.ssj.event.data;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public final class SSJTupleData extends SSJEventData implements XMLParseable, XMLWriteable
{
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The tuple data
    private final Map<String, String> mTupleMap = new HashMap<>();

    // Construct the tuple data
    public SSJTupleData()
    {
    }

    public String get(final String key)
    {
        return mTupleMap.get(key);
    }

    public void set(final String key, final String value)
    {
        mTupleMap.put(key, value);
    }

    public Map<String, String> getTupleMap()
    {
        return mTupleMap;
    }

    // Write the tuple data
    @Override
    public void writeXML(final IOSIndentWriter stream) throws XMLWriteError
    {
        stream.endl().push();

        for (String key : mTupleMap.keySet())
        {
            stream.println("<tuple string=\"" + key + "\" value=\"" + mTupleMap.get(key) + "\"></tuple>");
        }

        stream.pop();
    }

    // Parse the tuple data
    @Override
    public void parseXML(final Element element) throws XMLParseError
    {
        // Get The List Of Tuples
        final NodeList tupleList = element.getElementsByTagName("tuple");
        for (int j = 0; j < tupleList.getLength(); j++)
        {
            // Get The Tuple Element
            final Element tuple = ((Element) tupleList.item(j));
            // Get The Attributes
            final String string = tuple.getAttribute("string");
            final String value = tuple.getAttribute("value");
            //final String value = String.format(Locale.US, "%.6f", Float.valueOf(tuple.getAttribute("value").toLowerCase()));
            // Removed by PG, 14.6.2017: System.err.println(string +"->"+value);
            // Append The Tuple
            mTupleMap.put(string, value);
        }
    }

    // Get string representation
    @Override
    public final String toString()
    {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(stream);
        try
        {
            writeXML(writer);
        }
        catch (final XMLWriteError exc)
        {
            mLogger.failure(exc.toString());
        }
        writer.flush();
        writer.close();
        try
        {
            //return stream.toString("UTF-8");
            return stream.toString();
        }
        catch (final Exception exc)
        {
            mLogger.failure(exc.toString());
            return stream.toString();
        }
    }
}
