package de.dfki.vsm.xtension.ssi.event.data;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Locale;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public final class SSITupleData extends SSIEventData implements XMLParseable, XMLWriteable {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The tuple data
    final HashMap<String, String> mTupleMap
            = new HashMap();

    // Construct the tuple data
    public SSITupleData() {
    }

    // Write the tuple data
    @Override
    public void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        // TODO ...
    }

    // Parse the tuple data
    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Get The List Of Tuples
        final NodeList tupleList = element.getElementsByTagName("tuple");
        for (int j = 0; j < tupleList.getLength(); j++) {
            // Get The Tuple Element
            final Element tuple = ((Element) tupleList.item(j));
            // Get The Attributes
            final String string = tuple.getAttribute("string").toLowerCase();
            final String value = String.format(Locale.US, "%.6f",
                    Double.valueOf(tuple.getAttribute("value").toLowerCase()));
            // Append The Tuple
            mTupleMap.put(string, value);
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
