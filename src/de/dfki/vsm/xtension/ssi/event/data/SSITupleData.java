/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.ssi.event.data;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Locale;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public final class SSITupleData implements SSIEventData, XMLParseable, XMLWriteable {

    final HashMap<String, String> mTupleMap = new HashMap();

    @Override
    public void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        // TODO ...
    }

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

   //
    @Override
    public final String toString() {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(stream);
        try {
            writeXML(writer);
            writer.flush();
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
