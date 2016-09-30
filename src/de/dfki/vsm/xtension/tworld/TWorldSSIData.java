package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * @author Gregor Mehlmann
 */
public final class TWorldSSIData {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The XML DOM document
    private Document mDocument;

    // Create a new SSI XML Data
    public TWorldSSIData(final String xml) {
        try {
            final ByteArrayInputStream stream = new ByteArrayInputStream(
                    xml.getBytes("UTF-8"));
            // Construct the XML document parser
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            // Parse the XML document from the stream
            mDocument = parser.parse(stream);
            // Finally close the stream and the URL
            stream.close();

        } catch (final ParserConfigurationException | SAXException | IOException exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Get the value of a path
    private String get(final String path, final Element root) {
        final int index = path.indexOf(".");
        if (index != -1) {
            final NodeList list = root.getElementsByTagName(
                    path.substring(0, index));
            if (list.getLength() == 1) {
                return get(path.substring(index + 1), (Element) list.item(0));
            } else {
                return null;
            }
        } else {
            // We have a leaf element
            final NodeList list = root.getElementsByTagName(path);
            if (list.getLength() == 1) {
                return ((Element) list.item(0)).getTextContent();
            } else {
                return null;
            }
        }
    }

    // Get the value of a path
    public final String get(final String path) {
        return get(path, mDocument.getDocumentElement());
    }

}
