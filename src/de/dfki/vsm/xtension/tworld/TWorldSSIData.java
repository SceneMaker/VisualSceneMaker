package de.dfki.vsm.xtension.tworld;

import java.io.ByteArrayInputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public final class TWorldSSIData {

    private Document mDocument;

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

        } catch (final Exception exc) {
            exc.printStackTrace();
        }
    }

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

    public String get(final String path) {
        return get(path, mDocument.getDocumentElement());
    }

}
