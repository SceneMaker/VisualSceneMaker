package de.dfki.vsm.util.xml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Not me
 */
public abstract class XMLParseAction {

    // Perform the parse action on an XML document element
    public abstract void run(final Element element) throws XMLParseError;

    // Perform parse action on all child nodes of the element 
    public static void processChildNodes(
            final Element element,
            final XMLParseAction action) throws XMLParseError {
        // Run the action on each adequate child node
        final NodeList list = element.getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            if (list.item(i).getNodeType() != Node.ELEMENT_NODE) {
                continue;
            }
            final Element child = (Element) list.item(i);
            action.run(child);
        }
    }

    // Perform parse action on child nodes of the element with a certain name
    public static void processChildNodes(
            final Element element,
            final String tagname,
            final XMLParseAction action) throws XMLParseError {
        // Run the action on each adequate child node
        final NodeList list = element.getElementsByTagName(tagname);
        for (int i = 0; i < list.getLength(); i++) {
            final Element child = (Element) list.item(i);
            action.run(child);
        }
    }
}
