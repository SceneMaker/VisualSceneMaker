package de.dfki.vsm.util.xml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 */
public abstract class XMLParseAction {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public abstract void run(final Element element) throws XMLParseError;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void processChildNodes(
            final Element element,
            final XMLParseAction action) throws XMLParseError {
        // Get The List Of Child Nodes
        final NodeList list = element.getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            if (list.item(i).getNodeType() != Node.ELEMENT_NODE) {
                continue;
            }
            // Run The Action On The Child Node
            final Element child = (Element) list.item(i);
            action.run(child);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void processChildNodes(
            final Element element,
            final String attribute,
            final XMLParseAction action) throws XMLParseError {
        // Get The List Of Child Nodes
        final NodeList list = element.getElementsByTagName(attribute);
        for (int i = 0; i < list.getLength(); i++) {
            // Run The Action On The Child Node
            final Element child = (Element) list.item(i);
            action.run(child);
        }
    }
}
