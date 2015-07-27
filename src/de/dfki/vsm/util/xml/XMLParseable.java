package de.dfki.vsm.util.xml;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public interface XMLParseable {

    // Parse a parsable object from an XML document element
    public void parseXML(final Element element) throws XMLParseError;
}
