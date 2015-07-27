package de.dfki.vsm.util.xml;

import de.dfki.vsm.util.ios.IOSIndentWriter;

/**
 * @author Not me
 */
public interface XMLWriteable {

    // Write a writeable object in XML to an indent writer 
    public void writeXML(final IOSIndentWriter writer) throws XMLWriteError;
}
