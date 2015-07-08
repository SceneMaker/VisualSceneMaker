package de.dfki.vsm.util.xml;

import de.dfki.vsm.util.ios.IndentWriter;

/**
 * @author Gregor Mehlmann
 */
public interface XMLWriteable {

    // Write The Writable To A Stream
    public void writeXML(final IndentWriter stream) throws XMLWriteError;
}
