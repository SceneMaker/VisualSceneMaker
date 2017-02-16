package de.dfki.vsm.model;

import de.dfki.vsm.util.cpy.Copyable;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;

/**
 * @author Gregor Mehlmann
 */
public interface ModelObject extends XMLParseable, XMLWriteable, Copyable {

    
    // Get a deep copy of the model object
    @Override
    public abstract ModelObject getCopy();
}