package de.dfki.vsm.model;

import de.dfki.vsm.util.cpy.Copyable;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteable;

/**
 * An interface for all objects of the model of VSM.
 *
 * @author Gregor Mehlmann
 */
public interface ModelObject extends XMLParseable, XMLWriteable, Copyable {

    /**
     * Get a deep copy of the model object.
     *
     * @return A deep copy of the model object.
     */
    @Override
    public abstract ModelObject getCopy();
}
