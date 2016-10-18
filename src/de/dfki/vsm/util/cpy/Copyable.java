package de.dfki.vsm.util.cpy;

import java.io.Serializable;

/**
 * An interface for all copyable objects.
 *
 * @author Gregor Mehlmann
 */
public interface Copyable extends Serializable {

    /**
     * Create a deep copy of the copyable object.
     *
     * @return A deep copy of the copyable object.
     */
    public abstract Copyable getCopy();

}
