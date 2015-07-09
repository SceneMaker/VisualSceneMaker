package de.dfki.vsm.runtime.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
 */
public abstract class InterpreterEvent extends EventObject {
    public InterpreterEvent(Object source) {
        super(source);
    }
}
