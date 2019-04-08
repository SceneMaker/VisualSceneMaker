package de.dfki.vsm.runtime.interpreter.event;

import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public abstract class InterpreterEvent extends EventObject {

    // Create a runtime event
    public InterpreterEvent(final Object object) {
        super(object);
    }
}
