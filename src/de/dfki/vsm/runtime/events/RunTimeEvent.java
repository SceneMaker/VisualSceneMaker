package de.dfki.vsm.runtime.events;

import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimeEvent extends EventObject {

    // Create a runtime event
    public RunTimeEvent(final Object obj) {
        super(obj);
    }
}
