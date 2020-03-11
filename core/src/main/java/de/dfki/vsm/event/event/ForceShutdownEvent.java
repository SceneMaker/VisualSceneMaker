package de.dfki.vsm.event.event;

import de.dfki.vsm.event.EventObject;

public class ForceShutdownEvent extends EventObject {
    public ForceShutdownEvent(Object source) {
        super(source);
    }
}
