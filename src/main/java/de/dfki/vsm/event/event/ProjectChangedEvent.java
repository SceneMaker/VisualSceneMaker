package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;

/**
 * @author Martin Fallas
 */
public class ProjectChangedEvent extends EventObject {
    public ProjectChangedEvent(Object source) {
        super(source);
    }
}
