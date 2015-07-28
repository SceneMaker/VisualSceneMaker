package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Martin Fallas
 */
public class ProjectChangedEvent extends EventObject {
    public ProjectChangedEvent(Object source) {
        super(source);
    }
}
