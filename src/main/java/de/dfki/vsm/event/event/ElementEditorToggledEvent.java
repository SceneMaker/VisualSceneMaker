package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;

/**
 * @author Martin Fallas
 */
public class ElementEditorToggledEvent extends EventObject {

    public ElementEditorToggledEvent(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Element Editor toggled";
    }
}
