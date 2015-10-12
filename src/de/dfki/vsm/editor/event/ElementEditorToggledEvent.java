package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.util.evt.EventObject;

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
