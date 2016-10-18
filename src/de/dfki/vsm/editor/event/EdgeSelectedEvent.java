package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.AbstractEdge;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public class EdgeSelectedEvent extends EventObject {
    private AbstractEdge mEdge;

    public EdgeSelectedEvent(Object source, AbstractEdge edge) {
        super(source);
        mEdge = edge;
    }

    public AbstractEdge getEdge() {
        return mEdge;
    }
    
    public String getEventDescription() {
        return "NodeSelectedEvent(" + mEdge.getSource() + " -> " + mEdge.getTarget() + ")";
    }
}
