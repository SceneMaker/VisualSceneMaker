package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.AbstractEdge;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class EdgeEditEvent extends EventObject {
    private AbstractEdge mEdge;

    public EdgeEditEvent(Object source, AbstractEdge edge) {
        super(source);
        mEdge = edge;
    }

    public AbstractEdge getEdge() {
        return mEdge;
    }
    
    public String getEventDescription() {
        return "EdgeEditEvent(" + mEdge.getSource() + " -> " + mEdge.getTarget() + ")";
    }
}
