package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class EdgeEditEvent extends EventObject {
    private Edge mEdge;

    public EdgeEditEvent(Object source, Edge edge) {
        super(source);
        mEdge = edge;
    }

    public Edge getEdge() {
        return mEdge;
    }
    
    public String getEventDescription() {
        return "EdgeEditEvent(" + mEdge.getSource() + " -> " + mEdge.getTarget() + ")";
    }
}
