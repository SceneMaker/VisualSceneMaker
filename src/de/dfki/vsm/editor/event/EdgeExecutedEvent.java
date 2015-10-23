package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
 */
public class EdgeExecutedEvent extends EventObject {
    private Edge mEdge;

    public EdgeExecutedEvent(Object source, Edge edge) {
        super(source);
        mEdge = edge;
    }

    public Edge getEdge() {
        return mEdge;
    }

    public String getEventDescription() {
        //System.err.println(mEdge.getSourceNode());
        //System.err.println(mEdge.getTargetNode());

        return "EdgeEvent(" /* + mEdge.getSourceNode().getId() + "," + mEdge.getTargetNode().getId() */ + ")";
    }
}
