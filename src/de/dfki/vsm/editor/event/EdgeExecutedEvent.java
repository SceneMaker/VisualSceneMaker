package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
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
        System.err.println(mEdge.getSourceNode());
        System.err.println(mEdge.getTargetNode());

        return "EdgeEvent(" /*+ mEdge.getSourceNode().getId() + "," + mEdge.getTargetNode().getId() */ + ")";
    }
}
