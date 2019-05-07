package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;

/**
 * @author Gregor Mehlmann
 */
public class EdgeExecutedEvent extends EventObject {
    private AbstractEdge mEdge;

    public EdgeExecutedEvent(Object source, AbstractEdge edge) {
        super(source);
        mEdge = edge;
    }

    public AbstractEdge getEdge() {
        return mEdge;
    }

    public String getEventDescription() {
        //System.err.println(mEdge.getSourceNode());
        //System.err.println(mEdge.getTargetNode());

        return "EdgeEvent(" /* + mEdge.getSourceNode().getId() + "," + mEdge.getTargetNode().getId() */ + ")";
    }
}
