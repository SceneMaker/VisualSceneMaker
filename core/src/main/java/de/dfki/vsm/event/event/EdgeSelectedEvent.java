package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;

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
        return "NodeSelectedEvent(" + mEdge.getSourceUnid() + " -> " + mEdge.getTargetUnid() + ")";
    }
}
