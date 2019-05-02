package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;

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
        return "EdgeEditEvent(" + mEdge.getSourceUnid() + " -> " + mEdge.getTargetUnid() + ")";
    }
}
