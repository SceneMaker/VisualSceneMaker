package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;

/**
 * @author Gregor Mehlmann
 */
public class NodeSelectedEvent extends EventObject {
    private BasicNode mNode;

    public NodeSelectedEvent(Object source, BasicNode node) {
        super(source);
        mNode = node;
    }

    public BasicNode getNode() {
        return mNode;
    }

    public String getEventDescription() {
        return "NodeSelectedEvent(" + mNode.getId() + ")";
    }
}
