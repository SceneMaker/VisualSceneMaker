package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public class NodeTerminatedEvent extends EventObject {
    private BasicNode mNode;

    public NodeTerminatedEvent(Object source, BasicNode node) {
        super(source);
        mNode = node;
    }

    public BasicNode getNode() {
        return mNode;
    }

    public String getEventDescription() {
        return "NodeEvent(" + mNode.getId() + ")";
    }
}
