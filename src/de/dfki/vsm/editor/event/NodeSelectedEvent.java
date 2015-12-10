package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
 */
public class NodeSelectedEvent extends EventObject {
    private Node mNode;

    public NodeSelectedEvent(Object source, Node node) {
        super(source);
        mNode = node;
    }

    public Node getNode() {
        return mNode;
    }

    public String getEventDescription() {
        return "NodeSelectedEvent(" + mNode.getId() + ")";
    }
}
