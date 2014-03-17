package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public class NodeExecutedEvent extends EventObject {

    private Node mNode;

    public NodeExecutedEvent(Object source, Node node) {
        super(source);
        mNode = node;
    }

    public Node getNode() {
        return mNode;
    }

    public String getEventDescription() {
        return "NodeEvent(" + mNode.getId() + ")";
    }
}
