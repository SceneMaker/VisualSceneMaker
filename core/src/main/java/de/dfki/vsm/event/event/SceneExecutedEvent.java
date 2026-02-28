package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneObject;

/**
 * @author Gregor Mehlmann
 */
public class SceneExecutedEvent extends EventObject {
    private final SceneObject mScene;
    private final String mNodeId;
    private final String mParentId;

    public SceneExecutedEvent(Object source, SceneObject scene) {
        this(source, scene, "", "");
    }

    public SceneExecutedEvent(Object source, SceneObject scene, String nodeId, String parentId) {
        super(source);
        mScene = scene;
        mNodeId = nodeId == null ? "" : nodeId;
        mParentId = parentId == null ? "" : parentId;
    }

    public SceneObject getScene() {
        return mScene;
    }

    public String getNodeId() {
        return mNodeId;
    }

    public String getParentId() {
        return mParentId;
    }

    public String getEventDescription() {
        return "SceneExecutedEvent ( " + mScene.getText() + " ) ";
    }
}
