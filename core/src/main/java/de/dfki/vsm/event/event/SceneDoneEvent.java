package de.dfki.vsm.event.event;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneObject;

public class SceneDoneEvent extends EventObject {
    private final SceneObject mScene;
    private final String mNodeId;
    private final String mParentId;

    public SceneDoneEvent(Object source, SceneObject scene) {
        this(source, scene, "", "");
    }

    public SceneDoneEvent(Object source, SceneObject scene, String nodeId, String parentId) {
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
        return "SceneDoneEvent(" + mScene.getName() + ")";
    }
}
