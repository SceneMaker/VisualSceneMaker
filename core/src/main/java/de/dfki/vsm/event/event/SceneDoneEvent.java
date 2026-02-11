package de.dfki.vsm.event.event;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneObject;

public class SceneDoneEvent extends EventObject {
    private final SceneObject mScene;

    public SceneDoneEvent(Object source, SceneObject scene) {
        super(source);
        mScene = scene;
    }

    public SceneObject getScene() {
        return mScene;
    }

    public String getEventDescription() {
        return "SceneDoneEvent(" + mScene.getName() + ")";
    }
}
