package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneObject;

/**
 * @author Gregor Mehlmann
 */
public class SceneExecutedEvent extends EventObject {
    private SceneObject mScene;

    public SceneExecutedEvent(Object source, SceneObject scene) {
        super(source);
        mScene = scene;
    }

    public SceneObject getScene() {
        return mScene;
    }

    public String getEventDescription() {
        return "SceneExecutedEvent ( " + mScene.getText() + " ) ";
    }
}
