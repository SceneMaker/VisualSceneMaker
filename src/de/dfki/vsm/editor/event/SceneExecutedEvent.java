package de.dfki.vsm.editor.event;

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.util.evt.EventObject;

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
