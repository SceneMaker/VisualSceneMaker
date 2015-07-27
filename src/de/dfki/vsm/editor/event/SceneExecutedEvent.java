package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
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
