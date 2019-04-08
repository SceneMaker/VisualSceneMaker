package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class SceneStoppedEvent extends EventObject {
    public SceneStoppedEvent(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "SceneStopped";
    }
}
