package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;

import de.dfki.vsm.event.EventObject;

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
