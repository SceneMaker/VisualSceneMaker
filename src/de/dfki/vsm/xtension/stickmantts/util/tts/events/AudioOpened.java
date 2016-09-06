package de.dfki.vsm.xtension.stickmantts.util.tts.events;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class AudioOpened extends EventObject {
    public AudioOpened(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Audio Opened";
    }
}
