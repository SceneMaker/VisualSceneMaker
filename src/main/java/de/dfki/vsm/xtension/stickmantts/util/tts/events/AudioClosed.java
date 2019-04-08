package de.dfki.vsm.xtension.stickmantts.util.tts.events;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class AudioClosed extends EventObject {
    public AudioClosed(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Audio Closed";
    }
}
