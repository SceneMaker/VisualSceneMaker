package de.dfki.vsm.xtension.stickmanmarytts.util.tts.events;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class LineStart extends EventObject {
    public LineStart(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Line Started";
    }
}
