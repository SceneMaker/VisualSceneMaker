package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.utterance.Utterance;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneUttr;

/**
 * @author Gregor Mehlmann
 */
public class UtteranceExecutedEvent extends EventObject {
    private SceneUttr mUtterance;

    public UtteranceExecutedEvent(Object source, SceneUttr utterance) {
        super(source);
        mUtterance = utterance;
    }

    public SceneUttr getUtterance() {
        return mUtterance;
    }

    public String getEventDescription() {
        return "UtteranceEvent(" + mUtterance.getText() + ")";
    }
}
