package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.utterance.Utterance;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
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
