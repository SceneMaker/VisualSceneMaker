package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Turn;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
 */
public class TurnExecutedEvent extends EventObject {
    private SceneTurn mTurn;

    public TurnExecutedEvent(Object source, SceneTurn turn) {
        super(source);
        mTurn = turn;
    }

    public SceneTurn getTurn() {
        return mTurn;
    }

    public String getEventDescription() {

        // TODO: is getText right?
        return "TurnEvent(" + mTurn.getText() + ")";
    }
}
