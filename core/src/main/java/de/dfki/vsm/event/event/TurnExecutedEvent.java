package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Turn;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneTurn;

/**
 * @author Gregor Mehlmann
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
