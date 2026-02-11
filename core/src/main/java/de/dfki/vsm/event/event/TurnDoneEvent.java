package de.dfki.vsm.event.event;

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneTurn;

public class TurnDoneEvent extends EventObject {
    private final SceneTurn mTurn;

    public TurnDoneEvent(Object source, SceneTurn turn) {
        super(source);
        mTurn = turn;
    }

    public SceneTurn getTurn() {
        return mTurn;
    }

    public String getEventDescription() {
        return "TurnDoneEvent(" + mTurn.getSpeaker() + ")";
    }
}
