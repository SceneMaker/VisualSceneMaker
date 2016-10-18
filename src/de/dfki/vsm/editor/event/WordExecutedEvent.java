
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public class WordExecutedEvent extends EventObject {
    private UtteranceElement mTurn;

    public WordExecutedEvent(Object source, UtteranceElement turn) {
        super(source);
        mTurn = turn;
    }

    public UtteranceElement getWord() {
        return mTurn;
    }

    public String getEventDescription() {

        // TODO: is getText right?
        return "WordEvent(" + mTurn.getText() + ")";
    }
}
