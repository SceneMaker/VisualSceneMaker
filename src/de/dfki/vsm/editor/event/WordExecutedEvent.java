
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.scenescript.AbstractWord;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Not me
 */
public class WordExecutedEvent extends EventObject {
    private AbstractWord mTurn;

    public WordExecutedEvent(Object source, AbstractWord turn) {
        super(source);
        mTurn = turn;
    }

    public AbstractWord getWord() {
        return mTurn;
    }

    public String getEventDescription() {

        // TODO: is getText right?
        return "WordEvent(" + mTurn.getText() + ")";
    }
}
