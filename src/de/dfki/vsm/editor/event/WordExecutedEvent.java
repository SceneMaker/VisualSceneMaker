/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
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
