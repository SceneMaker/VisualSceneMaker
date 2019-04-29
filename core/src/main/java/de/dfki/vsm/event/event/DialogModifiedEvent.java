package de.dfki.vsm.event.event;

import de.dfki.vsm.event.EventObject;

/**
 * Created by alvaro on 2/16/17.
 */
public class DialogModifiedEvent extends EventObject {
    private String text;
    public DialogModifiedEvent(Object source) {
        super(source);
    }

    public DialogModifiedEvent(Object source, String text) {
        super(source);
        this.text = text;
    }

    public String getText(){
        return  text;
    }
}
