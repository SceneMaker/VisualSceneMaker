package de.dfki.vsm.editor.event;

/**
 * Created by alvaro on 2/16/17.
 */
public class CEdgeDialogModifiedEvent extends DialogModifiedEvent {
    public CEdgeDialogModifiedEvent(Object source) {
        super(source);
    }

    public CEdgeDialogModifiedEvent(Object source, String text) {
        super(source, text);
    }
}
