package de.dfki.vsm.editor.event;

/**
 * Created by alvaro on 2/16/17.
 */
public class IEdgeDialogModifiedEvent extends DialogModifiedEvent {
    public IEdgeDialogModifiedEvent(Object source) {
        super(source);
    }

    public IEdgeDialogModifiedEvent(Object source, String text) {
        super(source, text);
    }
}
