package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.TreeEntry;
import de.dfki.vsm.util.evt.EventObject;

/**
 *     @author Martin Fallas
 *     This event is used to know when elements of the left panel are selected
 */
public class TreeEntrySelectedEvent extends EventObject {
    private TreeEntry mEntry;

    public TreeEntrySelectedEvent(Object source, TreeEntry entry) {
        super(source);
        mEntry = entry;
    }

    public TreeEntry getmEntry() {
        return mEntry;
    }
}
