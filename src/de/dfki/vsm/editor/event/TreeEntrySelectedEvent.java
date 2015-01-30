package de.dfki.vsm.editor.event;

import de.dfki.vsm.editor.TreeEntry;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.util.evt.EventObject;

/**
     * @author Martin Fallas
     * This event is used to know when elements of the left panel are selected 
 */
public class TreeEntrySelectedEvent extends EventObject {

    private TreeEntry mEntry;

    public TreeEntry getmEntry() {
        return mEntry;
    }

    public TreeEntrySelectedEvent(Object source, TreeEntry entry) {
        super(source);
        mEntry = entry;
    }
}
