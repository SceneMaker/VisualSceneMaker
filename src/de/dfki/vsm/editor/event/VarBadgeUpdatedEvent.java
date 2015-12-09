package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.diagram.boards.VariableEntry;
import de.dfki.vsm.util.evt.EventObject;
import java.util.Vector;

/**
 *     @author Martin Fallas
 *     This event lets the MonitorDialog know that a var has been updated
 */
public class VarBadgeUpdatedEvent extends EventObject {
    
    Vector<VariableEntry> varEntries;
    
    public VarBadgeUpdatedEvent(Object source, Vector<VariableEntry>  VE) {
        super(source);
        varEntries = VE;
    }

    public Vector<VariableEntry> getVarEntries() {
        return varEntries;
    }

    public void setVarEntry(Vector<VariableEntry> entries) {
        varEntries = entries;
    }
}
