package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.VariableEntry;
import de.dfki.vsm.util.evt.EventObject;

/**
 *     @author Martin Fallas
 *     This event lets the MonitorDialog know that a var has been updated
 */
public class VarBadgeUpdatedEvent extends EventObject {
    VariableEntry varEntry;
    public VarBadgeUpdatedEvent(Object source, VariableEntry VE) {
        super(source);
        varEntry = VE;
    }

    public VariableEntry getVarEntry() {
        return varEntry;
    }

    public void setVarEntry(VariableEntry varEntry) {
        this.varEntry = varEntry;
    }
}
