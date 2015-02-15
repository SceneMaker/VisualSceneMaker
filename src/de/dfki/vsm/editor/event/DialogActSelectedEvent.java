package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class DialogActSelectedEvent extends EventObject {

    private final DialogAct mDialogAct;

    public DialogActSelectedEvent(Object source, DialogAct dialogAct) {
        super(source);
        mDialogAct = dialogAct;
    }

    public DialogAct getFunction() {
        return mDialogAct;
    }

    public String getEventDescription() {
        return "DialogActSelectedEvent(" + mDialogAct.getName() + ")";
    }
}
