package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.dialogact.DialogAct;

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
