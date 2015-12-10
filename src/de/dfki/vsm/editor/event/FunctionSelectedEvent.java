package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class FunctionSelectedEvent extends EventObject {
    private FunDef mFunDef;

    public FunctionSelectedEvent(Object source, FunDef funDef) {
        super(source);
        mFunDef = funDef;
    }

    public FunDef getFunction() {
        return mFunDef;
    }

    public String getEventDescription() {
        return "FunctionSelectedEvent(" + mFunDef.getName() + ")";
    }
}
