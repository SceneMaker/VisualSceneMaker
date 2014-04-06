package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class FunctionCreatedEvent extends EventObject {

    private FunDef mFunDef;

    public FunctionCreatedEvent(Object source, FunDef funDef) {
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
