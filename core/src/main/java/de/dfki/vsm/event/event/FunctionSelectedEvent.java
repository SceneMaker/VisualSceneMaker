package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;

/**
 * @author Sergio Soto
 */
public class FunctionSelectedEvent extends EventObject {
    private FunctionDefinition mFunDef;

    public FunctionSelectedEvent(Object source, FunctionDefinition funDef) {
        super(source);
        mFunDef = funDef;
    }

    public FunctionDefinition getFunction() {
        return mFunDef;
    }

    public String getEventDescription() {
        return "FunctionSelectedEvent(" + mFunDef.getName() + ")";
    }
}
