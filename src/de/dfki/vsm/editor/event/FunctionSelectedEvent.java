package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.util.evt.EventObject;

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
