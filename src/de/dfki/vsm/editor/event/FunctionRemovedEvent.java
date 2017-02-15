package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */
public class FunctionRemovedEvent extends EventObject {
    private final FunctionDefinition mFunDef;

    public FunctionRemovedEvent(Object source, FunctionDefinition funDef) {
        super(source);
        mFunDef = funDef;
    }

    public FunctionDefinition getFunction() {
        return mFunDef;
    }

    public String getEventDescription() {
        return "FunctionRemovedEvent(" + mFunDef.getName() + ")";
    }
}
