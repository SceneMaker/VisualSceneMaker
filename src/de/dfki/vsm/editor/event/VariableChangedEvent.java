package de.dfki.vsm.editor.event;

import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.tpl.TPLTuple;

/**
 * @author Gregor Mehlmann
 */
public class VariableChangedEvent extends EventObject {

    private TPLTuple<String, String> mVariableValuePair;

    public VariableChangedEvent(Object source, TPLTuple<String, String> variableValuePair) {
        super(source);
        mVariableValuePair = variableValuePair;
    }

    public TPLTuple<String, String> getVarValue() {
        return mVariableValuePair;
    }

    public String getEventDescription() {
        return "VariableChangedEvent(" + mVariableValuePair.getFirst() + ", " + mVariableValuePair.getSecond() + ")";
    }
}
