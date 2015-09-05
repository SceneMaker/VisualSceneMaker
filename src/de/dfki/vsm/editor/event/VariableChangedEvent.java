package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.tpl.TPLTuple;

/**
 * @author Not me
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

    @Override
    public String toString() {
        return getEventDescription();
    }

}
