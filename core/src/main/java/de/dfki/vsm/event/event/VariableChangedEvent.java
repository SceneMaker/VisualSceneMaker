package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.util.tpl.Tuple;

/**
 * @author Gregor Mehlmann
 */
public class VariableChangedEvent extends EventObject {

    private Tuple<String, String> mVariableValuePair;

    public VariableChangedEvent(Object source, Tuple<String, String> variableValuePair) {
        super(source);
        mVariableValuePair = variableValuePair;
    }

    public Tuple<String, String> getVarValue() {
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
