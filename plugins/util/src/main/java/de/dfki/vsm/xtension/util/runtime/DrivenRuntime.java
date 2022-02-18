package de.dfki.vsm.xtension.util.runtime;

import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;

public interface DrivenRuntime extends Runtime {
    void setVar(String name, AbstractValue value);

    default void setVar(String name, String value) {
        setVar(name, new StringValue(value));
    }

    void wait(String id);

    void handleMarker(String marker);

    void stopWaiting(String id);

    void purgeActivities();
}
