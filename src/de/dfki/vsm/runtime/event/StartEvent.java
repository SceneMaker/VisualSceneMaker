package de.dfki.vsm.runtime.event;

/**
 * @author Gregor
 */
public class StartEvent extends InterpreterEvent {
    public StartEvent(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Start Sceneflow";
    }
}
