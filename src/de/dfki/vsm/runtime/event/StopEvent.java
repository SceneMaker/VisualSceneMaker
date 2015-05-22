package de.dfki.vsm.runtime.event;

/**
 * @author Gregor
 */
public class StopEvent extends InterpreterEvent {
    public StopEvent(Object source) {
        super(source);
    }

    public String getEventDescription() {
        return "Stop Sceneflow";
    }
}
