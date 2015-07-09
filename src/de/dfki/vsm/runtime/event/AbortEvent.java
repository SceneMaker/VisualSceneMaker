package de.dfki.vsm.runtime.event;

/**
 * @author Not me
 */
public class AbortEvent extends InterpreterEvent {
    Exception mException;

    public AbortEvent(Object source, Exception exception) {
        super(source);
        mException = exception;
    }

    public String getEventDescription() {
        return mException.getMessage();
    }
}
