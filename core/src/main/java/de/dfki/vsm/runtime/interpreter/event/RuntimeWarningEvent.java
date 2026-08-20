package de.dfki.vsm.runtime.interpreter.event;

/**
 * A non-fatal problem the interpreter recovered from on its own, e.g. a PlayScene naming a scene
 * that does not exist yet. Unlike {@link TerminationEvent}, firing this does not imply the
 * interpreter thread stopped — it is still running and the flow keeps going.
 */
public final class RuntimeWarningEvent extends InterpreterEvent {

    private final String mMessage;

    public RuntimeWarningEvent(final Object object, final String message) {
        super(object);
        mMessage = message;
    }

    public String getMessage() {
        return mMessage;
    }
}
