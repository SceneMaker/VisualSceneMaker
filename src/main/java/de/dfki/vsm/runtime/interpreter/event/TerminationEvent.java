package de.dfki.vsm.runtime.interpreter.event;

/**
 * @author Gregor Mehlmann
 */
public final class TerminationEvent extends InterpreterEvent {

    // The triggering exception
    final Exception mException;

    // Create an abortion event
    public TerminationEvent(final Object object, final Exception exception) {
        super(object);
        // Initialize the exception
        mException = exception;
    }

    // Get the message string
    public final String getMessage() {
        return mException.getMessage();
    }
}
