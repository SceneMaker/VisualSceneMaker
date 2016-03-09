package de.dfki.vsm.runtime.event;

/**
 * @author Gregor Mehlmann
 */
public final class AbortionEvent extends RunTimeEvent {

    // The triggering exception
    final Exception mException;

    // Create an abortion event
    public AbortionEvent(final Object object, final Exception exception) {
        super(object);
        // Initialize the exception
        mException = exception;
    }

    // Get the message string
    public final String getMessage() {
        return mException.getMessage();
    }
}
