package de.dfki.vsm.runtime.events;

/**
 * @author Not me
 */
public final class AbortionEvent extends RunTimeEvent {

    // The triggering exception
    final Exception mExc;

    // Create an abortion exception
    public AbortionEvent(final Object obj, final Exception exc) {
        super(obj);
        // Initializt the exception
        mExc = exc;
    }

    // Get the message string
    public final String getMessage() {
        return mExc.getMessage();
    }
}
