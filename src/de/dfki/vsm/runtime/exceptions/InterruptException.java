package de.dfki.vsm.runtime.exceptions;

/**
 * @author Gregor Mehlmann
 */
public final class InterruptException extends RunTimeException {

    public InterruptException(final Object obj, final String msg) {
        super(obj, msg);
    }
}
