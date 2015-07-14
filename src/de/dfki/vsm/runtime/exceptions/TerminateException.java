package de.dfki.vsm.runtime.exceptions;

/**
 * @author Gregor Mehlmann
 */
public final class TerminateException extends RunTimeException {

    public TerminateException(final Object obj, final String msg) {
        super(obj, msg);
    }
}
