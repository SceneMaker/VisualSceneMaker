package de.dfki.vsm.runtime.exceptions;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimeException extends Exception {

    public RunTimeException(final Object obj, final String msg) {
        super(msg);
    }
}
