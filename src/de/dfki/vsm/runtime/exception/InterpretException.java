package de.dfki.vsm.runtime.exception;

/**
 * @author Gregor Mehlmann
 */
public final class InterpretException extends RunTimeException {
    
    public InterpretException(final Object obj, final String msg) {
        super(obj, msg);
    }
}
