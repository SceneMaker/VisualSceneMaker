package de.dfki.vsm.runtime.exceptions;

/**
 * @author Gregor Mehlmann
 */
public final class InterpretException extends RunTimeException {
    
    public InterpretException(final Object obj, final String msg) {
        super(obj, msg);
    }
}
