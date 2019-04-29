package de.dfki.vsm.runtime.interpreter.error;

/**
 * @author Gregor Mehlmann
 */
public final class InterpreterError extends Exception {

    // Create an interpret exception
    public InterpreterError(final Object obj, final String msg) {
        super(msg);
    }
}
