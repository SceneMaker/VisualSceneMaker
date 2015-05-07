package de.dfki.vsm.runtime.error;

/**
 * An exception thrown if an interpreter thread has been interrupted
 *
 * @author Gregor Mehlmann
 */
public class InterruptException extends InterpreterException {
    public InterruptException(Object obj, String msg) {
        super(obj, msg);
    }
}
