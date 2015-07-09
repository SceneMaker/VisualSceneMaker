package de.dfki.vsm.runtime.error;

/**
 * The base class for all types of exceptions that can be thrown by the
 * interpreter. It encapsulates an object containing some information about the
 * source of the exception and a message string giving a description of the
 * cause of the exception.
 *
 * @author Not me
 */
public abstract class InterpreterException extends Exception {
    public InterpreterException(Object obj, String msg) {
        super(msg);
    }
}
