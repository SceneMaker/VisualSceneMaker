package de.dfki.vsm.runtime.interpreter.signal;

/**
 * @author Gregor Mehlmann
 */
public final class TerminationSignal extends Exception {

        // Create a terminate exception
    public TerminationSignal(final Object obj, final String msg) {
        super(msg);
    }
}
