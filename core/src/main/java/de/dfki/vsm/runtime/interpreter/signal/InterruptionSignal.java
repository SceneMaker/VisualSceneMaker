package de.dfki.vsm.runtime.interpreter.signal;

/**
 * @author Gregor Mehlmann
 */
public final class InterruptionSignal extends Exception {

    // Create an interrupt exception
    public InterruptionSignal(final Object obj, final String msg) {
        super(msg);
    }
}
