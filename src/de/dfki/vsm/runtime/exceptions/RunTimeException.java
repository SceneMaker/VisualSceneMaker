package de.dfki.vsm.runtime.exceptions;

import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * @author Gregor Mehlmann
 */
public abstract class RunTimeException extends Exception {
    private final EventDispatcher  mEventCaster = EventDispatcher.getInstance();
    public RunTimeException(final Object obj, final String msg) {
        super(msg);
    }
}
