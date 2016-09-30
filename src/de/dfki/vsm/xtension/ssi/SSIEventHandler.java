package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.xtension.ssi.event.SSIEventArray;

/**
 * @author Gregor Mehlmann
 */
public interface SSIEventHandler {

    public abstract void handle(final SSIEventArray array);
}
