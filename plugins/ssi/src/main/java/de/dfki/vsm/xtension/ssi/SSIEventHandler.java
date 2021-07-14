package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.xtension.ssi.event.SSIEventArray;

/**
 * @author Gregor Mehlmann
 */
interface SSIEventHandler {

    void handle(final SSIEventArray array);
}
