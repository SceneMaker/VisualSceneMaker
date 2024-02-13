package de.dfki.vsm.xtension.ssj;

import de.dfki.vsm.xtension.ssj.event.SSJEventArray;

/**
 * @author Gregor Mehlmann
 */
interface SSJEventHandler
{

    public abstract void handle(final SSJEventArray array);
}
