package de.dfki.vsm.util.net;

import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public abstract class NETProcess extends Thread {

    // The System File Logger
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // Cancel The NET Process
    public abstract void abort();

    // Execute The NET Process
    protected abstract void execute();
}
