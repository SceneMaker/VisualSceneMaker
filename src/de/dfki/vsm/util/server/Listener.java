package de.dfki.vsm.util.server;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.service.Service;

/**
 *
 * @author Gregor Mehlmann
 */
public abstract class Listener extends Thread {

    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    protected final Server mServer;
    protected final int mPort;
    protected final Service mService;
    protected volatile boolean mStop = false;

    public Listener(ThreadGroup group, int port, Service service, Server server) {
        super(group, "Listener:" + port);
        mPort = port;
        mService = service;
        mServer = server;
    }

    public Service getService() {
        return mService;
    }

    public abstract void pleaseStop();
}
