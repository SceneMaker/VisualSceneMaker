package de.dfki.vsm.util.request;

import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public abstract class Client extends Thread {

    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    protected final Crowd mCrowd;
    protected final String mHost;
    protected final int mPort;
    protected final Request mRequest;
    protected volatile boolean mStop = false;

    public Client(ThreadGroup group, String host, int port, Request request, Crowd crowd) {
        super(group, "Client:" + host + ":" + port);
        mHost = host;
        mPort = port;
        mRequest = request;
        mCrowd = crowd;
    }

    public Request getRequest() {
        return mRequest;
    }

    public abstract void pleaseStop();
}
