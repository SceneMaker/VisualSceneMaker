package de.dfki.vsm.xtension.remote.client.agents;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public class DefaultRemoteAgent implements RemoteAgent {
    private final DataSendable sendable;

    public DefaultRemoteAgent(DataSendable sendable) {
        this.sendable = sendable;
    }

    @Override
    public DataSendable getSendable() {
        return sendable;
    }

    @Override
    public void buildData() {

    }
}
