package de.dfki.vsm.xtension.remote.client.agents;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public interface RemoteAgent {
    DataSendable getSendable();
    void buildData();
}
