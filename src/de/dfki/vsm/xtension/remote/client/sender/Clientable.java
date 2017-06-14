package de.dfki.vsm.xtension.remote.client.sender;

import java.io.IOException;

/**
 * Created by alvaro on 5/2/17.
 */
public interface Clientable {
    void setDataCreator(DataSendable dataCreator);

    void connect() throws IOException;
    void send() throws IOException;
    boolean isConnected();
}
