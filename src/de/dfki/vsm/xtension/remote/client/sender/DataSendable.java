package de.dfki.vsm.xtension.remote.client.sender;

/**
 * Created by alvaro on 5/2/17.
 */
public interface DataSendable {
    String buildDataToSent();
    void prepareData();
    String buildCloseConnectionCommand();
}
