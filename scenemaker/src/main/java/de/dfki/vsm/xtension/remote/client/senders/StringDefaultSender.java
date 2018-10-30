package de.dfki.vsm.xtension.remote.client.senders;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 5/2/17.
 */
public class StringDefaultSender implements DataSendable {

    private final String message;

    public StringDefaultSender(String message) {
        this.message = message;
    }

    @Override
    public String buildDataToSent() {
        return message;
    }

    @Override
    public void prepareData() {

    }

    @Override
    public String buildCloseConnectionCommand() {
        return null;
    }
}
