package de.dfki.vsm.xtension.remote.client.senders.decorators;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public class NewLineSendableDecorator extends DataSendableDecorator {
    public NewLineSendableDecorator(DataSendable decorable) {
        super(decorable);
    }

    @Override
    public String buildDataToSent() {
        String dataToSend = decorable.buildDataToSent();
        return dataToSend + "\n";
    }
}
