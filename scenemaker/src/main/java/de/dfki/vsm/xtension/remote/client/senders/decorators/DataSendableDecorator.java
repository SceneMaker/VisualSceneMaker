package de.dfki.vsm.xtension.remote.client.senders.decorators;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/13/17.
 */
public abstract class DataSendableDecorator implements DataSendable {
    protected DataSendable decorable;

    public DataSendableDecorator(DataSendable decorable){
        this.decorable = decorable;
    }

    @Override
    public abstract String buildDataToSent() ;


    @Override
    public  void prepareData(){
        decorable.prepareData();
    }

    @Override
    public String buildCloseConnectionCommand() {
        return null;
    }
}
