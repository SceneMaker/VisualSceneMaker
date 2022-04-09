package de.dfki.vsm.xtension.util.communication;

public interface ReceiveSenderPort<T, J> extends SenderPort<T>, ReceiverPort<J> {
    void registerDisconnectHandler(Runnable disconnectHandler);
}

