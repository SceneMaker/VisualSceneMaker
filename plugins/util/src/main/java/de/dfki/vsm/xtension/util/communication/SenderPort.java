package de.dfki.vsm.xtension.util.communication;

public interface SenderPort<T> extends BasicPort {
    void sendMessage(T message);

    boolean canSend();
}
