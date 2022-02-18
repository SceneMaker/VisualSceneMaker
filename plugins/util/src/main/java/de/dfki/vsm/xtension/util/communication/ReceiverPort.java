package de.dfki.vsm.xtension.util.communication;

import java.util.function.Consumer;

public interface ReceiverPort<J> extends BasicPort {
    void registerMessageHandler(Consumer<J> messageHandler);
}
