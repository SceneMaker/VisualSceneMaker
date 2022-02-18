package de.dfki.vsm.xtension.util.mocks;

import de.dfki.vsm.xtension.util.communication.ReceiveSenderPort;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class TestComm implements ReceiveSenderPort<String, String> {
    private final List<Consumer<String>> messageHandlers = new ArrayList<>();
    public List<String> messages = new ArrayList<>();

    @Override
    public void sendMessage(String message) {
        this.messages.add(message);
    }

    @Override
    public void registerMessageHandler(Consumer<String> messageHandler) {
        this.messageHandlers.add(messageHandler);
    }

    @Override
    public void registerDisconnectHandler(Runnable disconnectHandler) {

    }

    @Override
    public boolean canSend() {
        return true;
    }

    @Override
    public void stop() {

    }

    @Override
    public void start() {

    }
}
