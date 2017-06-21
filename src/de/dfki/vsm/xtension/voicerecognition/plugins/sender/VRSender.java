package de.dfki.vsm.xtension.voicerecognition.plugins.sender;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import de.dfki.vsm.xtension.remote.client.factories.ClientsFactory;
import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.IOException;

/**
 * Created by alvaro on 6/20/17.
 */
public class VRSender implements EventListener, VRPlugin {

    private final EventDispatcher dispatcher = EventDispatcher.getInstance();
    private final PluginConfig mConfig;
    private Clientable client;
    private int messageId = 0;

    public VRSender(PluginConfig mConfig){
        this.mConfig = mConfig;
        this.startPlugin();
    }

    @Override
    public void update(EventObject event) {
        if(event instanceof VoiceRecognitionEvent){
            sendDataToServer((VoiceRecognitionEvent) event);
        }
    }

    private void sendDataToServer(VoiceRecognitionEvent event)  {
        prepareData(event);
        Thread senderThread = new Thread(new SenderThread(client));
        senderThread.start();
    }

    private void prepareData(VoiceRecognitionEvent event) {
        String spokenText = event.getText();
        DataSendable sendable = new VRSendable(spokenText, buildMessageID());
        client.setDataCreator(sendable);
    }

    private String buildMessageID() {
        messageId++;
        return "#MessageID:" + messageId;
    }

    @Override
    public void startPlugin() {
        dispatcher.register(this);
        ClientsFactory clientsFactory = new ClientsFactory(mConfig);
        client = clientsFactory.buildClient();
    }

    @Override
    public void stopPlugin() {
        dispatcher.remove(this);
        //TODO: Close connection
    }

    private class SenderThread implements Runnable{

        private final Clientable client;

        public SenderThread(Clientable client){
            this.client = client;
        }

        @Override
        public void run() {
            try {
                client.send();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
