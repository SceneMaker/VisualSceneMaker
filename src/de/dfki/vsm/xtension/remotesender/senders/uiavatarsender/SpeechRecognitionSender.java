package de.dfki.vsm.xtension.remotesender.senders.uiavatarsender;

import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.xtension.remotesender.sender.Clientable;
import de.dfki.vsm.xtension.voicerecognition.observers.VoiceRecognitionEvent;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 6/4/17.
 */
public class SpeechRecognitionSender extends AvatarSender implements EventListener {
    private final Clientable client;
    private String option;
    private final EventDispatcher eventDispatcher = EventDispatcher.getInstance();
    private OptionsSender optionSender;

    public SpeechRecognitionSender(Clientable client) {
        super();
        eventDispatcher.register(this);
        this.client = client;

    }

    public void setOption(String option){
        this.option = option;
    }

    @Override
    public void prepareData() {
        options.clear();
        options.add(option);
    }

    @Override
    protected String getSendingType() {
        return "selection";
    }

    @Override
    public void update(EventObject event) {
        if(event instanceof VoiceRecognitionEvent){
            sendToServer();
        }
    }

    private void sendToServer() {
        client.setDataCreator(this);
        try {
            client.send();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void setOptionSender(OptionsSender optionSender) {
        this.optionSender = optionSender;
    }


}
