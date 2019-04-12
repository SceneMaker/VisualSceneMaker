package de.dfki.vsm.util.xtensions.observers;

import de.dfki.vsm.event.EventObject;

/**
 * Created by alvaro on 6/6/17.
 */
public class VoiceRecognitionEvent extends EventObject {
    private String text;

    public VoiceRecognitionEvent(Object source) {
        super(source);
        this.text = (String) source;
    }

    public String getText() {
        return text;
    }

    public void setText(String msg) {
        this.text = msg;
    }
}
