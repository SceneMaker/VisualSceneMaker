package de.dfki.vsm.xtension.decad.builders;

public class SpeechBuilder extends AbstractBuilder {
    public SpeechBuilder speak(String text) {
        this.add("speak");
        this.add(text);
        return this;
    }


}
