package de.dfki.vsm.xtension.decad.builders;

public class SpeechBuilder extends AbstractBuilder {

    private static final String SPEAK_PATH = "speak";

    public SpeechBuilder speak(String text) {
        this.add(SPEAK_PATH);
        this.add(text);
        return this;
    }

    public SpeechBuilder speak() {
        this.add(SPEAK_PATH);
        return this;
    }
}
