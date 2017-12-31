package de.dfki.vsm.xtension.decad.url.builders;

public class SpeechBuilder extends AbstractBuilder {

    private static final String SPEAK_PATH = "speak";

    public SpeechBuilder speak(String textToSpeak) {
        this.add(SPEAK_PATH);
        this.add(textToSpeak);
        return this;
    }

    public SpeechBuilder speak() {
        this.add(SPEAK_PATH);
        return this;
    }
}
