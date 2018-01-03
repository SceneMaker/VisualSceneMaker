package de.dfki.vsm.xtension.decad.url.builders;

public class SpeechBuilder extends AbstractBuilder {

    private static final String SPEAK_PATH = "speak";
    private static final String SPEECH_PATH = "speech";

    public SpeechBuilder speak(String textToSpeak) {
        this.add(SPEECH_PATH);
        this.add(SPEAK_PATH);
        this.add(textToSpeak);
        return this;
    }

    public SpeechBuilder speak() {
        this.add(SPEECH_PATH);
        this.add(SPEAK_PATH);
        return this;
    }
}
