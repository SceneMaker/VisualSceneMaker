package de.dfki.vsm.xtension.decad.url.builders;

public class SpeechBuilder extends AbstractBuilder {

    private static final String SPEAK_PATH = "speak";
    private static final String SPEECH_PATH = "speech";
    private static final String IS_SPEAKING_PATH = "isSpeaking";

    public SpeechBuilder speak(String textToSpeak) {
        this.add(SPEAK_PATH);
        this.add(textToSpeak);
        return this;
    }

    public SpeechBuilder speak() {
        this.add(SPEAK_PATH);
        return this;
    }

    public SpeechBuilder speech() {
        this.add(SPEECH_PATH);
        return this;
    }

    public SpeechBuilder isSpeaking() {
        this.add(IS_SPEAKING_PATH);
        return this;
    }
}
