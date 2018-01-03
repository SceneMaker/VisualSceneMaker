package de.dfki.vsm.xtension.decad.url.builders;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SpeechBuilderTest {
    @Test
    void shouldReturnSpeechURL() {
        SpeechBuilder builder = new SpeechBuilder();
        String text = "hello world";
        String urlToSpeak = builder
                .speak(text)
                .build();
        assertEquals("http://localhost:5005/speech/speak/hello world", urlToSpeak);
    }
}