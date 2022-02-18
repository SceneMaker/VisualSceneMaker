package de.dfki.vsm.xtension.decad.url.builders;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SpeechBuilderTest {


    private SpeechBuilder builder;

    @BeforeEach
    public void before() {
        builder = new SpeechBuilder();
    }
    @Test
    void shouldReturnSpeechURL() {

        String text = "hello world";
        String urlToSpeak = builder
                .speech()
                .speak(text)
                .build();
        assertEquals("http://localhost:5005/speech/speak/hello world", urlToSpeak);
    }

    @Test
    void shouldReturnIsSpeakingUrl() {
        String isSpeakingUrl = builder
                .speech()
                .isSpeaking()
                .build();
        assertEquals("http://localhost:5005/speech/isSpeaking", isSpeakingUrl);
    }
}