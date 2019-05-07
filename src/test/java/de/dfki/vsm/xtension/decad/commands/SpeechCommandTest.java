package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.SpeechActivity;
import fakes.FakeHttpClient;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.LinkedList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class SpeechCommandTest {
    @Test
    void shouldExecuteSpeechRequestGET() throws IOException, InterruptedException {
        LinkedList textToSpeak = new LinkedList();
        textToSpeak.add("hello");
        textToSpeak.add("world");
        SpeechCommand command = new SpeechCommand(new SpeechActivity("Test actor", textToSpeak, "."));
        FakeHttpClient client = new FakeHttpClient();
        command.setHttpClient(client);
        command.execute();
        assertEquals("http://localhost:5005/speech/speak", client.getUrl());
    }

    @Test
    void shouldHaveEmptyUrlWhenEmptyText() throws IOException, InterruptedException {
        LinkedList textToSpeak = new LinkedList();
        SpeechCommand command = new SpeechCommand(new SpeechActivity("Test actor", textToSpeak, "."));
        FakeHttpClient client = new FakeHttpClient();
        command.setHttpClient(client);
        command.execute();
        assertNull(client.getUrl());
    }
}