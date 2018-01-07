package de.dfki.vsm.xtension.decad.commands;

import fakes.FakeHttpClient;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class IsSpeakingCommandTest {
    @Test
    void shouldExecuteCommand() throws IOException, InterruptedException {
        DecadCommand isSpeaking = new IsSpeakingCommand();
        FakeHttpClient fakeHttpClient = new FakeHttpClient();
        isSpeaking.setHttpClient(fakeHttpClient);
        isSpeaking.execute();
        assertEquals("http://localhost:5005/speech/isSpeaking", fakeHttpClient.getUrl());
    }
}