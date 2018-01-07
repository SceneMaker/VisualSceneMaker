package de.dfki.vsm.xtension.decad.sender;

import de.dfki.vsm.util.http.HttpClientWrapper;
import de.dfki.vsm.util.http.PostParametersBuilder;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class HTTPClientTest {

    @Test
    @Disabled
    public void testSendToLocalhost() throws InterruptedException, IOException {

        HttpClientWrapper client = new HttpClientWrapper();
        client
                .openUrl("http://localhost:5005/animation/list")
                .get()
                .read();
        assertEquals("[\"Salute\",\"Salsa1\",\"Waving\",\"FullBodyNodYes\",\"FullBodyShakeNo\"]", client.getResponse());
        assertTrue(client.wasRequestSuccessful());
    }


    @Test
    @Disabled
    public void testSpeakToLocalhost() throws InterruptedException, IOException {

        HttpClientWrapper client = new HttpClientWrapper();
        PostParametersBuilder parameters = new PostParametersBuilder();
        parameters.addParameter("text", "Hello world");
        client
                .openUrl("http://localhost:5005/speech/speak")
                .post(parameters)
                .read();
        assertEquals("Speaking...", client.getResponse());
        assertTrue(client.wasRequestSuccessful());
    }
}


