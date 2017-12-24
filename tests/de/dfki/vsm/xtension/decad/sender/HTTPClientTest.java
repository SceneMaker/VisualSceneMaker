package de.dfki.vsm.xtension.decad.sender;

import de.dfki.vsm.util.http.HttpClientWrapper;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class HTTPClientTest {

    @Test
    public void testSendToLocalhost() throws InterruptedException, IOException {
        HttpClientWrapper client = new HttpClientWrapper();
        client
                .openUrl("http://localhost:5005/animation/list")
                .get()
                .read();
        assertEquals("[\"Salute\",\"Salsa1\",\"Waving\",\"FullBodyNodYes\",\"FullBodyShakeNo\"]", client.getResponse());
        assertTrue(client.wasRequestSuccessful());
    }
}


