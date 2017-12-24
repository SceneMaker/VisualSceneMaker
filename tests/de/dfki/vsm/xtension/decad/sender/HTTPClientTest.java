package de.dfki.vsm.xtension.decad.sender;

import de.dfki.vsm.util.http.HttpClientWrapper;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class HTTPClientTest {

    @Test
    public void testSendToLocalhost() throws InterruptedException, IOException {
        HttpClientWrapper client = new HttpClientWrapper();
        client.openUrl("http://localhost:5005/animation/list")
                .get();
        while (!client.wasRequestSuccessful()) {
            Thread.sleep(100);
        }
        assertEquals("[\"Salute\",\"Salsa1\",\"Waving\",\"FullBodyNodYes\",\"FullBodyShakeNo\"]", client.getResponse());
    }
}


