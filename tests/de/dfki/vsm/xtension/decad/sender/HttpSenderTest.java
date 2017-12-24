package de.dfki.vsm.xtension.decad.sender;

import org.junit.jupiter.api.Test;
import sun.net.www.http.HttpClient;

import static org.junit.jupiter.api.Assertions.*;

class HttpSenderTest {

    private HttpSender sender;

    @Test
    void shouldHaveHostDefined() {
        makeDefaultSender();
        assertEquals("http://localhost:5005", sender.getHost() );
    }

    @Test
    void shouldHaveUTF8Charset() {
        makeDefaultSender();
        assertEquals("UTF-8", sender.getCharSet());
    }

    private void makeDefaultSender() {
        sender = new HttpSender();
    }

    @Test
    void sendGetRequestWithoutWaiting() {
        makeDefaultSender();
        sender.get("/animate/Waving");

    }
}