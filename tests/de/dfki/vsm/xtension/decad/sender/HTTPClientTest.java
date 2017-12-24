package de.dfki.vsm.xtension.decad.sender;

import de.dfki.vsm.util.http.HttpClientWrapper;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.Scanner;
import static org.junit.jupiter.api.Assertions.*;
public class HTTPClientTest {

    @Test
    public void testSendToLocalhost() throws InterruptedException, IOException {
        HttpClientWrapper client = new HttpClientWrapper( );
        client.openUrl("http://localhost:5005/animation/list");
        while (client.read().equals("")){
            Thread.sleep(100);
        }
        assertEquals("[\"Salute\",\"Salsa1\",\"Waving\",\"FullBodyNodYes\",\"FullBodyShakeNo\"]", client.getResponse());
    }
}


