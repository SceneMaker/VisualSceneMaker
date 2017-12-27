package de.dfki.vsm.xtension.decad.sender;

public class HttpSender {
    private static final String SERVER_HOST = "http://localhost:5005";
    private static final String UTF_8 = "UTF-8";
    private final String host;

    HttpSender() {
        host = SERVER_HOST;
    }

    public String getHost() {
        return host;
    }



    public String getCharSet() {
        return UTF_8;
    }
}
