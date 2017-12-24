package de.dfki.vsm.xtension.decad.sender;

public class HttpSender {
    public static final String SERVER_HOST = "http://localhost:5005";
    public static final String UTF_8 = "UTF-8";
    private String host;

    public HttpSender(){
        host = SERVER_HOST;
    }

    public String getHost() {
        return host;
    }

    public void get(String url) {

    }

    public String getCharSet() {
        return UTF_8;
    }
}
