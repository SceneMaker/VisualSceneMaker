package de.dfki.vsm.xtension.decad;

public class Constants {
    private static final String HOST_PORT_SEPARATOR = ":";
    public static String URL = "http://localhost:5005";
    public static final String URL_PATH_SEPARATOR = "/";
    public static final String UTF_8 = "UTF-8";

    public static final String getURL() {
        return URL;
    }

    public static void setURL(String baseUrl) {
        URL = baseUrl;
    }

    static String buildURL(String host, String port) {
        if (host == null || port == null || host.isEmpty() || port.isEmpty()) {
            return URL;
        }
        URL = host + HOST_PORT_SEPARATOR + port;
        return URL;
    }

}
