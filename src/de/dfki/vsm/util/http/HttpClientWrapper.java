package de.dfki.vsm.util.http;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.Scanner;

public class HttpClientWrapper {

    private  String url;
    private final String charset;
    private String response;
    private InputStream responseStream;

    public HttpClientWrapper(){

        charset = "UTF-8";
    }

    public HttpClientWrapper(String charset){

        this.charset = charset;
    }


    private InputStream tryToOpenConnection()  {
        try {
            return openConnection();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    private void collectResponse() {
        try (Scanner scanner = new Scanner(responseStream)) {
            response = scanner.useDelimiter("\\A").next();
        }
    }

    private InputStream openConnection() throws IOException {
        URLConnection connection = new URL(url ).openConnection();
        connection.setRequestProperty("Accept-Charset", charset);
        return connection.getInputStream();
    }


    public String getResponse() {
        return response;
    }

    public void openUrl(String url) {
        this.url = url;
        responseStream = tryToOpenConnection();
    }

    public String read() {
        collectResponse();
        return  response;
    }
}
