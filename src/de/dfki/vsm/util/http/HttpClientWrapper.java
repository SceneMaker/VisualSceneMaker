package de.dfki.vsm.util.http;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpClientWrapper {

    private final String charset;
    private  String url;
    private HttpURLConnection connection;
    private  HttpResponse response;

    public HttpClientWrapper(){

        charset = "UTF-8";
    }

    public HttpClientWrapper(String charset){

        this.charset = charset;
    }

    public HttpClientWrapper openUrl(String url) {
        this.url = url;
        tryToOpenConnection();
        return this;
    }

    public HttpClientWrapper get() throws IOException {
        connection.setRequestMethod("GET");
        this.response = new HttpResponse(this.connection);
        response.collectResponse();
        return this;
    }

    private void tryToOpenConnection()  {
        try {
             openConnection();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void openConnection() throws IOException {
        connection = (HttpURLConnection) new URL(url).openConnection();
        connection.setRequestProperty("Accept-Charset", charset);
    }


    private void waitForResponse() throws InterruptedException {
        while (!response.isDone()) {
            Thread.sleep(100);
        }
    }

    public boolean wasRequestSuccessful() {
        return response.wasRequestOk();
    }

    public HttpClientWrapper read() throws InterruptedException {
        waitForResponse();
        connection.disconnect();
        return this;
    }

    public String getResponse() {
        return response.getResponse();
    }
}
