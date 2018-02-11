package de.dfki.vsm.util.http;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

import static de.dfki.vsm.xtension.decad.utils.constants.Constants.UTF_8;

public class HttpClientWrapper implements HttpClient {

    private final String charset;
    private  String url;
    private HttpURLConnection connection;
    private  HttpResponse response;

    public HttpClientWrapper(){

        charset = UTF_8;
    }

    public HttpClientWrapper(String charset){

        this.charset = charset;
    }

    public HttpClientWrapper openUrl(String url) {
        this.url = url;
        tryToOpenConnection();
        return this;
    }

    public HttpClient get() throws IOException {
        connection.setRequestMethod("GET");
        makeRequest();
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

    public HttpClient read() throws InterruptedException {
        waitForResponse();
        connection.disconnect();
        return this;
    }

    public String getResponse() {
        return response.getResponse();
    }

    @Override
    public HttpClient post(PostParametersBuilder parameters) throws IOException {
        connection.setRequestMethod("POST");
        byte[] postDataBytes = parameters.build(charset);
        connection.setRequestProperty("Content-Length", String.valueOf(postDataBytes.length));
        connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        connection.setDoOutput(true);
        connection.getOutputStream().write(postDataBytes);
        makeRequest();
        return this;
    }

    private void makeRequest() throws IOException {
        this.response = new HttpResponse(this.connection);
        response.collectResponse();
    }
}
