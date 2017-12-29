package de.dfki.vsm.util.http;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;

class HttpResponse {
    private static final int OK_STATUS = 200;
    private final BufferedReader inputStream;
    private final HttpURLConnection connection;
    private StringBuffer responseBuilder;
    private int status = -1;
    private boolean isDone = false;

    public HttpResponse(HttpURLConnection httpURLConnection) throws IOException {
        this.connection = httpURLConnection;
        this.inputStream = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        responseBuilder = new StringBuffer();
    }

    public void collectResponse() throws IOException {
        fetchStatusFromConnection();
        appendLinesFromStream();
        inputStream.close();
        isDone = true;
    }

    int getStatus() {
        if(this.status <= 0){
            fetchStatusFromConnection();
        }
        return  this.status;
    }

    public boolean isDone(){
        return isDone;
    }

    public boolean wasRequestOk(){
        return getStatus() == OK_STATUS;
    }

    public String getResponse(){
        return responseBuilder.toString();
    }
    private void fetchStatusFromConnection() {
        try {
            this.status = connection.getResponseCode();
        } catch (IOException e) {
            this.status = 500;
        }
    }

    private void appendLinesFromStream() throws IOException {
        String output;
        while ((output = inputStream.readLine()) != null) {
            responseBuilder.append(output);
        }
    }
}
