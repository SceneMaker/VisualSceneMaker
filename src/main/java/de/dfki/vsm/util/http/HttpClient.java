package de.dfki.vsm.util.http;

import java.io.IOException;

public interface HttpClient {
    HttpClient openUrl(String url);

    HttpClient get() throws IOException;

    boolean wasRequestSuccessful();

    HttpClient read() throws InterruptedException;

    String getResponse();


    HttpClient post(PostParametersBuilder parameters) throws IOException;
}
