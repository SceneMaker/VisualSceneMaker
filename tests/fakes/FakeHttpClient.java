package fakes;

import de.dfki.vsm.util.http.HttpClient;
import de.dfki.vsm.util.http.PostParametersBuilder;

public class FakeHttpClient implements HttpClient {
    private String customUrl;

    @Override
    public HttpClient openUrl(String url) {
        this.customUrl = url;
        return this;
    }

    @Override
    public HttpClient get() {
        return this;
    }

    @Override
    public boolean wasRequestSuccessful() {
        return true;
    }

    @Override
    public HttpClient read() {
        return this;
    }

    @Override
    public String getResponse() {
        return null;
    }

    @Override
    public HttpClient post(PostParametersBuilder parameters) {
        return this;
    }

    public String getUrl() {
        return customUrl;
    }
}
