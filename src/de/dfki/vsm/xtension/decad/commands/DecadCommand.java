package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.util.http.HttpClient;
import de.dfki.vsm.util.http.HttpClientWrapper;
import de.dfki.vsm.util.http.PostParametersBuilder;

import java.io.IOException;

public abstract class DecadCommand {
    AbstractActivity activity;
    private HttpClient httpClient;

    public DecadCommand(AbstractActivity activity) {
        this.activity = activity;
        this.httpClient = new HttpClientWrapper();
    }

    public DecadCommand() {
        this.httpClient = new HttpClientWrapper();
    }

    protected HttpClient get() throws IOException, InterruptedException {
        return httpClient.openUrl(buildUrl())
                .get()
                .read();
    }

    HttpClient post(PostParametersBuilder parameters) throws IOException, InterruptedException {
        return httpClient.openUrl(buildUrl())
                .post(parameters)
                .read();
    }

    protected abstract String buildUrl();

    public abstract boolean isBlocking();

    public abstract void execute() throws IOException, InterruptedException;

    public void setHttpClient(HttpClient httpClient) {
        this.httpClient = httpClient;
    }

}
