package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.util.http.HttpClient;

import java.io.IOException;

public abstract class DecadCommand {
    final AbstractActivity activity;
    HttpClient httpClient;

    public DecadCommand(AbstractActivity activity) {
        this.activity = activity;
    }

    public abstract void execute() throws IOException, InterruptedException;

    public void setHttpClient(HttpClient httpClient) {
        this.httpClient = httpClient;
    }
}
