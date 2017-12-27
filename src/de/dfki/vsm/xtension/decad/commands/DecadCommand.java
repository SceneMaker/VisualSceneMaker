package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.util.http.HttpClient;

public abstract class DecadCommand {
    protected final AbstractActivity activity;
    protected HttpClient httpClient;

    public DecadCommand(AbstractActivity activity) {
        this.activity = activity;
    }

    public abstract void execute();

    public void setHttpClient(HttpClient httpClient) {
        this.httpClient = httpClient;
    }
}
