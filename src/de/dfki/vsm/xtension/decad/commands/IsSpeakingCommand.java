package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.util.http.HttpClient;
import de.dfki.vsm.xtension.decad.url.builders.SpeechBuilder;

import java.io.IOException;

public class IsSpeakingCommand extends DecadCommand {

    private String response;

    public IsSpeakingCommand() {
        super();
    }

    @Override
    protected String buildUrl() {
        SpeechBuilder builder = new SpeechBuilder();
        return builder
                .speech()
                .isSpeaking()
                .build();
    }

    @Override
    public boolean isBlocking() {
        return false;
    }

    @Override
    public void execute() throws IOException, InterruptedException {
        HttpClient client = super.get();
        this.response = client.getResponse();
    }

    public String getResponse() {
        return this.response;
    }

}
