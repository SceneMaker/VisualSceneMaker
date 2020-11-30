package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.xtension.decad.url.builders.SpeechBuilder;

import java.io.IOException;

public class IsSpeakingCommand extends DecadCommand {



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
    public void execute() throws IOException, InterruptedException {
        super.get();

    }


}
