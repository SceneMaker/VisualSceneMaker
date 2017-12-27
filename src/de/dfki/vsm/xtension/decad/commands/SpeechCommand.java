package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.xtension.decad.builders.SpeechBuilder;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;


public class SpeechCommand extends DecadCommand {


    public SpeechCommand(AbstractActivity activity) {

        super(activity);
    }

    @Override
    public void execute() throws IOException, InterruptedException {
        httpClient.openUrl(buildSpeechUrl())
                .get()
                .read();
    }

    private String buildSpeechUrl() {
        SpeechBuilder builder = new SpeechBuilder();
        return builder
                .speak(this.getSpokeText())
                .build();
    }

    @NotNull
    private String getSpokeText() {
        return getSpeechActivity().getTextOnly("$");
    }

    @Contract(pure = true)
    @NotNull
    private SpeechActivity getSpeechActivity() {
        return (SpeechActivity) activity;
    }
}
