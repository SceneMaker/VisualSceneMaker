package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.util.http.PostParametersBuilder;
import de.dfki.vsm.xtension.decad.url.builders.SpeechBuilder;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;


public class SpeechCommand extends DecadCommand {


    public SpeechCommand(AbstractActivity activity) {

        super(activity);
    }

    @Override
    public void execute() throws IOException, InterruptedException {
        if (this.getSpokenText().isEmpty()) {
            return;
        }
        speak();
    }

    private void speak() throws InterruptedException, IOException {
        PostParametersBuilder parameters = createPostParameters();
        httpClient.openUrl(buildSpeechUrl())
                .post(parameters)
                .read();
    }

    @NotNull
    private PostParametersBuilder createPostParameters() {
        PostParametersBuilder parameters = new PostParametersBuilder();
        parameters.addParameter("text", this.getSpokenText());
        return parameters;
    }

    private String buildSpeechUrl() {
        SpeechBuilder builder = new SpeechBuilder();
        return builder
                .speak()
                .build();
    }

    @NotNull
    private String getSpokenText() {
        return getSpeechActivity().getTextOnly("$");
    }

    @Contract(pure = true)
    @NotNull
    private SpeechActivity getSpeechActivity() {
        return (SpeechActivity) activity;
    }
}
