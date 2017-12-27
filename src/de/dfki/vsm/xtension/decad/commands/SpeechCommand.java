package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;

import java.io.IOException;

import static de.dfki.vsm.xtension.decad.Constants.URL;

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
        return URL + "/speak/" + ((SpeechActivity) activity).getTextOnly("$");
    }
}
