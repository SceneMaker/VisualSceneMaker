package de.dfki.vsm.xtension.decad.builders;

import static de.dfki.vsm.xtension.decad.Constants.URL;
import static de.dfki.vsm.xtension.decad.Constants.URL_PATH_SEPARATOR;

public class SpeechBuilder {
    private String url;

    public SpeechBuilder() {
        url = URL;
    }

    public SpeechBuilder speak(String text) {
        url += URL_PATH_SEPARATOR + "speak" + URL_PATH_SEPARATOR + text;
        return this;
    }

    public String build() {
        return url;
    }
}
