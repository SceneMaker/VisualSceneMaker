package de.dfki.vsm.xtension.decad.url.builders;

import static de.dfki.vsm.xtension.decad.utils.constants.Constants.URL;
import static de.dfki.vsm.xtension.decad.utils.constants.Constants.URL_PATH_SEPARATOR;

public class AbstractBuilder {
    private final StringBuilder url;

    AbstractBuilder() {
        this.url = new StringBuilder();
        this.url.append(URL);
    }

    void add(String path) {
        this.url.append(URL_PATH_SEPARATOR);
        this.url.append(path);
    }

    public String build() {
        return url.toString();
    }
}
