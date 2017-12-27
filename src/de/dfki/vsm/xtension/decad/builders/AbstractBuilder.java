package de.dfki.vsm.xtension.decad.builders;

import static de.dfki.vsm.xtension.decad.Constants.URL;
import static de.dfki.vsm.xtension.decad.Constants.URL_PATH_SEPARATOR;

public class AbstractBuilder {
    protected StringBuilder url;

    public AbstractBuilder() {
        this.url = new StringBuilder();
        this.url.append(URL);
    }

    protected void add(String path) {
        this.url.append(URL_PATH_SEPARATOR);
        this.url.append(path);
    }

    public String build() {
        return url.toString();
    }
}
