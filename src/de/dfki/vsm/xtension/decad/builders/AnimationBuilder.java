package de.dfki.vsm.xtension.decad.builders;

import static de.dfki.vsm.xtension.decad.Constants.URL;
import static de.dfki.vsm.xtension.decad.Constants.URL_PATH_SEPARATOR;

public class AnimationBuilder {

    public static final String ANIMATE_PATH = "animate";
    public static final String ANIMATION_PATH = "animation";
    private String url;

    public AnimationBuilder() {
        url = URL;
    }

    public AnimationBuilder animation() {
        url += URL_PATH_SEPARATOR + ANIMATION_PATH;
        return this;
    }

    public AnimationBuilder animate(String animationName) {
        url += URL_PATH_SEPARATOR + ANIMATE_PATH + URL_PATH_SEPARATOR + animationName;
        return this;
    }

    public String build() {
        return url;
    }
}
