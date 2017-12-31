package de.dfki.vsm.xtension.decad.url.builders;


public class AnimationBuilder extends AbstractBuilder {

    private static final String ANIMATE_PATH = "animate";
    private static final String ANIMATION_PATH = "animation";


    public AnimationBuilder animation() {
        this.add(ANIMATION_PATH);
        return this;
    }

    public AnimationBuilder animate(String animationName) {
        this.add(ANIMATE_PATH);
        this.add(animationName);
        return this;
    }


}
