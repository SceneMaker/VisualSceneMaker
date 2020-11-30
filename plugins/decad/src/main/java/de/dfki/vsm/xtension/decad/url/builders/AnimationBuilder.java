package de.dfki.vsm.xtension.decad.url.builders;


public class AnimationBuilder extends AbstractBuilder {

    private static final String ANIMATE_PATH = "animate";
    private static final String ANIMATION_PATH = "animation";
    private static final String LIST = "list";


    public AnimationBuilder animation() {
        super.add(ANIMATION_PATH);
        return this;
    }

    public AnimationBuilder animate(String animationName) {
        super.add(ANIMATE_PATH);
        super.add(animationName);
        return this;
    }


    public AnimationBuilder list() {
        super.add(LIST);
        return this;
    }
}
