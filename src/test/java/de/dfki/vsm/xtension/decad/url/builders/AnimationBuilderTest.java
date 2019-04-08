package de.dfki.vsm.xtension.decad.url.builders;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AnimationBuilderTest {
    @Test
    void shouldReturnWavingUrl() {
        AnimationBuilder builder = new AnimationBuilder();
        String animationName = "Waving";
        String urlToWaving = builder
                .animation()
                .animate(animationName)
                .build();
        assertEquals("http://localhost:5005/animation/animate/Waving", urlToWaving);
    }

    @Test
    void shouldReturnAnimationList() {
        AnimationBuilder builder = new AnimationBuilder();
        String urlToWaving = builder
                .animation()
                .list()
                .build();
        assertEquals("http://localhost:5005/animation/list", urlToWaving);
    }
}