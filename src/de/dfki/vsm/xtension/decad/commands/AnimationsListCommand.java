package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.xtension.decad.url.builders.AnimationBuilder;

import java.io.IOException;

public class AnimationsListCommand extends DecadCommand {
    @Override
    protected String buildUrl() {
        AnimationBuilder builder = new AnimationBuilder();
        return builder
                .animation()
                .list()
                .build();
    }

    @Override
    public boolean isBlocking() {
        return false;
    }

    @Override
    public void execute() throws IOException, InterruptedException {
        super.get();

    }
}
