package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.url.builders.AnimationBuilder;

import java.io.IOException;


public class AnimationCommand extends DecadCommand {


    public AnimationCommand(AbstractActivity activity) {
        super(activity);
    }

    @Override
    protected String buildUrl() {
        AnimationBuilder builder = new AnimationBuilder();
        return builder
                .animation()
                .animate(activity.getName())
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
