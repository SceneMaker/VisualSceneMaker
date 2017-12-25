package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;

public abstract class DecadCommand {
    private final AbstractActivity activity;

    public DecadCommand(AbstractActivity activity) {
        this.activity = activity;
    }
}
