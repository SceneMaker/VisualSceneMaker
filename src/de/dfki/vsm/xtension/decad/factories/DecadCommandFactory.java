package de.dfki.vsm.xtension.decad.factories;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.xtension.decad.commands.AnimationCommand;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import de.dfki.vsm.xtension.decad.commands.DummyCommand;
import de.dfki.vsm.xtension.decad.commands.SpeechCommand;

public class DecadCommandFactory {
    public DecadCommand getCommand(AbstractActivity activity) {
        DecadCommand command;
        if (activity instanceof SpeechActivity) {
            command = new SpeechCommand(activity);
        } else if (activity instanceof ActionActivity) {
            command = new AnimationCommand(activity);
        } else {
            command = new DummyCommand(activity);
        }
        return command;
    }
}
