package fakes;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeFactory extends de.dfki.vsm.xtension.decad.factories.DecadCommandFactory {
    public FakeCommand command;

    public DecadCommand getCommand(AbstractActivity activity) {
        this.command = new FakeCommand(activity);
        return this.command;
    }
}
