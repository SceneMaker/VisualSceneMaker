package fakes;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeCommand extends DecadCommand {
    public FakeCommand(AbstractActivity activity) {
        super(activity);
    }
}
