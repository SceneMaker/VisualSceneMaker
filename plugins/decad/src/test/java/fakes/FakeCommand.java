package fakes;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeCommand extends DecadCommand {
    public boolean executed = false;
    public FakeCommand(AbstractActivity activity) {
        super(activity);
    }

    @Override
    protected String buildUrl() {
        return "";
    }

    @Override
    public void execute() {
        executed = true;
    }
}
