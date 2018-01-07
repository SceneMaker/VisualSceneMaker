package fakes;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeIsSpeakingCommand extends DecadCommand {
    public FakeIsSpeakingCommand(AbstractActivity activity) {
        super(activity);
    }

    @Override
    protected String buildUrl() {
        return null;
    }

    @Override
    public void execute() {

    }
}
