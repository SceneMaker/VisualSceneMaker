package fakes;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeIsSpeakingCommand extends DecadCommand {
    public boolean isSpeakingResponse;
    public FakeIsSpeakingCommand(AbstractActivity activity) {
        super(activity);
        isSpeakingResponse = false;
    }

    @Override
    protected String buildUrl() {
        return null;
    }

    @Override
    public boolean isBlocking() {
        return false;
    }

    @Override
    public void execute() {

    }

    public String getResponse() {
        return isSpeakingResponse ? "1" : "0";
    }
}
