package fakes;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.decad.DecadExecutor;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;

public class FakeDecadExecutor extends DecadExecutor {
    public boolean isBlocked = true;
    public FakeDecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        factory = new FakeFactory();
    }

    public DecadCommand getExecutedCommand() {
        return ((FakeFactory) factory).command;
    }

    public void handle() {
        isBlocked = false;
    }


}
