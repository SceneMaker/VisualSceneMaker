package fakes;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.decad.DecadExecutor;

public class FakeDecadExecutor extends DecadExecutor {
    public FakeDecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        factory = new FakeFactory();
    }


}
