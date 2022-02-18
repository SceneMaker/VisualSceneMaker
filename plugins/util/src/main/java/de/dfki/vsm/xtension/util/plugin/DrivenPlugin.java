package de.dfki.vsm.xtension.util.plugin;

import de.dfki.vsm.runtime.activity.ActionActivity;

public interface DrivenPlugin extends Plugin {
    void execute(ActionActivity activity);
}
