package de.dfki.vsm.xtension.util.plugin;

import de.dfki.vsm.runtime.activity.SpeechActivity;

public interface AgentPlugin extends DrivenPlugin {
    void execute(SpeechActivity activity);
}
