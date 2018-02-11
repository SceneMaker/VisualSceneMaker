package de.dfki.vsm.editor.util.autocompletation;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.util.extensions.ExportableCompletion;

import java.util.ArrayList;

public class AutoCompletionPluginDiscover {
    private final EditorProject project;

    public AutoCompletionPluginDiscover(EditorProject project) {
        this.project = project;
    }

    public void discover(){
        for (AgentConfig agent: project.getProjectConfig().getAgentConfigList()) {
            String agentName = agent.getAgentName();
            ArrayList<String> actions = getActionsForAgent(agent);
            PluginProvider.getInstance().registerProvider(agentName, actions);
        }
    }

    private ArrayList<String> getActionsForAgent(AgentConfig agent) {
        ArrayList<String> actions = new ArrayList<>();
        ActivityExecutor device = project.getAgentDevice(agent.getAgentName());
        if(device instanceof ExportableCompletion){ //TODO: Try to remove this! It isn't clean
            actions = (ArrayList<String>) ((ExportableCompletion) device).getExportableActions();
        }
        return actions;
    }
}
