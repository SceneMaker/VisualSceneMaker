package de.dfki.vsm.xtesting.NewPropertyManager.model;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.util.AbstractTreeItem;
import de.dfki.vsm.xtesting.NewPropertyManager.util.ContextTreeItem;

/**
 * Created by alvaro on 6/2/16.
 */
public class EntryAgent extends AbstractTreeEntry {
    private AgentConfig agentConfig;
    private AbstractTreeItem contextTreeItem;
    public EntryAgent(String pName){
        name = pName;
    }
    public void setContextTreeItem(AbstractTreeItem contextItem){
        contextTreeItem = contextItem;
    }

    public AbstractTreeItem getContextTreeItem(){
        return  contextTreeItem;
    }

    public EntryAgent(AgentConfig agent){
        agentConfig = agent;
        name = agentConfig.getAgentName();
    }

    public AgentConfig getAgentConfig(){
        return agentConfig;
    }

    public String getPluginName(){
        return agentConfig.getDeviceName();
    }

    public void setAgentConfig(AgentConfig agent){
        agentConfig = agent;
    }



}
