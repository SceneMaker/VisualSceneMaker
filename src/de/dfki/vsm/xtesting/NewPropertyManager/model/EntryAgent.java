package de.dfki.vsm.xtesting.NewPropertyManager.model;

import de.dfki.vsm.model.project.AgentConfig;

/**
 * Created by alvaro on 6/2/16.
 */
public class EntryAgent extends AbstractTreeEntry {
    private AgentConfig agentConfig;
    public EntryAgent(String pName){
        name = pName;
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
