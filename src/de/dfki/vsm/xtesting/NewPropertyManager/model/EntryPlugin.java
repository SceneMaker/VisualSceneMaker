package de.dfki.vsm.xtesting.NewPropertyManager.model;

import de.dfki.vsm.model.project.PluginConfig;

import java.util.LinkedList;

/**
 * Created by alvaro on 6/2/16.
 */
public class EntryPlugin extends AbstractTreeEntry {
    private LinkedList<EntryAgent> agents = new LinkedList<>();
    private PluginConfig pluginConfig;
    public EntryPlugin(String pName){
        name = pName;
    }

    public EntryPlugin(PluginConfig pConfig){
        pluginConfig = pConfig;
        name = pluginConfig.getPluginName();
    }

    public void addAgent(EntryAgent agent){
        agents.add(agent);
    }

    public LinkedList<EntryAgent> getAgents(){
        return agents;
    }

    public PluginConfig getPluginConfig(){
        return pluginConfig;
    }

    public String getPluginClassName(){
        return pluginConfig.getClassName();
    }

    public void setPluginConfig(PluginConfig plugin){
        pluginConfig = plugin;
    }
}
