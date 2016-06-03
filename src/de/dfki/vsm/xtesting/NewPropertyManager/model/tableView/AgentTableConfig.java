package de.dfki.vsm.xtesting.NewPropertyManager.model.tableView;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryAgent;

import java.util.Iterator;

/**
 * Created by alvaro on 6/3/16.
 */
public class AgentTableConfig  extends TableConfig{

    private AgentConfig agent;
    public AgentTableConfig(AgentConfig pAgent){
        agent = pAgent;
    }

    public AgentTableConfig(String pKey, String pValue, AgentConfig pAgent){
        agent = pAgent;
        key = pKey;
        value = pValue;
    }

    public AgentConfig getAgentConfig(){
        return agent;
    }

    public void saveEntry(){
        agent.setProperty(key, value);
    }

    public void removeProperty(String oldKey){
        try {
            ConfigFeature feature = getConfig(oldKey);
            agent.remove(feature);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private ConfigFeature getConfig(String key) throws Exception {
        boolean found = false;
        ConfigFeature configFeature = null;
        Iterator it = getAgentConfig().getEntryList().iterator();
        while (it.hasNext() && !found){
            ConfigFeature feat = (ConfigFeature) it.next();
            if(feat.getKey().equals(key)){
                found = true;
                configFeature = feat;
            }
        }
        if(!found){
            throw new Exception("Feature not found");
        }
        return configFeature;
    }




}
