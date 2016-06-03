package de.dfki.vsm.xtesting.NewPropertyManager.model.tableView;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.Iterator;

/**
 * Created by alvaro on 6/3/16.
 */
public class PluginTableConfig extends TableConfig{
    private PluginConfig plugin;
    public PluginTableConfig(PluginConfig pluginConfig){
        plugin = pluginConfig;
    }

    public PluginTableConfig(String pKey, String pValue, PluginConfig pluginConfig){
        plugin = pluginConfig;
        key = pKey;
        value = pValue;
    }

    public PluginConfig getPluginConfig(){
        return plugin;
    }

    public void saveEntry(){
        plugin.setProperty(key, value);
    }

    public void removeProperty(String oldKey){
        try {
            ConfigFeature feature = getConfig(oldKey);
            plugin.remove(feature);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private ConfigFeature getConfig(String key) throws Exception {
        boolean found = false;
        ConfigFeature configFeature = null;
        Iterator it = getPluginConfig().getEntryList().iterator();
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
