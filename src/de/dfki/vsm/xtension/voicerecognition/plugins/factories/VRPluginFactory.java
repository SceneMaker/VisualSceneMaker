package de.dfki.vsm.xtension.voicerecognition.plugins.factories;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicerecognition.plugins.sender.VRPlugin;
import de.dfki.vsm.xtension.voicerecognition.plugins.sender.VRSender;

import java.util.LinkedList;

/**
 * Created by alvaro on 6/20/17.
 */
public class VRPluginFactory {
    private final PluginConfig mConfig;
    private final LinkedList<VRPlugin> plugins = new LinkedList<>();

    public VRPluginFactory(PluginConfig mConfig) {
        this.mConfig = mConfig;
    }

    public void startPlugins() {
        String pluginName = mConfig.getProperty("pluginName");
        if(pluginName.equalsIgnoreCase("VRSender")){
             VRPlugin plugin = new VRSender(mConfig);
             plugin.startPlugin();
             plugins.add(plugin);
        }
    }

    public void stopPlugins() {
        for(VRPlugin plugin: plugins){
            plugin.stopPlugin();
        }
    }
}
