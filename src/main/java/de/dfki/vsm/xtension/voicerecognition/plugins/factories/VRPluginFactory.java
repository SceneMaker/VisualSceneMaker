package de.dfki.vsm.xtension.voicerecognition.plugins.factories;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicerecognition.plugins.VRPlugin;
import de.dfki.vsm.xtension.voicerecognition.plugins.sender.VRSender;
import de.dfki.vsm.xtension.voicerecognition.plugins.variablesetter.VariablePlugin;

import java.util.LinkedList;

/**
 * Created by alvaro on 6/20/17.
 */
public class VRPluginFactory {
    private final PluginConfig mConfig;
    private final LinkedList<VRPlugin> plugins = new LinkedList<>();
    private final RunTimeProject project;
    private String[] pluginsNames;

    public VRPluginFactory(PluginConfig mConfig, RunTimeProject project) {

        this.project = project;
        this.mConfig = mConfig;
    }

    public void startPlugins() {
        readPlugins();
        for (String pluginName : pluginsNames) {
            VRPlugin plugin = startPlugin(pluginName);
            plugin.startPlugin();
            plugins.add(plugin);
        }

    }

    private VRPlugin startPlugin(String pluginName) {
        VRPlugin plugin = new DummyPlugin();
        if (pluginName.equalsIgnoreCase("VRSender"))
            plugin = new VRSender(mConfig);
        else if (pluginName.equalsIgnoreCase("VariablePlugin"))
            plugin = new VariablePlugin(project);

        return plugin;

    }

    private void readPlugins() {
        String pluginsConf = mConfig.getProperty("plugins");
        pluginsNames = pluginsConf.split(",");
    }

    public void stopPlugins() {
        for (VRPlugin plugin : plugins) {
            plugin.stopPlugin();
        }
    }

    private class DummyPlugin implements VRPlugin {

        @Override
        public void startPlugin() {

        }

        @Override
        public void stopPlugin() {

        }
    }
}





