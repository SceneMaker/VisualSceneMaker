package de.dfki.vsm.editor.util.autocompletation;

import org.fife.ui.autocomplete.BasicCompletion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.DefaultCompletionProvider;

import java.util.HashMap;
import java.util.Map;

public class PluginProvider {

    private static PluginProvider instance;
    private HashMap<String, CompletionProvider> pluginProviders;

    private PluginProvider(){
        pluginProviders = new HashMap<>();
    }

    public static PluginProvider getInstance(){
        if(instance == null){
            instance = new PluginProvider();
        }
        return instance;
    }

    public void registerProvider(String pluginName, CompletionProvider provider){
        pluginProviders.put(pluginName, provider);
    }

    public void unregisterProvider(String pluginName){
        if(pluginProviders.containsKey(pluginName)){
            pluginProviders.remove(pluginName);
        }
    }

    public CompletionProvider getProviderForPlugin(String pluginName){
        if(pluginProviders.containsKey(pluginName)){
            return pluginProviders.get(pluginName);
        }
        return getDefaultProvider();
    }

    public static CompletionProvider createProviderForAgentFromList(String characterName, HashMap<String, String> replacements){
        PluginCompletionProvider provider = new PluginCompletionProvider(characterName);
        for(Map.Entry<String, String> entry : replacements.entrySet()) {
            String replacement = entry.getKey();
            String dataType = entry.getValue();
            provider.addCompletion(new BasicCompletion(provider, replacement));

        }
        return provider;
    }

    public static void createPluginProviderAndRegisterIt(
            String pluginName
            , String characterName
            , HashMap<String, String> replacements){

        CompletionProvider provider = createProviderForAgentFromList(characterName, replacements);
        getInstance().registerProvider(pluginName, provider);
    }


    private CompletionProvider getDefaultProvider() {
        return new DefaultCompletionProvider();
    }
}
