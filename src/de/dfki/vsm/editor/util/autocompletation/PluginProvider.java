package de.dfki.vsm.editor.util.autocompletation;

import de.dfki.vsm.editor.project.auxiliary.scenescript.ScriptEditorPane;
import org.fife.ui.autocomplete.CompletionProvider;

import java.util.ArrayList;
import java.util.HashMap;

public class PluginProvider {
    //Todo make close to register

    private static PluginProvider instance;
    private HashMap<String, ArrayList> pluginProviders;

    private PluginProvider(){
        pluginProviders = new HashMap<>();
    }

    public static PluginProvider getInstance(){
        if(instance == null){
            instance = new PluginProvider();
        }
        return instance;
    }

    public static CompletionProvider getProvider(ScriptEditorPane mEditorPane){
        return new PluginCompletionProvider(getInstance().pluginProviders, mEditorPane);
    }

    public void registerProvider(String characterName, ArrayList<String> availableCompletions){
        pluginProviders.put(characterName, availableCompletions);
    }

    public void unregisterProvider(String characterName){
        if(pluginProviders.containsKey(characterName)){
            pluginProviders.remove(characterName);
        }
    }


}
