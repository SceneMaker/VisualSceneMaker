package de.dfki.vsm.xtension.voicerecognition.plugins.variablesetter;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import de.dfki.vsm.xtension.voicerecognition.plugins.VRPlugin;

/**
 * Created by alvaro on 6/21/17.
 */
public class VADPlugin implements VRPlugin, EventListener {

    private final RunTimeProject project;
    private String variableName = "speaking";
    private final EventDispatcher dispatcher = EventDispatcher.getInstance();
    public VADPlugin(RunTimeProject project){
        this.project = project;
    }

    @Override
    public void startPlugin() {
        dispatcher.register(this);
    }

    @Override
    public void stopPlugin() {
        dispatcher.remove(this);
    }


    @Override
    public void update(EventObject event) {
        if(event instanceof VoiceRecognitionEvent){
            if(project.hasVariable(variableName)){
                project.setVariable(variableName, true);
            }
        }
    }
}
