package de.dfki.vsm.xtension.voicerecognition.plugins.variablesetter;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import de.dfki.vsm.xtension.voicerecognition.plugins.VRPlugin;

/**
 * Created by alvaro on 6/21/17.
 */
public class VariablePlugin implements VRPlugin, EventListener {

    private final RunTimeProject project;
    private final EventDispatcher dispatcher = EventDispatcher.getInstance();
    private String variableName = "spokenText";

    public VariablePlugin(RunTimeProject project) {
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
        if (event instanceof VoiceRecognitionEvent) {
            if (project.hasVariable(variableName)) {
                String spokenText = ((VoiceRecognitionEvent) event).getText();
                project.setVariable(variableName, spokenText);
            }
        }
    }
}
