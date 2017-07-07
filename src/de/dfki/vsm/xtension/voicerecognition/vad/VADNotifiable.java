package de.dfki.vsm.xtension.voicerecognition.vad;

import de.dfki.vsm.runtime.project.RunTimeProject;
import main.voiceactivitydetector.observer.VoiceNotifiable;

/**
 * Created by alvaro on 7/7/17.
 */
public class VADNotifiable implements VoiceNotifiable {

    public static final int SILENCE_THRESHOLD_MILLISECONDS = 1500;
    private final RunTimeProject project;
    private final String variableName;
    private Ticker ticker;
    public VADNotifiable(RunTimeProject project, String variableName){
        this.project = project;
        this.variableName = variableName;
        ticker = new Ticker();
    }



    @Override
    public void handleSpeakingActivity(boolean speaking) {
        if(project.hasVariable(variableName)){
            setVariable(speaking);
        }
    }

    private void setVariable(boolean speaking) {
        if(!speaking){
            setNotSpeaking();
        }else{
            setSpeaking();
        }
    }

    private void setNotSpeaking() {
        if(ticker.tick() >= SILENCE_THRESHOLD_MILLISECONDS){
            ticker.markStartTick();
            project.setVariable(variableName, false);
        }
    }

    private void setSpeaking() {
        project.setVariable(variableName, true);
        ticker.markStartTick();
    }
}
