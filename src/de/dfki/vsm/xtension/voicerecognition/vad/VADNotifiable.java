package de.dfki.vsm.xtension.voicerecognition.vad;

import de.dfki.vsm.runtime.project.RunTimeProject;
import main.voiceactivitydetector.observer.VoiceNotifiable;

/**
 * Created by alvaro on 7/7/17.
 */
public class VADNotifiable implements VoiceNotifiable {

    private final RunTimeProject project;
    private final String variableName;
    private boolean currentStatus = false;
    private Ticker ticker;
    public VADNotifiable(RunTimeProject project, String variableName){
        this.project = project;
        this.variableName = variableName;
        ticker = new Ticker();
    }



    @Override
    public void handleSpeakingActivity(boolean speaking) {
        if(project.hasVariable(variableName)){
            if(!speaking){
                if(ticker.tick() >= 1500){
                    ticker.markStartTick();
                    currentStatus = false;
                    project.setVariable(variableName, false);
                }
            }

            if(speaking){
                project.setVariable(variableName, true);
                currentStatus = true;
                ticker.markStartTick();
            }

        }
    }
}
