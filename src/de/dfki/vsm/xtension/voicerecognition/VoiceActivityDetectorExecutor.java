package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import main.voiceactivitydetector.VoiceActivityDetector;
import main.voiceactivitydetector.observer.VoiceNotifiable;

/**
 * Created by alvaro on 7/6/17.
 */
public class VoiceActivityDetectorExecutor extends RunTimePlugin {

    final VoiceActivityDetector voiceActivityDetector ;
    private String variableName = "speaking";
    private VADNotifiable notifiable;


    public VoiceActivityDetectorExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        voiceActivityDetector = new VoiceActivityDetector();
    }

    @Override
    public void launch() {
        notifiable = new VADNotifiable();
        voiceActivityDetector.register(notifiable);
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                voiceActivityDetector.startListening();
            }
        });
        thread.start();
    }

    @Override
    public void unload() {
        voiceActivityDetector.unregister(notifiable);
    }

    private class VADNotifiable implements VoiceNotifiable{
        private boolean currentStatus = false;
        long silenceStart = System.currentTimeMillis();
        long silenceEnd = System.currentTimeMillis();

        @Override
        public void handleSpeakingActivity(boolean speaking) {
            if(mProject.hasVariable(variableName)){
                if(!speaking){
                    silenceEnd = System.currentTimeMillis();
                    long tDelta = silenceEnd - silenceStart;
                    if(tDelta >= 1500){
                        silenceStart = System.currentTimeMillis();
                        currentStatus = false;
                        mProject.setVariable(variableName, false);
                    }
                }

                if(speaking){
                    mProject.setVariable(variableName, true);
                    currentStatus = true;
                    silenceStart = System.currentTimeMillis();
                }

            }
        }
    }
}
