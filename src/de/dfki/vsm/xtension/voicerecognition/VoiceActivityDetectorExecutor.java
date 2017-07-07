package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicerecognition.vad.VADNotifiable;
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
        notifiable = new VADNotifiable(mProject, variableName);
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

}
