package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicerecognition.vad.VADNotifiable;
import main.speechrecognition.audioproviders.Audible;
import vad.moannar.VAD;
import vad.observer.VoiceActivityObserver;

/**
 * Created by alvaro on 7/6/17.
 */
public class VoiceActivityDetectorExecutor extends RunTimePlugin {

    final Audible voiceActivityDetector;
    private String variableName;
    private VADNotifiable notifiable;


    public VoiceActivityDetectorExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        voiceActivityDetector = new VAD();
        variableName = config.getProperty("variable");
    }

    @Override
    public void launch() {
        notifiable = new VADNotifiable(mProject, variableName);
        getAsObserver().register(notifiable);
        Thread thread = new Thread(voiceActivityDetector::startListening);
        thread.start();
    }

    private VoiceActivityObserver getAsObserver() {
        return (VoiceActivityObserver) voiceActivityDetector;
    }

    @Override
    public void unload() {
        getAsObserver().unregister(notifiable);
    }

}
