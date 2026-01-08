package de.dfki.vsm.util.tts;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * Created by alvaro on 7/25/16.
 */
public class TTSFactory {
    private PluginConfig pluginConfig;
    private SpeechActivity pSpeech;
    private RunTimeProject mProject;
    public TTSFactory(PluginConfig config, SpeechActivity speech, RunTimeProject project){
        pluginConfig = config;
        pSpeech = speech;
        mProject = project;
    }
    public SpeakerTts getTTs(){
        return new DummyTTsSpeaker();
    }
}
