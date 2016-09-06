package de.dfki.vsm.util.tts;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.tts.cereproc.CereProgTTsSpeaker;
import de.dfki.vsm.util.tts.marytts.MaryTTsSpeaker;

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
        String voiceLang = getLanguageFromAgentConfig();
        String ttsType = pluginConfig.getProperty("tts");
        String voice = getVoiceFromAgentConfig(voiceLang);
        String licensePath = pluginConfig.getProperty("license");
        if(ttsType.equalsIgnoreCase("cereproc")){
            return new CereProgTTsSpeaker(pSpeech, voiceLang, voice, licensePath);
        }else if(ttsType.equalsIgnoreCase("marytts")){
            VoiceName voiceName = new VoiceName(voice);
            return new MaryTTsSpeaker(pSpeech, voiceLang, voiceName);
        }
        return new DummyTTsSpeaker();
    }

    private String getLanguageFromAgentConfig() {
        AgentConfig agent = getAgentConfig();
        String langVoice = agent.getProperty("default-voice");
        return langVoice;
    }

    private String getVoiceFromAgentConfig(String lang){
        AgentConfig agent = getAgentConfig();
        String voice = agent.getProperty(lang);
        return voice;
    }

    private AgentConfig getAgentConfig() {
        String actor = pSpeech.getActor();
        return mProject.getAgentConfig(actor);
    }
}
