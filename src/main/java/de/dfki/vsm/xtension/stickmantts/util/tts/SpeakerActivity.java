package de.dfki.vsm.xtension.stickmantts.util.tts;

import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.util.tts.SpeakerTts;
import de.dfki.vsm.util.tts.VoiceName;
import de.dfki.vsm.util.tts.marytts.MaryTTsSpeaker;


/**
 * Created by alvaro on 5/31/16.
 */
public class SpeakerActivity {
    private SpeechActivity speechActivity;
    private SpeakerTts ttsSpeak;
    private String language;
    private VoiceName voiceName;
    public SpeakerActivity(SpeechActivity sa){
        speechActivity = sa;

    }

    public SpeakerActivity(SpeechActivity sa, String lang, VoiceName voice){
        speechActivity = sa;
        language = lang;
        voiceName = voice;
        ttsSpeak = new MaryTTsSpeaker(sa, language, voice);
    }

    public SpeakerActivity(SpeakerTts pSpeak){
        ttsSpeak = pSpeak;
        speechActivity = ttsSpeak.getSpeech();

    }

    public WordTimeMarkSequence getWordTimeSequence(){
        return ttsSpeak.getWordTimeSequence();
    }

    public SpeakerTts getTtsSpeak(){
        return ttsSpeak;
    }

    public String speak(String executionId) throws Exception {
        return ttsSpeak.speak(executionId);
    }


    public SpeechActivity getSpeechActivity(){
        return speechActivity;
    }

    public void getPhonemeList(){

    }
}