package de.dfki.vsm.util.tts.cereproc;

import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.util.tts.SpeakerTts;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import java.io.UnsupportedEncodingException;
import java.util.LinkedList;

/**
 * Created by alvaro on 25/06/16.
 */
public class CereProgTTsSpeaker extends SpeakerTts {

    private String langVoice;
    private String voiceName;
    private String gender;

    public CereProgTTsSpeaker(){
    }

    public  CereProgTTsSpeaker(SpeechActivity pSpeech, String pLanguage, String pVoiceName){
        speech = pSpeech;
        langVoice = pLanguage;
        //voiceName = pVoiceName;
        speechClient = new Cereproc();
    }

    public  CereProgTTsSpeaker(SpeechActivity pSpeech, String pLanguage, String pVoiceFilePath, String pLicenseName, String audioDevice){
        speech = pSpeech;
        langVoice = pLanguage;
        speechClient = new Cereproc(pLicenseName, pVoiceFilePath, audioDevice);
    }

    @Override
    public LinkedList<Phoneme> getWordPhonemeList(int index) {
        LinkedList<Phoneme> wordPhonemes = new LinkedList<>();
        addWords();
        getAsCereproc().setText(getPhrase());
        try {
            if(phonemes.size() <= 0){
                phonemes = getAsCereproc().getPhonemes();
            }
            if(phonemes.containsKey(index)) {
                wordPhonemes = phonemes.get(index);
            }
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return wordPhonemes;
    }

    private Cereproc getAsCereproc() {
        return (Cereproc)speechClient;
    }


    @Override
    public String speak(String executionId) throws Exception {
        addWords();
        return getAsCereproc().speak(executionId);
    }




}
