package de.dfki.vsm.util.tts;

import de.dfki.action.sequence.TimeMark;
import de.dfki.action.sequence.Word;
import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.stickman.Stickman;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.xtension.stickmanmarytts.action.ActionMouthActivity;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.I4GMaryClient;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.MaryStickmanPhonemes;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.VoiceName;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence.Phoneme;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/24/16.
 */
public class MaryTTsSpeaker {
    private SpeechActivity speech;
    private String langVoice;
    private VoiceName voiceName;
    private String gender;
    private LinkedList blockText = new LinkedList(); //Comes from speechActivity
    MaryStickmanPhonemes maryPhonemes = new MaryStickmanPhonemes();
    HashMap<Integer, LinkedList<Phoneme>> phonemes = new HashMap<>();
    private I4GMaryClient maryTTs;
    public MaryTTsSpeaker(SpeechActivity pSpeech, String pLanguage, VoiceName pVoiceName){
        speech = pSpeech;
        langVoice = pLanguage;
        voiceName = pVoiceName;
        maryPhonemes =  new MaryStickmanPhonemes();
        initMaryClientInstance();
    }

    public MaryTTsSpeaker(SpeechActivity pSpeech, String pLanguage, VoiceName pVoiceName, MaryStickmanPhonemes phonemesList){
        speech = pSpeech;
        langVoice = pLanguage;
        voiceName = pVoiceName;
        maryPhonemes =  phonemesList;
        initMaryClientInstance();
    }

    public MaryTTsSpeaker(SpeechActivity pSpeech, String pLanguage, VoiceName pVoiceName, String pGender){
        speech = pSpeech;
        langVoice = pLanguage;
        voiceName = pVoiceName;
        gender = pGender;
        initMaryClientInstance();
    }

    public MaryTTsSpeaker(LinkedList pBlockText, String pLanguage, VoiceName pVoiceName){
        blockText = pBlockText;
        langVoice = pLanguage;
        voiceName = pVoiceName;
        maryPhonemes =  new MaryStickmanPhonemes();
        initMaryClientInstance();
    }

    private void initMaryClientInstance(){
        if (maryTTs == null) {
            try {
                maryTTs = I4GMaryClient.instance();
            } catch (Exception e) {
                System.out.println("MaryTT not initiated yet");
            }

        }
    }

    public void setSpeechActivity(SpeechActivity pSpeech){
        speech = pSpeech;
    }

    public LinkedList getSpeechActivityTextBlocs(){
        return speech.getBlocks();
    }

    public LinkedList<Phoneme> getWordPhonemeList(int index){
        LinkedList<Phoneme> wordPhonemes = new LinkedList<>();
        if(phonemes.size() <= 0){
            phonemes = maryPhonemes.getPhonemesAndMouthPosition(speech, voiceName, langVoice);
        }
        if(phonemes.containsKey(index)) {
            wordPhonemes = phonemes.get(index);
        }
        return wordPhonemes;
    }

    public String speak(String executionId) throws Exception {
        String textToSepak = "";
        try {
            addWordsToMaryClient();
            textToSepak = maryTTs.getText();
            if(textToSepak.length()>0) {
                maryTTs.speak(getGenderTypeFromString(), executionId, voiceName, langVoice);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
        return  textToSepak;
    }

    public WordTimeMarkSequence getWordTimeSequence(){
        WordTimeMarkSequence wts = new WordTimeMarkSequence(speech.getTextOnly("$"));
        LinkedList blocks = speech.getBlocks();
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                Word w = new Word(item.toString());
                wts.add(w);
            } else {
                wts.add(new TimeMark(item.toString()));
            }
        }
        return wts;
    }

    private void addWordsToMaryClient(){
        LinkedList blocks = speech.getBlocks();
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                Word w = new Word(item.toString());
                maryTTs.addWord(item.toString());
            }
        }
    }

    private Stickman.TYPE getGenderTypeFromString(){
        if(gender == Stickman.TYPE.MALE.toString()){
            return Stickman.TYPE.MALE;
        }
        return Stickman.TYPE.FEMALE;
    }




}
