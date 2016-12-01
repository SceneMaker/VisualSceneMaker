package de.dfki.vsm.util.tts.marytts;

import de.dfki.common.Gender;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.util.tts.SpeakerTts;
import de.dfki.vsm.util.tts.VoiceName;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import java.util.LinkedList;

/**
 * Created by alvaro on 5/24/16.
 */
public class MaryTTsSpeaker extends SpeakerTts {

    private String langVoice;
    private VoiceName voiceName;
    private String gender;
    private LinkedList blockText = new LinkedList(); //Comes from speechActivity
    MaryStickmanPhonemes maryPhonemes = new MaryStickmanPhonemes();

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
        if (speechClient == null) {
            try {

                speechClient = I4GMaryClient.instance();
            } catch (Exception e) {
                System.out.println("MaryTT not initiated yet");
            }

        }
    }

    public void setSpeechActivity(SpeechActivity pSpeech){
        speech = pSpeech;
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
            addWords();
            textToSepak = getAsMaryClient().getText();
            if(textToSepak.length()>0) {
                getAsMaryClient().speak(getGenderTypeFromString(), executionId, voiceName, langVoice);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
        return  textToSepak;
    }



    private I4GMaryClient getAsMaryClient(){
        return (I4GMaryClient) speechClient;
    }


    private Gender.TYPE getGenderTypeFromString(){
        if(gender == Gender.TYPE.MALE.toString()){
            return Gender.TYPE.MALE;
        }
        return Gender.TYPE.FEMALE;
    }




}
