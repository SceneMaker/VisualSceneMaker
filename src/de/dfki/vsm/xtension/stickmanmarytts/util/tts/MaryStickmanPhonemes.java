package de.dfki.vsm.xtension.stickmanmarytts.util.tts;

import de.dfki.action.sequence.Word;
import de.dfki.stickman.Stickman;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence.Phoneme;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 4/9/16.
 */
public class MaryStickmanPhonemes {
    private HashMap<Integer, LinkedList<Phoneme>> mWordPhonemes;

    private final I4GMaryClient mary = I4GMaryClient.instance();

    public  MaryStickmanPhonemes(){
        mWordPhonemes = new HashMap<>();
    }



    public LinkedList<Phoneme> getPhonemesSpeechActivity(SpeechActivity sa, Stickman.TYPE gender, VoiceName voiceName, String language){
        LinkedList<Phoneme> phonemes = new LinkedList<>();
        try {
            if(!sa.getTextOnly("$").equals("")){
                phonemes = mary.getWordPhonemeList(sa.getTextOnly("$"), gender, voiceName, language);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return phonemes;
    }

    public HashMap<Integer, LinkedList<Phoneme>> getPhonemesAndMouthPosition(SpeechActivity sa, VoiceName voiceName, String language){
        return getPhonemesAndMouthPosition(sa, null, voiceName, language);
    }

    public HashMap<Integer, LinkedList<Phoneme>> getPhonemesAndMouthPosition(SpeechActivity sa, Stickman.TYPE gender, VoiceName voiceName, String language){
        LinkedList<Phoneme> phonemes = getPhonemesSpeechActivity(sa, gender, voiceName, language);
       //For computing correctly the whole sentnce
        //Must be index related because with hashmap can happen that two words occurre in the same utterance
        LinkedList blocks = sa.getBlocks();
        int index = 0;
        int word_idx = 0;
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                Word w = new Word(item.toString());
                try {
                    LinkedList<Phoneme> wordPhonems = new LinkedList<>();
                    LinkedList<Phoneme> phonemes2 = mary.getWordPhonemeList(w.toString(), gender, voiceName, language);
                    int i = 0;
                    for (; i < phonemes2.size(); i++) {
                        wordPhonems.add(phonemes.get(index + i));
                    }
                    mWordPhonemes.put(word_idx, wordPhonems);
                    index += i;
                    word_idx++;

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return mWordPhonemes;
    }


}
