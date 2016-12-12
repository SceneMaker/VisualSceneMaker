package de.dfki.vsm.util.tts.cereproc;


import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 7/31/16.
 */
public class PhrasePhonemeCache {
    private final HashMap<String, HashMap<Integer, LinkedList<Phoneme>>> phrasePhonemes = new HashMap<>();

    public HashMap<Integer,LinkedList<Phoneme>> retrieve(String phrase) {
        return phrasePhonemes.get(phrase);
    }

    public boolean isPhraseCached(String phrase) {
        return phrasePhonemes.containsKey(phrase);
    }

    public void add(String finalWord, HashMap<Integer, LinkedList<Phoneme>> phonemes) {
        phrasePhonemes.put(finalWord, phonemes);
    }
}
