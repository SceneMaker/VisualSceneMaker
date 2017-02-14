package de.dfki.vsm.util.tts;

import de.dfki.vsm.util.tts.cereproc.util.CereProcTag;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import java.util.LinkedList;

/**
 * Created by alvaro on 7/25/16.
 */
public class DummyTTsSpeaker extends SpeakerTts {

    @Override
    public LinkedList<Phoneme> getWordPhonemeList(int index) {
        return new LinkedList<>();
    }

    @Override
    public String speak(String executionId) throws Exception {
        throw new UnsupportedOperationException("Dummy class does not implement this method");
    }

    @Override
    protected String processEmotionTags(String str) {
        String voice = " ";
        if (CereProcTag.vocalGesture.get(str) != null) {
            voice = " ";
            throw new UnsupportedOperationException("Gesture Not Supported ");

        } else {
            voice = str;
        }

        return voice;
    }
}
