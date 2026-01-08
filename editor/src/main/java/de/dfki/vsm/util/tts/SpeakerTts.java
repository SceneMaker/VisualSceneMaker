package de.dfki.vsm.util.tts;

import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.util.tts.sequence.Phoneme;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 25/06/16.
 */
public abstract class SpeakerTts {

    protected SpeechActivity speech;
    protected SpeechClient speechClient;
    protected HashMap<Integer, LinkedList<Phoneme>> phonemes = new HashMap<>();

    public LinkedList getSpeechActivityTextBlocs() {
        return speech.getBlocks();
    }

    public abstract LinkedList<Phoneme> getWordPhonemeList(int index);

    public abstract String speak(String executionId) throws Exception;

    public void addWords() {
        LinkedList blocks = speech.getBlocks();
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                try {
                    String str = processEmotionTags(item.toString());
                    speechClient.addWord(str);
                } catch (UnsupportedOperationException e) {
                }
//                Word w = new Word(item.toString());
//                speechClient.addWord(item.toString());
            }
        }
    }

    public String getFinalWord() {
        return speechClient.getFinalWord();
    }

    public String getPhrase() {
        return speechClient.getPhrase();
    }

    public SpeechActivity getSpeech() {
        return speech;
    }

    protected abstract String processEmotionTags(String str);

}
