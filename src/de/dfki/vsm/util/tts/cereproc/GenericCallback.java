package de.dfki.vsm.util.tts.cereproc;

import com.cereproc.cerevoice_eng.*;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.tts.cereproc.audioplayer.Audioplayer;
import de.dfki.vsm.util.tts.cereproc.audioplayer.ClipPlayer;
import de.dfki.vsm.util.tts.cereproc.audioplayer.LinePlayer;
import de.dfki.vsm.util.tts.cereproc.phonemes.ScottishPhoneme;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStart;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import javax.sound.sampled.SourceDataLine;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 2/07/16.
 */
/*The Callback function is fired for every phrase returned by the synthesiser.
  First collect all the phoneme data and then speak
  This callback colletcts the phoneme information so it can be cached later and the
  Sends the stream to the audio line in order to create the audio file
  */
public class GenericCallback extends TtsEngineCallback {
    private SourceDataLine line;
    private HashMap<Integer, LinkedList<Phoneme>> phonemes;
    private String executionId;
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();
    private PhrasePhonemeCache phraseCache;
    private String toSpeakPhrase;
    private String  audioDevice;

    public GenericCallback(SourceDataLine line) {
        this.line = line;
        phonemes = new HashMap<Integer, LinkedList<Phoneme>>();
    }

    public GenericCallback(SourceDataLine line, String pExecutionId, PhrasePhonemeCache cache, String phrase, String audioDevice) {
        this.line = line;
        phonemes = new HashMap<Integer, LinkedList<Phoneme>>();
        executionId = pExecutionId;
        phraseCache = cache;
        toSpeakPhrase = phrase;
        this.audioDevice = audioDevice;

    }



    public void Callback(SWIGTYPE_p_CPRC_abuf abuf) {
        System.out.println("INFO: firing engine callback");

        int word_counter = -1;
        if(phonemes.isEmpty()){
            word_counter = phonemes.size();
        }
        LinkedList<Phoneme> wordPhoneme = new LinkedList<>();
        CereprocBuff cereprocBuff = new CereprocBuff(abuf, word_counter, wordPhoneme).invoke();
        word_counter = cereprocBuff.getWord_counter();
        wordPhoneme = cereprocBuff.getWordPhoneme();
        if(word_counter >= 0) {
            phonemes.put(word_counter, wordPhoneme);
        }
        phraseCache.add(toSpeakPhrase, phonemes);
        speak(abuf);
    }

    protected Audioplayer createAudioPlayer(SWIGTYPE_p_CPRC_abuf abuf){
        if( audioDevice == null || audioDevice.equals("") || audioDevice.equals("default")){
            return new LinePlayer(line, abuf);
        }else{
            return new ClipPlayer(line.getFormat(), abuf, audioDevice);
        }
    }


    private void speak(SWIGTYPE_p_CPRC_abuf abuf){

        Audioplayer audioplayer = createAudioPlayer(abuf);
        mEventCaster.convey(new LineStart(this, executionId)); //Notify we start speaking
        try {
            audioplayer.play();
        } catch (Exception e1) {
            e1.printStackTrace();
        }

    }



    public HashMap<Integer, LinkedList<Phoneme>> getPhonemes(){
        return phonemes;
    }

    private class CereprocBuff {
        private SWIGTYPE_p_CPRC_abuf abuf;
        private int word_counter;
        private LinkedList<Phoneme> wordPhoneme;

        public CereprocBuff(SWIGTYPE_p_CPRC_abuf abuf, int word_counter, LinkedList<Phoneme> wordPhoneme) {
            this.abuf = abuf;
            this.word_counter = word_counter;
            this.wordPhoneme = wordPhoneme;
        }

        public int getWord_counter() {
            return word_counter;
        }

        public LinkedList<Phoneme> getWordPhoneme() {
            return wordPhoneme;
        }

        public CereprocBuff invoke() {
            int i;SWIGTYPE_p_CPRC_abuf_trans trans;CPRC_ABUF_TRANS_TYPE transtype;
            float start;
            float end;
            String name;
            for(i = 0; i < cerevoice_eng.CPRC_abuf_trans_sz(abuf); i++) {
                trans = cerevoice_eng.CPRC_abuf_get_trans(abuf, i);
                transtype = cerevoice_eng.CPRC_abuf_trans_type(trans);
                start = cerevoice_eng. CPRC_abuf_trans_start(trans);
                end = cerevoice_eng.CPRC_abuf_trans_end(trans);
                name = cerevoice_eng.CPRC_abuf_trans_name(trans);
                if (transtype == CPRC_ABUF_TRANS_TYPE.CPRC_ABUF_TRANS_PHONE) {
                    handlePhoneme(start, end, name);
                }
                else if (transtype == CPRC_ABUF_TRANS_TYPE.CPRC_ABUF_TRANS_WORD) {
                    handleWord(start, end, name);
                }
                else if (transtype == CPRC_ABUF_TRANS_TYPE.CPRC_ABUF_TRANS_MARK) {
                }
                else if (transtype == CPRC_ABUF_TRANS_TYPE.CPRC_ABUF_TRANS_ERROR) {
                    System.err.printf("ERROR: could not retrieve transcription at '%d'", i);
                }
            }
            return this;
        }

        private void handleWord(float start, float end, String name) {
            if(word_counter >= 0) {
                phonemes.put(word_counter, wordPhoneme);
            }
            word_counter++;
            wordPhoneme = new LinkedList<>();
        }

        private void handlePhoneme(float start, float end, String name) {
            //TODO: Make factory method for Phoneme Class
            wordPhoneme.add(new ScottishPhoneme(name, (long) (start*1000), (long) (end * 1000)));
        }
    }
}
