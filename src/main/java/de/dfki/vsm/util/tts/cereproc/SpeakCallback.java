package de.dfki.vsm.util.tts.cereproc;

import com.cereproc.cerevoice_eng.SWIGTYPE_p_CPRC_abuf;
import com.cereproc.cerevoice_eng.TtsEngineCallback;
import com.cereproc.cerevoice_eng.cerevoice_eng;

import javax.sound.sampled.SourceDataLine;

/**
 * Created by alvaro on 25/06/16.
 */
// In CereVoice, a callback can be specified to receive data every
// time a phrase has been synthesised. The user application can
// process that data safely, as the engine does not continue until the
// user returns from the callback.  Ideally, the processing done in
// the callback itself should be kept simple, such as sending audio
// data to the audio player, or sending timed phonetic information to
// a seperate thread for lip animation on a talking head.
class SpeakCallback extends TtsEngineCallback {
    private SourceDataLine line;


    // Initialise with an audio  line for playback
    public SpeakCallback(SourceDataLine line) {
        this.line = line;
    }

    // Process the data in the callback.  Here we convert audio from
    // shorts to bytes to play in Java.  The callback function
    // receives a CereVoice audio buffer object, extracts the audio
    // and plays it.
    public void Callback(SWIGTYPE_p_CPRC_abuf abuf) {
        System.out.println("INFO: firing engine callback");
        int i, sz;
        // sz is the number of 16-bits samples
        System.out.println("INFO: checking audio size");
        sz =  cerevoice_eng.CPRC_abuf_wav_sz(abuf);
        byte[] b = new byte[sz * 2];
        short s;
        // This is not the most elegant way to do this conversion, but shows
        // how e.g. audio effects could be applied.
        for(i = 0; i < sz; i++) {
            // Sample at position i, a short
            s = cerevoice_eng.CPRC_abuf_wav(abuf, i);
            // The sample is written in Big Endian to the buffer
            b[i * 2] = (byte) ((s & 0xff00) >> 8);
            b[i * 2 + 1] = (byte) (s & 0x00ff);
        }
        // Send the audio data to the Java audio player
        line.write(b, 0, sz * 2);
    }
}
