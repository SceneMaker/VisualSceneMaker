package de.dfki.vsm.util.tts.cereproc.audioplayer;

import com.cereproc.cerevoice_eng.SWIGTYPE_p_CPRC_abuf;
import com.cereproc.cerevoice_eng.cerevoice_eng;

import javax.sound.sampled.SourceDataLine;

/**
 * Created by alvaro on 10/4/16.
 */
public class LinePlayer implements Audioplayer {
    //This uses Big Endian
    private SourceDataLine line;
    private final SWIGTYPE_p_CPRC_abuf cereprocAudioBuffer;
    public LinePlayer(SourceDataLine line, SWIGTYPE_p_CPRC_abuf abuf){
        this.line = line;
        cereprocAudioBuffer = abuf;

    }
    @Override
    public void play() {
        int size =  cerevoice_eng.CPRC_abuf_wav_sz(cereprocAudioBuffer);
        byte[] b = createWavAudio(size);
        line.write(b, 0, size * 2);
    }

    @Override
    public byte[] createWavAudio(int size) {
        int i = 0;
        System.out.println("INFO: firing engine callback");
        byte[] b = new byte[size * 2];
        short s;
        for(i = 0; i < size; i++) {
            // Sample at position i, a short
            s = cerevoice_eng.CPRC_abuf_wav(cereprocAudioBuffer, i);
            // The sample is written in Big Endian to the buffer
            b[i * 2] = (byte) ((s & 0xff00) >> 8);
            b[i * 2 + 1] = (byte) (s & 0x00ff);
        }
        return b;
    }
}
