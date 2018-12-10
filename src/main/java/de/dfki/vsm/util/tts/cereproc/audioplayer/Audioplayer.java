package de.dfki.vsm.util.tts.cereproc.audioplayer;

/**
 * Created by alvaro on 10/4/16.
 */
public interface Audioplayer {
    void play() throws Exception;
    byte[] createWavAudio(int size);
}
