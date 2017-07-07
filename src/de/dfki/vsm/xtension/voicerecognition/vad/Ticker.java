package de.dfki.vsm.xtension.voicerecognition.vad;

/**
 * Created by alvaro on 7/7/17.
 */
class Ticker {
    private long silenceStart = System.currentTimeMillis();
    private long silenceEnd = System.currentTimeMillis();

    void markStartTick(){
        silenceStart = System.currentTimeMillis();
    }

    private void markEndTick(){
        silenceEnd = System.currentTimeMillis();
    }

    long tick(){
        markEndTick();
        long tDelta = silenceEnd - silenceStart;
        return tDelta;
    }
}
