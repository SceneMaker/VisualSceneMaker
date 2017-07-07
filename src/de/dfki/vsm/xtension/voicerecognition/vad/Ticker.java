package de.dfki.vsm.xtension.voicerecognition.vad;

/**
 * Created by alvaro on 7/7/17.
 */
public class Ticker {
    private long silenceStart = System.currentTimeMillis();
    private long silenceEnd = System.currentTimeMillis();
    private long tDelta;

    public  void markStartTick(){
        silenceStart = System.currentTimeMillis();
    }

    private void markEndTick(){
        silenceEnd = System.currentTimeMillis();
    }

    public long tick(){
        markEndTick();
        tDelta = silenceEnd - silenceStart;
        return tDelta;
    }
}
