package de.dfki.vsm.util.tts.cereproc.util;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.SourceDataLine;

/**
 * Created by alvaro on 7/31/16.
 */

// Simple class to wrap a Java audio line
public class Audioline {
    private SourceDataLine line;
    private AudioFormat format;

    public Audioline(float sampleRate){
        format =  getAudioFormat(sampleRate);
        DataLine.Info info = new DataLine.Info(SourceDataLine.class,
                format);
        // Obtain and open the line.
        try {
            line = (SourceDataLine) AudioSystem.getLine(info);
            line.open(format);
            line.start();
        }catch (Exception e){
            System.err.println("Error: " + e.getMessage());
        }
    }

    public SourceDataLine line(){
        return line;
    }

    private static AudioFormat getAudioFormat(float sampleRate){
        int sampleSizeInBits = 16;
        int channels = 1;
        boolean signed = true;
        boolean bigEndian = true;
        return new AudioFormat(
                sampleRate,
                sampleSizeInBits,
                channels,
                signed,
                bigEndian);
    }

    public void flush(){
        line.drain();
        line.close();
    }
}
