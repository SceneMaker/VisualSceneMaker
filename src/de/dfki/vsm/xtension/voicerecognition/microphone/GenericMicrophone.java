package de.dfki.vsm.xtension.voicerecognition.microphone;

import com.darkprograms.speech.microphone.Microphone;
import main.speechrecognition.audioproviders.Audible;
import net.sourceforge.javaflacencoder.FLACFileWriter;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.TargetDataLine;

/**
 * Created by alvaro on 7/6/17.
 */
public class GenericMicrophone implements Audible {
    private Microphone mic;


    @Override
    public void startListening() {
        mic = new Microphone(FLACFileWriter.FLAC);
    }

    @Override
    public AudioInputStream getAudioStream() {
        return null;
    }

    @Override
    public void stopListening() {
        mic.close();
    }

    @Override
    public TargetDataLine getDataLine() {
        return mic.getTargetDataLine();
    }

    @Override
    public AudioFormat getAudioFormat() {
        return mic.getAudioFormat();
    }
}
