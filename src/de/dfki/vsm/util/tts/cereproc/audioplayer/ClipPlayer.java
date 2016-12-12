package de.dfki.vsm.util.tts.cereproc.audioplayer;

import com.cereproc.cerevoice_eng.SWIGTYPE_p_CPRC_abuf;
import com.cereproc.cerevoice_eng.cerevoice_eng;

import javax.sound.sampled.*;
import javax.sound.sampled.Mixer.Info;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by alvaro on 10/4/16.
 */
public class ClipPlayer implements Audioplayer {

    public static final int AUDIO_FORMAT = 1;
    public static final int MONO = 1;
    public static final int BYTES_PER_SAMPLE = 2;
    public static final int HEADER_SIZE = 36;

    //This uses Little Endian

    private final AudioFormat format;
    private final SWIGTYPE_p_CPRC_abuf cereprocAudioBuffer;
    private final String deviceName;

    public ClipPlayer(AudioFormat format, SWIGTYPE_p_CPRC_abuf abuf, String device){
        this.format = format;
        cereprocAudioBuffer = abuf;
        deviceName = device;
    }

    @Override
    public void play() throws Exception {
        int size =  cerevoice_eng.CPRC_abuf_wav_sz(cereprocAudioBuffer);
        byte[] audioWav = createWavAudio(size);
        InputStream is = new ByteArrayInputStream(audioWav);
        playSoundDevice(is);
    }

    @Override
    public byte[] createWavAudio(int size) {
        int i = 0;
        byte [] header = wavHeader(size*2);
        byte[] audioBuffer = new byte[header.length + size * 2];
        short s;
        for (int j = 0; j< header.length; j++){
            audioBuffer[j] = header[j];
        }
        for(i = 0; i < size; i++) {
            // Sample at position i, a short
            s = cerevoice_eng.CPRC_abuf_wav(cereprocAudioBuffer, i);
            // The sample is written in Little Endian to the buffer
            audioBuffer[i * 2 + header.length] = (byte) (s & 0x00ff);
            audioBuffer[i * 2 + 1 + header.length] = (byte) (s  >> 8 & 0x00ff);
        }
        return audioBuffer;
    }


    private byte[] wavHeader(int size){
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        try {
            outputStream.write("RIFF".getBytes());
            outputStream.write(ClipPlayer.intToByteArray(size + HEADER_SIZE), 0, 4);		                                // 04 - how big is the rest of this file?
            outputStream.write("WAVE".getBytes());					                                            // 08 - WAVE
            outputStream.write("fmt ".getBytes());					                                            // 12 - fmt
            outputStream.write(intToByteArray(format.getSampleSizeInBits()), 0, 4);	                            // 16 - size of this chunk
            outputStream.write(shortToByteArray((short) AUDIO_FORMAT), 0, 2);		                            // 20 - what is the audio format? 1 for PCM = Pulse Code Modulation
            outputStream.write(shortToByteArray((short) MONO), 0, 2);	                                        // 22 - mono or stereo? 1 or 2?  (or 5 or ???)
            outputStream.write(intToByteArray((int) format.getSampleRate()), 0, 4);		                        // 24 - samples per second (numbers per second)
            outputStream.write(intToByteArray((int) format.getSampleRate() * BYTES_PER_SAMPLE), 0, 4);		    // 28 - bytes per second
            outputStream.write(shortToByteArray((short) BYTES_PER_SAMPLE), 0, 2);	                            // 32 - # of bytes in one sample, for all channels
            outputStream.write(shortToByteArray((short)format.getSampleSizeInBits()), 0, 2);	                // 34 - how many bits in a sample(number)?  usually 16 or 24
            outputStream.write("data".getBytes());
            outputStream.write(intToByteArray(size), 0, 4);

        } catch (IOException e1) {
            e1.printStackTrace();
        }

        return outputStream.toByteArray();
    }

    private  void playSoundDevice(final InputStream is) throws Exception {
        Info[] mixerInfo = AudioSystem.getMixerInfo();
        int deviceIndex = findDevice(deviceName);
        AudioInputStream inputStream = AudioSystem.getAudioInputStream(is);
        Info info = mixerInfo[deviceIndex];
        try {
            playClipAudio(inputStream, info);
        } catch (Throwable t) {
            System.out.println(t);
        }
    }

    private void playClipAudio(final AudioInputStream inputStream, Info info) throws LineUnavailableException, IOException, InterruptedException {
        Clip clip = AudioSystem.getClip(info);
        clip.open(inputStream);
        clip.start();
        waitPlayToEnd(clip);
        clip.close();
    }

    private void waitPlayToEnd(Clip clip) throws InterruptedException {
        while (!clip.isRunning()){//Wait for clip to start
            Thread.sleep(50);
        }
        while (clip.isRunning()){ //Wait clip to end
            Thread.sleep(100);
        }
    }

    private int findDevice(String deviceName) throws Exception {
        boolean found = false;
        int i = 0;
        int deviceIndex = 0;
        Info[] mixerInfo = AudioSystem.getMixerInfo();
        while (!found && i < mixerInfo.length){
            Info info = mixerInfo[i];
            if (info.getName().contains(deviceName)){
                found = true;
                deviceIndex  = i;
            }
            i++;
        }
        if(!found){
            throw new Exception("Device: "+ deviceName+" not found" );
        }
        return deviceIndex;
    }

    // returns a byte array of length 4
    private static byte[] intToByteArray(int i)
    {
        byte[] b = new byte[4];
        b[0] = (byte) (i & 0x00FF);
        b[1] = (byte) (i >> 8 & 0x000000FF);
        b[2] = (byte) (i >> 16 & 0x000000FF);
        b[3] = (byte) (i >> 24 & 0x000000FF);
        return b;
    }

    // convert a short to a byte array
    public static byte[] shortToByteArray(short data)
    {
        return new byte[]{(byte)(data & 0xff),(byte)(data >>> 8 & 0xff)};
    }

}
