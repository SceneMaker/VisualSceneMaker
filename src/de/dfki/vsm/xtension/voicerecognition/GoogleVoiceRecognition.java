package de.dfki.vsm.xtension.voicerecognition;

import com.darkprograms.speech.microphone.Microphone;
import com.darkprograms.speech.recognizer.GSpeechDuplex;
import com.darkprograms.speech.recognizer.GSpeechResponseListener;
import com.darkprograms.speech.recognizer.GoogleResponse;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.xtensions.observers.Observer;
import de.dfki.vsm.util.xtensions.observers.VoiceObersverNotifier;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import de.dfki.vsm.xtension.voicerecognition.plugins.factories.VRPluginFactory;
import net.sourceforge.javaflacencoder.FLACFileWriter;

import javax.sound.sampled.LineUnavailableException;
import java.io.IOException;

/**
 * Created by alvaro on 5/22/17.
 */
public class GoogleVoiceRecognition  implements VoiceRecognizer{

    private GSpeechDuplex duplex;
    private Microphone mic;
    private boolean isRecording;
    private VoiceObersverNotifier notifier;
    private final VRPluginFactory pluginFactory;

    public GoogleVoiceRecognition(PluginConfig mProject) {
        notifier = new VoiceObersverNotifier();
        pluginFactory = new VRPluginFactory(mProject);
        startPlugins();
    }

    private void startPlugins() {
        pluginFactory.startPlugins();
    }

    public void init(){
        mic = new Microphone(FLACFileWriter.FLAC);
        duplex = new GSpeechDuplex("AIzaSyBOti4mM-6x9WDnZIjIeyEU21OpBXqWBgw");
        duplex.setLanguage("de");
    }


    private void registerListen() {
        duplex.addResponseListener(new CustomGoogleResponse());
    }

    public void startRecording() throws IOException, LineUnavailableException {
        init();
        registerListen();
        duplex.recognize(mic.getTargetDataLine(), mic.getAudioFormat());
    }

    @Override
    public void run() {
        try {
            startRecording();
            isRecording = true;
        } catch (IOException | LineUnavailableException e) {
            e.printStackTrace();
        }
    }

    public void stopRecording(){
        mic.close();
        isRecording = false;
        pluginFactory.stopPlugins();
    }

    private class CustomGoogleResponse implements GSpeechResponseListener{
        private EventDispatcher dispatcher = EventDispatcher.getInstance();

        public CustomGoogleResponse(){
        }

        @Override
        public void onResponse(GoogleResponse gr) {
            String output = "";
            output = gr.getResponse();
            VoiceRecognitionEvent responseEvent = new VoiceRecognitionEvent(output);
            dispatcher.convey(responseEvent);
            System.out.println(output);
        }

    }
}
