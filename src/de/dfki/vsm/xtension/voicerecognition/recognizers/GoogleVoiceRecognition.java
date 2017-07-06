package de.dfki.vsm.xtension.voicerecognition.recognizers;

import com.darkprograms.speech.recognizer.GSpeechDuplex;
import com.darkprograms.speech.recognizer.GSpeechResponseListener;
import com.darkprograms.speech.recognizer.GoogleResponse;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import de.dfki.vsm.xtension.voicerecognition.microphone.GenericMicrophone;

import javax.sound.sampled.LineUnavailableException;
import java.io.IOException;

/**
 * Created by alvaro on 5/22/17.
 */
public class GoogleVoiceRecognition  extends PluggableRecognizer {

    private GSpeechDuplex duplex;

    public GoogleVoiceRecognition(PluginConfig mProject, RunTimeProject project) {
        super(mProject, project);

    }

    public void init() throws IOException, LineUnavailableException {
        mic = new GenericMicrophone();
        mic.startListening();
        duplex = new GSpeechDuplex("AIzaSyBOti4mM-6x9WDnZIjIeyEU21OpBXqWBgw");
        duplex.recognize(mic.getDataLine(), mic.getAudioFormat());
        duplex.setLanguage("en");
        registerListen();
    }

    private void registerListen() {
        duplex.addResponseListener(new CustomGoogleResponse());
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
