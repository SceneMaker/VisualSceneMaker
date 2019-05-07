package de.dfki.vsm.xtension.voicerecognition.recognizers;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import main.speechrecognition.notification.SpeechObserver;
import main.speechrecognition.recognizers.voiceactivated.SpeechRecognitionVoiceActivated;

/**
 * Created by alvaro on 7/6/17.
 */
public class OnlineVoiceRecognition extends PluggableRecognizer {
    private final String speechRecognitionAlgorithm;
    private SpeechRecognitionVoiceActivated watsonVoiceActivated;

    public OnlineVoiceRecognition(PluginConfig mProject, RunTimeProject project, String type) {
        super(mProject, project);
        this.speechRecognitionAlgorithm = type;


    }


    @Override
    protected void init() {
        watsonVoiceActivated = new SpeechRecognitionVoiceActivated(speechRecognitionAlgorithm);
        watsonVoiceActivated.startListening();
        watsonVoiceActivated.register(new SpeechRecognitionResponse());
    }


    private class SpeechRecognitionResponse implements SpeechObserver {
        private EventDispatcher dispatcher = EventDispatcher.getInstance();

        @Override
        public void onSpeech(String spokenText) {
            VoiceRecognitionEvent responseEvent = new VoiceRecognitionEvent(spokenText);
            if (!spokenText.isEmpty()) {
                dispatcher.convey(responseEvent);
                System.out.println(spokenText);
            }
        }
    }
}
