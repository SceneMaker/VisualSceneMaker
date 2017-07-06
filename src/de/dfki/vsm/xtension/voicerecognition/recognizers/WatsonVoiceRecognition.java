package de.dfki.vsm.xtension.voicerecognition.recognizers;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.xtensions.observers.VoiceRecognitionEvent;
import main.speechrecognition.audioproviders.Microphone;
import main.speechrecognition.notification.WatsonSpeechObserver;
import main.speechrecognition.recognizers.watson.voiceactivated.WatsonVoiceActivated;

/**
 * Created by alvaro on 7/6/17.
 */
public class WatsonVoiceRecognition extends PluggableRecognizer {
    private WatsonVoiceActivated watsonVoiceActivated;
    public WatsonVoiceRecognition(PluginConfig mProject, RunTimeProject project) {
        super(mProject, project);


    }


    @Override
    protected void init() {
        mic = new Microphone();
        watsonVoiceActivated = new WatsonVoiceActivated(mic);
        watsonVoiceActivated.startListening();
        watsonVoiceActivated.register(new CustomWatsonResponse());
    }



    private class CustomWatsonResponse implements WatsonSpeechObserver {
        private EventDispatcher dispatcher = EventDispatcher.getInstance();
        @Override
        public void onSpeech(String spokenText) {
            VoiceRecognitionEvent responseEvent = new VoiceRecognitionEvent(spokenText);
            if(!spokenText.isEmpty()){
                dispatcher.convey(responseEvent);
                System.out.println(spokenText);
            }
        }
    }
}
