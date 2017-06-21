package de.dfki.vsm.xtension.baxter.utils.messagehandlers.VAD;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;

/**
 * Created by alvaro on 22.09.16.
 */
public class WaitForSpeech implements MessageObserver {

    private RunTimeProject project;
    private String variableName;
    private boolean isSpeaking;
    private boolean isListening;
    private MessageObservable observable;

    public WaitForSpeech(final RunTimeProject p, final String v, MessageObservable observable){
        project = p;
        variableName = v;
        this.observable = observable;
        isSpeaking = false;
        isListening = false;
    }

    @Override
    public void update(final String message) {
        if(isListening){
            handleMessage(message);
        }else{
            isSpeaking = false;
            project.setVariable(variableName, isSpeaking());
        }
    }

    private void handleMessage(final String message) {
        if(message.contains("#DETECTEDSPEECH#end#")){
            isSpeaking = true;
            project.setVariable(variableName, isSpeaking());
        }
        if(message.contains("#NONDETECTEDSPEECH#end#")) {
            isSpeaking = false;
            project.setVariable(variableName, isSpeaking());
        }
    }

    public boolean isSpeaking() {
        return isSpeaking;
    }

    public boolean isListening() {
        return isListening;
    }

    public void startListening() {
        observable.register(this);
        isListening = true;
    }

    public void stopListening(){
        isListening = false;
        observable.unregister(this);
    }
}
