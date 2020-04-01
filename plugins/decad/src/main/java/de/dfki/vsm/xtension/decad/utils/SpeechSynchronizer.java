package de.dfki.vsm.xtension.decad.utils;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;

import java.io.IOException;
import java.util.concurrent.Semaphore;

public class SpeechSynchronizer implements CommandResponseHandler {
    private final Semaphore speechSemaphore;
    private final DECADLongPoller speechPoller;

    public SpeechSynchronizer() {
        speechSemaphore = new Semaphore(1);
        speechPoller = new DECADLongPoller();
    }

    public void synchronizeSpeech(AbstractActivity activity) throws InterruptedException, IOException {
        if (activity instanceof SpeechActivity) {
            speechSemaphore.acquire();
            speechPoller.pollIsSpeaking(this);
        }
    }

    @Override
    public void handle() {
        speechSemaphore.release();
    }
}
