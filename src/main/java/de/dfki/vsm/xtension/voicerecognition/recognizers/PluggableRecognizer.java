package de.dfki.vsm.xtension.voicerecognition.recognizers;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.xtensions.observers.VoiceObersverNotifier;
import de.dfki.vsm.xtension.voicerecognition.plugins.factories.VRPluginFactory;
import main.speechrecognition.audioproviders.Audible;

import javax.sound.sampled.LineUnavailableException;
import java.io.IOException;

/**
 * Created by alvaro on 7/6/17.
 */
public abstract class PluggableRecognizer implements VoiceRecognizer {
    protected final VRPluginFactory pluginFactory;
    protected VoiceObersverNotifier notifier;
    protected boolean isRecording;
    protected Audible mic;


    public PluggableRecognizer(PluginConfig mProject, RunTimeProject project) {
        isRecording = false;
        notifier = new VoiceObersverNotifier();
        pluginFactory = new VRPluginFactory(mProject, project);
        startPlugins();
    }

    private void startPlugins() {
        pluginFactory.startPlugins();
    }

    public void stopRecording() {
        //mic.stopListening();
        isRecording = false;
        pluginFactory.stopPlugins();
    }

    public void startRecording() throws IOException, LineUnavailableException {
        init();
        isRecording = true;
    }

    protected abstract void init() throws IOException, LineUnavailableException;

    @Override
    public void run() {
        try {
            startRecording();
            isRecording = true;
        } catch (IOException | LineUnavailableException e) {
            e.printStackTrace();
        }
    }

}
