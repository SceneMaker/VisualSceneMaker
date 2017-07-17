/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicerecognition.recognizers.SphinxVoiceRecognition;
import de.dfki.vsm.xtension.voicerecognition.recognizers.VoiceRecognizer;
import de.dfki.vsm.xtension.voicerecognition.recognizers.OnlineVoiceRecognition;
import org.jetbrains.annotations.NotNull;

/**
 *
 * @author EmpaT
 */
public class VoiceRecognitionExecutor extends RunTimePlugin
{
   
    RunTimeProject mProject;
    VoiceRecognizer vr;
    public VoiceRecognitionExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        this.mProject = project;
    }

    @Override
    public void launch() 
    {
        vr =getVoiceRecognizer();
        Thread voiceThread = new Thread(vr);
        voiceThread.start();
    }

    private VoiceRecognizer getVoiceRecognizer(){
        if(mConfig.getProperty("recognizer") == null)
            return getDefaultVoiceRecognition();
        if(mConfig.getProperty("recognizer").equalsIgnoreCase("watson")){
            return new OnlineVoiceRecognition(mConfig, mProject, "watson");
        }else if(mConfig.getProperty("recognizer").equalsIgnoreCase("google")){
            return new OnlineVoiceRecognition(mConfig, mProject, "google");
        }
        return getDefaultVoiceRecognition();
    }

    @NotNull
    private VoiceRecognizer getDefaultVoiceRecognition() {
        return new SphinxVoiceRecognition(mProject);
    }

    @Override
    public void unload() 
    {
        vr.stopRecording();
    } 
}
