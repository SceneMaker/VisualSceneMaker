/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

import javax.sound.sampled.LineUnavailableException;
import java.io.IOException;

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
        vr = new GoogleVoiceRecognition(mProject);
        Thread voiceThread = new Thread(vr);
        voiceThread.start();
    }

    @Override
    public void unload() 
    {
        vr.stopRecording();
    } 
}
