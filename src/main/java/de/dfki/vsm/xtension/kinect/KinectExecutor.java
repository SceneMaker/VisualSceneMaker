/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.kinect;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author EmpaT
 */
public class KinectExecutor extends RunTimePlugin
{

    RunTimeProject mProject;
    private Runtime runtime;

    public KinectExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
        this.mProject = project;
    }

    @Override
    public void launch()
    {
        try
        {
            runtime = Runtime.getRuntime();
            runtime.exec("FaceDetection/SingleFace.exe", null,
                    new File("FaceDetection/"));
        } catch (IOException ex)
        {
            Logger.getLogger(KinectExecutor.class.getName()).log(Level.SEVERE, null, ex);
        }
        SmileReceiver ec = new SmileReceiver(mProject);
        RotationReceiver rc = new RotationReceiver(mProject);
        SurprisedReceiver sr = new SurprisedReceiver(mProject);
        AngryReceiver er = new AngryReceiver(mProject);
        ec.start();
        rc.start();
        //sr.start();
        //er.start();
    }

    @Override
    public void unload()
    {
    }

}
