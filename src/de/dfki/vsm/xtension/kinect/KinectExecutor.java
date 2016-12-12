/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.kinect;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 *
 * @author EmpaT
 */
public class KinectExecutor extends RunTimePlugin{

    RunTimeProject mProject;
    public KinectExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        this.mProject = project;
    }

    @Override
    public void launch() 
    {
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
