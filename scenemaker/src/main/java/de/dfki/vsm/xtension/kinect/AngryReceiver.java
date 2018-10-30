/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.kinect;

import de.dfki.stickman3D.animationlogic.Animation3D;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author EmpaT
 */
public class AngryReceiver extends Thread
{
    DatagramSocket receiverSocket;
    byte[] receiveData = new byte[1024];
    
    RunTimeProject mProject;
    
    public static boolean go = true;
    String action = "";
    
    public AngryReceiver(RunTimeProject project) 
    {
        this.mProject = project;
        try 
        {
            receiverSocket = new DatagramSocket(1237);
        } 
        catch (SocketException ex) 
        {
            Logger.getLogger(AngryReceiver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    @Override
    public void run()
    {
	while(go)
	{
            try
            {
                DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
                receiverSocket.receive(receivePacket);
		this.action = new String(receivePacket.getData()).trim();
		if(action.equalsIgnoreCase("angry"))
		{
                    if(!Animation3D.isAngryInAction)
                    {
			mProject.setVariable("angryAction", "angry");
                    }
                    
		}
            }
            catch(Exception e)
            {
                e.printStackTrace();
            }
	}
    }
}
