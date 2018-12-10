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
public class SurprisedReceiver extends Thread
{
    DatagramSocket receiverSocket;
    byte[] receiveData = new byte[1024];
    
    RunTimeProject mProject;
    
    public static boolean go = true;
    String action = "";
    
    public SurprisedReceiver(RunTimeProject project) 
    {
        this.mProject = project;
        try 
        {
            receiverSocket = new DatagramSocket(1236);
        } 
        catch (SocketException ex) 
        {
            Logger.getLogger(SurprisedReceiver.class.getName()).log(Level.SEVERE, null, ex);
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
		if(action.equalsIgnoreCase("surprised"))
		{
                    if(!Animation3D.isSurprisedInAction)
                    {
			mProject.setVariable("surprisedAction", "surprised");
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
