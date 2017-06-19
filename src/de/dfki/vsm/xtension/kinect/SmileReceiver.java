/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.kinect;

import de.dfki.stickman3D.animationlogic.Animation3D;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Beka Aptsiauri
 */
public class SmileReceiver extends Thread
{
    private static int smileCounter = 0;
    private DatagramSocket receiverSocket;
    private byte[] receiveData = new byte[1024];

    private RunTimeProject mProject;

    public static boolean go = true;
    String action = "";

    public SmileReceiver(RunTimeProject project)
    {
        this.mProject = project;
        try
        {
            receiverSocket = new DatagramSocket(1234);
        } catch (SocketException ex)
        {
            Logger.getLogger(SmileReceiver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void run()
    {
        while (go)
        {
            try
            {
                DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
                receiverSocket.receive(receivePacket);
                this.action = new String(receivePacket.getData()).trim();
                if (action.equalsIgnoreCase("smile"))
                {
                    smileCounter++;
                    if (!Animation3D.isSmileInAction)
                    {
                        if (smileCounter > 10)
                        {
                            mProject.setVariable("smileAction", "smile");
                            smileCounter = 0;
                        }
                    } else
                    {
                        smileCounter = 0;
                    }


                }
            } catch (Exception e)
            {
                e.printStackTrace();
            }
        }
    }
}
