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
import java.text.NumberFormat;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Beka Aptsiauri
 */
public class RotationReceiver extends Thread
{
    DatagramSocket receiverSocket;
    byte[] receiveData = new byte[1024];

    RunTimeProject mProject;

    public static boolean go = true;
    String action = "";

    NumberFormat nf;

    public RotationReceiver(RunTimeProject project)
    {
        this.mProject = project;
        nf = NumberFormat.getInstance();
        try
        {
            receiverSocket = new DatagramSocket(1235);
        } catch (SocketException ex)
        {
            Logger.getLogger(RotationReceiver.class.getName()).log(Level.SEVERE, null, ex);
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

                if (!action.equalsIgnoreCase("smile"))
                {
                    String[] splitArray = action.split(",");
                    int x = (int) Float.parseFloat(splitArray[0]);
                    int y = (int) Float.parseFloat(splitArray[1]);
                    int z = (int) Float.parseFloat(splitArray[2]);

                    if (!Animation3D.isHeadTiltInAction)
                    {
                        if (x < -5)
                            mProject.setVariable("headTilt", "down");
                        if (z > 15)
                            mProject.setVariable("headTilt", "left");
                        if (z < -15)
                            mProject.setVariable("headTilt", "right");
                    }
                    //		mStickmanFX.mHeadFX.mXRotation = -x;
                    //		mStickmanFX.mHeadFX.mYRotation = y;
                    //		mStickmanFX.mHeadFX.mZRotation = -z;
                    //		mStickmanFX.mHeadFX.calculate(0);
                }
            } catch (Exception e)
            {
                e.printStackTrace();
            }
        }
    }
}
