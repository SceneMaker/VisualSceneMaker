package de.dfki.vsm.xtension.DialogManager;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.net.*;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;


import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutionException;




/**
 * @author Chirag Bhuvaneshwara
 */
public class DialogManagerListener extends Thread {

    private DatagramSocket udpSocket;
    private int port;
    private final DialogManagerExecutor executor;


    public DialogManagerListener(int port, DialogManagerExecutor executor) {
        this.port = port;
        this.executor = executor;
    }

    @Override
    public final void start() {
        try {
            this.udpSocket = new DatagramSocket(this.port);
        } catch (SocketException e) {
            throw new RuntimeException(e);
        }
        super.start();
    }

    @Override
    public final void run() {
        try {
            System.out.println("-- Running Server at " + InetAddress.getLocalHost() + "--");
        } catch (UnknownHostException e) {
            throw new RuntimeException(e);
        }
        String msg;

        while (true) {

            byte[] buf = new byte[256];
            DatagramPacket packet = new DatagramPacket(buf, buf.length);

            // blocks until a packet is received
            try {
                udpSocket.receive(packet);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            msg = new String(packet.getData()).trim();

            System.out.println(
                    "Message from " + packet.getAddress().getHostAddress() + ": " + msg);

            executor.set_transcript(msg);
            System.out.println(executor.get_transcript());

        }
    }
}