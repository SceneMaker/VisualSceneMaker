/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.VLCRemoteController;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;
import java.util.List;

/**
 *
 * @author Manuel Anglet, Fabrizio Nunnari
 */
public class VLCRemoteControllerExecutor extends ActivityExecutor{

    /** The TCP socket connecting to VLC. */
    Socket mSocket;

    /** The PrintStream to write strings in the TCP socket. */
    PrintStream mStreamOut;

    /** The root of the media files. */
    String mMediaPath = null;

    
    public VLCRemoteControllerExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        mLogger.message("video player initialised ");
    }
    
    @Override
    public String marker( long id){
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            List<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();
            
            if (name.equalsIgnoreCase("play")) {
                // retrieve the value of the first parameter
                ActionFeature first_arg = activity.getFeatures().element();
                String media_path = first_arg.getValNoQuotes() ;
                String param = first_arg.getKey() ;
                if (param.equals("url")) {
                    // Nothing to add.
                } else if (param.equals("file")) {
                    media_path = first_arg.getValNoQuotes();
                    if (mMediaPath != null) {
                        media_path = mMediaPath + File.separator + media_path;
                    }
                } else {
                    mLogger.failure("Argument '" + param + "' not recognized.");
                }

                mLogger.message("Playing media file '" + media_path + "'." );
                // Clear the playlist first
                mStreamOut.println("clear");
                // Adding to the playlist automatically start playing it
                mStreamOut.println("add " + media_path);

            } else if (name.equalsIgnoreCase("stop")) {
                mLogger.message("Stopping player.");
                mStreamOut.println("stop");
            }
        }
    }


    // Launch the plugin
    @Override
    public void launch() {
        mLogger.message("Launching VLC Remote Controller ...");

        //
        // Parse the mediarootdir property
        String media_root_dir = mConfig.getProperty("mediarootdir");
        if(media_root_dir != null) {
            mMediaPath = media_root_dir;
        } else {
            mMediaPath = new File(mProject.getProjectPath()).getParent() ;
        }

        //
        // Parse the port property
        int vlcport = Integer.parseInt(mConfig.getProperty("vlcport"));

        //
        // Connect to VLC
        try {
            mLogger.message("Connecting to VLC...");

            mSocket = new Socket("localhost",vlcport);
            mStreamOut = new PrintStream(mSocket.getOutputStream());

        } catch (IOException ex) {
            mLogger.failure("Connection to VLC failed: " + ex);
        }

    }


    // Unload the plugin
    @Override
    public void unload(){

        if(mStreamOut != null) {
            mStreamOut.close();
            mStreamOut = null;
        }

        if(mSocket != null) {
            try {
                mSocket.close();
            } catch (IOException e) {
                mLogger.warning("Exception closing VLC socket: " + e);
            }
            mSocket = null;
        }
    }
}
