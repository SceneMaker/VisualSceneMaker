/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.VideoPlayer;

/**
 *
 * @author stoma
 */
public class VideoPlayer implements Runnable {

    private VideoPlayerExecutor mVideoPLayerExecutor;
    
    public VideoPlayer(){
    }
    
    
    @Override
    public void run(){
        setUpSocket();
        listen();
    }

    void setVideoPlayerExecutor(VideoPlayerExecutor vpExec) {
        mVideoPLayerExecutor = vpExec;
    }

    private void setUpSocket() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void listen() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    void playVideo(String video) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
        
}
