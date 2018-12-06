package de.dfki.vsm.xtension.videoplayer;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author stoma
 */
public class VideoPlayer implements Runnable {

    private VideoPlayerExecutor mVideoPLayerExecutor;
    Socket mSocket; 
    PrintStream out;
     private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    
    public VideoPlayer(){
    }
    
    @Override
    public void run(){
        setUpSocket();
        //listen();
    }

    void setVideoPlayerExecutor(VideoPlayerExecutor vpExec) {
        mVideoPLayerExecutor = vpExec;
    }

    private void setUpSocket() {
        try {
            mSocket = new Socket("localhost",9000);
            out = new PrintStream(mSocket.getOutputStream());
        } catch (IOException ex) {
            Logger.getLogger(VideoPlayer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void listen() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    void playVideo(String video) {
        out.println(video);
    }
        
}
