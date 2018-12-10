package de.dfki.vsm.xtension.videoplayer;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.LinkedList;

/**
 *
 * @author Manuel
 */
public class VideoPlayerExecutor extends ActivityExecutor{
    
    VideoPlayer mVideoPlayer = null;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    String path,fending;
    Thread mVideoPlayerThread = null;
    
    
    public VideoPlayerExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        mLogger.message("video player initialised ");
    }
    
    @Override
    public String marker( long id){
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {
        mLogger.message("checking activities for Video player");
        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();
            
            if (name.equalsIgnoreCase("playVideo")) {
                String video = activity.getFeatures().element().getVal();
                mLogger.message("found play video with path "+ path + "\\" +video+fending);
                mVideoPlayer.playVideo(path+"\\"+video+fending);
            }
        }
    }
    
    // Launch the plugin
    @Override
    public void launch(){
        mLogger.message("Lauching VideoPlayer ...");
        
        path = mConfig.getProperty("path");
        fending = mConfig.getProperty("fileending");
        mVideoPlayer = new VideoPlayer();
        mVideoPlayer.setVideoPlayerExecutor(this);
        mVideoPlayerThread = new Thread(mVideoPlayer);
        mVideoPlayerThread.setName("VideoPlayer Thread");
        mVideoPlayerThread.start();
    }

    // Unload the plugin
    @Override
    public void unload(){
        
    }
}
